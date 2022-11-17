use std::collections::HashMap;
use std::rc::Rc;

use super::chunk::{Chunk, ChunkConstant};
use super::errors::{VmError, VmResult};
use super::gc::{Gc, HasSubHeap, Heap, Manages, SubHeap};
use super::native_function;
use super::native_function::NativeFn;
use super::opcode::{ConstantIndex, StructOpCode, UpvalueLocation};
use super::string_interner::{StringIntern, StringInterner};
use super::value::{
    LoxBoundMethod, LoxClass, LoxClosure, LoxInstance, PropertySearch, Upvalue, Value,
};

const GC_RESET_RATIO: f64 = 2.0;
const MAX_CALLFRAMES: usize = 64;

struct CallFrame {
    ip: usize,
    base_ptr: usize,
    name: StringIntern,
    chunk: Rc<Chunk>,
    upvalues: Rc<[Gc<Upvalue>]>,
}

#[derive(Debug)]
struct Stack<T> {
    stack: Vec<T>,
}

pub struct VmHeap {
    closure_heap: SubHeap<LoxClosure>,
    class_heap: SubHeap<LoxClass>,
    instance_heap: SubHeap<LoxInstance>,
    bound_method_heap: SubHeap<LoxBoundMethod>,
    upvalue_heap: SubHeap<Upvalue>,
    next_gc_threshold: usize,
}

pub struct VM<S> {
    call_stack: Vec<CallFrame>,
    stack: Stack<Value>,
    open_upvalues: Vec<Gc<Upvalue>>,
    string_table: StringInterner,
    globals: HashMap<StringIntern, Value>,
    heap: VmHeap,
    output_sink: S,
}

impl<T> Stack<T> {
    fn new() -> Self {
        Stack { stack: vec![] }
    }

    fn len(&self) -> usize {
        self.stack.len()
    }

    fn depth(&self, depth: usize) -> VmResult<usize> {
        if depth < self.len() {
            Ok(self.len() - 1 - depth)
        } else {
            Err(VmError::InvalidStackDepth(depth))
        }
    }

    fn get(&self, index: usize) -> VmResult<&T> {
        self.stack
            .get(index)
            .ok_or(VmError::InvalidStackIndex(index))
    }

    fn set(&mut self, index: usize, item: T) -> VmResult<()> {
        match self.stack.get_mut(index) {
            Some(slot) => {
                *slot = item;
                Ok(())
            }
            None => Err(VmError::InvalidStackIndex(index)),
        }
    }

    fn peek(&self, depth: usize) -> VmResult<&T> {
        let idx = self.depth(depth)?;
        self.get(idx)
    }

    fn set_back(&mut self, depth: usize, item: T) -> VmResult<()> {
        let idx = self.depth(depth)?;
        self.set(idx, item)
    }

    fn peek_n(&self, depth: usize) -> VmResult<&[T]> {
        let start_idx = self.depth(depth)? + 1;
        Ok(self.stack.get(start_idx..).unwrap())
    }

    fn push(&mut self, item: T) {
        self.stack.push(item)
    }

    fn pop(&mut self) -> VmResult<T> {
        self.stack.pop().ok_or(VmError::StackUnderflow)
    }

    fn pop_n(&mut self, depth: usize) -> VmResult<()> {
        let idx = self.depth(depth)? + 1;
        self.truncate(idx);
        Ok(())
    }

    fn truncate(&mut self, index: usize) {
        self.stack.truncate(index)
    }

    fn clear(&mut self) {
        self.stack.clear()
    }

    fn iter(&self) -> std::slice::Iter<'_, T> {
        self.stack.iter()
    }
}

impl VmHeap {
    fn new() -> Self {
        VmHeap {
            closure_heap: SubHeap::new(),
            class_heap: SubHeap::new(),
            instance_heap: SubHeap::new(),
            bound_method_heap: SubHeap::new(),
            upvalue_heap: SubHeap::new(),
            next_gc_threshold: 1024 * 1024,
        }
    }

    fn size(&self) -> usize {
        self.closure_heap.size()
            + self.class_heap.size()
            + self.instance_heap.size()
            + self.bound_method_heap.size()
            + self.upvalue_heap.size()
    }

    fn sweep(&mut self) {
        self.closure_heap.sweep();
        self.class_heap.sweep();
        self.instance_heap.sweep();
        self.bound_method_heap.sweep();
        self.next_gc_threshold = (self.size() as f64 * GC_RESET_RATIO) as usize
    }

    fn should_run_garbage_collection(&self) -> bool {
        self.size() > self.next_gc_threshold
    }

    fn mark_value(&self, value: &Value) {
        match value {
            Value::Number(_) | Value::Boolean(_) | Value::Nil | Value::String(_) => {}
            Value::Closure(ptr) => self.mark(*ptr),
            Value::NativeFn(_) => {}
            Value::Class(ptr) => self.mark(*ptr),
            Value::Instance(ptr) => self.mark(*ptr),
            Value::BoundMethod(ptr) => self.mark(*ptr),
        }
    }

    fn read_property(&self, instance_ptr: Gc<LoxInstance>, name: &StringIntern) -> PropertySearch {
        let instance = self.get(instance_ptr);

        // Look up fields first, then methods
        if let Some(value) = instance.fields.get(name) {
            return PropertySearch::Field(value.clone());
        }

        if let Some(method_ptr) = self.get(instance.class).methods.get(name) {
            return PropertySearch::Method(*method_ptr);
        }

        PropertySearch::Missing
    }

    pub fn value_string(&self, value: &Value) -> String {
        match value {
            Value::Number(n) => n.to_string(),
            Value::Boolean(true) => String::from("true"),
            Value::Boolean(false) => String::from("false"),
            Value::Nil => String::from("nil"),
            Value::String(s) => String::from(s.as_ref()),
            Value::Closure(closure) => format!("<fn {}>", self.get(*closure).name),
            Value::NativeFn(_) => String::from("<native fn>"),
            Value::Class(class) => format!("{}", self.get(*class).name),
            Value::Instance(instance) => {
                let instance = self.get(*instance);
                format!("{} instance", self.get(instance.class).name)
            }
            Value::BoundMethod(method) => {
                let method = self.get(*method);
                format!("<fn {}>", self.get(method.closure).name)
            }
        }
    }
}

impl Heap for VmHeap {
    fn sweep(&mut self) {
        self.closure_heap.sweep();
        self.class_heap.sweep();
        self.instance_heap.sweep();
        self.bound_method_heap.sweep();
        self.upvalue_heap.sweep();
        self.next_gc_threshold = (self.size() as f64 * GC_RESET_RATIO) as usize
    }
}

impl HasSubHeap<LoxClosure> for VmHeap {
    fn get_subheap(&self) -> &SubHeap<LoxClosure> {
        &self.closure_heap
    }

    fn get_subheap_mut(&mut self) -> &mut SubHeap<LoxClosure> {
        &mut self.closure_heap
    }

    fn trace(&self, content: &LoxClosure) {
        for upvalue in content.upvalues.iter() {
            self.mark(*upvalue);
        }
    }
}

impl HasSubHeap<LoxClass> for VmHeap {
    fn get_subheap(&self) -> &SubHeap<LoxClass> {
        &self.class_heap
    }

    fn get_subheap_mut(&mut self) -> &mut SubHeap<LoxClass> {
        &mut self.class_heap
    }

    fn trace(&self, content: &LoxClass) {
        for m in content.methods.values().copied() {
            self.mark(m)
        }
    }
}

impl HasSubHeap<LoxInstance> for VmHeap {
    fn get_subheap(&self) -> &SubHeap<LoxInstance> {
        &self.instance_heap
    }

    fn get_subheap_mut(&mut self) -> &mut SubHeap<LoxInstance> {
        &mut self.instance_heap
    }

    fn trace(&self, content: &LoxInstance) {
        self.mark(content.class);
        for value in content.fields.values() {
            self.mark_value(value);
        }
    }
}

impl HasSubHeap<LoxBoundMethod> for VmHeap {
    fn get_subheap(&self) -> &SubHeap<LoxBoundMethod> {
        &self.bound_method_heap
    }

    fn get_subheap_mut(&mut self) -> &mut SubHeap<LoxBoundMethod> {
        &mut self.bound_method_heap
    }

    fn trace(&self, content: &LoxBoundMethod) {
        self.mark(content.receiver);
        self.mark(content.closure);
    }
}

impl HasSubHeap<Upvalue> for VmHeap {
    fn get_subheap(&self) -> &SubHeap<Upvalue> {
        &self.upvalue_heap
    }

    fn get_subheap_mut(&mut self) -> &mut SubHeap<Upvalue> {
        &mut self.upvalue_heap
    }

    fn trace(&self, content: &Upvalue) {
        match content {
            Upvalue::Open(_) => {}
            Upvalue::Closed(value) => self.mark_value(value),
        }
    }
}

impl VM<std::io::Stdout> {
    pub fn new() -> Self {
        VM::new_with_output(std::io::stdout())
    }
}

impl<S: std::io::Write> VM<S> {
    pub fn new_with_output(output_sink: S) -> Self {
        let mut vm = VM {
            call_stack: vec![],
            stack: Stack::new(),
            open_upvalues: vec![],
            string_table: StringInterner::new(),
            globals: HashMap::new(),
            heap: VmHeap::new(),
            output_sink,
        };

        for (name, arity, func) in native_function::get_native_fns().iter().copied() {
            let name = vm.get_string_intern(name);
            let native_fn = NativeFn::new(&name, arity, func);
            vm.globals.insert(name, Value::NativeFn(native_fn));
        }

        vm
    }

    pub fn interpret(&mut self, main_chunk: Chunk) -> VmResult<()> {
        self.call_stack.clear();
        self.stack.clear();

        let main_name = self.get_string_intern("<main>");
        let main_fn = self.heap.manage(LoxClosure {
            name: main_name,
            arity: 0,
            chunk: Rc::new(main_chunk),
            upvalues: Rc::from([]),
        });
        self.stack.push(Value::Closure(main_fn));

        self.call(0)?;

        let result = self.run();
        if result.is_err() {
            for frame in self.call_stack.iter().rev() {
                let line_no = frame.chunk.get_line(frame.ip - 1);
                println!("[line {}] in {}", line_no, frame.name);
            }
        }

        result
    }

    fn run(&mut self) -> VmResult<()> {
        loop {
            #[cfg(feature = "trace-execution")]
            {
                let frame = self.frame_mut();
                let ip = frame.ip;
                let base_ptr = frame.base_ptr;

                println!("STACK     {:?}", self.stack);
                println!(
                    "ip = {}, bp = {} ({:?})",
                    ip,
                    base_ptr,
                    self.stack.get(base_ptr)
                );
                self.frame_mut().chunk.disassemble_at_offset(ip);
                println!();
            }

            if self.heap.should_run_garbage_collection() {
                self.run_garbage_collection();
            }

            // Run instruction
            match self.try_read_op()? {
                StructOpCode::True => self.stack.push(Value::Boolean(true)),
                StructOpCode::False => self.stack.push(Value::Boolean(false)),
                StructOpCode::Nil => self.stack.push(Value::Nil),
                StructOpCode::Constant(index) => {
                    let value = match self.read_constant(index) {
                        ChunkConstant::Number(n) => Value::Number(n),
                        ChunkConstant::String(s) => Value::String(s),
                        c => return Err(VmError::CannotParseConstant(c)),
                    };
                    self.stack.push(value);
                }
                StructOpCode::Add => self.run_add()?,
                StructOpCode::Subtract => self.numerical_binop(|lhs, rhs| lhs - rhs)?,
                StructOpCode::Multiply => self.numerical_binop(|lhs, rhs| lhs * rhs)?,
                StructOpCode::Divide => {
                    self.numerical_binop(|lhs, rhs| lhs / rhs)?;
                }
                StructOpCode::Negate => match self.stack.peek(0)? {
                    Value::Number(n) => {
                        let n = n.to_owned();
                        self.stack.pop()?;
                        self.stack.push(Value::Number(-n));
                    }
                    _ => return Err(VmError::NonNumericOperandPrefix),
                },
                StructOpCode::Not => {
                    let value = self.stack.pop()?;
                    self.stack.push(Value::Boolean(!value.is_truthy()));
                }
                StructOpCode::Equal => {
                    let rhs = self.stack.pop()?;
                    let lhs = self.stack.pop()?;
                    self.stack.push(Value::Boolean(lhs == rhs));
                }
                StructOpCode::GreaterThan => self.logical_binop(|lhs, rhs| lhs > rhs)?,
                StructOpCode::LessThan => self.logical_binop(|lhs, rhs| lhs < rhs)?,
                StructOpCode::DefineGlobal(index) => {
                    let name = self.read_string(index);
                    let value = self.stack.pop()?;
                    self.globals.insert(name, value);
                }
                StructOpCode::GetGlobal(index) => {
                    let name = self.read_string(index);
                    let value = match self.globals.get(&name) {
                        Some(value) => value.clone(),
                        None => {
                            let name: String = (*name).to_owned();
                            return Err(VmError::UndefinedGlobal(name));
                        }
                    };
                    self.stack.push(value);
                }
                StructOpCode::SetGlobal(index) => {
                    let name = self.read_string(index);
                    if !self.globals.contains_key(&name) {
                        let name: String = (*name).to_owned();
                        return Err(VmError::UndefinedGlobal(name));
                    }
                    // Do not pop value, since assignment is an expression and can
                    // be nested inside another expression that needs the value.
                    let value = self.stack.peek(0)?.clone();
                    self.globals.insert(name, value);
                }
                StructOpCode::GetLocal(index) => {
                    let index = usize::from(index);
                    let base_ptr = self.frame().base_ptr;
                    let value = self.stack.get(base_ptr + index)?.clone();
                    self.stack.push(value);
                }
                StructOpCode::SetLocal(index) => {
                    let index = usize::from(index);
                    let base_ptr = self.frame().base_ptr;
                    let value = self.stack.peek(0)?.clone();
                    self.stack.set(base_ptr + index, value)?;
                }
                StructOpCode::Jump(offset) => {
                    let offset = usize::from(offset);
                    self.frame_mut().ip += offset;
                }
                StructOpCode::JumpIfFalse(offset) => {
                    let offset = usize::from(offset);
                    if !self.stack.peek(0)?.is_truthy() {
                        self.frame_mut().ip += offset;
                    }
                }
                StructOpCode::Loop(offset) => {
                    let offset = usize::from(offset);
                    self.frame_mut().ip -= offset;
                }
                StructOpCode::MakeClosure(index) => {
                    let (name, arity, chunk, upvalue_count) = match self.read_constant(index) {
                        ChunkConstant::FnTemplate {
                            name,
                            arity,
                            chunk,
                            upvalue_count,
                        } => (name, arity, chunk, upvalue_count),
                        _ => return Err(VmError::NotACallable),
                    };

                    let mut upvalues = Vec::with_capacity(upvalue_count);
                    for _i in 0..upvalue_count {
                        let upvalue_location = self.try_read_upvalue()?;
                        let upvalue_obj = match upvalue_location {
                            UpvalueLocation::Immediate(local_index) => {
                                let stack_index = self.frame().base_ptr + local_index as usize;
                                self.make_open_value(stack_index)
                            }
                            UpvalueLocation::Recursive(upvalue_index) => {
                                self.frame().upvalues[upvalue_index as usize]
                            }
                        };
                        upvalues.push(upvalue_obj);
                    }

                    let closure = self.heap.manage(LoxClosure {
                        name,
                        arity,
                        chunk: chunk.clone(),
                        upvalues: Rc::from(upvalues),
                    });
                    self.stack.push(Value::Closure(closure));
                }
                StructOpCode::GetUpvalue(index) => {
                    let index = usize::from(index);
                    let value = match self.heap.get(self.frame().upvalues[index]) {
                        Upvalue::Closed(v) => v.clone(),
                        Upvalue::Open(index) => self.stack.get(*index)?.clone(),
                    };
                    self.stack.push(value);
                }
                StructOpCode::SetUpvalue(index) => {
                    let index = usize::from(index);
                    let value = self.stack.peek(0)?.clone();

                    match self.heap.get_mut(self.frame().upvalues[index]) {
                        Upvalue::Closed(v) => *v = value,
                        Upvalue::Open(i) => self.stack.set(*i, value)?,
                    };
                }
                StructOpCode::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1)?;
                    self.stack.pop()?;
                }
                StructOpCode::MakeClass(index) => {
                    let name = self.read_string(index);
                    let klass = self.heap.manage(LoxClass {
                        name,
                        methods: HashMap::new(),
                    });
                    self.stack.push(Value::Class(klass));
                }
                StructOpCode::GetProperty(index) => {
                    let name = self.read_string(index);
                    let instance_ptr = self
                        .stack
                        .peek(0)?
                        .to_instance()
                        .ok_or(VmError::IncorrectPropertyAccess)?;

                    let value = match self.heap.read_property(instance_ptr, &name) {
                        PropertySearch::Field(value) => value,
                        PropertySearch::Method(method) => {
                            let bound_method = self.heap.manage(LoxBoundMethod {
                                receiver: instance_ptr,
                                closure: method,
                            });
                            Value::BoundMethod(bound_method)
                        }
                        PropertySearch::Missing => {
                            return Err(VmError::UnknownProperty(name.to_string()))
                        }
                    };

                    self.stack.pop()?;
                    self.stack.push(value);
                }
                StructOpCode::SetProperty(index) => {
                    let name = self.read_string(index);
                    let value = self.stack.peek(0)?.clone();
                    let instance_ptr = self
                        .stack
                        .peek(1)?
                        .to_instance()
                        .ok_or(VmError::IncorrectFieldAccess)?;
                    self.heap
                        .get_mut(instance_ptr)
                        .fields
                        .insert(name, value.clone());
                    self.stack.pop()?;
                    self.stack.pop()?;
                    self.stack.push(value);
                }
                StructOpCode::MakeMethod(index) => {
                    let method_name = self.read_string(index);
                    let method_ptr = self
                        .stack
                        .peek(0)?
                        .to_closure()
                        .ok_or(VmError::NotAClosure)?;

                    let class_ptr = self.stack.peek(1)?.to_class().ok_or(VmError::NotAClass)?;
                    self.heap
                        .get_mut(class_ptr)
                        .methods
                        .insert(method_name, method_ptr);

                    self.stack.pop()?;
                }
                StructOpCode::Invoke(index, num_args) => {
                    let method_name = self.read_string(index);
                    let num_args = usize::from(num_args);

                    let receiver_ptr = self
                        .stack
                        .peek(num_args)?
                        .to_instance()
                        .ok_or(VmError::NotAnInstance)?;

                    match self.heap.read_property(receiver_ptr, &method_name) {
                        PropertySearch::Field(value) => {
                            self.stack.set_back(num_args, value)?;
                            self.call(num_args)?;
                        }
                        PropertySearch::Method(method) => self.call_closure(method, num_args)?,
                        PropertySearch::Missing => {
                            return Err(VmError::UnknownProperty(method_name.to_string()))
                        }
                    };
                }
                StructOpCode::Inherit => {
                    let superclass_ptr = self
                        .stack
                        .peek(1)?
                        .to_class()
                        .ok_or(VmError::IncorrectSuperclass)?;
                    let class_ptr = self.stack.peek(0)?.to_class().ok_or(VmError::NotAClass)?;

                    let methods = self.heap.get(superclass_ptr).methods.clone();
                    self.heap.get_mut(class_ptr).methods = methods;
                }
                StructOpCode::GetSuper(index) => {
                    let method_name = self.read_string(index);
                    let class_ptr = self.stack.peek(0)?.to_class().ok_or(VmError::NotAClass)?;
                    let instance_ptr = self
                        .stack
                        .peek(1)?
                        .to_instance()
                        .ok_or(VmError::NotAnInstance)?;

                    let method_ptr = match self.heap.get(class_ptr).methods.get(&method_name) {
                        Some(ptr) => *ptr,
                        None => return Err(VmError::UnknownProperty(method_name.to_string())),
                    };

                    let bound_method = self.heap.manage(LoxBoundMethod {
                        receiver: instance_ptr,
                        closure: method_ptr,
                    });

                    self.stack.pop()?;
                    self.stack.pop()?;
                    self.stack.push(Value::BoundMethod(bound_method));
                }
                StructOpCode::InvokeSuper(index, num_args) => {
                    let method_name = self.read_string(index);
                    let num_args = usize::from(num_args);
                    let superclass_ptr =
                        self.stack.peek(0)?.to_class().ok_or(VmError::NotAClass)?;
                    self.stack.pop()?;

                    match self
                        .heap
                        .get(superclass_ptr)
                        .methods
                        .get(&method_name)
                        .cloned()
                    {
                        Some(method) => self.call_closure(method, num_args)?,
                        None => return Err(VmError::UnknownProperty(method_name.to_string())),
                    };
                }
                StructOpCode::Call(num_args) => {
                    let num_args = usize::from(num_args);
                    self.call(num_args)?;
                }
                StructOpCode::Return => {
                    let result = self.pop_frame()?;

                    if self.call_stack.is_empty() {
                        return Ok(());
                    } else {
                        self.stack.push(result);
                    }
                }
                StructOpCode::Print => {
                    let value = self.stack.pop()?;
                    let output = self.heap.value_string(&value);
                    writeln!(&mut self.output_sink, "{}", output).expect("Unable to write output.");
                }
                StructOpCode::Pop => {
                    self.stack.pop()?;
                }
            }
        }
    }

    pub fn call(&mut self, num_args: usize) -> VmResult<()> {
        let callee = self.stack.peek(num_args)?.clone();

        match callee {
            Value::Closure(ptr) => self.call_closure(ptr, num_args),
            Value::NativeFn(native_fn) => {
                if native_fn.data.arity != num_args {
                    return Err(VmError::WrongArity(native_fn.data.arity, num_args));
                }

                let arg_slice = self.stack.peek_n(num_args)?;

                match (native_fn.data.func)(arg_slice) {
                    Ok(return_value) => {
                        self.stack.pop_n(num_args + 1)?;
                        self.stack.push(return_value);
                        Ok(())
                    }
                    Err(s) => Err(VmError::NativeFnError(s)),
                }
            }
            Value::Class(ptr) => {
                let instance = self.heap.manage(LoxInstance {
                    class: ptr,
                    fields: HashMap::new(),
                });
                self.stack.set_back(num_args, Value::Instance(instance))?;

                if let Some(init) = self.heap.get(ptr).methods.get("init").cloned() {
                    self.call_closure(init, num_args)?
                } else if num_args > 0 {
                    return Err(VmError::WrongArity(0, num_args));
                }

                Ok(())
            }
            Value::BoundMethod(ptr) => {
                let bound_method = self.heap.get(ptr);
                let receiver = Value::Instance(bound_method.receiver);
                let closure = bound_method.closure;
                self.stack.set_back(num_args, receiver)?;
                self.call_closure(closure, num_args)
            }
            _ => Err(VmError::NotACallable),
        }
    }

    fn call_closure(&mut self, closure_ptr: Gc<LoxClosure>, num_args: usize) -> VmResult<()> {
        let closure = self.heap.get(closure_ptr);
        if closure.arity != num_args {
            return Err(VmError::WrongArity(closure.arity, num_args));
        }
        let name = closure.name.clone();
        let chunk = closure.chunk.clone();
        let upvalues = closure.upvalues.clone();

        self.push_new_frame(num_args, name, chunk, upvalues)?;

        Ok(())
    }

    fn frame(&self) -> &CallFrame {
        match self.call_stack.last() {
            Some(frame) => frame,
            None => panic!("Empty call stack."),
        }
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        match self.call_stack.last_mut() {
            Some(frame) => frame,
            None => panic!("Empty call stack."),
        }
    }

    fn push_new_frame(
        &mut self,
        num_args: usize,
        name: StringIntern,
        chunk: Rc<Chunk>,
        upvalues: Rc<[Gc<Upvalue>]>,
    ) -> VmResult<()> {
        if self.call_stack.len() == MAX_CALLFRAMES {
            return Err(VmError::StackOverflow);
        }

        let new_frame = CallFrame {
            ip: 0,
            base_ptr: self.stack.len() - (num_args + 1),
            name,
            chunk,
            upvalues,
        };
        self.call_stack.push(new_frame);

        Ok(())
    }

    fn pop_frame(&mut self) -> VmResult<Value> {
        let result = self.stack.pop()?;
        let frame = self.call_stack.pop().expect("Empty call stack.");
        self.close_upvalues(frame.base_ptr)?;
        self.stack.truncate(frame.base_ptr);
        Ok(result)
    }

    fn try_read_op(&mut self) -> VmResult<StructOpCode> {
        let frame = self.frame_mut();
        let result = frame.chunk.try_read_op(frame.ip);

        match result {
            Ok((op, next_ip)) => {
                frame.ip = next_ip;
                Ok(op)
            }
            Err(e) => Err(VmError::from(e)),
        }
    }

    fn read_constant(&mut self, index: ConstantIndex) -> ChunkConstant {
        self.frame().chunk.read_constant(index)
    }

    fn read_string(&mut self, index: ConstantIndex) -> StringIntern {
        match self.read_constant(index) {
            ChunkConstant::String(s) => s,
            _ => panic!("Global table contains non-string."),
        }
    }

    fn try_read_upvalue(&mut self) -> VmResult<UpvalueLocation> {
        let frame = self.frame_mut();
        let result = frame.chunk.try_read_upvalue(frame.ip);
        match result {
            Ok(location) => {
                frame.ip += 2;
                Ok(location)
            }
            Err(e) => Err(VmError::from(e)),
        }
    }

    fn make_open_value(&mut self, stack_index: usize) -> Gc<Upvalue> {
        // Sibling closures need to share an upvalue, so we first
        // check if already exists and clone it. Otherwise, create a new one.

        fn index_match(upvalue: &Upvalue, stack_idx: usize) -> bool {
            match upvalue {
                Upvalue::Open(idx) => stack_idx == *idx,
                Upvalue::Closed(_) => panic!("Open upvalues list contains closed upvalue!"),
            }
        }

        match self
            .open_upvalues
            .iter()
            .find(|uv| index_match(self.heap.get(**uv), stack_index))
        {
            Some(upvalue) => *upvalue,
            None => {
                let upvalue = self.heap.manage(Upvalue::Open(stack_index));
                self.open_upvalues.push(upvalue);
                upvalue
            }
        }
    }

    fn close_upvalues(&mut self, stack_index: usize) -> VmResult<()> {
        for upvalue_ptr in self.open_upvalues.iter() {
            let upvalue = self.heap.get_mut(*upvalue_ptr);
            match upvalue {
                Upvalue::Open(idx) => {
                    if *idx >= stack_index {
                        let value = self.stack.get(*idx)?.clone();
                        *upvalue = Upvalue::Closed(value);
                    }
                }
                Upvalue::Closed(_) => panic!("open_upvalues contains closed upvalue!"),
            }
        }

        let heap_ref = &self.heap;
        self.open_upvalues
            .retain(|u| matches!(heap_ref.get(*u), Upvalue::Open(_)));

        Ok(())
    }

    pub fn get_string_intern(&mut self, s: &str) -> StringIntern {
        self.string_table.get_string_intern(s)
    }

    pub fn borrow_string_table(&mut self) -> &mut StringInterner {
        &mut self.string_table
    }

    fn run_garbage_collection(&mut self) {
        // Values on stack are reachable.
        for value in self.stack.iter() {
            self.heap.mark_value(value);
        }

        // Globals are reachable.
        for value in self.globals.values() {
            self.heap.mark_value(value);
        }

        // Call frames and open upvalues contain reachable objects, but only
        // through objects also reachable through the corresponding closure.
        // Since closure is on the stack, all of those are already reachable.
        self.heap.sweep();
        self.string_table.sweep();
    }

    fn run_add(&mut self) -> VmResult<()> {
        let lhs = self.stack.peek(1)?;
        let rhs = self.stack.peek(0)?;

        let result = match (lhs, rhs) {
            (Value::Number(x), Value::Number(y)) => Value::Number(x + y),
            (Value::String(x), Value::String(y)) => {
                let dynamic_string = x.as_ref().to_owned() + y;
                let string_intern = self.string_table.get_string_intern(dynamic_string);
                Value::String(string_intern)
            }
            _ => return Err(VmError::IncorrectOperandTypeAdd),
        };

        self.stack.pop()?;
        self.stack.pop()?;
        self.stack.push(result);

        Ok(())
    }

    fn numerical_binop<F>(&mut self, func: F) -> VmResult<()>
    where
        F: Fn(f64, f64) -> f64,
    {
        self.binop(|a, b| Value::Number(func(a, b)))
    }

    fn logical_binop<F>(&mut self, func: F) -> VmResult<()>
    where
        F: Fn(f64, f64) -> bool,
    {
        self.binop(|a, b| Value::Boolean(func(a, b)))
    }

    fn binop<F>(&mut self, func: F) -> VmResult<()>
    where
        F: Fn(f64, f64) -> Value,
    {
        let rhs = self.stack.peek(0)?;
        let lhs = self.stack.peek(1)?;
        match (lhs, rhs) {
            (Value::Number(l), Value::Number(r)) => {
                let outcome = func(*l, *r);
                self.stack.pop()?;
                self.stack.pop()?;
                self.stack.push(outcome);
                Ok(())
            }
            (_, _) => Err(VmError::NonNumericOperandInfix),
        }
    }
}
