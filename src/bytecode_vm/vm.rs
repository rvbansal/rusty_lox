use super::chunk::{Chunk, ChunkConstant};
use super::compiler::{INIT_STR, UPVALUE_IMMEDIATE_VALUE, UPVALUE_RECURSIVE_VALUE};
use super::errors::{VmError, VmResult};
use super::gc::{GcHeap, GcPtr};
use super::native_function;
use super::native_function::NativeFn;
use super::opcode::OpCode;
use super::string_interner::{StringIntern, StringInterner};
use super::value::{
    LoxBoundMethod, LoxClass, LoxClosure, LoxInstance, PropertySearch, UpvalueData, UpvaluePtr,
    Value,
};

use std::collections::HashMap;
use std::rc::Rc;

const GC_CYCLES_PERIOD: u32 = 1000;

struct CallFrame {
    ip: usize,
    base_ptr: usize,
    name: StringIntern,
    chunk: Rc<Chunk>,
    upvalues: Rc<Vec<UpvaluePtr>>,
}

struct Stack<T> {
    stack: Vec<T>,
}

struct Heap {
    closure_heap: GcHeap<LoxClosure>,
    class_heap: GcHeap<LoxClass>,
    instance_heap: GcHeap<LoxInstance>,
    bound_method_heap: GcHeap<LoxBoundMethod>,
}

pub struct VM {
    call_stack: Vec<CallFrame>,
    stack: Stack<Value>,
    open_upvalues: Vec<UpvaluePtr>,
    string_table: StringInterner,
    globals: HashMap<StringIntern, Value>,
    heap: Heap,
}

impl<T> Stack<T> {
    fn new() -> Self {
        Stack { stack: vec![] }
    }

    fn len(&self) -> usize {
        self.stack.len()
    }

    fn get(&self, index: usize) -> VmResult<&T> {
        self.stack.get(index).ok_or(VmError::InvalidStackIndex)
    }

    fn set(&mut self, index: usize, item: T) -> VmResult<()> {
        match self.stack.get_mut(index) {
            Some(slot) => {
                *slot = item;
                Ok(())
            }
            None => Err(VmError::InvalidStackIndex),
        }
    }

    fn peek(&self, depth: usize) -> VmResult<&T> {
        self.get(self.len() - 1 - depth)
    }

    fn set_back(&mut self, depth: usize, item: T) -> VmResult<()> {
        self.set(self.len() - 1 - depth, item)
    }

    fn peek_n(&self, depth: usize) -> VmResult<&[T]> {
        let start_index = self.len() - depth;
        self.stack
            .get(start_index..)
            .ok_or(VmError::InvalidStackIndex)
    }

    fn push(&mut self, item: T) {
        self.stack.push(item)
    }

    fn pop(&mut self) -> VmResult<T> {
        self.stack.pop().ok_or(VmError::StackEmpty)
    }

    fn pop_n(&mut self, depth: usize) -> VmResult<()> {
        if depth > self.len() {
            Err(VmError::InvalidStackIndex)
        } else {
            let index = self.len() - depth;
            self.truncate(index);
            Ok(())
        }
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

impl Heap {
    fn new() -> Self {
        Heap {
            closure_heap: GcHeap::new(),
            class_heap: GcHeap::new(),
            instance_heap: GcHeap::new(),
            bound_method_heap: GcHeap::new(),
        }
    }

    fn insert_closure(
        &mut self,
        name: StringIntern,
        arity: usize,
        chunk: Rc<Chunk>,
        upvalues: Rc<Vec<UpvaluePtr>>,
    ) -> Value {
        let closure_obj = LoxClosure {
            name,
            arity,
            chunk,
            upvalues,
        };
        let ptr = self.closure_heap.insert(closure_obj);
        Value::Closure(ptr)
    }

    fn insert_class(
        &mut self,
        name: StringIntern,
        methods: HashMap<StringIntern, GcPtr<LoxClosure>>,
    ) -> Value {
        let class_obj = LoxClass { name, methods };
        let ptr = self.class_heap.insert(class_obj);
        Value::Class(ptr)
    }

    fn insert_instance(
        &mut self,
        class: GcPtr<LoxClass>,
        fields: HashMap<StringIntern, Value>,
    ) -> Value {
        let instance_obj = LoxInstance { class, fields };
        let ptr = self.instance_heap.insert(instance_obj);
        Value::Instance(ptr)
    }

    fn insert_bound_method(
        &mut self,
        receiver: GcPtr<LoxInstance>,
        closure: GcPtr<LoxClosure>,
    ) -> Value {
        let bound_method_obj = LoxBoundMethod { receiver, closure };
        let ptr = self.bound_method_heap.insert(bound_method_obj);
        Value::BoundMethod(ptr)
    }

    fn sweep(&mut self) {
        self.closure_heap.sweep();
        self.class_heap.sweep();
        self.instance_heap.sweep();
        self.bound_method_heap.sweep();
    }
}

impl VM {
    pub fn new() -> Self {
        let mut vm = VM {
            call_stack: vec![],
            stack: Stack::new(),
            open_upvalues: vec![],
            string_table: StringInterner::new(),
            globals: HashMap::new(),
            heap: Heap::new(),
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
        let main_fn = self
            .heap
            .insert_closure(main_name, 0, Rc::new(main_chunk), Rc::new(vec![]));
        self.stack.push(main_fn);

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
        let mut num_cycles = 0;

        loop {
            if num_cycles == GC_CYCLES_PERIOD {
                self.run_garbage_collection();
                num_cycles = 0;
            }
            num_cycles += 1;

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

            // Convert bytecode to opcode
            let op = match self.try_read_op() {
                Ok(op) => op,
                Err(byte) => return Err(VmError::UnknownOpCode(byte)),
            };

            // Run instruction
            match op {
                OpCode::True => self.stack.push(Value::Boolean(true)),
                OpCode::False => self.stack.push(Value::Boolean(false)),
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::Constant => {
                    let value = match self.read_index_as_constant() {
                        ChunkConstant::Number(n) => Value::Number(n),
                        ChunkConstant::String(s) => Value::String(s),
                        c => return Err(VmError::CannotParseConstant(c)),
                    };
                    self.stack.push(value);
                }
                OpCode::Add => self.run_add()?,
                OpCode::Subtract => self.numerical_binop(|lhs, rhs| lhs - rhs)?,
                OpCode::Multiply => self.numerical_binop(|lhs, rhs| lhs * rhs)?,
                OpCode::Divide => {
                    if let Value::Number(n) = self.stack.peek(0)? {
                        if *n == 0.0 {
                            return Err(VmError::DivideByZero);
                        }
                    }
                    self.numerical_binop(|lhs, rhs| lhs / rhs)?;
                }
                OpCode::Negate => match self.stack.peek(0)? {
                    Value::Number(n) => {
                        let n = n.to_owned();
                        self.stack.pop()?;
                        self.stack.push(Value::Number(-n));
                    }
                    _ => return Err(VmError::WrongOperandType),
                },
                OpCode::Not => {
                    let value = self.stack.pop()?;
                    self.stack.push(Value::Boolean(!value.is_truthy()));
                }
                OpCode::Equal => {
                    let rhs = self.stack.pop()?;
                    let lhs = self.stack.pop()?;
                    self.stack.push(Value::Boolean(lhs == rhs));
                }
                OpCode::GreaterThan => self.logical_binop(|lhs, rhs| lhs > rhs)?,
                OpCode::LessThan => self.logical_binop(|lhs, rhs| lhs < rhs)?,
                OpCode::DefineGlobal => {
                    let name = self.read_index_as_string();
                    let value = self.stack.pop()?;
                    self.globals.insert(name, value);
                }
                OpCode::GetGlobal => {
                    let name = self.read_index_as_string();
                    let value = match self.globals.get(&name) {
                        Some(value) => value.clone(),
                        None => {
                            let name: String = (*name).to_owned();
                            return Err(VmError::UndefinedGlobal(name));
                        }
                    };
                    self.stack.push(value);
                }
                OpCode::SetGlobal => {
                    let name = self.read_index_as_string();
                    if !self.globals.contains_key(&name) {
                        let name: String = (*name).to_owned();
                        return Err(VmError::UndefinedGlobal(name));
                    }
                    // Do not pop value, since assignment is an expression and can
                    // be nested inside another expression that needs the value.
                    let value = self.stack.peek(0)?.clone();
                    self.globals.insert(name, value);
                }
                OpCode::GetLocal => {
                    let base_ptr = self.frame().base_ptr;
                    let index = self.read_byte() as usize;
                    let value = self.stack.get(base_ptr + index)?.clone();
                    self.stack.push(value);
                }
                OpCode::SetLocal => {
                    let base_ptr = self.frame().base_ptr;
                    let index = self.read_byte() as usize;
                    let value = self.stack.peek(0)?.clone();
                    self.stack.set(base_ptr + index, value)?;
                }
                OpCode::Jump => {
                    let jump_by = usize::from(self.read_short());
                    self.frame_mut().ip += jump_by;
                }
                OpCode::JumpIfFalse => {
                    let jump_by = usize::from(self.read_short());
                    if !self.stack.peek(0)?.is_truthy() {
                        self.frame_mut().ip += jump_by;
                    }
                }
                OpCode::Loop => {
                    let jump_by = usize::from(self.read_short());
                    self.frame_mut().ip -= jump_by;
                }
                OpCode::MakeClosure => {
                    let (name, arity, chunk, upvalue_count) = match self.read_index_as_constant() {
                        ChunkConstant::FnTemplate {
                            name,
                            arity,
                            chunk,
                            upvalue_count,
                        } => (name, arity, chunk, upvalue_count),
                        _ => return Err(VmError::NotCallable),
                    };

                    let mut upvalues = Vec::with_capacity(upvalue_count);
                    for _i in 0..upvalue_count {
                        let upvalue_type = self.read_byte();
                        let upvalue_index = self.read_byte();
                        let upvalue_ptr = match upvalue_type {
                            UPVALUE_IMMEDIATE_VALUE => {
                                let stack_index = self.frame().base_ptr + upvalue_index as usize;
                                self.make_open_value(stack_index)
                            }
                            UPVALUE_RECURSIVE_VALUE => {
                                self.frame().upvalues[upvalue_index as usize].clone()
                            }
                            _ => return Err(VmError::CannotParseUpvalue),
                        };
                        upvalues.push(upvalue_ptr);
                    }

                    let closure =
                        self.heap
                            .insert_closure(name, arity, chunk.clone(), Rc::new(upvalues));
                    self.stack.push(closure);
                }
                OpCode::GetUpvalue => {
                    let index = self.read_byte() as usize;
                    let value = match &*self.frame().upvalues[index].borrow() {
                        UpvalueData::Closed(v) => v.clone(),
                        UpvalueData::Open(i) => self.stack.get(*i)?.clone(),
                    };
                    self.stack.push(value);
                }
                OpCode::SetUpvalue => {
                    let index = self.read_byte() as usize;
                    let value = self.stack.peek(0)?.clone();

                    let mut upvalue = self.frame().upvalues[index].clone();
                    match &mut *upvalue.borrow_mut() {
                        UpvalueData::Closed(v) => *v = value,
                        UpvalueData::Open(i) => self.stack.set(*i, value)?,
                    };
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1)?;
                    self.stack.pop()?;
                }
                OpCode::MakeClass => {
                    let name = self.read_index_as_string();
                    let klass = self.heap.insert_class(name, HashMap::new());
                    self.stack.push(klass);
                }
                OpCode::GetProperty => {
                    let name = self.read_index_as_string();

                    let instance_ptr = match self.stack.peek(0)? {
                        Value::Instance(ptr) => ptr,
                        _ => return Err(VmError::NotAnInstance),
                    };

                    let value = match instance_ptr.borrow().search(&name) {
                        PropertySearch::Field(value) => value,
                        PropertySearch::Method(method) => {
                            self.heap.insert_bound_method(instance_ptr.clone(), method)
                        }
                        PropertySearch::Missing => return Err(VmError::UnknownProperty),
                    };

                    self.stack.pop()?;
                    self.stack.push(value);
                }
                OpCode::SetProperty => {
                    let name = self.read_index_as_string();
                    let value = self.stack.peek(0)?;

                    let mut instance_ptr = match self.stack.peek(1)? {
                        Value::Instance(ptr) => ptr.clone(),
                        _ => return Err(VmError::NotAnInstance),
                    };
                    let mut instance = instance_ptr.borrow_mut();

                    let value = value.clone();
                    instance.fields.insert(name, value.clone());

                    self.stack.pop()?;
                    self.stack.pop()?;
                    self.stack.push(value);
                }
                OpCode::MakeMethod => {
                    let method_name = self.read_index_as_string();

                    let method_ptr = match self.stack.peek(0)? {
                        Value::Closure(ptr) => ptr.clone(),
                        _ => return Err(VmError::NotCallable),
                    };
                    let mut class_ptr = match self.stack.peek(1)? {
                        Value::Class(ptr) => ptr.clone(),
                        _ => return Err(VmError::NotAClass),
                    };
                    class_ptr
                        .borrow_mut()
                        .methods
                        .insert(method_name, method_ptr.clone());

                    self.stack.pop()?;
                }
                OpCode::Invoke => {
                    let method_name = self.read_index_as_string();
                    let num_args: usize = self.read_byte().into();

                    let receiver_ptr = match self.stack.peek(num_args)? {
                        Value::Instance(ptr) => ptr,
                        _ => return Err(VmError::NotAnInstance),
                    };

                    let search_result = receiver_ptr.borrow().search(&method_name);

                    match search_result {
                        PropertySearch::Field(value) => {
                            self.stack.set_back(num_args, value)?;
                            self.call(num_args)?;
                        }
                        PropertySearch::Method(method) => self.call_closure(method, num_args)?,
                        PropertySearch::Missing => return Err(VmError::UnknownProperty),
                    }
                }
                OpCode::Inherit => {
                    let superclass_ptr = match self.stack.peek(1)? {
                        Value::Class(ptr) => ptr.clone(),
                        _ => return Err(VmError::NotAClass),
                    };

                    let mut class_ptr = match self.stack.peek(0)? {
                        Value::Class(ptr) => ptr.clone(),
                        _ => return Err(VmError::NotAClass),
                    };

                    let methods = superclass_ptr.borrow().methods.clone();
                    class_ptr.borrow_mut().methods = methods;
                }
                OpCode::GetSuper => {
                    let method_name = self.read_index_as_string();

                    let method_ptr = match self.stack.peek(0)? {
                        Value::Class(ptr) => match ptr.borrow().methods.get(&method_name) {
                            Some(method_ptr) => method_ptr.clone(),
                            None => return Err(VmError::UnknownProperty),
                        },
                        _ => return Err(VmError::NotAClass),
                    };

                    let instance_ptr = match self.stack.peek(1)? {
                        Value::Instance(ptr) => ptr,
                        _ => return Err(VmError::NotAnInstance),
                    };

                    let value = self
                        .heap
                        .insert_bound_method(instance_ptr.clone(), method_ptr);

                    self.stack.pop()?;
                    self.stack.pop()?;
                    self.stack.push(value);
                }
                OpCode::InvokeSuper => {
                    let method_name = self.read_index_as_string();
                    let num_args: usize = self.read_byte().into();

                    let superclass_ptr = match self.stack.pop()? {
                        Value::Class(ptr) => ptr,
                        _ => return Err(VmError::NotAClass),
                    };

                    match superclass_ptr.borrow().methods.get(&method_name) {
                        Some(method) => self.call_closure(method.clone(), num_args)?,
                        None => return Err(VmError::UnknownProperty),
                    };
                }
                OpCode::Call => {
                    let num_args: usize = self.read_byte().into();
                    self.call(num_args)?;
                }
                OpCode::Return => {
                    let result = self.pop_frame()?;

                    if self.call_stack.is_empty() {
                        return Ok(());
                    } else {
                        self.stack.push(result);
                    }
                }
                OpCode::Print => {
                    let value = self.stack.pop()?;
                    println!("[out] {:?}", value);
                }
                OpCode::Pop => {
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
                    return Err(VmError::WrongArity);
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
                let instance = self.heap.insert_instance(ptr.clone(), HashMap::new());
                self.stack.set_back(num_args, instance)?;

                // Call initializer
                if let Some(init) = ptr.borrow().methods.get(INIT_STR) {
                    self.call_closure(init.clone(), num_args)?;
                } else if num_args > 0 {
                    return Err(VmError::NoArgumentInitializer);
                }

                Ok(())
            }
            Value::BoundMethod(ptr) => {
                let bound_method = ptr.borrow();
                let receiver = Value::Instance(bound_method.receiver.clone());
                self.stack.set_back(num_args, receiver)?;
                self.call_closure(bound_method.closure.clone(), num_args)
            }
            _ => Err(VmError::NotCallable),
        }
    }

    fn call_closure(&mut self, closure_ptr: GcPtr<LoxClosure>, num_args: usize) -> VmResult<()> {
        let closure = closure_ptr.borrow();
        if closure.arity != num_args {
            return Err(VmError::WrongArity);
        }
        self.push_new_frame(
            num_args,
            closure.name.clone(),
            closure.chunk.clone(),
            closure.upvalues.clone(),
        );

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
        upvalues: Rc<Vec<UpvaluePtr>>,
    ) {
        let new_frame = CallFrame {
            ip: 0,
            base_ptr: self.stack.len() - (num_args + 1),
            name,
            chunk,
            upvalues,
        };
        self.call_stack.push(new_frame);
    }

    fn pop_frame(&mut self) -> VmResult<Value> {
        let result = self.stack.pop()?;
        let frame = self.call_stack.pop().expect("Empty call stack.");
        self.close_upvalues(frame.base_ptr)?;
        self.stack.truncate(frame.base_ptr);
        Ok(result)
    }

    fn read_byte(&mut self) -> u8 {
        let frame = self.frame_mut();
        let byte = frame.chunk.read_byte(frame.ip);
        frame.ip += 1;
        byte
    }

    fn read_short(&mut self) -> u16 {
        let frame = self.frame_mut();
        let short = frame.chunk.read_short(frame.ip);
        frame.ip += 2;
        short
    }

    fn try_read_op(&mut self) -> Result<OpCode, u8> {
        let frame = self.frame_mut();
        let result = frame.chunk.try_read_op(frame.ip);
        frame.ip += 1;
        result
    }

    fn read_index_as_constant(&mut self) -> ChunkConstant {
        let index = self.read_byte();
        self.frame().chunk.read_constant(index)
    }

    fn read_index_as_string(&mut self) -> StringIntern {
        match self.read_index_as_constant() {
            ChunkConstant::String(s) => s,
            _ => panic!("Global table contains non-string."),
        }
    }

    fn make_open_value(&mut self, stack_index: usize) -> UpvaluePtr {
        // Search

        fn index_match(upvalue: &UpvaluePtr, stack_index: usize) -> bool {
            stack_index
                == upvalue
                    .get_open_index()
                    .expect("Closed upvalue in vm open upvalues.")
        }

        match self
            .open_upvalues
            .iter().find(|uv| index_match(uv, stack_index))
        {
            Some(upvalue) => upvalue.clone(),
            None => {
                let upvalue = UpvaluePtr::new(stack_index);
                self.open_upvalues.push(upvalue.clone());
                upvalue
            }
        }
    }

    fn close_upvalues(&mut self, stack_index: usize) -> VmResult<()> {
        for upvalue in self.open_upvalues.iter() {
            let index = upvalue
                .get_open_index()
                .expect("Closed upvalue in vm open upvalues.");
            if index >= stack_index {
                let value = self.stack.get(index)?.clone();
                upvalue.close_over_value(value);
            }
        }

        self.open_upvalues
            .retain(|uv| uv.get_open_index().is_some());

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
            value.mark_internals();
        }

        // Globals are reachable.
        for value in self.globals.values() {
            value.mark_internals();
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
            _ => return Err(VmError::WrongOperandType),
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
            (_, _) => Err(VmError::WrongOperandType),
        }
    }
}
