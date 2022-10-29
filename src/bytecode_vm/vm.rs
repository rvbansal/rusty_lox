use super::chunk::{Chunk, ChunkConstant, ConstantIndex};
use super::errors::{VmError, VmResult};
use super::gc::{GcHeap, GcPtr};
use super::native_function;
use super::opcode::OpCode;
use super::string_interner::{StringIntern, StringInterner};
use super::value::{ActiveUpvalue, HeapObject, Value};

use super::compiler::Upvalue;

use core::num;
use std::collections::HashMap;
use std::rc::Rc;

const GC_CYCLES_PERIOD: u32 = 1000;

struct CallFrame {
    ip: usize,
    base_ptr: usize,
    name: StringIntern,
    chunk: Rc<Chunk>,
    upvalues: Rc<Vec<ActiveUpvalue>>,
}

impl CallFrame {
    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.read_byte(self.ip);
        self.ip += 1;
        byte
    }

    fn read_short(&mut self) -> u16 {
        let short = self.chunk.read_short(self.ip);
        self.ip += 2;
        short
    }

    fn try_read_op(&mut self) -> Result<OpCode, u8> {
        let result = self.chunk.try_read_op(self.ip);
        self.ip += 1;
        result
    }

    fn try_read_upvalue(&mut self) -> Result<Upvalue, u8> {
        let kind = self.read_byte();
        match kind {
            1 => Ok(Upvalue::Immediate(self.read_byte())),
            0 => Ok(Upvalue::Recursive(self.read_byte())),
            _ => Err(kind),
        }
    }
}

pub struct VM {
    call_stack: Vec<CallFrame>,
    stack: Vec<Value>,
    open_upvalues: Vec<ActiveUpvalue>,
    heap: GcHeap<HeapObject>,
    string_table: StringInterner,
    globals: HashMap<StringIntern, Value>,
}

impl VM {
    pub fn new() -> Self {
        let mut vm = VM {
            call_stack: vec![],
            stack: vec![],
            open_upvalues: vec![],
            heap: GcHeap::new(),
            string_table: StringInterner::new(),
            globals: HashMap::new(),
        };

        for (name, arity, function) in native_function::get_native_fns().iter().copied() {
            let name = vm.get_string_intern(name);
            let native_fn = vm.make_heap_value(HeapObject::NativeFn {
                name: name.clone(),
                arity,
                function,
            });
            vm.globals.insert(name, native_fn);
        }

        vm
    }

    pub fn borrow_string_table(&mut self) -> &mut StringInterner {
        &mut self.string_table
    }

    pub fn interpret(&mut self, main_chunk: Chunk) -> VmResult<()> {
        self.call_stack.clear();
        self.stack.clear();

        let main_name = self.get_string_intern("<main>");
        let main_fn = self.make_heap_value(HeapObject::LoxClosure {
            name: main_name,
            arity: 0,
            chunk: Rc::new(main_chunk),
            upvalues: Rc::new(vec![]),
        });
        self.push(main_fn);

        self.call(self.peek(0)?, 0)?;

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
                    ip, base_ptr, self.stack[base_ptr]
                );
                self.frame_mut().chunk.disassemble_at_offset(ip);
                println!();
            }

            // Convert bytecode to opcode
            let op = match self.frame_mut().try_read_op() {
                Ok(op) => op,
                Err(byte) => return Err(VmError::UnknownOpCode(byte)),
            };

            // Run instruction
            match op {
                OpCode::True => self.push(Value::Boolean(true)),
                OpCode::False => self.push(Value::Boolean(false)),
                OpCode::Nil => self.push(Value::Nil),
                OpCode::Constant => {
                    let index = self.frame_mut().read_byte();
                    let value = match self.frame().chunk.read_constant(index) {
                        ChunkConstant::Number(n) => Value::Number(n),
                        ChunkConstant::String(s) => Value::String(s),
                        c => return Err(VmError::CannotParseConstant(c)),
                    };
                    self.push(value);
                }
                OpCode::Add => self.run_add()?,
                OpCode::Subtract => self.numerical_binop(|lhs, rhs| lhs - rhs)?,
                OpCode::Multiply => self.numerical_binop(|lhs, rhs| lhs * rhs)?,
                OpCode::Divide => {
                    if let Value::Number(n) = self.peek(0)? {
                        if n == 0.0 {
                            return Err(VmError::DivideByZero);
                        }
                    }
                    self.numerical_binop(|lhs, rhs| lhs / rhs)?;
                }
                OpCode::Negate => match self.peek(0)? {
                    Value::Number(n) => {
                        self.pop()?;
                        self.push(Value::Number(-n));
                    }
                    _ => return Err(VmError::WrongOperandType),
                },
                OpCode::Not => {
                    let value = self.pop()?;
                    self.push(Value::Boolean(!value.is_truthy()));
                }
                OpCode::Equal => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    self.push(Value::Boolean(lhs == rhs));
                }
                OpCode::GreaterThan => self.logical_binop(|lhs, rhs| lhs > rhs)?,
                OpCode::LessThan => self.logical_binop(|lhs, rhs| lhs < rhs)?,
                OpCode::DefineGlobal => {
                    let index = self.frame_mut().read_byte();
                    let name = self.read_string(index);
                    let value = self.pop()?;
                    self.globals.insert(name, value);
                }
                OpCode::GetGlobal => {
                    let index = self.frame_mut().read_byte();
                    let name = self.read_string(index);
                    let value = match self.globals.get(&name) {
                        Some(value) => value.clone(),
                        None => {
                            let name: String = (*name).to_owned();
                            return Err(VmError::UndefinedGlobal(name));
                        }
                    };
                    self.push(value);
                }
                OpCode::SetGlobal => {
                    let index = self.frame_mut().read_byte();
                    let name = self.read_string(index);
                    if !self.globals.contains_key(&name) {
                        let name: String = (*name).to_owned();
                        return Err(VmError::UndefinedGlobal(name));
                    }
                    // Do not pop value, since assignment is an expression and can
                    // be nested inside another expression that needs the value.
                    let value = self.peek(0)?;
                    self.globals.insert(name, value);
                }
                OpCode::GetLocal => {
                    let base_ptr = self.frame().base_ptr;
                    let index = self.frame_mut().read_byte() as usize;
                    let value = self.stack[base_ptr + index].clone();
                    self.push(value);
                }
                OpCode::SetLocal => {
                    let base_ptr = self.frame().base_ptr;
                    let value = self.peek(0)?;
                    let index = self.frame_mut().read_byte() as usize;
                    self.stack[base_ptr + index] = value;
                }
                OpCode::MakeClosure => {
                    let index = self.frame_mut().read_byte();
                    let closure_obj = match self.frame().chunk.read_constant(index) {
                        ChunkConstant::FnTemplate {
                            name,
                            arity,
                            chunk,
                            upvalue_count,
                        } => {
                            let mut upvalues = vec![];
                            for i in 0..upvalue_count {
                                let upvalue = match self.frame_mut().try_read_upvalue() {
                                    Ok(uv) => match uv {
                                        Upvalue::Immediate(index) => {
                                            let upvalue = ActiveUpvalue::new(
                                                self.frame().base_ptr + index as usize,
                                            );
                                            self.open_upvalues.push(upvalue.clone());
                                            upvalue
                                        }
                                        Upvalue::Recursive(index) => {
                                            self.frame().upvalues[index as usize].clone()
                                        }
                                    },
                                    Err(_) => return Err(VmError::CannotParseUpvalue),
                                };
                                upvalues.push(upvalue);
                            }

                            HeapObject::LoxClosure {
                                name,
                                arity,
                                chunk: chunk.clone(),
                                upvalues: Rc::new(upvalues),
                            }
                        }
                        _ => return Err(VmError::NotCallable),
                    };

                    let closure_value = self.make_heap_value(closure_obj);
                    self.push(closure_value);
                }
                OpCode::GetUpvalue => {
                    let index = self.frame_mut().read_byte() as usize;
                    let value = match self.frame().upvalues[index].get_if_closed() {
                        Ok(v) => v,
                        Err(i) => self.stack[i].clone(),
                    };
                    self.push(value);
                }
                OpCode::SetUpvalue => {
                    let index = self.frame_mut().read_byte() as usize;
                    let value = self.peek(0)?;
                    match self.frame().upvalues[index].set_if_closed(&value) {
                        Ok(()) => {}
                        Err(i) => self.stack[i] = value,
                    };
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1);
                    self.pop()?;
                }
                OpCode::Return => {
                    let result = self.pop_frame()?;

                    if self.call_stack.is_empty() {
                        return Ok(());
                    } else {
                        self.push(result);
                    }
                }
                OpCode::Print => {
                    let value = self.pop()?;
                    println!("[out] {:?}", value);
                }
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::Jump => {
                    let jump_by = usize::from(self.frame_mut().read_short());
                    self.frame_mut().ip += jump_by;
                }
                OpCode::JumpIfFalse => {
                    let jump_by = usize::from(self.frame_mut().read_short());
                    if !self.peek(0)?.is_truthy() {
                        self.frame_mut().ip += jump_by;
                    }
                }
                OpCode::Loop => {
                    let jump_by = usize::from(self.frame_mut().read_short());
                    self.frame_mut().ip -= jump_by;
                }
                OpCode::Call => {
                    let num_args: usize = self.frame_mut().read_byte().into();
                    self.call(self.peek(num_args)?, num_args)?;
                }
            }
        }
    }

    pub fn make_heap_value(&mut self, obj: HeapObject) -> Value {
        Value::Object(self.insert_into_heap(obj))
    }

    pub fn call(&mut self, callee: Value, num_args: usize) -> VmResult<()> {
        let heap_obj_ref = match &callee {
            Value::Object(gc_ptr) => gc_ptr.borrow(),
            _ => return Err(VmError::NotCallable),
        };

        match &*heap_obj_ref {
            HeapObject::LoxClosure {
                name,
                arity,
                chunk,
                upvalues,
            } => {
                if *arity != num_args {
                    return Err(VmError::WrongArity);
                }
                self.push_new_frame(num_args, name.clone(), chunk.clone(), upvalues.clone());
            }

            HeapObject::NativeFn {
                arity, function, ..
            } => {
                if *arity != num_args {
                    return Err(VmError::WrongArity);
                }

                let start_idx = self.stack.len() - num_args;
                let arg_slice = &self.stack[start_idx..];
                let value = match function(arg_slice) {
                    Ok(value) => value,
                    Err(s) => return Err(VmError::NativeFnError(s)),
                };

                self.stack.truncate(self.stack.len() - num_args - 1);
                self.push(value);
            }
        }

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
        upvalues: Rc<Vec<ActiveUpvalue>>,
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
        let result = self.pop()?;
        let frame = self.call_stack.pop().expect("Empty call stack.");
        self.close_upvalues(frame.base_ptr);
        self.stack.truncate(frame.base_ptr);
        Ok(result)
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> VmResult<Value> {
        self.stack.pop().ok_or(VmError::StackEmpty)
    }

    fn peek(&self, distance_from_top: usize) -> VmResult<Value> {
        let stack_len = self.stack.len();
        self.stack
            .get(stack_len - 1 - distance_from_top)
            .cloned()
            .ok_or(VmError::InvalidStackIndex)
    }

    fn close_upvalues(&mut self, stack_index: usize) {
        for upvalue in self.open_upvalues.iter() {
            match upvalue.get_open_index() {
                Some(slot) if slot >= stack_index => {
                    let value = self.stack[slot].clone();
                    upvalue.close(value);
                }
                _ => {}
            }
        }

        self.open_upvalues
            .retain(|uv| uv.get_open_index().is_some());
    }

    pub fn get_string_intern(&mut self, s: &str) -> StringIntern {
        self.string_table.get_string_intern(s)
    }

    fn read_string(&self, index: ConstantIndex) -> StringIntern {
        let chunk = &self.frame().chunk;
        match chunk.read_constant(index) {
            ChunkConstant::String(s) => s,
            _ => panic!("Global var contains non-string identifier."),
        }
    }

    pub fn insert_into_heap(&mut self, obj: HeapObject) -> GcPtr<HeapObject> {
        self.heap.insert(obj)
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
        let lhs = self.peek(1)?;
        let rhs = self.peek(0)?;

        let result = match (lhs, rhs) {
            (Value::Number(x), Value::Number(y)) => Value::Number(x + y),
            (Value::String(x), Value::String(y)) => {
                let dynamic_string = x.as_ref().to_owned() + &y;
                let string_intern = self.string_table.get_string_intern(dynamic_string);
                Value::String(string_intern)
            }
            _ => return Err(VmError::WrongOperandType),
        };

        self.pop()?;
        self.pop()?;
        self.push(result);

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
        let rhs = self.peek(0)?;
        let lhs = self.peek(1)?;
        match (lhs, rhs) {
            (Value::Number(l), Value::Number(r)) => {
                let outcome = func(l, r);
                self.pop()?;
                self.pop()?;
                self.push(outcome);
                Ok(())
            }
            (_, _) => Err(VmError::WrongOperandType),
        }
    }
}
