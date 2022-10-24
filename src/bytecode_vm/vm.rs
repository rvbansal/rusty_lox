use super::chunk::Chunk;
use super::errors::{VmError, VmResult};
use super::opcode::OpCode;
use super::string_interner::{StringIntern, StringInterner};
use super::value::Value;

use std::collections::HashMap;
use std::convert::TryFrom;

const DEBUG_TRACE_EXECUTION: bool = true;

pub struct VM {
    stack: Vec<Value>,
    string_table: StringInterner,
    globals: HashMap<StringIntern, Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            stack: vec![],
            string_table: StringInterner::new(),
            globals: HashMap::new(),
        }
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

    pub fn get_string_intern(&mut self, s: &str) -> StringIntern {
        self.string_table.get_string_intern(s)
    }

    fn read_string(&self, chunk: &Chunk, index: usize) -> StringIntern {
        match chunk.read_constant(chunk.code[index]) {
            Value::String(s) => s,
            _ => panic!("Global var contains non-string identifier."),
        }
    }

    pub fn add(&mut self, lhs: Value, rhs: Value) -> Option<Value> {
        let obj = match (lhs, rhs) {
            (Value::Number(x), Value::Number(y)) => Value::Number(x + y),
            (Value::String(x), Value::String(y)) => {
                let dynamic_string = x.as_ref().to_owned() + &y;
                let string_intern = self.string_table.get_string_intern(dynamic_string);
                Value::String(string_intern)
            }
            _ => return None,
        };

        Some(obj)
    }

    pub fn interpret(&mut self, chunk: &Chunk) -> VmResult<()> {
        self.run(chunk, 0)
    }

    fn run(&mut self, chunk: &Chunk, mut ip: usize) -> VmResult<()> {
        loop {
            if DEBUG_TRACE_EXECUTION {
                println!("          {:?}", self.stack);
                chunk.disassemble_at_offset(ip);
            }

            // Get current bytecode
            let byte = chunk.code[ip];

            // Convert bytecode to opcode
            let op = match OpCode::try_from(byte) {
                Ok(op) => op,
                Err(_) => return Err(VmError::UnknownOpCode(byte)),
            };

            // Run instruction
            match op {
                OpCode::True => self.push(Value::Boolean(true)),
                OpCode::False => self.push(Value::Boolean(false)),
                OpCode::Nil => self.push(Value::Nil),
                OpCode::Constant => {
                    let constant = chunk.read_constant(chunk.code[ip + 1]);
                    self.push(constant);
                }
                OpCode::Add => {
                    let lhs = self.peek(1)?;
                    let rhs = self.peek(0)?;

                    let result = match self.add(lhs, rhs) {
                        Some(obj) => obj,
                        None => return Err(VmError::WrongOperandType),
                    };

                    self.pop()?;
                    self.pop()?;
                    self.push(result);
                }
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
                OpCode::Return => {
                    return Ok(());
                }
                OpCode::Print => {
                    let value = self.pop()?;
                    println!("[out] {:?}", value);
                }
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::DefineGlobal => {
                    let name = self.read_string(chunk, ip + 1);
                    let value = self.pop()?;
                    self.globals.insert(name, value);
                }
                OpCode::GetGlobal => {
                    let name = self.read_string(chunk, ip + 1);
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
                    let name = self.read_string(chunk, ip + 1);
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
                    let index = chunk.code[ip + 1];
                    let value = self.stack[index as usize].clone();
                    self.push(value);
                }
                OpCode::SetLocal => {
                    let value = self.pop()?;
                    let index = chunk.code[ip + 1];
                    self.stack[index as usize] = value;
                }
            }

            ip += op.num_operands() + 1;
        }
    }
}
