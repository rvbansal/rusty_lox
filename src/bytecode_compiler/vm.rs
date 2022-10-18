use super::chunk::{Chunk, Value};
use super::opcode::OpCode;

use std::convert::TryFrom;

const DEBUG_TRACE_EXECUTION: bool = true;

pub struct VM {
    stack: Vec<Value>,
}

#[derive(Debug)]
pub enum VmError {
    UnknownOpCode(u8),
    DivideByZero,
}

pub type VmResult<T> = Result<T, VmError>;

impl VM {
    pub fn new() -> Self {
        VM { stack: vec![] }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Popped from empty stack!")
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
                OpCode::Return => {
                    let return_value = self.pop();
                    // Just print for now
                    println!("Return: {}", return_value);
                    return Ok(());
                }
                OpCode::Constant => {
                    let constant = chunk.read_constant(chunk.code[ip + 1]);
                    self.push(constant);
                }
                OpCode::Add => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(rhs + lhs);
                }
                OpCode::Subtract => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(lhs - rhs);
                }
                OpCode::Multiply => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(rhs * lhs);
                }
                OpCode::Divide => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    if rhs != 0.0 {
                        self.push(lhs / rhs);
                    } else {
                        return Err(VmError::DivideByZero);
                    }
                }
                OpCode::Negate => {
                    let value = -self.pop();
                    self.push(value);
                }
            }

            ip += op.num_operands() + 1;
        }
    }
}
