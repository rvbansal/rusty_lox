use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Copy, Clone, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    True,
    False,
    Nil,
    Constant,
    Return,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Not,
    Equal,
    GreaterThan,
    LessThan,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
}

impl OpCode {
    pub fn num_operands(&self) -> usize {
        match self {
            OpCode::True | OpCode::False => 0,
            OpCode::Nil => 0,
            OpCode::Constant => 1,
            OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide | OpCode::Negate => {
                0
            }
            OpCode::Not => 0,
            OpCode::Equal | OpCode::GreaterThan | OpCode::LessThan => 0,
            OpCode::Return | OpCode::Print | OpCode::Pop => 0,
            OpCode::DefineGlobal | OpCode::GetGlobal | OpCode::SetGlobal => 1,
            OpCode::GetLocal | OpCode::SetLocal => 1,
        }
    }
}
