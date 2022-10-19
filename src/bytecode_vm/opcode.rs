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
}

impl OpCode {
    pub fn num_operands(&self) -> usize {
        match self {
            OpCode::True | OpCode::False => 0,
            OpCode::Nil => 0,
            OpCode::Constant => 1,
            OpCode::Return => 0,
            OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide | OpCode::Negate => {
                0
            }
            OpCode::Not => 0,
            OpCode::Equal | OpCode::GreaterThan | OpCode::LessThan => 0,
        }
    }
}
