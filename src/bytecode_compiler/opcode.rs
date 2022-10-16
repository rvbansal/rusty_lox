use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Copy, Clone, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Return = 0,
    Constant = 1,
}

impl OpCode {
    pub fn num_operands(&self) -> usize {
        match self {
            OpCode::Return => 0,
            OpCode::Constant => 1,
        }
    }
}
