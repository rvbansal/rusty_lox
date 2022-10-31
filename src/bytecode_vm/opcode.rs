use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Copy, Clone, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    True,
    False,
    Nil,
    Constant,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Not,
    Equal,
    GreaterThan,
    LessThan,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    Jump,
    JumpIfFalse,
    Loop,
    MakeClosure,
    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,
    MakeClass,
    GetProperty,
    SetProperty,
    MakeMethod,
    Invoke,
    Inherit,
    GetSuper,
    InvokeSuper,
    Call,
    Return,
    Print,
    Pop,
}

impl OpCode {
    pub fn operand_size_in_bytes(&self) -> Option<usize> {
        let arg_bytes = match self {
            OpCode::True | OpCode::False => 0,
            OpCode::Nil => 0,
            OpCode::Constant => 1,
            OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide | OpCode::Negate => {
                0
            }
            OpCode::Not => 0,
            OpCode::Equal | OpCode::GreaterThan | OpCode::LessThan => 0,
            OpCode::DefineGlobal | OpCode::GetGlobal | OpCode::SetGlobal => 1,
            OpCode::GetLocal | OpCode::SetLocal => 1,
            OpCode::Jump | OpCode::JumpIfFalse | OpCode::Loop => 2,
            OpCode::MakeClosure => return None,
            OpCode::GetUpvalue | OpCode::SetUpvalue => 1,
            OpCode::CloseUpvalue => 0,
            OpCode::MakeClass => 1,
            OpCode::GetProperty | OpCode::SetProperty => 1,
            OpCode::MakeMethod => 1,
            OpCode::Invoke => 2,
            OpCode::Inherit => 0,
            OpCode::GetSuper => 1,
            OpCode::InvokeSuper => 2,
            OpCode::Return | OpCode::Print | OpCode::Pop => 0,
            OpCode::Call => 1,
        };

        Some(arg_bytes)
    }
}
