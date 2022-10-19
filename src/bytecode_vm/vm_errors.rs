#[derive(Debug)]
pub enum VmError {
    UnknownOpCode(u8),
    DivideByZero,
    StackEmpty,
    InvalidStackIndex,
    WrongOperandType,
}

pub type VmResult<T> = Result<T, VmError>;
