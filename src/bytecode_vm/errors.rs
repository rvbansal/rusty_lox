#[derive(Debug)]
pub enum VmError {
    UnknownOpCode(u8),
    DivideByZero,
    StackEmpty,
    InvalidStackIndex,
    WrongOperandType,
    UndefinedGlobal(String),
}

#[derive(Debug)]
pub enum CompilerError {
    TooManyLocalVars,
    UseVarInInitialization(String),
    LocalVarDefinedAlready(String),
}

pub type VmResult<T> = Result<T, VmError>;
pub type CompilerResult<T> = Result<T, CompilerError>;
