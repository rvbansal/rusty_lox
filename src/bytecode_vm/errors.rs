use super::chunk::ChunkConstant;

#[derive(Debug)]
pub enum VmError {
    UnknownOpCode(u8),
    DivideByZero,
    StackEmpty,
    InvalidStackIndex,
    WrongOperandType,
    UndefinedGlobal(String),
    WrongArity,
    NativeFnError(String),
    CannotParseConstant(ChunkConstant),
    CannotParseUpvalue,
    UnknownProperty,
    NoArgumentInitializer,
    NotACallable,
    NotAnInstance,
    NotAClass,
    NotAClosure,
}

#[derive(Debug)]
pub enum CompilerError {
    TooManyLocalVars,
    UseVarInInitialization(String),
    LocalVarDefinedAlready(String),
    TooManyUpvalues,
}

pub type VmResult<T> = Result<T, VmError>;
pub type CompilerResult<T> = Result<T, CompilerError>;
