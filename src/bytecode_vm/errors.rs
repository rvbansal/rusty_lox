use super::chunk::ChunkConstant;
use super::opcode::OpCodeError;

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
    OpCodeOutOfBounds,
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

impl From<OpCodeError> for VmError {
    fn from(e: OpCodeError) -> Self {
        match e {
            OpCodeError::OutOfBounds => VmError::OpCodeOutOfBounds,
            OpCodeError::UnknownOpCode(byte) => VmError::UnknownOpCode(byte),
            OpCodeError::UnknownUpvalueLocation(_) => VmError::CannotParseUpvalue,
        }
    }
}
