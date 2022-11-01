use super::chunk::ChunkConstant;
use super::opcode::OpCodeError;

#[derive(Debug)]
pub enum VmError {
    UnknownOpCode(u8),
    StackUnderflow,
    InvalidStackIndex(usize),
    InvalidStackDepth(usize),
    UndefinedGlobal(String),
    WrongArity(usize, usize),
    NativeFnError(String),
    CannotParseConstant(ChunkConstant),
    CannotParseUpvalue,
    UnknownProperty(String),
    NotACallable,
    NotAnInstance,
    NotAClass,
    NotAClosure,
    OpCodeOutOfBounds,
    StackOverflow,
    IncorrectOperandTypeAdd,
    NonNumericOperandInfix,
    NonNumericOperandPrefix,
    IncorrectSuperclass,
    IncorrectFieldAccess,
    IncorrectPropertyAccess,
}

#[derive(Debug)]
pub enum CompilerError {
    TooManyLocalVars,
    UseVarInInitialization(String),
    LocalVarDefinedAlready(String),
    TooManyUpvalues,
    SelfInherit(String),
    TooManyConstants,
    JumpTooLong,
    ThisOutsideClass,
    SuperOutsideClass,
    SuperWithoutSuperclass,
    ReturnAtTopLevel,
    ReturnInInitializer,
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

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VmError::UnknownOpCode(byte) => write!(f, "Unrecognized opcode {}.", byte),
            VmError::IncorrectOperandTypeAdd => {
                write!(f, "Operands must be two numbers or two strings.")
            }
            VmError::NonNumericOperandInfix => write!(f, "Operands must be numbers."),
            VmError::NonNumericOperandPrefix => write!(f, "Operand must be a number."),
            VmError::StackUnderflow => write!(f, "Stack underflow."),
            VmError::StackOverflow => write!(f, "Stack overflow."),
            VmError::InvalidStackIndex(idx) => {
                write!(f, "Invalid stack access at index {}.", idx)
            }
            VmError::InvalidStackDepth(depth) => {
                write!(f, "Invalid stack access at depth {}.", depth)
            }
            VmError::UndefinedGlobal(var_name) => {
                write!(f, "Undefined variable '{}'.", var_name)
            }
            VmError::NotACallable => write!(f, "Can only call functions and classes."),
            VmError::WrongArity(expected, actual) => {
                write!(f, "Expected {} arguments but got {}.", expected, actual)
            }
            VmError::NativeFnError(err_msg) => {
                write!(f, "Error when running native function: {}", err_msg)
            }
            VmError::CannotParseConstant(_) => write!(f, "{:?}", self),
            VmError::CannotParseUpvalue => write!(f, "{:?}", self),
            VmError::NotAClass => write!(f, "{:?}", self),
            VmError::NotAnInstance => write!(f, "Only instances have properties."),
            VmError::NotAClosure => write!(f, "{:?}", self),
            VmError::UnknownProperty(prop_name) => {
                write!(f, "Undefined property '{}'.", prop_name)
            }
            VmError::IncorrectSuperclass => write!(f, "Superclass must be a class."),
            VmError::IncorrectFieldAccess => write!(f, "Only instances have fields."),
            VmError::IncorrectPropertyAccess => write!(f, "Only instances have properties."),
            VmError::OpCodeOutOfBounds => write!(f, "{:?}", self),
        }
    }
}

impl std::error::Error for VmError {}
