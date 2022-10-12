use super::object::Object;
use super::operator::{InfixOperator, PrefixOperator};

#[derive(Debug, PartialEq)]
pub enum InterpreterError {
    IllegalInfixOperation(InfixOperator, Object, Object),
    IllegalPrefixOperation(PrefixOperator, Object),
    UndefinedVariable(String),
    WrongArity(usize, usize),
    NotCallable(Object),
    MissingProperty(Object, String),
    NotAnInstance(Object),
    // We want to unwind the call stack at a return statement, so
    // it functions much like an error.
    Return(Object),
}

pub type RuntimeResult<T> = Result<T, InterpreterError>;
