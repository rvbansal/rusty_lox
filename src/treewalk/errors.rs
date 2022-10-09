use super::object::Object;
use super::operator::{InfixOperator, PrefixOperator};

#[derive(Debug, PartialEq)]
pub enum InterpreterError {
    IllegalInfixOperation(InfixOperator, Object, Object),
    IllegalPrefixOperation(PrefixOperator, Object),
    UndefinedVariable(String),
}

pub type RuntimeResult<T> = Result<T, InterpreterError>;
