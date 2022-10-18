use super::class::{LoxClassDataPtr, LoxInstanceDataPtr};
use super::errors::{InterpreterError, RuntimeResult};
use super::function::LoxFn;
use super::interpreter::Interpreter;
use super::native_function::NativeFn;
use crate::lox_frontend::operator::{InfixOperator, PrefixOperator};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Number(f64),
    Boolean(bool),
    String(String),
    Nil,
    NativeFunc(NativeFn),
    LoxFunc(LoxFn),
    LoxClass(LoxClassDataPtr),
    LoxInstance(LoxInstanceDataPtr),
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Object::Nil | Object::Boolean(false))
    }

    pub fn execute(
        &self,
        args: Vec<Object>,
        interpreter: &mut Interpreter,
    ) -> RuntimeResult<Object> {
        match self {
            Object::NativeFunc(f) => f.execute(args, interpreter),
            Object::LoxFunc(f) => f.execute(args, interpreter),
            Object::LoxClass(class) => class.execute(args, interpreter),
            _ => Err(InterpreterError::NotCallable(self.clone())),
        }
    }

    pub fn apply_infix_op(op: InfixOperator, lhs: Object, rhs: Object) -> RuntimeResult<Object> {
        match op {
            InfixOperator::Add => match (lhs, rhs) {
                (Object::Number(a), Object::Number(b)) => Ok(Object::Number(a + b)),
                (Object::String(a), Object::String(b)) => Ok(Object::String(a + &b)),
                (a, b) => Err(InterpreterError::IllegalInfixOperation(op, a, b)),
            },
            InfixOperator::Subtract => numerical_binop(op, lhs, rhs, |a, b| Object::Number(a - b)),
            InfixOperator::Multiply => numerical_binop(op, lhs, rhs, |a, b| Object::Number(a * b)),
            InfixOperator::Divide => match (lhs, rhs) {
                (Object::Number(a), Object::Number(b)) => {
                    if b != 0.0 {
                        Ok(Object::Number(a / b))
                    } else {
                        Err(InterpreterError::DivideByZero)
                    }
                }
                (lhs, rhs) => Err(InterpreterError::IllegalInfixOperation(op, lhs, rhs)),
            },
            InfixOperator::EqualTo => Ok(Object::Boolean(lhs == rhs)),
            InfixOperator::NotEqualTo => Ok(Object::Boolean(lhs != rhs)),
            InfixOperator::GreaterEq => {
                numerical_binop(op, lhs, rhs, |a, b| Object::Boolean(a >= b))
            }
            InfixOperator::GreaterThan => {
                numerical_binop(op, lhs, rhs, |a, b| Object::Boolean(a > b))
            }
            InfixOperator::LessEq => numerical_binop(op, lhs, rhs, |a, b| Object::Boolean(a <= b)),
            InfixOperator::LessThan => numerical_binop(op, lhs, rhs, |a, b| Object::Boolean(a < b)),
        }
    }

    pub fn apply_prefix_op(op: PrefixOperator, value: Object) -> RuntimeResult<Object> {
        match op {
            PrefixOperator::Negate => match value {
                Object::Number(n) => Ok(Object::Number(-n)),
                _ => Err(InterpreterError::IllegalPrefixOperation(op, value)),
            },
            PrefixOperator::LogicalNot => Ok(Object::Boolean(!value.is_truthy())),
        }
    }
}

fn numerical_binop<F>(op: InfixOperator, lhs: Object, rhs: Object, func: F) -> RuntimeResult<Object>
where
    F: Fn(f64, f64) -> Object,
{
    match (lhs, rhs) {
        (Object::Number(a), Object::Number(b)) => Ok(func(a, b)),
        (a, b) => Err(InterpreterError::IllegalInfixOperation(op, a, b)),
    }
}
