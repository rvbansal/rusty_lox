use super::errors::{InterpreterError, RuntimeResult};
use super::function::LoxFn;
use super::interpreter::Interpreter;
use super::native_function::NativeFn;
use super::class::{LoxClassDataPtr, LoxInstanceDataPtr};


#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Number(f64),
    Boolean(bool),
    String(String),
    Nil,
    NativeFunc(NativeFn),
    LoxFunc(LoxFn),
    LoxClass(LoxClassDataPtr),
    LoxInstance(LoxInstanceDataPtr)
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
}
