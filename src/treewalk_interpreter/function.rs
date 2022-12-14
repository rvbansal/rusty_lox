use std::fmt;
use std::rc::Rc;

use super::environment::Environment;
use super::errors::{InterpreterError, RuntimeResult};
use super::grammar::FuncInfo;
use super::interpreter::Interpreter;
use super::object::Object;
use crate::lox_frontend::constants::THIS_STR;

pub struct LoxFnData {
    fn_data: FuncInfo,
    is_initializer: bool,
    closure: Environment,
}

#[derive(Clone)]
pub struct LoxFn(Rc<LoxFnData>);

impl LoxFn {
    pub fn new(fn_data: FuncInfo, is_initializer: bool, closure: Environment) -> Self {
        let data = LoxFnData {
            fn_data,
            is_initializer,
            closure,
        };
        LoxFn(Rc::new(data))
    }

    pub fn arity(&self) -> usize {
        self.0.fn_data.params.len()
    }

    pub fn execute<S: std::io::Write>(
        &self,
        args: Vec<Object>,
        interpreter: &mut Interpreter<S>,
    ) -> RuntimeResult<Object> {
        if self.arity() != args.len() {
            return Err(InterpreterError::WrongArity(self.arity(), args.len()));
        }

        // Create a new environment pointing to surrounding closure
        let env = Environment::with_enclosing(&self.0.closure);

        // Define params in environment
        for (param, arg) in self.0.fn_data.params.iter().zip(args.into_iter()) {
            env.define(param.clone(), arg);
        }

        // Swap interpreter to new env
        let prev_env = interpreter.swap_env(env);
        let result = match interpreter.eval_statements(&self.0.fn_data.body) {
            Ok(_) => Ok(Object::Nil),
            Err(InterpreterError::Return(object)) => Ok(object),
            Err(e) => Err(e),
        };

        // Swap back to closure env
        interpreter.swap_env(prev_env);
        result
    }

    pub fn bind(&self, instance: Object) -> Self {
        let new_env = Environment::with_enclosing(&self.0.closure);
        new_env.define(THIS_STR.to_owned(), instance);
        LoxFn::new(self.0.fn_data.clone(), self.0.is_initializer, new_env)
    }
}

impl fmt::Debug for LoxFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<function {}>", self.0.fn_data.name)
    }
}

impl PartialEq<LoxFn> for LoxFn {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for LoxFn {}
