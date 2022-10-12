use super::ast::Stmt;
use super::constants::THIS_STR;
use super::environment::Environment;
use super::errors::{InterpreterError, RuntimeResult};
use super::interpreter::Interpreter;
use super::object::Object;

use std::fmt;
use std::rc::Rc;

pub struct LoxFnData {
    name: String,
    params: Vec<String>,
    body: Stmt,
    is_initializer: bool,
    closure: Environment,
}

#[derive(Clone)]
pub struct LoxFn(Rc<LoxFnData>);

impl LoxFn {
    pub fn new(
        name: String,
        params: Vec<String>,
        body: Stmt,
        is_initializer: bool,
        closure: Environment,
    ) -> Self {
        let data = LoxFnData {
            name,
            params,
            body,
            is_initializer,
            closure,
        };
        LoxFn(Rc::new(data))
    }

    pub fn arity(&self) -> usize {
        self.0.params.len()
    }

    pub fn execute(
        &self,
        args: Vec<Object>,
        interpreter: &mut Interpreter,
    ) -> RuntimeResult<Object> {
        if self.arity() != args.len() {
            return Err(InterpreterError::WrongArity(self.arity(), args.len()));
        }

        // Create a new environment pointing to surrounding closure
        let env = Environment::with_enclosing(&self.0.closure);

        for (param, arg) in self.0.params.iter().zip(args.into_iter()) {
            env.define(param.clone(), arg);
        }

        let prev_env = interpreter.swap_env(env);
        let result = match interpreter.eval_statement(&self.0.body) {
            Ok(_) => Ok(Object::Nil),
            Err(InterpreterError::Return(object)) => Ok(object),
            Err(e) => Err(e),
        };

        interpreter.swap_env(prev_env);

        let result = result?;

        if self.0.is_initializer {
            let this = self.0.closure.get(THIS_STR);
            let this = this.expect("`this` unavailable in closure");
            Ok(this)
        } else {
            Ok(result)
        }
    }

    pub fn bind(&self, instance: Object) -> Self {
        let new_env = Environment::with_enclosing(&self.0.closure);
        new_env.define(THIS_STR.to_owned(), instance);

        LoxFn::new(
            self.0.name.clone(),
            self.0.params.clone(),
            self.0.body.clone(),
            self.0.is_initializer,
            new_env,
        )
    }
}

impl fmt::Debug for LoxFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<function {}>", self.0.name)
    }
}

impl PartialEq<LoxFn> for LoxFn {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for LoxFn {}
