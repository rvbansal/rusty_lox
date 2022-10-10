use super::errors::{InterpreterError, RuntimeResult};
use super::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Environment {
    env_ptr: Rc<RefCell<EnvironmentData>>,
}

struct EnvironmentData {
    values: HashMap<String, Object>,
    enclosing: Option<Environment>,
}

impl Environment {
    pub fn new() -> Self {
        let env_data = EnvironmentData {
            values: HashMap::new(),
            enclosing: None,
        };
        Environment {
            env_ptr: Rc::new(RefCell::new(env_data)),
        }
    }

    /// Returns an environment pointing to its enclosing environment.
    pub fn with_enclosing(env: &Environment) -> Self {
        let env_data = EnvironmentData {
            values: HashMap::new(),
            enclosing: Some(env.clone()),
        };
        Environment {
            env_ptr: Rc::new(RefCell::new(env_data)),
        }
    }

    /// Add or replace current definition.
    pub fn define(&self, name: String, value: Object) {
        self.env_ptr.borrow_mut().values.insert(name, value);
    }

    /// Get current variable.
    pub fn get(&self, name: &str) -> RuntimeResult<Object> {
        match self.env_ptr.borrow_mut().values.get(name) {
            Some(obj) => Ok(obj.clone()),
            None => Err(InterpreterError::UndefinedVariable(name.to_owned())),
        }
    }

    /// Set current variable.
    pub fn set(&self, name: &str, value: Object) -> RuntimeResult<()> {
        match self.env_ptr.borrow_mut().values.get_mut(name) {
            Some(slot) => {
                *slot = value;
                Ok(())
            }
            None => Err(InterpreterError::UndefinedVariable(name.to_owned())),
        }
    }

    fn ancestor(&self, env_hops: usize) -> Environment {
        if env_hops == 0 {
            self.clone()
        } else {
            self.env_ptr
                .borrow()
                .enclosing
                .as_ref()
                .expect("Went past global env.")
                .ancestor(env_hops - 1)
        }
    }

    /// Set variable value.
    pub fn set_at(&self, env_hops: usize, name: &str, value: Object) -> RuntimeResult<()> {
        self.ancestor(env_hops).set(name, value)
    }

    /// Get variable value.
    pub fn get_at(&self, env_hops: usize, name: &str) -> RuntimeResult<Object> {
        self.ancestor(env_hops).get(name)
    }
}
