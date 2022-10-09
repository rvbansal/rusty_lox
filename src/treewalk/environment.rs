use super::errors::{InterpreterError, RuntimeResult};
use super::object::Object;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;


#[derive(Clone)]
pub struct Environment {
    env_ptr: Rc<RefCell<EnvironmentData>>
}

struct EnvironmentData {
    values: HashMap<String, Object>,
    enclosing: Option<Environment>
}

impl Environment {
    pub fn new() -> Self {
        let env_data = EnvironmentData {
            values: HashMap::new(),
            enclosing: None
        };
        Environment {
            env_ptr: Rc::new(RefCell::new(env_data))
        }
    }

    /// Returns an environment pointing to its enclosing environment.
    pub fn with_enclosing(env: &Environment) -> Self {
        let env_data = EnvironmentData {
            values: HashMap::new(),
            enclosing: Some(env.clone())
        };
        Environment {
            env_ptr: Rc::new(RefCell::new(env_data))
        }
    }

    /// Add or replace current definition.
    pub fn define(&mut self, name: String, value: Object) {
        self.env_ptr.borrow_mut().values.insert(name, value);
    }

    /// Set current variable.
    pub fn set(&mut self, name: String, value: Object) -> RuntimeResult<()> {
        let mut env_data = self.env_ptr.borrow_mut();

        if let Some(old_value_ref) = env_data.values.get_mut(&name) {
            *old_value_ref = value;
            return Ok(());
        }

        match env_data.enclosing {
            Some(ref mut env) => env.set(name, value),
            None => Err(InterpreterError::UndefinedVariable(name))
        }
    }

    /// Get variable value.
    pub fn get(&self, name: &str) -> RuntimeResult<Object> {
        let env_data = self.env_ptr.borrow();
        
        match env_data.values.get(name) {
            Some(obj) => Ok(obj.clone()),
            None => match env_data.enclosing {
                Some(ref env) => env.get(name),
                None => Err(InterpreterError::UndefinedVariable(name.to_owned())),
            }
        }
    }
}
