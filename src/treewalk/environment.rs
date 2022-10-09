use super::errors::{InterpreterError, RuntimeResult};
use super::object::Object;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub struct Environment {
    values: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    /// Add or replace current definition.
    pub fn define(&mut self, name: String, value: Object) {
        self.values.insert(name, value);
    }

    /// Set current variable.
    pub fn set(&mut self, name: String, value: Object) -> RuntimeResult<()> {
        match self.values.entry(name) {
            Entry::Occupied(mut o) => {
                o.insert(value);
                Ok(())
            }
            Entry::Vacant(v) => Err(InterpreterError::UndefinedVariable(v.key().clone())),
        }
    }

    /// Get variable value.
    pub fn get(&self, name: &str) -> RuntimeResult<Object> {
        match self.values.get(name) {
            Some(obj) => Ok(obj.clone()),
            None => Err(InterpreterError::UndefinedVariable(name.to_owned())),
        }
    }
}
