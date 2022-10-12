use super::errors::{InterpreterError, RuntimeResult};
use super::interpreter::Interpreter;
use super::object::Object;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

struct LoxClassData {
    name: String,
}

#[derive(Clone)]
pub struct LoxClassDataPtr(Rc<LoxClassData>);

pub struct LoxInstanceData {
    class: LoxClassDataPtr,
    properties: RefCell<HashMap<String, Object>>,
}

#[derive(Clone)]
pub struct LoxInstanceDataPtr(Rc<LoxInstanceData>);

impl LoxClassDataPtr {
    pub fn new(name: String) -> Self {
        let data = LoxClassData { name };
        LoxClassDataPtr(Rc::new(data))
    }

    pub fn execute(
        &self,
        args: Vec<Object>,
        interpreter: &mut Interpreter,
    ) -> RuntimeResult<Object> {
        if !args.is_empty() {
            return Err(InterpreterError::WrongArity(0, args.len()));
        }

        let instance = LoxInstanceDataPtr::new(self);
        Ok(Object::LoxInstance(instance))
    }
}

impl LoxInstanceDataPtr {
    pub fn new(class: &LoxClassDataPtr) -> Self {
        let data = LoxInstanceData {
            class: class.clone(),
            properties: RefCell::new(HashMap::new()),
        };
        LoxInstanceDataPtr(Rc::new(data))
    }

    pub fn get(&self, property: &str) -> RuntimeResult<Object> {
        match self.0.properties.borrow().get(property) {
            Some(value) => Ok(value.clone()),
            None => {
                let obj = Object::LoxInstance(self.clone());
                Err(InterpreterError::MissingProperty(obj, property.to_owned()))
            }
        }
    }

    pub fn set(&self, property: &str, value: Object) -> RuntimeResult<()> {
        self.0
            .properties
            .borrow_mut()
            .insert(property.to_owned(), value);
        Ok(())
    }
}

impl PartialEq<LoxClassDataPtr> for LoxClassDataPtr {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl PartialEq<LoxInstanceDataPtr> for LoxInstanceDataPtr {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for LoxClassDataPtr {}

impl Eq for LoxInstanceDataPtr {}

impl fmt::Debug for LoxClassDataPtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<class {}>", self.0.name)
    }
}

impl fmt::Debug for LoxInstanceDataPtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<instance of {}>", self.0.class.0.name)
    }
}
