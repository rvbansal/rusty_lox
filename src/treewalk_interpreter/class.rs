use super::errors::{InterpreterError, RuntimeResult};
use super::function::LoxFn;
use super::interpreter::Interpreter;
use super::object::Object;
use crate::lox_frontend::constants::INIT_STR;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

struct LoxClassData {
    name: String,
    superclass: Option<LoxClassDataPtr>,
    methods: HashMap<String, LoxFn>,
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
    pub fn new(
        name: String,
        superclass: Option<LoxClassDataPtr>,
        methods: HashMap<String, LoxFn>,
    ) -> Self {
        let data = LoxClassData {
            name,
            superclass,
            methods,
        };
        LoxClassDataPtr(Rc::new(data))
    }

    pub fn find_method(&self, name: &str) -> Option<LoxFn> {
        let method = self.0.methods.get(name).cloned();
        match &self.0.superclass {
            Some(superclass) => method.or_else(|| superclass.find_method(name)),
            None => method,
        }
    }

    pub fn execute(
        &self,
        args: Vec<Object>,
        interpreter: &mut Interpreter,
    ) -> RuntimeResult<Object> {
        if self.arity() != args.len() {
            return Err(InterpreterError::WrongArity(self.arity(), args.len()));
        }

        let instance = LoxInstanceDataPtr::new(self);
        if let Some(init) = instance.find_bound_method(INIT_STR) {
            init.execute(args, interpreter)?;
        }

        Ok(Object::LoxInstance(instance))
    }

    pub fn arity(&self) -> usize {
        match self.0.methods.get(INIT_STR) {
            Some(init) => init.arity(),
            None => 0,
        }
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

    pub fn get(&self, name: &str) -> RuntimeResult<Object> {
        if let Some(obj) = self.find_property(name) {
            return Ok(obj);
        }
        if let Some(method) = self.find_bound_method(name) {
            return Ok(method);
        }

        let self_instance = Object::LoxInstance(self.clone());
        Err(InterpreterError::MissingProperty(
            self_instance,
            name.to_owned(),
        ))
    }

    pub fn set(&self, property: &str, value: Object) {
        self.0
            .properties
            .borrow_mut()
            .insert(property.to_owned(), value);
    }

    fn find_property(&self, name: &str) -> Option<Object> {
        self.0.properties.borrow().get(name).cloned()
    }

    fn find_bound_method(&self, name: &str) -> Option<Object> {
        match self.0.class.find_method(name) {
            Some(method) => {
                let self_instance = Object::LoxInstance(self.clone());
                let bound_method = method.bind(self_instance);
                Some(Object::LoxFunc(bound_method))
            }
            None => None,
        }
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
