use super::chunk::Chunk;
use super::gc::{GcPtr, Traceable};
use super::string_interner::StringIntern;

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    Object(GcPtr<HeapObject>),
    String(StringIntern),
}

pub type NativeFnType = fn(&[Value]) -> Result<Value, String>;

#[derive(Clone)]
pub enum UpvalueType {
    Open(usize),   // Lives on the stack
    Closed(Value), // Popped off the stack to the heap
}

#[derive(Clone)]
pub struct ActiveUpvalue {
    location: Rc<RefCell<UpvalueType>>,
}

pub enum HeapObject {
    LoxClosure {
        name: StringIntern,
        arity: usize,
        chunk: Rc<Chunk>,
        upvalues: Rc<Vec<ActiveUpvalue>>,
    },
    NativeFn {
        name: StringIntern,
        arity: usize,
        function: NativeFnType,
    },
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(x), Value::Number(y)) => x == y,
            (Value::Boolean(x), Value::Boolean(y)) => x == y,
            (Value::Nil, Value::Nil) => true,
            (Value::Object(x), Value::Object(y)) => x == y,
            (Value::String(x), Value::String(y)) => x == y,
            _ => false,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(n) => n.fmt(f),
            Value::Boolean(b) => b.fmt(f),
            Value::Nil => write!(f, "nil"),
            Value::Object(obj) => write!(f, "(heap) {:?}", obj),
            Value::String(s) => s.fmt(f),
        }
    }
}

impl Traceable for HeapObject {
    fn trace(&self) {
        match self {
            HeapObject::LoxClosure { .. } => {}
            HeapObject::NativeFn { .. } => {}
        }
    }
}

impl ActiveUpvalue {
    pub fn new(index: usize) -> Self {
        ActiveUpvalue {
            location: Rc::new(RefCell::new(UpvalueType::Open(index))),
        }
    }

    pub fn close(&self, value: Value) {
        self.location.replace(UpvalueType::Closed(value));
    }

    pub fn get_if_closed(&self) -> Result<Value, usize> {
        match &*self.location.borrow() {
            UpvalueType::Open(i) => Err(*i),
            UpvalueType::Closed(v) => Ok(v.clone()),
        }
    }

    pub fn set_if_closed(&self, value: &Value) -> Result<(), usize> {
        match &mut *self.location.borrow_mut() {
            UpvalueType::Open(i) => Err(*i),
            UpvalueType::Closed(v) => {
                *v = value.clone();
                Ok(())
            }
        }
    }

    pub fn get_open_index(&self) -> Option<usize> {
        match &*self.location.borrow() {
            UpvalueType::Open(index) => Some(*index),
            UpvalueType::Closed(_) => None,
        }
    }
}
