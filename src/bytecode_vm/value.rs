use super::string_interner::StringIntern;
use std::fmt;
use std::{ops::Deref, rc::Rc};

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    HeapObject(Rc<Object>),
    String(StringIntern),
}

pub enum Object {}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }

    pub fn make_heap_object(obj: Object) -> Self {
        Value::HeapObject(Rc::new(obj))
    }
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(x), Value::Number(y)) => x == y,
            (Value::Boolean(x), Value::Boolean(y)) => x == y,
            (Value::Nil, Value::Nil) => true,
            (Value::HeapObject(x), Value::HeapObject(y)) => x == y,
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
            Value::HeapObject(obj) => write!(f, "(heap) {:?}", obj),
            Value::String(s) => s.fmt(f),
        }
    }
}

impl PartialEq<Object> for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            _ => unreachable!(),
        }
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            _ => unreachable!(),
        }
    }
}
