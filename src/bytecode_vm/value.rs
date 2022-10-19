use std::fmt;
use std::{ops::Deref, rc::Rc};

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    HeapObject(Rc<Object>),
}

pub enum Object {
    String(String),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }

    pub fn make_heap_object(obj: Object) -> Self {
        Value::HeapObject(Rc::new(obj))
    }

    pub fn add(&self, other: &Self) -> Option<Value> {
        let obj = match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs + rhs),
            (Value::HeapObject(lhs), Value::HeapObject(rhs)) => match (lhs.deref(), rhs.deref()) {
                (Object::String(l), Object::String(r)) => {
                    let obj = Object::String(l.clone() + r);
                    Value::make_heap_object(obj)
                }
            },
            _ => return None,
        };

        Some(obj)
    }
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(x), Value::Number(y)) => x == y,
            (Value::Boolean(x), Value::Boolean(y)) => x == y,
            (Value::Nil, Value::Nil) => true,
            (Value::HeapObject(x), Value::HeapObject(y)) => x == y,
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
        }
    }
}

impl PartialEq<Object> for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::String(s), Object::String(t)) => s == t,
        }
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::String(s) => write!(f, "\"{}\"", s),
        }
    }
}
