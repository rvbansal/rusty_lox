use super::chunk::Chunk;
use super::gc::{GcPtr, Traceable};
use super::string_interner::StringIntern;

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

pub enum HeapObject {
    LoxClosure {
        name: StringIntern,
        arity: usize,
        chunk: Rc<Chunk>,
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
