use super::chunk::Chunk;
use super::gc::Gc;
use super::native_function::NativeFn;
use super::string_interner::StringIntern;

use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    String(StringIntern),
    Closure(Gc<LoxClosure>),
    NativeFn(NativeFn),
    Class(Gc<LoxClass>),
    Instance(Gc<LoxInstance>),
    BoundMethod(Gc<LoxBoundMethod>),
}

#[derive(Debug)]
pub struct LoxClosure {
    pub name: StringIntern,
    pub arity: usize,
    pub chunk: Rc<Chunk>,
    pub upvalues: Rc<[Gc<Upvalue>]>,
}

#[derive(Debug)]
pub struct LoxClass {
    pub name: StringIntern,
    pub methods: HashMap<StringIntern, Gc<LoxClosure>>,
}

#[derive(Debug)]
pub struct LoxInstance {
    pub class: Gc<LoxClass>,
    pub fields: HashMap<StringIntern, Value>,
}

#[derive(Debug)]
pub struct LoxBoundMethod {
    pub receiver: Gc<LoxInstance>,
    pub closure: Gc<LoxClosure>,
}

pub enum PropertySearch {
    Field(Value),
    Method(Gc<LoxClosure>),
    Missing,
}

#[derive(Debug, Clone)]
pub enum Upvalue {
    Open(usize),   // Lives on the stack
    Closed(Value), // Popped off the stack to the heap
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }

    pub fn to_class(&self) -> Option<Gc<LoxClass>> {
        match self {
            Value::Class(ptr) => Some(*ptr),
            _ => None,
        }
    }

    pub fn to_instance(&self) -> Option<Gc<LoxInstance>> {
        match self {
            Value::Instance(ptr) => Some(*ptr),
            _ => None,
        }
    }

    pub fn to_closure(&self) -> Option<Gc<LoxClosure>> {
        match self {
            Value::Closure(ptr) => Some(*ptr),
            _ => None,
        }
    }
}
