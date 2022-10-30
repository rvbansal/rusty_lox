use super::chunk::Chunk;
use super::gc::{GcPtr, Traceable};
use super::native_function::NativeFn;
use super::string_interner::StringIntern;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    String(StringIntern),
    Closure(GcPtr<LoxClosure>),
    NativeFn(NativeFn),
    Class(GcPtr<LoxClass>),
    Instance(GcPtr<LoxInstance>),
    BoundMethod(GcPtr<LoxBoundMethod>),
}

pub struct LoxClosure {
    pub name: StringIntern,
    pub arity: usize,
    pub chunk: Rc<Chunk>,
    pub upvalues: Rc<Vec<ActiveUpvalue>>,
}

pub struct LoxClass {
    pub name: StringIntern,
    pub methods: HashMap<StringIntern, GcPtr<LoxClosure>>,
}

pub struct LoxInstance {
    pub class: GcPtr<LoxClass>,
    pub fields: HashMap<StringIntern, Value>,
}

pub struct LoxBoundMethod {
    pub receiver: GcPtr<LoxInstance>,
    pub closure: GcPtr<LoxClosure>,
}

#[derive(Clone)]
pub enum UpvalueType {
    Open(usize),   // Lives on the stack
    Closed(Value), // Popped off the stack to the heap
}

#[derive(Clone)]
pub struct ActiveUpvalue {
    location: Rc<RefCell<UpvalueType>>,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }

    pub fn mark_internals(&self) {
        match self {
            Value::Number(_) | Value::Boolean(_) | Value::Nil | Value::String(_) => {}
            Value::Closure(ptr) => ptr.mark(),
            Value::NativeFn(_) => {}
            Value::Class(ptr) => ptr.mark(),
            Value::Instance(ptr) => ptr.mark(),
            Value::BoundMethod(ptr) => ptr.mark(),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        macro_rules! fmt_or_garbage {
            ($ptr:expr, $f:expr) => {
                match &*$ptr.try_borrow() {
                    Some(obj) => obj.fmt($f),
                    None => write!($f, "<garbage>"),
                }
            };
        }

        match self {
            Value::Number(n) => n.fmt(f),
            Value::Boolean(b) => b.fmt(f),
            Value::Nil => write!(f, "nil"),
            Value::String(s) => s.fmt(f),
            Value::Closure(ptr) => fmt_or_garbage!(ptr, f),
            Value::NativeFn(func) => func.fmt(f),
            Value::Class(ptr) => fmt_or_garbage!(ptr, f),
            Value::Instance(ptr) => fmt_or_garbage!(ptr, f),
            Value::BoundMethod(ptr) => fmt_or_garbage!(ptr, f),
        }
    }
}

impl fmt::Debug for LoxClosure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<fn {}>", self.data.name)
    }
}

impl fmt::Debug for LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Debug for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "instance of {:?}", self.class.borrow())
    }
}

impl fmt::Debug for LoxBoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.closure.borrow().fmt(f)
    }
}

impl Traceable for LoxClosure {
    fn trace(&self) {
        for uv in self.upvalues.iter() {
            uv.mark_internals();
        }
    }
}

impl Traceable for LoxClass {
    fn trace(&self) {
        for method in self.methods.values() {
            method.mark();
        }
    }
}

impl Traceable for LoxInstance {
    fn trace(&self) {
        self.class.mark();
        for field in self.fields.values() {
            field.mark_internals();
        }
    }
}

impl Traceable for LoxBoundMethod {
    fn trace(&self) {
        self.receiver.mark();
        self.closure.mark();
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

    pub fn mark_internals(&self) {
        match &*self.location.borrow() {
            UpvalueType::Open(_) => {}
            UpvalueType::Closed(value) => value.mark_internals(),
        }
    }
}
