use super::errors::{InterpreterError, RuntimeResult};
use super::interpreter::Interpreter;
use super::object::Object;
use std::fmt;

pub struct NativeFn {
    pub func: fn(Vec<Object>, &mut Interpreter) -> RuntimeResult<Object>,
    pub arity: usize,
    pub name: String,
}

impl NativeFn {
    pub fn name(&self) -> String {
        self.name.clone()
    }

    pub fn execute(
        &self,
        args: Vec<Object>,
        interpreter: &mut Interpreter,
    ) -> RuntimeResult<Object> {
        if self.arity == args.len() {
            (self.func)(args, interpreter)
        } else {
            Err(InterpreterError::WrongArity(self.arity, args.len()))
        }
    }
}

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native-func {}>", self.name)
    }
}

impl PartialEq<NativeFn> for NativeFn {
    // You cannot derive Eq for function pointers in Rust. Also, LLVM
    // can combine two different functions into one that have identical
    // bodies. Safest option is to compare by name for native funcs.
    fn eq(&self, other: &NativeFn) -> bool {
        self.name == other.name
    }
}

impl Eq for NativeFn {}

pub fn get_native_funcs() -> Vec<NativeFn> {
    vec![NativeFn {
        func: clock,
        arity: 0,
        name: "clock".to_owned(),
    }]
}

fn clock(_args: Vec<Object>, _interpreter: &mut Interpreter) -> RuntimeResult<Object> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("SystemTime before UNIX EPOCH.");

    return Ok(Object::Number(duration.as_secs() as f64));
}
