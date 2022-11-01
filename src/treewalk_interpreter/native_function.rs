use super::errors::{InterpreterError, RuntimeResult};
use super::interpreter::Interpreter;
use super::object::Object;
use std::fmt;
use std::rc::Rc;

type FnType = fn(Vec<Object>) -> RuntimeResult<Object>;

pub struct NativeFnData {
    pub func: FnType,
    pub arity: usize,
    pub name: String,
}

#[derive(Clone)]
pub struct NativeFn(Rc<NativeFnData>);

impl NativeFn {
    fn new(name: &str, func: FnType, arity: usize) -> Self {
        let name = name.to_owned();
        let data = NativeFnData { func, arity, name };
        NativeFn(Rc::new(data))
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }

    pub fn execute<S: std::io::Write>(
        &self,
        args: Vec<Object>,
        _interpreter: &mut Interpreter<S>,
    ) -> RuntimeResult<Object> {
        if self.0.arity == args.len() {
            (self.0.func)(args)
        } else {
            Err(InterpreterError::WrongArity(self.0.arity, args.len()))
        }
    }
}

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native-func {}>", self.0.name)
    }
}

impl PartialEq<NativeFn> for NativeFn {
    // You cannot derive Eq for function pointers in Rust. Also, LLVM
    // can combine two different functions into one that have identical
    // bodies. Wrap function pointer in Rc and compare the Rcs.
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for NativeFn {}

pub fn get_native_funcs() -> Vec<NativeFn> {
    vec![NativeFn::new("clock", clock, 0)]
}

fn clock(_args: Vec<Object>) -> RuntimeResult<Object> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("SystemTime before UNIX EPOCH.");

    Ok(Object::Number(duration.as_secs() as f64))
}
