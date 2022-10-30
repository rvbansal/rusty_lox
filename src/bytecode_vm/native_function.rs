use super::value::Value;
use std::rc::Rc;

pub type NativeFnType = fn(&[Value]) -> Result<Value, String>;

#[derive(Clone)]
pub struct NativeFn {
    pub data: Rc<NativeFnData>,
}

pub struct NativeFnData {
    pub name: String,
    pub arity: usize,
    pub func: NativeFnType,
}

impl NativeFn {
    pub fn new(name: &str, arity: usize, func: NativeFnType) -> Self {
        NativeFn {
            data: Rc::new(NativeFnData {
                name: name.to_owned(),
                arity,
                func,
            }),
        }
    }
}

impl PartialEq for NativeFn {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.data, &other.data)
    }
}

impl Eq for NativeFn {}

pub fn get_native_fns() -> &'static [(&'static str, usize, NativeFnType)] {
    &[("clock", 0, clock)]
}

fn clock(_args: &[Value]) -> Result<Value, String> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("System time before UNIX EPOCH.");

    Ok(Value::Number(duration.as_secs_f64()))
}
