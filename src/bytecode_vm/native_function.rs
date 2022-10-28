use super::value::{NativeFnType, Value};

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
