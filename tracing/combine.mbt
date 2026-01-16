///|
pub struct Tracing[M] {
  mut msg : M
}

///|
pub fn[M : Default] Tracing::new() -> Tracing[M] {
  Tracing::{ msg: M::default() }
}

///|
fn[M] Tracing::update(tracing : Tracing[M], value : M) -> Unit {
  tracing.msg = value
}

pub fn[M : Add] Tracing::add(tracing : Tracing[M], value : M) -> Unit {
  tracing.update(tracing.msg.add(value))
}
