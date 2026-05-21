///|
pub struct Tracing[M] {
  mut msg : M
  stack : Array[M]
}

///|
pub fn[M : Default] Tracing::new() -> Tracing[M] {
  Tracing::{ msg: M::default(), stack: [] }
}

///|
pub fn[M] Tracing::set(tracing : Tracing[M], value : M) -> Unit {
  tracing.msg = value
}

///|
pub fn[M] Tracing::snapshot(tracing : Tracing[M]) -> M {
  tracing.msg
}

///|
pub fn[M : Default] Tracing::reset(tracing : Tracing[M]) -> Unit {
  tracing.msg = M::default()
  tracing.stack.clear()
}

///|
pub fn[M : Default] Tracing::take(tracing : Tracing[M]) -> M {
  let msg = tracing.msg
  tracing.msg = M::default()
  msg
}

///|
pub fn[M : Add] Tracing::add(tracing : Tracing[M], value : M) -> Unit {
  tracing.msg = tracing.msg.add(value)
}

///|
pub fn[M : Default] Tracing::enter(tracing : Tracing[M]) -> Unit {
  tracing.stack.push(tracing.msg)
  tracing.msg = M::default()
}

///|
pub fn[M : Default] Tracing::discard(tracing : Tracing[M]) -> Unit {
  match tracing.stack.pop() {
    Some(parent) => tracing.msg = parent
    None => tracing.msg = M::default()
  }
}

///|
pub fn[M] Tracing::leave(tracing : Tracing[M], combine : (M, M) -> M) -> Unit {
  let child = tracing.msg
  match tracing.stack.pop() {
    Some(parent) => tracing.msg = combine(parent, child)
    None => tracing.msg = child
  }
}

///|
test "Tracing accumulates, scopes, and resets messages" {
  let trace : Tracing[String] = Tracing::new()
  trace.add("a")
  trace.enter()
  trace.add("b")
  trace.leave(fn(parent, child) { parent + "[" + child + "]" })
  assert_eq(trace.snapshot(), "a[b]")
  assert_eq(trace.take(), "a[b]")
  assert_eq(trace.snapshot(), "")
  trace.add("x")
  trace.enter()
  trace.add("ignored")
  trace.discard()
  assert_eq(trace.snapshot(), "x")
  trace.reset()
  assert_eq(trace.snapshot(), "")
}
