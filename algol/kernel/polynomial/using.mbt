///|
using @core {join, to_super_string}

///|
using @number {
  trait Semiring,
  trait Ring,
  trait Field,
  trait ToUnicode,
  rat_zero,
  rat_one,
  rational_root_candidates,
  is_root,
  gcd,
}

///|
using @trace {type Message, label, step}
