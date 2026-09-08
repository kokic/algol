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
  str_abs,
}

///|
using @writer {type TraceValue, type Writer, field, sequence, step}
