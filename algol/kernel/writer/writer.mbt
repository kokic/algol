///|
pub enum TraceValue {
  String(String)
  Int(Int)
  Bool(Bool)
  Redacted
} derive(Debug)

///|
pub extend TraceValue with Debug::{to_repr}

///|
pub fn TraceValue::string(value : String) -> TraceValue {
  String(value)
}

///|
pub fn TraceValue::integer(value : Int) -> TraceValue {
  Int(value)
}

///|
pub fn TraceValue::boolean(value : Bool) -> TraceValue {
  Bool(value)
}

///|
pub fn TraceValue::redacted() -> TraceValue {
  Redacted
}

///|
/// A pure, append-only diagnostic trace.
///
/// `Append` represents an ordered sibling sequence. `Step` is the only
/// constructor that introduces nesting.
pub enum Trace {
  Empty
  Text(String)
  Field(String, TraceValue)
  Step(String, Trace)
  Append(Trace, Trace)
} derive(Debug)

///|
pub extend Trace with Debug::{to_repr}

///|
/// A computed value paired with its immutable diagnostic trace.
pub struct Writer[A] {
  value : A
  trace : Trace
} derive(Debug)

///|
pub extend Writer with Debug::{to_repr}

///|
pub fn Trace::empty() -> Trace {
  Empty
}

///|
pub fn Trace::append(self : Trace, other : Trace) -> Trace {
  match (self, other) {
    (Empty, trace) => trace
    (trace, Empty) => trace
    (left, right) => Append(left, right)
  }
}

///|
pub fn sequence(parts : Array[Trace]) -> Trace {
  parts.fold(init=Trace::empty(), (trace, part) => trace.append(part))
}

///|
pub fn text(content : String) -> Trace {
  Text(content)
}

///|
pub fn field(name : String, value : TraceValue) -> Trace {
  Field(name, value)
}

///|
pub fn step(name : String, children : Trace) -> Trace {
  Step(name, children)
}

///|
fn escape_for_display(value : String) -> String {
  value.to_array().map(char => char.escape(quote=false)).join("")
}

///|
fn TraceValue::display(self : TraceValue) -> String {
  match self {
    String(value) => escape_for_display(value)
    Int(value) => value.to_string()
    Bool(value) => value.to_string()
    Redacted => "[REDACTED]"
  }
}

///|
fn Trace::collect_lines(
  self : Trace,
  depth : Int,
  lines : Array[String],
) -> Unit {
  let indent = " ".repeat(depth * 2)
  match self {
    Empty => ()
    Text(content) => lines.push(indent + escape_for_display(content))
    Field(name, value) =>
      lines.push("\{indent}[\{escape_for_display(name)}]: \{value.display()}")
    Step(name, children) => {
      lines.push("\{indent}: \{escape_for_display(name)}")
      children.collect_lines(depth + 1, lines)
    }
    Append(left, right) => {
      left.collect_lines(depth, lines)
      right.collect_lines(depth, lines)
    }
  }
}

///|
/// Renders a trace with a fixed two-space indentation and escaped text fields.
pub fn Trace::display(self : Trace) -> String {
  let lines = []
  self.collect_lines(0, lines)
  lines.join("\n")
}

///|
pub fn[A] Writer::new(value : A, trace : Trace) -> Writer[A] {
  Writer::{ value, trace, }
}

///|
pub fn[A] Writer::pure(value : A) -> Writer[A] {
  Writer::{ value, trace: Trace::empty(), }
}

///|
pub fn[A, B] Writer::map(self : Writer[A], transform : (A) -> B) -> Writer[B] {
  Writer::{ value: transform(self.value), trace: self.trace, }
}

///|
pub fn[A, B] Writer::flat_map(
  self : Writer[A],
  transform : (A) -> Writer[B],
) -> Writer[B] {
  let next = transform(self.value)
  Writer::{ value: next.value, trace: self.trace.append(next.trace), }
}

///|
pub fn[A] Writer::with_step(self : Writer[A], name : String) -> Writer[A] {
  Writer::{ value: self.value, trace: step(name, self.trace), }
}
