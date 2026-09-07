///|
test "Trace append has stable sibling order" {
  let first = text("first")
  let second = text("second")
  let third = field("count", TraceValue::integer(3))
  let left = first.append(second).append(third)
  let right = first.append(second.append(third))
  @test.assert_eq(left.display(), right.display())
  assert_true(left.display().contains("first\nsecond\n[count]: 3"))
}

///|
test "Trace display escapes control characters" {
  let trace = step(
    "outer\nstep",
    sequence([
      field("input\tname", TraceValue::string("line one\nline two")),
      field("enabled", TraceValue::boolean(true)),
      field("secret", TraceValue::redacted()),
    ]),
  )
  let rendered = trace.display()
  assert_true(rendered.contains("\\n"))
  assert_true(rendered.contains("\\t"))
  assert_true(!rendered.contains("line one\nline two"))
  assert_true(rendered.contains("[REDACTED]"))
}

///|
test "Writer preserves values and combines traces" {
  let result = Writer::pure(1)
    .flat_map(value => Writer::new(value + 1, text("increment")))
    .with_step("calculation")
  @test.assert_eq(result.value, 2)
  assert_true(result.trace.display().contains(": calculation"))
  assert_true(result.trace.display().contains("increment"))
}
