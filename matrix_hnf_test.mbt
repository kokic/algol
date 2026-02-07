///|
test "MatrixZ hnf report verifies square full-rank input" {
  let a : MatrixZ = Matrix::from_rows([[2N, 4N], [6N, 8N]])
  let report = a.hnf_report()

  assert_true(report.is_verified)
  assert_true(report.hnf.is_row_hnf())
  assert_eq(report.pivot_cols, [0, 1])
  assert_eq(report.hnf, Matrix::from_rows([[2N, 0N], [0N, 4N]]))
  assert_eq(report.left_unimodular.mul(report.original), report.hnf)
  assert_true(report.trace.display(2).contains(": hnf_report"))
}

///|
test "MatrixZ hnf report handles rank-deficient rectangular matrix" {
  let a : MatrixZ = Matrix::from_rows([[1N, 2N, 3N], [2N, 4N, 6N]])
  let report = a.hnf_report()

  assert_true(report.is_verified)
  assert_eq(report.pivot_cols, [0])
  assert_eq(report.hnf, Matrix::from_rows([[1N, 2N, 3N], [0N, 0N, 0N]]))
  assert_eq(a.hnf(), report.hnf)
}

///|
test "MatrixZ hnf report handles all-zero matrix" {
  let a : MatrixZ = Matrix::from_rows([[0N, 0N, 0N], [0N, 0N, 0N]])
  let report = a.hnf_report()

  assert_true(report.is_verified)
  assert_eq(report.pivot_cols, [])
  assert_eq(report.hnf, a)
  assert_true(report.hnf.is_row_hnf())
}

///|
test "MatrixZ is_row_hnf rejects non-canonical forms" {
  let good : MatrixZ = Matrix::from_rows([[2N, 1N, 0N], [0N, 3N, 2N], [0N, 0N, 0N]])
  assert_true(good.is_row_hnf())

  let bad_above : MatrixZ = Matrix::from_rows([[2N, 3N], [0N, 3N]])
  assert_false(bad_above.is_row_hnf())

  let bad_zero_row_order : MatrixZ = Matrix::from_rows([[0N, 0N], [1N, 0N]])
  assert_false(bad_zero_row_order.is_row_hnf())
}

///|
test "Matrix generic transpose works for non-numeric element type" {
  let a : Matrix[String] = Matrix::from_rows([["a", "b"], ["c", "d"]])
  let expected : Matrix[String] = Matrix::from_rows([["a", "c"], ["b", "d"]])
  assert_eq(a.transpose(), expected)
}

///|
test "Matrix generic multiplication works over BigRational" {
  let half = @rational.new(1N, 2N).unwrap()
  let two = @rational.new(2N, 1N).unwrap()
  let three = @rational.new(3N, 1N).unwrap()
  let seven = @rational.new(7N, 1N).unwrap()

  let a : Matrix[@rational.BigRational] = Matrix::from_rows([[half, two]])
  let b : Matrix[@rational.BigRational] = Matrix::from_rows([[two], [three]])
  let expected : Matrix[@rational.BigRational] = Matrix::from_rows([[seven]])
  assert_eq(a.mul(b), expected)
}
