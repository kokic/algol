///|
test "MatrixZ snf report diagonalizes full-rank square matrix" {
  let a : MatrixZ = Matrix::from_rows([[4N, 6N], [3N, 9N]])
  let report = a.snf_report()

  assert_true(report.is_verified)
  assert_true(report.snf.is_snf())
  assert_eq(report.diagonal, [1N, 18N])
  assert_eq(report.rank, 2)
  assert_eq(report.left_unimodular.mul(report.original).mul(report.right_unimodular), report.snf)
  assert_true(report.trace.display(2).contains(": snf_report"))
}

///|
test "MatrixZ snf report handles rank-deficient rectangular matrix" {
  let a : MatrixZ = Matrix::from_rows([[2N, 4N, 6N], [4N, 8N, 12N]])
  let report = a.snf_report()

  assert_true(report.is_verified)
  assert_true(report.snf.is_snf())
  assert_eq(report.diagonal, [2N])
  assert_eq(report.rank, 1)
}

///|
test "MatrixZ snf report handles all-zero matrix" {
  let a : MatrixZ = Matrix::from_rows([[0N, 0N], [0N, 0N]])
  let report = a.snf_report()

  assert_true(report.is_verified)
  assert_true(report.snf.is_snf())
  assert_eq(report.diagonal, [])
  assert_eq(report.rank, 0)
  assert_eq(a.snf(), report.snf)
}

///|
test "MatrixZ is_snf rejects non-canonical forms" {
  let bad_divisibility : MatrixZ = Matrix::from_rows([[2N, 0N], [0N, 3N]])
  assert_false(bad_divisibility.is_snf())

  let bad_off_diagonal : MatrixZ = Matrix::from_rows([[1N, 1N], [0N, 2N]])
  assert_false(bad_off_diagonal.is_snf())
}
