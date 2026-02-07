///|
fn approx_eq_double(a : Double, b : Double, eps : Double) -> Bool {
  (a - b).abs() <= eps
}

///|
fn matmul_double(a : Matrix[Double], b : Matrix[Double]) -> Matrix[Double] raise {
  assert_true(
    a.cols == b.rows,
    msg="dimension mismatch: \{a.rows}x\{a.cols} times \{b.rows}x\{b.cols}",
  )
  let data = Array::makei(a.rows, i => {
    Array::makei(b.cols, j => {
      let mut acc = 0.0
      for k in 0..<a.cols {
        acc = acc + a.data[i][k] * b.data[k][j]
      }
      acc
    })
  })
  Matrix::from_rows(data)
}

///|
fn approx_eq_matrix(a : Matrix[Double], b : Matrix[Double], eps : Double) -> Bool {
  guard a.rows == b.rows && a.cols == b.cols else { false }
  for i in 0..<a.rows {
    for j in 0..<a.cols {
      if !approx_eq_double(a.data[i][j], b.data[i][j], eps) {
        return false
      }
    }
  }
  true
}

///|
test "Matrix[Double] positive-definite check distinguishes SPD from others" {
  let spd : Matrix[Double] = Matrix::from_rows([[4.0, 2.0], [2.0, 3.0]])
  let nonsym : Matrix[Double] = Matrix::from_rows([[1.0, 2.0], [0.0, 1.0]])
  let semidef : Matrix[Double] = Matrix::from_rows([[1.0, 1.0], [1.0, 1.0]])

  assert_true(spd.is_positive_definite())
  assert_false(nonsym.is_positive_definite())
  assert_false(semidef.is_positive_definite())
}

///|
test "Matrix[Double] cholesky reconstructs original matrix" {
  let a : Matrix[Double] = Matrix::from_rows([[4.0, 2.0], [2.0, 3.0]])
  let l = a.cholesky()
  let reconstructed = matmul_double(l, l.transpose())

  // L should be lower triangular.
  assert_true(approx_eq_double(l.data[0][1], 0.0, 0.0000000001))
  assert_true(approx_eq_matrix(reconstructed, a, 0.000000001))
}

///|
test "Matrix[Double] cholesky raises on non-positive-definite matrix" {
  let bad : Matrix[Double] = Matrix::from_rows([[1.0, 2.0], [2.0, 1.0]])
  let result : Result[Matrix[Double], Error] = try? bad.cholesky()
  assert_true(result is Err(_))
}
