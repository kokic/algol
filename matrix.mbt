///|
pub struct Matrix[T] {
  rows : Int
  cols : Int
  data : Array[Array[T]]
} derive(Show, Eq)

///|
pub type MatrixZ = Matrix[BigInt]

///|
pub struct HNFReport {
  original : MatrixZ
  hnf : MatrixZ
  left_unimodular : MatrixZ
  pivot_cols : Array[Int]
  is_verified : Bool
  trace : Message
} derive(Show)

///|
pub fn[T] Matrix::from_rows(rows : Array[Array[T]]) -> Matrix[T] raise {
  let row_count = rows.length()
  guard row_count > 0 else {
    return Matrix::{ rows: 0, cols: 0, data: [] }
  }
  let col_count = rows[0].length()
  for i in 1..<row_count {
    assert_true(
      rows[i].length() == col_count,
      msg="inconsistent row length at index \{i}",
    )
  }
  let data = Array::makei(row_count, i => Array::makei(col_count, j => rows[i][j]))
  Matrix::{ rows: row_count, cols: col_count, data }
}

///|
pub fn[T] Matrix::transpose(a : Matrix[T]) -> Matrix[T] {
  let data = Array::makei(a.cols, j => Array::makei(a.rows, i => a.data[i][j]))
  Matrix::{ rows: a.cols, cols: a.rows, data }
}

///|
pub fn[T : Semiring] Matrix::identity(n : Int) -> Matrix[T] raise {
  assert_true(n >= 0, msg="matrix size must be non-negative")
  let data = Array::makei(
    n,
    i => Array::makei(n, j => if i == j { T::one() } else { T::zero() }),
  )
  Matrix::{ rows: n, cols: n, data }
}

///|
pub fn[T : Add] Matrix::add(a : Matrix[T], b : Matrix[T]) -> Matrix[T] raise {
  assert_true(
    a.rows == b.rows && a.cols == b.cols,
    msg="dimension mismatch: \{a.rows}x\{a.cols} plus \{b.rows}x\{b.cols}",
  )
  let data = Array::makei(a.rows, i => Array::makei(a.cols, j => a.data[i][j] + b.data[i][j]))
  Matrix::{ rows: a.rows, cols: a.cols, data }
}

///|
pub fn[T : Semiring] Matrix::mul(a : Matrix[T], b : Matrix[T]) -> Matrix[T] raise {
  assert_true(
    a.cols == b.rows,
    msg="dimension mismatch: \{a.rows}x\{a.cols} times \{b.rows}x\{b.cols}",
  )
  let data = Array::makei(a.rows, i => {
    Array::makei(b.cols, j => {
      let mut acc = T::zero()
      for k in 0..<a.cols {
        acc = acc + a.data[i][k] * b.data[k][j]
      }
      acc
    })
  })
  Matrix::{ rows: a.rows, cols: b.cols, data }
}

///|
fn[T] clone_matrix(mat : Matrix[T]) -> Matrix[T] {
  Matrix::{
    rows: mat.rows,
    cols: mat.cols,
    data: Array::makei(mat.rows, i => Array::makei(mat.cols, j => mat.data[i][j])),
  }
}

///|
fn[T : Eq] matrix_equal(a : Matrix[T], b : Matrix[T]) -> Bool {
  guard a.rows == b.rows && a.cols == b.cols else { false }
  for i in 0..<a.rows {
    for j in 0..<a.cols {
      if a.data[i][j] != b.data[i][j] {
        return false
      }
    }
  }
  true
}

///|
fn swap_rows(mat : MatrixZ, i : Int, j : Int) -> Unit {
  guard i != j else { return }
  let tmp = mat.data[i]
  mat.data[i] = mat.data[j]
  mat.data[j] = tmp
}

///|
fn row_pair_transform(
  mat : MatrixZ,
  i : Int,
  j : Int,
  a : BigInt,
  b : BigInt,
  c : BigInt,
  d : BigInt,
) -> Unit {
  let row_i = Array::makei(mat.cols, k => mat.data[i][k])
  let row_j = Array::makei(mat.cols, k => mat.data[j][k])
  for k in 0..<mat.cols {
    mat.data[i][k] = a * row_i[k] + b * row_j[k]
    mat.data[j][k] = c * row_i[k] + d * row_j[k]
  }
}

///|
fn row_add_multiple(mat : MatrixZ, target : Int, source : Int, scalar : BigInt) -> Unit {
  guard scalar != 0N else { return }
  for k in 0..<mat.cols {
    mat.data[target][k] = mat.data[target][k] + scalar * mat.data[source][k]
  }
}

///|
fn row_negate(mat : MatrixZ, row : Int) -> Unit {
  for k in 0..<mat.cols {
    mat.data[row][k] = -mat.data[row][k]
  }
}

///|
fn egcd(a : BigInt, b : BigInt) -> (BigInt, BigInt, BigInt) {
  let mut old_r = a
  let mut r = b
  let mut old_s = 1N
  let mut s = 0N
  let mut old_t = 0N
  let mut t = 1N
  while r != 0N {
    let q = old_r / r
    let next_r = old_r - q * r
    let next_s = old_s - q * s
    let next_t = old_t - q * t
    old_r = r
    r = next_r
    old_s = s
    s = next_s
    old_t = t
    t = next_t
  }
  if old_r < 0N {
    (-old_r, -old_s, -old_t)
  } else {
    (old_r, old_s, old_t)
  }
}

///|
fn euclid_divmod(a : BigInt, b : BigInt) -> (BigInt, BigInt) {
  // Preconditions: b > 0
  let mut q = a / b
  let mut r = a % b
  if r < 0N {
    q = q - 1
    r = r + b
  }
  (q, r)
}

///|
pub fn Matrix::is_row_hnf(mat : MatrixZ) -> Bool {
  let mut prev_pivot_col = -1
  let mut found_zero_row = false
  for i in 0..<mat.rows {
    let mut pivot_col : Int? = None
    for j in 0..<mat.cols {
      if mat.data[i][j] != 0N {
        pivot_col = Some(j)
        break
      }
    }
    match pivot_col {
      None => found_zero_row = true
      Some(pc) => {
        if found_zero_row {
          return false
        }
        if pc <= prev_pivot_col {
          return false
        }
        let pivot = mat.data[i][pc]
        if pivot <= 0N {
          return false
        }
        // entries below pivot are zero
        for r in (i + 1)..<mat.rows {
          if mat.data[r][pc] != 0N {
            return false
          }
        }
        // entries above pivot satisfy 0 <= x < pivot
        for r in 0..<i {
          let x = mat.data[r][pc]
          if x < 0N || x >= pivot {
            return false
          }
        }
        prev_pivot_col = pc
      }
    }
  }
  true
}

///|
pub fn Matrix::hnf_report(a : MatrixZ) -> HNFReport raise {
  let h = clone_matrix(a)
  let u : MatrixZ = Matrix::identity(a.rows)
  let pivot_cols = []
  let mut row = 0

  for col in 0..<h.cols {
    guard row < h.rows else { break }

    let mut pivot_row : Int? = None
    for r in row..<h.rows {
      if h.data[r][col] != 0N {
        pivot_row = Some(r)
        break
      }
    }
    guard pivot_row is Some(pr) else { continue }

    swap_rows(h, row, pr)
    swap_rows(u, row, pr)

    // Euclidean elimination below the pivot.
    for r in (row + 1)..<h.rows {
      while h.data[r][col] != 0N {
        let a0 = h.data[row][col]
        let b0 = h.data[r][col]
        let (g, s, t) = egcd(a0, b0)
        let c = -(b0 / g)
        let d = a0 / g
        row_pair_transform(h, row, r, s, t, c, d)
        row_pair_transform(u, row, r, s, t, c, d)
      }
    }

    if h.data[row][col] < 0N {
      row_negate(h, row)
      row_negate(u, row)
    }

    // Reduce entries above pivot to canonical interval [0, pivot).
    let pivot = h.data[row][col]
    for r in 0..<row {
      let a0 = h.data[r][col]
      if a0 != 0N {
        let (q, _) = euclid_divmod(a0, pivot)
        row_add_multiple(h, r, row, -q)
        row_add_multiple(u, r, row, -q)
      }
    }

    pivot_cols.push(col)
    row = row + 1
  }

  let product = u.mul(a)
  let is_verified = matrix_equal(product, h) && h.is_row_hnf()
  let trace = Child(
    Text(": hnf_report"),
    group([
      Label("shape", "\{a.rows}x\{a.cols}"),
      Label("pivot_cols", pivot_cols.to_string()),
      Label("is_verified", "\{is_verified}"),
    ]),
  )
  HNFReport::{
    original: a,
    hnf: h,
    left_unimodular: u,
    pivot_cols,
    is_verified,
    trace,
  }
}

///|
pub fn Matrix::hnf(a : MatrixZ) -> MatrixZ raise {
  a.hnf_report().hnf
}
