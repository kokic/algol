///|
pub struct Matrix[T] {
  rows : Int
  cols : Int
  data : Array[Array[T]]
} derive(Show, Eq)

///|
pub type MatrixZ = Matrix[BigInt]

///|
pub struct HNFReport[T] {
  original : Matrix[T]
  hnf : Matrix[T]
  left_unimodular : Matrix[T]
  pivot_cols : Array[Int]
  is_verified : Bool
  trace : Message
} derive(Show)

///|
pub type HNFReportZ = HNFReport[BigInt]

///|
pub struct SNFReport[T] {
  original : Matrix[T]
  snf : Matrix[T]
  left_unimodular : Matrix[T]
  right_unimodular : Matrix[T]
  diagonal : Array[T]
  rank : Int
  is_verified : Bool
  trace : Message
} derive(Show)

///|
pub type SNFReportZ = SNFReport[BigInt]

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
fn[T] swap_rows(mat : Matrix[T], i : Int, j : Int) -> Unit {
  guard i != j else { return }
  let tmp = mat.data[i]
  mat.data[i] = mat.data[j]
  mat.data[j] = tmp
}

///|
fn[T] swap_cols(mat : Matrix[T], i : Int, j : Int) -> Unit {
  guard i != j else { return }
  for r in 0..<mat.rows {
    let tmp = mat.data[r][i]
    mat.data[r][i] = mat.data[r][j]
    mat.data[r][j] = tmp
  }
}

///|
fn[T : Ring] row_pair_transform(
  mat : Matrix[T],
  i : Int,
  j : Int,
  a : T,
  b : T,
  c : T,
  d : T,
) -> Unit {
  let row_i = Array::makei(mat.cols, k => mat.data[i][k])
  let row_j = Array::makei(mat.cols, k => mat.data[j][k])
  for k in 0..<mat.cols {
    mat.data[i][k] = a * row_i[k] + b * row_j[k]
    mat.data[j][k] = c * row_i[k] + d * row_j[k]
  }
}

///|
fn[T : Ring] col_pair_transform(
  mat : Matrix[T],
  i : Int,
  j : Int,
  a : T,
  b : T,
  c : T,
  d : T,
) -> Unit {
  for r in 0..<mat.rows {
    let col_i = mat.data[r][i]
    let col_j = mat.data[r][j]
    mat.data[r][i] = a * col_i + b * col_j
    mat.data[r][j] = c * col_i + d * col_j
  }
}

///|
fn[T : Ring] row_add_multiple(
  mat : Matrix[T],
  target : Int,
  source : Int,
  scalar : T,
) -> Unit {
  guard !scalar.is_zero() else { return }
  for k in 0..<mat.cols {
    mat.data[target][k] = mat.data[target][k] + scalar * mat.data[source][k]
  }
}

///|
fn[T : Ring] col_add_multiple(
  mat : Matrix[T],
  target : Int,
  source : Int,
  scalar : T,
) -> Unit {
  guard !scalar.is_zero() else { return }
  for r in 0..<mat.rows {
    mat.data[r][target] = mat.data[r][target] + scalar * mat.data[r][source]
  }
}

///|
fn[T : Ring] row_negate(mat : Matrix[T], row : Int) -> Unit {
  for k in 0..<mat.cols {
    mat.data[row][k] = Ring::neg(mat.data[row][k])
  }
}

///|
fn[T : EuclideanDomain] exact_div(a : T, b : T) -> T raise {
  assert_true(!b.is_zero(), msg="division by zero in exact_div")
  let (q, r) = EuclideanDomain::div_mod_euclid(a, b)
  assert_true(
    r.is_zero(),
    msg="non-exact division in Euclidean-domain elimination",
  )
  q
}

///|
fn min_int(a : Int, b : Int) -> Int {
  if a < b {
    a
  } else {
    b
  }
}

///|
pub fn[T : EuclideanDomain] Matrix::is_row_hnf(mat : Matrix[T]) -> Bool {
  let zero = T::zero()
  let mut prev_pivot_col = -1
  let mut found_zero_row = false
  for i in 0..<mat.rows {
    let mut pivot_col : Int? = None
    for j in 0..<mat.cols {
      if mat.data[i][j] != zero {
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
        if pivot.compare(zero) <= 0 {
          return false
        }
        // entries below pivot are zero
        for r in (i + 1)..<mat.rows {
          if mat.data[r][pc] != zero {
            return false
          }
        }
        // entries above pivot satisfy 0 <= x < pivot
        for r in 0..<i {
          let x = mat.data[r][pc]
          if x.compare(zero) < 0 || x.compare(pivot) >= 0 {
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
pub fn[T : EuclideanDomain] Matrix::hnf_report(
  a : Matrix[T],
) -> HNFReport[T] raise {
  let zero = T::zero()
  let h = clone_matrix(a)
  let u : Matrix[T] = Matrix::identity(a.rows)
  let pivot_cols = []
  let mut row = 0

  for col in 0..<h.cols {
    guard row < h.rows else { break }

    let mut pivot_row : Int? = None
    for r in row..<h.rows {
      if h.data[r][col] != zero {
        pivot_row = Some(r)
        break
      }
    }
    guard pivot_row is Some(pr) else { continue }

    swap_rows(h, row, pr)
    swap_rows(u, row, pr)

    // Euclidean elimination below the pivot.
    for r in (row + 1)..<h.rows {
      while h.data[r][col] != zero {
        let a0 = h.data[row][col]
        let b0 = h.data[r][col]
        let (g, s, t) = T::extended_gcd(a0, b0)
        let c = Ring::neg(exact_div(b0, g))
        let d = exact_div(a0, g)
        row_pair_transform(h, row, r, s, t, c, d)
        row_pair_transform(u, row, r, s, t, c, d)
      }
    }

    if h.data[row][col].compare(zero) < 0 {
      row_negate(h, row)
      row_negate(u, row)
    }

    // Reduce entries above pivot to canonical interval [0, pivot).
    let pivot = h.data[row][col]
    for r in 0..<row {
      let a0 = h.data[r][col]
      if a0 != zero {
        let (q, _) = EuclideanDomain::div_mod_euclid(a0, pivot)
        let neg_q = Ring::neg(q)
        row_add_multiple(h, r, row, neg_q)
        row_add_multiple(u, r, row, neg_q)
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
pub fn[T : EuclideanDomain] Matrix::hnf(a : Matrix[T]) -> Matrix[T] raise {
  a.hnf_report().hnf
}

///|
pub fn Matrix::is_symmetric(
  a : Matrix[Double],
  eps? : Double = 0.000000000001,
) -> Bool {
  guard a.rows == a.cols else { false }
  for i in 0..<a.rows {
    for j in (i + 1)..<a.cols {
      if (a.data[i][j] - a.data[j][i]).abs() > eps {
        return false
      }
    }
  }
  true
}

///|
fn cholesky_lower_double(a : Matrix[Double], eps : Double) -> Matrix[Double]? {
  guard a.rows == a.cols else { return None }
  guard a.is_symmetric(eps=eps) else { return None }

  let n = a.rows
  let l = Array::makei(n, _ => Array::makei(n, _ => 0.0))
  for i in 0..<n {
    for j in 0..=i {
      let mut value = a.data[i][j]
      for k in 0..<j {
        value = value - l[i][k] * l[j][k]
      }

      if i == j {
        if value <= eps || value.is_nan() {
          return None
        }
        l[i][j] = value.sqrt()
      } else {
        let denom = l[j][j]
        if denom.abs() <= eps || denom.is_nan() {
          return None
        }
        l[i][j] = value / denom
      }
    }
  }
  Some(Matrix::{ rows: n, cols: n, data: l })
}

///|
pub fn Matrix::is_positive_definite(
  a : Matrix[Double],
  eps? : Double = 0.000000000001,
) -> Bool {
  guard eps > 0.0 else { false }
  cholesky_lower_double(a, eps) is Some(_)
}

///|
pub fn Matrix::cholesky(
  a : Matrix[Double],
  eps? : Double = 0.000000000001,
) -> Matrix[Double] raise {
  assert_true(eps > 0.0, msg="eps must be positive")
  let maybe_l = cholesky_lower_double(a, eps)
  assert_true(
    maybe_l is Some(_),
    msg="Cholesky requires a symmetric positive definite matrix",
  )
  match maybe_l {
    Some(l) => l
    None => Matrix::{ rows: 0, cols: 0, data: [] }
  }
}

///|
pub fn[T : EuclideanDomain] Matrix::is_snf(mat : Matrix[T]) -> Bool {
  let zero = T::zero()
  for i in 0..<mat.rows {
    for j in 0..<mat.cols {
      if i != j && mat.data[i][j] != zero {
        return false
      }
    }
  }

  let diag_len = min_int(mat.rows, mat.cols)
  let mut seen_zero = false
  let mut prev : T? = None
  for i in 0..<diag_len {
    let d = mat.data[i][i]
    if d == zero {
      seen_zero = true
      continue
    }
    if seen_zero {
      return false
    }
    if EuclideanDomain::compare(d, zero) <= 0 {
      return false
    }
    match prev {
      None => ()
      Some(p) => {
        let (_, r) = EuclideanDomain::div_mod_euclid(d, p)
        if !r.is_zero() {
          return false
        }
      }
    }
    prev = Some(d)
  }
  true
}

///|
pub fn[T : EuclideanDomain] Matrix::snf_report(
  a : Matrix[T],
) -> SNFReport[T] raise {
  let zero = T::zero()
  let one = T::one()
  let s = clone_matrix(a)
  let u : Matrix[T] = Matrix::identity(a.rows)
  let v : Matrix[T] = Matrix::identity(a.cols)
  let limit = min_int(s.rows, s.cols)
  let mut k = 0

  while k < limit {
    let mut pivot_pos : (Int, Int)? = None
    for i in k..<s.rows {
      for j in k..<s.cols {
        if s.data[i][j] != zero {
          pivot_pos = Some((i, j))
          break
        }
      }
      if pivot_pos is Some(_) {
        break
      }
    }
    guard pivot_pos is Some((pi, pj)) else { break }

    swap_rows(s, k, pi)
    swap_rows(u, k, pi)
    swap_cols(s, k, pj)
    swap_cols(v, k, pj)

    while true {
      let mut changed = false

      for i in (k + 1)..<s.rows {
        if s.data[i][k] != zero {
          let a0 = s.data[k][k]
          let b0 = s.data[i][k]
          let (g, s0, t0) = T::extended_gcd(a0, b0)
          let c = Ring::neg(exact_div(b0, g))
          let d = exact_div(a0, g)
          row_pair_transform(s, k, i, s0, t0, c, d)
          row_pair_transform(u, k, i, s0, t0, c, d)
          changed = true
        }
      }

      for j in (k + 1)..<s.cols {
        if s.data[k][j] != zero {
          let a0 = s.data[k][k]
          let b0 = s.data[k][j]
          let (g, s0, t0) = T::extended_gcd(a0, b0)
          let c = Ring::neg(exact_div(b0, g))
          let d = exact_div(a0, g)
          col_pair_transform(s, k, j, s0, t0, c, d)
          col_pair_transform(v, k, j, s0, t0, c, d)
          changed = true
        }
      }

      let mut witness : (Int, Int)? = None
      if !changed {
        for i in (k + 1)..<s.rows {
          for j in (k + 1)..<s.cols {
            let (_, r) = EuclideanDomain::div_mod_euclid(s.data[i][j], s.data[k][k])
            if !r.is_zero() {
              witness = Some((i, j))
              break
            }
          }
          if witness is Some(_) {
            break
          }
        }
      }

      match witness {
        None => ()
        Some((wi, wj)) => {
          row_add_multiple(s, k, wi, one)
          row_add_multiple(u, k, wi, one)
          col_add_multiple(s, k, wj, one)
          col_add_multiple(v, k, wj, one)
          changed = true
        }
      }

      if !changed {
        break
      }
    }

    if EuclideanDomain::compare(s.data[k][k], zero) < 0 {
      row_negate(s, k)
      row_negate(u, k)
    }

    k = k + 1
  }

  let diagonal = []
  for i in 0..<limit {
    let d = s.data[i][i]
    if d != zero {
      diagonal.push(d)
    }
  }
  let rank = diagonal.length()
  let product = u.mul(a).mul(v)
  let is_verified = matrix_equal(product, s) && s.is_snf()
  let trace = Child(
    Text(": snf_report"),
    group([
      Label("shape", "\{a.rows}x\{a.cols}"),
      Label("rank", "\{rank}"),
      Label("is_verified", "\{is_verified}"),
    ]),
  )
  SNFReport::{
    original: a,
    snf: s,
    left_unimodular: u,
    right_unimodular: v,
    diagonal,
    rank,
    is_verified,
    trace,
  }
}

///|
pub fn[T : EuclideanDomain] Matrix::snf(a : Matrix[T]) -> Matrix[T] raise {
  a.snf_report().snf
}
