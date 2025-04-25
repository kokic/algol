
import Algol.Abbrev

def exponentBySquaring
    (op : Bi α) (id : α) : α → Nat → α :=
fun
  | _, 0 => id
  | a, 1 => a
  | a, n@(m + 1) =>
    if h : 0 < n ∧ n.mod 2 == 0 then
      have : n / 2 < n :=
        by simp [Nat.div_lt_self, h.1]
      exponentBySquaring op id (op a a) (n / 2)
    else
      op a (exponentBySquaring op id a m)
