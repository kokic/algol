
declare_syntax_cat compClause
syntax "for " term " in " term : compClause
syntax "for" term "<-" term : compClause
syntax "for" term "←" term : compClause
syntax "if " term : compClause
syntax "[" term " | " compClause,* "]" : term

def range_aux (m : Nat) (n : Nat) : List Nat :=
  ite (m ≤ n) (range_by m n)
              (range_by n m |>.reverse)
where
  range_by (m n : Nat) : List Nat :=
    List.range (n - m + 1) |>.map (m + ·)

def liftNats (xs : List Nat) :=
  xs.map (Int.ofNat ·)

def List.rangeInt : Int -> Int -> List Int
  | .ofNat m, .ofNat n => liftNats (range_aux m n)
  | m, .ofNat n => fromNeg m (-1) ++ liftNats (range (n + 1))
  | .ofNat m, n => (range_aux m 0 |> liftNats) ++ fromNeg (-1) n
  | m, n => fromNeg m n
where
  fromNeg (m : Int) (n : Int) :=
    if m ≤ n then range_aux (-n).toNat (-m).toNat
      |> liftNats |>.map (-·) |>.reverse
    else range_aux m.natAbs n.natAbs
      |> liftNats |>.map (-·)

def arithSequence (s : Int) (t : Int) (u : Int) :=
  range_aux 0 ((u - s) / d).toNat
    |> liftNats
    |>.map (s + · * d)
where d := t - s

macro_rules
  | `([$t:term |]) => `([$t])

  | `([$t:term | for $x in $xs]) => `(List.map $xs (f := λ $x => $t))
  | `([$t:term | for $x <- $xs]) => `(List.map $xs (f := λ $x => $t))
  | `([$t:term | for $x ← $xs]) => `(List.map $xs (f := λ $x => $t))

  | `([$t:term | if $x]) => `(if $x then [$t] else [])
  | `([$t:term | $c, $cs,*]) => `(List.flatten [[$t | $cs,*] | $c])
  | `([$t:term .. $u:term]) => `(List.rangeInt $t $u)
  | `([$t:term, $u:term .. $v:term]) => `(arithSequence $t $u $v)

def List.prod (xs : List α)
                    (ys : List β)
    : List (α × β) :=
  [(x, y) | for x in xs, for y in ys]
