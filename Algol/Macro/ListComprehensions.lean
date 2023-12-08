
declare_syntax_cat compClause
syntax "for " term " in " term : compClause
syntax "for" term "<-" term : compClause
syntax "for" term "←" term : compClause
syntax "if " term : compClause
syntax "[" term " | " compClause,* "]" : term
-- syntax "[" term ".." term "]" : term
-- syntax "[" term "," term ".." term "]" : term


def List.map' (xs : List α) (f : α → β) : List β := List.map f xs


def List.rangeFromNNeg (m : Nat) (n : Nat) :=
  if m ≤ n then List.range (n - m + 1) |>.map (m + ·)
  else List.range (m - n + 1) |>.map (n + ·) |>.reverse

def castNatsToInts (xs : List Nat) := xs.map (Int.ofNat ·)

-- we assume m < 0 and n < 0 but not bounded
def List.rangeFromNeg (m : Int) (n : Int) :=
  if m ≤ n then List.rangeFromNNeg (-n).toNat (-m).toNat 
    |> castNatsToInts |>.map (-·) |>.reverse
  else List.rangeFromNNeg m.natAbs n.natAbs 
    |> castNatsToInts |> .map (-·)
-- #eval List.rangeFromNeg (-1) (-3)
-- #eval List.rangeFromNeg (-3) (-1)


def List.rangeFrom : Int -> Int -> List Int  
  | Int.ofNat m, Int.ofNat n => rangeFromNNeg m n |> castNatsToInts
  | m, Int.ofNat n => rangeFromNeg m (-1) ++ (range (n + 1) |> castNatsToInts)
  | Int.ofNat m, n => (rangeFromNNeg m 0 |> castNatsToInts) ++ rangeFromNeg (-1) n
  | m, n => rangeFromNeg m n


def arithmeticProgression (s : Int) (t : Int) (u : Int) :=
  List.rangeFromNNeg 0 ((u - s) / d).toNat 
    |> castNatsToInts |>.map (s + · * d)
  where d := (t - s)

macro_rules
  | `([$t:term |]) => `([$t])
  
  | `([$t:term | for $x in $xs]) => `(List.map' $xs  (λ $x => $t))
  | `([$t:term | for $x <- $xs]) => `(List.map' $xs  (λ $x => $t))  
  | `([$t:term | for $x ← $xs]) => `(List.map' $xs  (λ $x => $t))  
  
  | `([$t:term | if $x]) => `(if $x then [$t] else [])
  | `([$t:term | $c, $cs,*]) => `(List.join [[$t | $cs,*] | $c])
  | `([$t:term .. $u:term]) => `(List.rangeFrom $t $u)
  | `([$t:term, $u:term .. $v:term]) => `(arithmeticProgression $t $u $v)



def List.prod (xs : List α) (ys : List β) : List (α × β) := [(x, y) | for x in xs, for y in ys]

-- #eval [1 .. 3]
-- #eval [3 .. 1]
-- #eval [(-5) .. 5]
-- #eval [7 .. (-2)]

#eval [1 .. 2].prod [ 3 ]

#eval [x % 5 | for x ← [(-1) .. 6]]
#eval [[0 .. x] | for x in [4 .. (-1)]]

#eval [1, (-3) .. (-20)]
