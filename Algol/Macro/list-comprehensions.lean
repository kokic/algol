
declare_syntax_cat compClause
syntax "for " term " in " term : compClause
syntax "for" term "<-" term : compClause
syntax "for" term "←" term : compClause
syntax "if " term : compClause
syntax "[" term " | " compClause,* "]" : term
-- syntax "[" term ".." term "]" : term
-- syntax "[" term "," term ".." term "]" : term


def List.map' (xs : List α) (f : α → β) : List β := List.map f xs


def List.range_from_nn (m : Nat) (n : Nat) :=
  if m ≤ n then List.range (n - m + 1) |>.map (m + ·)
  else List.range (m - n + 1) |>.map (n + ·) |>.reverse

def nats_to_ints (xs : List Nat) := xs.map (Int.ofNat ·)

-- we assume m < 0 and n < 0 but not bounded
def List.range_from_neg (m : Int) (n : Int) :=
  if m ≤ n then List.range_from_nn (-n).toNat (-m).toNat 
    |> nats_to_ints |>.map (-·) |>.reverse
  else List.range_from_nn m.natAbs n.natAbs 
    |> nats_to_ints |> .map (-·)
-- #eval List.range_from_neg (-1) (-3)
-- #eval List.range_from_neg (-3) (-1)


def List.range_from : Int -> Int -> List Int  
  | Int.ofNat m, Int.ofNat n => range_from_nn m n |> nats_to_ints
  | m, Int.ofNat n => range_from_neg m (-1) ++ (range (n + 1) |> nats_to_ints)
  | Int.ofNat m, n => (range_from_nn m 0 |> nats_to_ints) ++ range_from_neg (-1) n
  | m, n => range_from_neg m n


def arithmetic_progression (s : Int) (t : Int) (u : Int) :=
  List.range_from_nn 0 ((u - s) / d).toNat 
    |> nats_to_ints |>.map (s + · * d)
  where d := (t - s)

macro_rules
  | `([$t:term |]) => `([$t])
  
  | `([$t:term | for $x in $xs]) => `(List.map' $xs  (λ $x => $t))
  | `([$t:term | for $x <- $xs]) => `(List.map' $xs  (λ $x => $t))  
  | `([$t:term | for $x ← $xs]) => `(List.map' $xs  (λ $x => $t))  
  
  | `([$t:term | if $x]) => `(if $x then [$t] else [])
  | `([$t:term | $c, $cs,*]) => `(List.join [[$t | $cs,*] | $c])
  | `([$t:term .. $u:term]) => `(List.range_from $t $u)
  | `([$t:term, $u:term .. $v:term]) => `(arithmetic_progression $t $u $v)



def List.prod (xs : List α) (ys : List β) : List (α × β) := [(x, y) | for x in xs, for y in ys]

-- #eval [1 .. 3]
-- #eval [3 .. 1]
-- #eval [(-5) .. 5]
-- #eval [7 .. (-2)]

#eval [1 .. 2].prod [ 3 ]

#eval [x % 5 | for x ← [(-1) .. 6]]
#eval [[0 .. x] | for x in [4 .. (-1)]]

#eval [1, (-3) .. (-20)]
