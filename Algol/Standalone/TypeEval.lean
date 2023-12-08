

inductive Expression where
  | x : Expression
  | const : Float → Expression
  | neg : Expression → Expression
  | add : Expression → Expression → Expression
  | sub : Expression → Expression → Expression
  | mul : Expression → Expression → Expression
  | div : Expression → Expression → Expression
  | pow : Expression → Expression → Expression
  | mod : Expression → Expression → Expression

  | exp : Expression → Expression
  | log : Expression → Expression
  -- | sin : Expression → Expression
  -- | cos : Expression → Expression
deriving BEq
-- deriving Repr

instance : Neg Expression := ⟨.neg⟩
instance : Add Expression := ⟨.add⟩
instance : Sub Expression := ⟨.sub⟩
instance : Mul Expression := ⟨.mul⟩
instance : Div Expression := ⟨.div⟩
instance : HPow Expression Expression Expression := ⟨.pow⟩
instance : Mod Expression := ⟨.mod⟩


-- instance : Coe Nat Expression := ⟨.const ∘ Float.ofNat⟩
-- instance : Coe Int Expression := ⟨.const ∘ Float.ofInt⟩
-- instance : Coe Float Expression := ⟨.const⟩



def prettyConst (c : Float) : String :=
  let s₁ := c.toString
  let s₂ := s₁.extract 0 (s₁.posOf '.')
  if c == c.floor then s₂ else s₁

-- def prettyParentheses : Expression → String

def Expression.toString : Expression → String
  | .x => "x"
  | .const c => prettyConst c
  | - e => "-" ++ e.toString
  | e₁ + e₂ => s!"({e₁.toString} + {e₂.toString})"
  | e₁ - e₂ => s!"({e₁.toString} - {e₂.toString})"
  | e₁ * e₂ => s!"({e₁.toString} * {e₂.toString})"
  | e₁ / e₂ => s!"({e₁.toString} / {e₂.toString})"
  | pow e₁ e₂ => s!"({e₁.toString} ^ {e₂.toString})"
  | e₁ % e₂ => s!"({e₁.toString} mod {e₂.toString})"

  | .exp e => s!"exp " ++ e.toString
  | .log e => s!"log " ++ e.toString
  -- | _ => "?"

instance : Repr Expression := ⟨λ e _ => e.toString⟩
-- instance : Repr Expression where
--   reprPrec e _ := e.toString















def Expression.apply : Expression → Expression → Expression
  | .x, y => y
  | - e, y => - e.apply y
  | e₁ + e₂, y => e₁.apply y + e₂.apply y
  | e₁ - e₂, y => e₁.apply y - e₂.apply y
  | e₁ * e₂, y => e₁.apply y * e₂.apply y
  | e₁ / e₂, y => e₁.apply y / e₂.apply y
  | pow e₁ e₂, y => e₁.apply y ^ e₂.apply y
  | .exp e, y => exp (e.apply y)
  | .log e, y => log (e.apply y)
  | e, _ => e






-- g f x
-- def derivative.compose (d g : Expression → Expression)
--              (f : Expression) : Expression :=
--   (d (g .x)).apply f * d f

-- instance : Coe Nat Expression := ⟨λ x => x.toFloat⟩

instance (n : Nat) : OfNat Expression n where
  ofNat := Expression.const n.toFloat

instance : Coe Float Expression := ⟨.const⟩

-- derivative of x

partial def Expression.derivative : Expression → Expression
  | const _ => 0
  | x => 1
  | exp x => exp x
  | log x => 1 / x

  | const c * e => .const c * e.derivative

  | - e => - D e
  | e₁ + e₂ => D e₁ + D e₂
  | e₁ - e₂ => D e₁ - D e₂
  | e₁ * e₂ => D e₁ * e₂ + e₁ * D e₂
  | e₁ / e₂ => (e₂ * D e₁ - e₁ * D e₂) / e₂ ^ (2 : Expression)

  | pow (const n) x => log n * n ^ x
  -- | pow x (const n) => n * x ^ (n-1)
  -- | e₁ % e₂ =>

  | exp f => exp f * D f -- compose .exp f
  | log f => 1 / f * D f -- compose .log f
  -- | .sin f
  -- | .cos f =>
  | _ => 0
where
  D := derivative

-- where
--   compose (g : Expression → Expression)
--           (f : Expression) :=
--     (derivative (g .x)).apply f * derivative f


/-
To avoid some very obvious non terminating recursion, we must check whether the expression is equal before and after simplification
-/
def simplifyBinary (simplify : Expression → Expression)
                   (op : Expression → Expression → Expression)
                   (e₁ e₂ : Expression) :=
  let e₁simp := simplify e₁
  let e₂simp := simplify e₂
  if e₁ != e₁simp || e₂ != e₂simp
  then simplify (op e₁simp e₂simp)
  else op e₁ e₂


-- partial def Expression.simplify : Expression → Expression
-- -- specialization

--   | e@(a * b + c * d) => if b == d then simplify ((a + c) * b) else e


-- -- constant operation

-- /-
-- Note that the `const` here cannot be removed. Due to the presence of
-- `coe: Float → Expression`, the operation between two arbitrary floats
-- will actually be considered as the operation between `Expression`.
-- -/
--   | const c₁ + const c₂ => const (c₁ + c₂)
--   | const c₁ - const c₂ => const (c₁ - c₂)
--   | const c₁ * const c₂ => const (c₁ * c₂)
--   | const c₁ / const c₂ => const (c₁ / c₂)

-- -- simple variable operation
--   | x - x => 0

--   | x + const c * x => const (1 + c) * x
--   | x - const c * x => const (1 - c) * x

--   | x * const c / x => c

--   | x ^ const n => if n == 0 then 1 else
--                    if n == 1 then x else x ^ n

--   | (x ^ const n) ^ const m => x ^ const (n + m)

--   | x ^ const n / x => x ^ (n - 1)


--   | const c * .x => if c == 0 then 0 else
--                     if c == 1 then x else c * x

--   | const c * x - x => simplify (const (c - 1) * x)
--   | const c₁ * x - const c₂ * x => const (c₁ - c₂) * x



--   | neg (.neg e) => e.simplify
--   | e + const c => if c == 0 then e.simplify else e.simplify + c
--   | e - const c => if c == 0 then e.simplify else e.simplify - c
--   | e * const c => if c == 0 then c else
--                    if c == 1 then e.simplify else c * e.simplify
--   | e / const c => if c == 1 then e.simplify else e.simplify / c

-- -- operation
--   | e₁ + (- e₂) => (e₁ - e₂).simplify

-- -- operation level
--   | e₁ * (e₂ / e₃) => (e₁ * e₂ / e₃).simplify

--   -- | e₁ + (- e₂ * e₃) => if e₁ == e₃
--   --     then simplify (.const 1 - e₂) * e₁
--   --     else e₁ + (- e₂ * e₃)
--   -- (e₁ + (-e₂) * e₃)

--   | e₁ + e₂ => simplifyBinary simplify .add e₁ e₂
--   | e₁ - e₂ => simplifyBinary simplify .sub e₁ e₂
--   | e₁ * e₂ => simplifyBinary simplify .mul e₁ e₂
--   | e₁ / e₂ => if e₁ == e₂ then 1
--                else simplifyBinary simplify .div e₁ e₂
--   | e₁ ^ e₂ => simplifyBinary simplify .pow e₁ e₂

-- -- function
--   | .exp (.log e) => e.simplify
--   | .log (.exp e) => e.simplify


-- -- identity
--   | e => e


open Expression

-- #eval simplify (1 + 2)

-- -- consider .x as 1 * .x to be simplifed

-- #eval ( 2 * x + (- const 2 * x)).simplify
-- #eval (x + - (const 2 * x)).simplify



-- #eval (x ^ (2 - 1)) |>.simplify

-- #eval derivative (x ^ 2) |>.simplify
-- #eval derivative (x ^ 3) |>.simplify
-- #eval derivative (log (log (log x)))


-- -- #eval derivative (.log (.x + .x))
-- -- #eval derivative (.log (.x * .x))

-- -- #eval derivative (.exp (.x + .x))
-- -- #eval derivative (.exp (.x * .x))

-- #eval derivative (log x / x^2) |>.simplify

-- #eval derivative (exp (log x))
-- #eval (x * (1/x)).simplify
-- #eval derivative (exp (log x)) |>.simplify
