
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
instance : Pow Expression Expression := ⟨.pow⟩
instance : Mod Expression := ⟨.mod⟩


instance (n : Nat) : OfNat Expression n where
  ofNat := Expression.const n.toFloat

-- instance : Coe Float Expression := ⟨.const⟩


def prettyConst (c : Float) : String :=
  let literal := c.toString
  let floor := literal.extract 0 (literal.posOf '.')
  if c == c.floor then floor else literal

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

instance : ToString Expression := ⟨Expression.toString⟩
instance : Repr Expression := ⟨λ e _ => e.toString⟩


def DualNumber := Expression × Expression deriving Repr
def dual₁ (a : Expression): DualNumber := (a, 1)
def dual (a b : Expression): DualNumber := (a, b)

instance : Add DualNumber := ⟨λ ⟨a, b⟩ ⟨c, d⟩ => ⟨a + c, b + d⟩⟩
instance : Sub DualNumber := ⟨λ ⟨a, b⟩ ⟨c, d⟩ => ⟨a - c, b - d⟩⟩
instance : Mul DualNumber := ⟨λ ⟨u, u'⟩ ⟨v, v'⟩ => ⟨u * v, u' * v + u * v'⟩⟩
instance : Div DualNumber := ⟨λ ⟨a, b⟩ ⟨c, d⟩ => ⟨a / c, (b * c - a * d) / c ^ (2 : Expression)⟩⟩

namespace DualNumber

def exp : DualNumber → DualNumber | ⟨u, u'⟩ => ⟨.exp u, u' * .exp u⟩
def log : DualNumber → DualNumber | ⟨u, u'⟩ => ⟨.log u, u' / u⟩

end DualNumber








def Expression.simplifyBinary (simplify : Expression → Expression)
                   (op : Expression → Expression → Expression)
                   (e₁ e₂ : Expression) :=
  let e₁simp := simplify e₁
  let e₂simp := simplify e₂
  if e₁ != e₁simp || e₂ != e₂simp
  then simplify (op e₁simp e₂simp)
  -- e₁ or e₂ cannot be simplified
  else match op e₁ e₂ with

    -- | e@(a / b) => if a == b then 1 else e

    | e@(const n * u) => if n == 1 then u else e
    | e@(u * const n) => if n == 1 then u else e

    | e@(const n / _) => if n == 0 then 0 else e


    | e@((a * b) * c) =>
      let bc := simplify (b * c)
      if bc != b * c then simplify (a * bc) else e

    | e@(a + const n * b) =>
      if a == b then const (1 + n) * a else e


    | x + x => 2 * x
    | x - x => 0
    | x * x => x ^ const 2

    | e => e


partial def Expression.simplify : Expression → Expression
  | e₁ + e₂ => simplifyBinary simplify .add e₁ e₂
  | e₁ - e₂ => simplifyBinary simplify .sub e₁ e₂
  | e₁ * e₂ => simplifyBinary simplify .mul e₁ e₂
  | e₁ / e₂ => simplifyBinary simplify .div e₁ e₂
  | e => e

def x := dual₁ .x

def expr : DualNumber := .log x + x + x * x

#eval println! s!"{expr |>.snd}\n{expr |>.snd |>.simplify}"
