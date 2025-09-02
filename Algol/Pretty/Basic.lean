/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/
def toSuperscript (s : String) : Option String :=
  s.toInt?.map case_int
where
  case_int (n : Int) :=
    let str := n.natAbs.toSuperscriptString
    ite (n >= 0) str ("⁻" ++ str)

def forExp (raw : String) (o : Option String) :=
  o.getD s!"^{raw}"

def forCoeff (raw : String) (o : Option String) :=
  o.getD s!"{raw}"

def prettyExp (term : String) (exp : String) :=
  ite (exp == "0") "1"
      (ite (exp == "1") term (term ++ super))
where
  super := forExp exp (toSuperscript exp)

def prettyCoeff (term : String) (coeff : String) :=
  ite (term == "" || term == "1") coeff
      (ite (coeff == "0") "0"
           (ite (coeff == "1") term (coeff ++ term)))
