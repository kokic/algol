
def backslash (s : String) := s!"\\{s}"
def brace (s : String) := s!"\{{s}}"

def mkTeXStr (name: String)
             (args : List String) : String :=
  (args.map brace).foldl (init := backslash name)
    fun s t => s ++ t

def String.tex := mkTeXStr
