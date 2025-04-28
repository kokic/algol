import Lake

open Lake DSL

package «algol» {
  -- add package configuration options here
}

lean_lib «Algol» {
  -- add library configuration options here
}

@[default_target]
lean_exe «algol» {
  root := `Algol
}
