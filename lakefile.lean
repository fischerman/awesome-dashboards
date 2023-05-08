import Lake
import Init.System.IO
open Lake DSL

package «awesome-dashboards» {
  -- add configuration options here
}

lean_lib «awesome-dashboards»

@[default_target]
lean_exe dashboard {
  root := `Main
}

script abc := do
  IO.println "Hello"
  return 0
