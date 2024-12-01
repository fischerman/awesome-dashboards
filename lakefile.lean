import Lake
import Init.System.IO
open Lake DSL

package AwesomeDashboards {
  -- add configuration options here
}

lean_lib AwesomeDashboards

@[default_target]
lean_exe dashboard {
  root := `Main
}

script abc := do
  IO.println "Hello"
  return 0
