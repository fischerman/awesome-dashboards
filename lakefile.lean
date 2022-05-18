import Lake
import Init.System.IO
open Lake DSL

package «awesome-dashboards» {
  -- add configuration options here
}

script abc := do
  IO.println "Hello"
  return 0
