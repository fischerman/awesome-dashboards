import Lake
import Init.System.IO
open Lake DSL

package «lean-playground» {
  -- add configuration options here
}

script abc := do
  IO.println "Hello"
  return 0
