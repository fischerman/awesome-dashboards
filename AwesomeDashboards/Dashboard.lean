import Init.Data
import AwesomeDashboards.Prometheus

set_option linter.unusedVariables false

open InstantVectorType

structure GraphPanel {e : Environment} {vars : Vars} where
  promql : TypesafeInstantVector .vector e ⊕ TemplatedInstantVector vars .vector
  legendFormat : Option String := .none

def evalGraph' {e : Environment} {vars : Vars} (g : @GraphPanel e vars) (res : Nat) (endd : Nat) (steps : Nat) := ""

structure Column (e : Environment) where
name : String
promql : TypesafeInstantVector .vector e
additionalLabels : List String := []
deriving Lean.ToJson


structure TablePanel (e : Environment) where
name : String
/-- This only defines the value columns. Labels might produce more columns. -/
columns : List $ Column e
joinLabel : String
deriving Lean.ToJson

structure InstantValue where
labels : List LabelMatcher
value : Float

/-- sample data -/
def prometheusQueryInstant (v : InstantVector vector) : IO (List InstantValue) := do
  return [
    { labels := [.equal "__name__" "up", .equal "job" "prometheus"], value := 1 },
    { labels := [.equal "__name__" "up", .equal "job" "alertmanager"], value := 0 }
  ]

-- def rowsToMap (t : TablePanel) : Std.HashMapImp → List InstantValue
-- | idx, (row :: rows) =>

-- def TablePanel.columnsToRows_aux (t : TablePanel) : Nat → (List (List InstantValue)) → Std.HashMapImp String Nat → (List (List InstantValue))
-- | i, (col :: cols), idx => columnsToRows_aux t (i+1) cols (idx.insert (t.columns.get! i).index_label col)
-- | i, [], idx => []

-- def TablePanel.columnsToRows (t : TablePanel) (data : List (List InstantValue)) : List (List InstantValue) := columnsToRows_aux t data Std.mkHashMapImp

inductive Panel {e : Environment} {vars : Vars}
| graph (g : @GraphPanel e vars)
| table (t : TablePanel e)

structure Row (e : Environment) (vars : Vars := []) where
  panels : List $ @Panel e vars
  height := 10

structure Dashboard (e : Environment) (vars : Vars := []) where
  name : String
  panels : List $ Row e vars
