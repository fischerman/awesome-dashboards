import Init.Data
import AwesomeDashboards.Prometheus

set_option linter.unusedVariables false

open InstantVectorType

structure Datasource where
  type : String := "prometheus"
  uid : String
  deriving Repr, BEq, Lean.FromJson, Lean.ToJson

structure TimeRange where
  fromTime : String := "now-1h"
  toTime : String := "now"
  deriving Repr, BEq

structure GraphTarget {e : Environment} {vars : Vars} where
  promql : CheckedQuery e vars .vector
  legendFormat : Option String := .none

structure GraphPanel {e : Environment} {vars : Vars} where
  title : String := ""
  description : Option String := .none
  targets : List (@GraphTarget e vars)

/-- Convenience constructor for a single-target graph panel. -/
def GraphPanel.single {e : Environment} {vars : Vars}
    (promql : CheckedQuery e vars .vector)
    (title : String := "")
    (legendFormat : Option String := .none)
    (description : Option String := .none) : @GraphPanel e vars :=
  { title := title, description := description, targets := [{ promql := promql, legendFormat := legendFormat }] }

def evalGraph' {e : Environment} {vars : Vars} (g : @GraphPanel e vars) (res : Nat) (endd : Nat) (steps : Nat) := ""

structure Column (e : Environment) (vars : Vars := []) where
  name : String
  promql : CheckedQuery e vars .vector
  additionalLabels : List String := []
deriving Lean.ToJson

structure TablePanel (e : Environment) (vars : Vars := []) where
  name : String
  description : Option String := .none
  /-- This only defines the value columns. Labels might produce more columns. -/
  columns : List $ Column e vars
  joinLabel : String
deriving Lean.ToJson

structure InstantValue where
  labels : List (LabelMatcher .pure)
  value : Float

/-- sample data -/
def prometheusQueryInstant (v : PureInstantVector vector) : IO (List InstantValue) := do
  return [
    { labels := [.equal "__name__" "up", .equal "job" "prometheus"], value := 1 },
    { labels := [.equal "__name__" "up", .equal "job" "alertmanager"], value := 0 }
  ]

inductive Panel {e : Environment} {vars : Vars}
| graph (g : @GraphPanel e vars)
| table (t : TablePanel e vars)

structure Row (e : Environment) (vars : Vars := []) where
  panels : List $ @Panel e vars
  height : Nat := 10

structure Dashboard (e : Environment) (vars : Vars := []) where
  name : String
  uid : Option String := .none
  description : Option String := .none
  tags : List String := []
  datasource : Datasource
  time : Option TimeRange := .none
  panels : List $ Row e vars
