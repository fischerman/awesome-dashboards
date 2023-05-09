import Init.Data
import AwesomeDashboards.Prometheus

open InstantVectorType

structure GraphPanel {e : Environment} where
  promql : TypesafeInstantVector vector e
  --data : InstantVector
  deriving Lean.ToJson

def evalGraph' {e : Environment} (g : @GraphPanel e) (res : Nat) (endd : Nat) (steps : Nat) := ""

def graphData := "

"

def graphToHTML {e : Environment} (g : @GraphPanel e) := s!"
<div>
  <h3>GraphPanel: {g.promql.v.toString}</h3>
  <div style=\"height: 300px; width: 100%\">
    <canvas id=\"myChart\"></canvas>
  </div>
  <script>
    {graphData}
  </script>
</div>
"

inductive ColumnValueSource
  | PrometheusLabelColumn (v : InstantVector vector)
  | PrometheusValueColumn (v : InstantVector vector)
  deriving Lean.ToJson

structure Column where
name : String
data : ColumnValueSource
index_label : String -- { s : String // s ∈ metric.labels}
deriving Lean.ToJson


structure TablePanel where
name : String
columns : List Column
deriving Lean.ToJson

structure InstantValue where
labels : List KeyValuePair
value : Float

-- sample data
def prometheusQueryInstant (v : InstantVector vector) : IO (List InstantValue) := do
  return [
    { labels := [{key := "__name__", value := "up"}, {key := "job", value := "prometheus"}], value := 1 },
    { labels := [{key := "__name__", value := "up"}, {key := "job", value := "alertmanager"}], value := 0 }
  ]

-- def rowsToMap (t : TablePanel) : Std.HashMapImp → List InstantValue
-- | idx, (row :: rows) => 

-- def TablePanel.columnsToRows_aux (t : TablePanel) : Nat → (List (List InstantValue)) → Std.HashMapImp String Nat → (List (List InstantValue))
-- | i, (col :: cols), idx => columnsToRows_aux t (i+1) cols (idx.insert (t.columns.get! i).index_label col)
-- | i, [], idx => []

-- def TablePanel.columnsToRows (t : TablePanel) (data : List (List InstantValue)) : List (List InstantValue) := columnsToRows_aux t data Std.mkHashMapImp

inductive Panel {e : Environment}
| graph (g : @GraphPanel e)
| table (t : TablePanel)
deriving Lean.ToJson

structure Dashboard (e : Environment) where
  name : String
  panels : List $ @Panel e
  deriving Lean.ToJson