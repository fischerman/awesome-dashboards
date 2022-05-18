import Init.Data
import Std.Data

def hello := "world"

inductive MetricType
| counter
| gauge
| histogram
| untyped

inductive MetricUnit
| seconds
| bytes
| bool

structure Metric where
name : String
type : MetricType
labels : List String  -- TODO: some metrics have dynamic label keys
unit : MetricUnit

def up : Metric := {
  name := "up"
  type := MetricType.gauge
  labels := ["instance", "job"]
  unit := MetricUnit.bool
}

def node_boot_time_seconds : Metric := {
  name := "node_boot_time_seconds"
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.seconds
}

def node_filesystem_avail_bytes : Metric := {
  name := "node_filesystem_avail_bytes"
  type := MetricType.gauge
  labels := ["device", "fstype", "mountpoint"]
  unit := MetricUnit.bytes
}

inductive VectorMatching
| ignoring
| on

inductive AggregationSelector 

structure KeyValuePair where
(key : String)
(value : String)

inductive RangeVector
| selector (literal_match : List KeyValuePair) (duration : Nat)

def RangeVector.to_string : RangeVector → String
  | (selector lm d) => s!"\{{String.join (lm.map $ λ l => l.key ++ "=" ++l.value ++ "")}}[{d}]"

inductive InstantVectorType
| scalar
| vector

open InstantVectorType

-- TODO: maybe this has to be a type family to differentiate between a scalar and instant vector return type
inductive InstantVector : InstantVectorType → Type
  | selector (literal_match : List KeyValuePair) (offset : Nat) : InstantVector vector -- TODO: regex, negative, and proof for minimal label requirements
  | literal (v : Float) : InstantVector InstantVectorType.scalar
  | add_vector (vector_matching : Option VectorMatching) (a b : InstantVector scalar) : InstantVector scalar
  | add_scalar_left (a : InstantVector scalar) (b : InstantVector vector) : InstantVector vector
  | gt (vector_matching : Option VectorMatching) (a b : InstantVector vector) : InstantVector vector
  | sum (a : AggregationSelector) (v : InstantVector vector) : InstantVector vector
  | topk (a : AggregationSelector) (k : Nat) (v : InstantVector vector) : InstantVector vector
  | range (r : RangeVector) : InstantVector vector
  | deriv (r : RangeVector) : InstantVector vector
  | label_replace (v : InstantVector vector) (dst replacement src regex : String) : InstantVector vector
  | time : InstantVector scalar

def InstantVector.toString {t : InstantVectorType} : InstantVector t → String
  | (selector lm offset) => s!"\{{String.join (lm.map $ λ l => l.key ++ "=\"" ++ l.value ++ "\"")}}"
  | time => "time()"
  | (label_replace v dst replace src regex) => "label_replace()"
  | (range r) => s!"range({r.to_string})"
  | _ => ""

#eval (InstantVector.range (RangeVector.selector [{key := "instance", value := "test"}] 5)).toString

structure GraphPanel where
  bottomY : Nat
  topY : Nat
  promql : InstantVector vector
  --data : InstantVector

def evalGraph' (g : GraphPanel) (res : Nat) (endd : Nat) (steps : Nat) := ""

def graphData := "

"

def graphToHTML (g : GraphPanel) := s!"
<div>
  <h3>GraphPanel: {g.promql.toString}</h3>
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

structure Column where
name : String
data : ColumnValueSource
index_label : String -- { s : String // s ∈ metric.labels}


structure TablePanel where
name : String
columns : List Column

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

inductive Panel
| graph (g : GraphPanel)
| table (t : TablePanel)

structure Dashboard where
name : String
panels : List Panel
