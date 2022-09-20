import Lean

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

structure Exporter where
metrics : List Metric

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

def name_label := "__name__"

-- TODO: maybe this has to be a type family to differentiate between a scalar and instant vector return type
inductive InstantVector : InstantVectorType → Type
  | selector (literal_match : List KeyValuePair) (offset : Nat) : InstantVector vector -- TODO: regex, negative, and proof for minimal label requirements
  | literal (v : Float) : InstantVector InstantVectorType.scalar
  | add_vector (vector_matching : Option VectorMatching) (a b : InstantVector scalar) : InstantVector scalar
  | add_scalar_left (a : InstantVector scalar) (b : InstantVector vector) : InstantVector vector
  | gt (vector_matching : Option VectorMatching) (a b : InstantVector vector) : InstantVector vector
  | sum (a : AggregationSelector) (v : InstantVector vector) : InstantVector vector
  | topk (a : AggregationSelector) (k : Nat) (v : InstantVector vector) : InstantVector vector
  | rate (r : RangeVector) : InstantVector vector
  | deriv (r : RangeVector) : InstantVector vector
  | label_replace (v : InstantVector vector) (dst replacement src regex : String) : InstantVector vector
  | time : InstantVector scalar 

def is_name := λ (l : KeyValuePair) => l.key == name_label

def TypeSafeSelector (lm : List KeyValuePair) (e : Exporter) : Bool := 
  Option.isSome $ e.metrics.find? (λ m =>
    -- (List.all (lm.filter $ not ∘ is_name) (λ l => m.labels.contains l.key ))
    (List.all (lm.filter $ is_name) (λ l => m.name = l.value ))
  )

def InstantVector.typesafe {t : InstantVectorType} (v : InstantVector t) (e : Exporter) : Bool := match v with
  | (InstantVector.selector lm offset) => TypeSafeSelector lm e
  | (InstantVector.add_vector _ a b) => typesafe a e && typesafe b e
  | _ => true

structure TypesafeInstantVector (t : InstantVectorType) (e : Exporter) where
v : InstantVector t
h : InstantVector.typesafe v e := by simp

def InstantVector.toString {t : InstantVectorType} : InstantVector t → String
  | (selector lm offset) => (String.join $ List.map (λ l => l.value) (lm.filter (λ l => l.key == name_label))) ++ "{" ++ String.join (List.map (λ l => l.key ++ "=\"" ++ l.value ++ "\"") (lm.filter λ l => l.key != name_label)) ++ "}"
  | time => "time()"
  | (label_replace v dst replace src regex) => "label_replace()"
  | (rate r) => s!"rate({r.to_string})"
  | _ => ""

instance {t : InstantVectorType} : ToString $ InstantVector t where
  toString := InstantVector.toString

instance {t : InstantVectorType} : Repr $ InstantVector t where
  reprPrec v _ := InstantVector.toString v

#eval InstantVector.rate (RangeVector.selector [{key := "instance", value := "test"}] 5)

open Lean
open Lean.Parser

-- def name : Parser := withAntiquot (mkAntiquot "name" `LX.text) {
--   fn := fun c s => 
--     let startPos := s.pos;
--     let s := takeWhile1Fn (fun c => c.isAlphanum) "Invalid name" c s;
--     mkNodeToken `LX.text startPos c s
-- }

declare_syntax_cat labelmatcher
syntax "l{" strLit,* "}" : labelmatcher
syntax labelmatcher : term

syntax "pql!" strLit (labelmatcher)? : term

macro_rules
| `(labelmatcher| l{}) => `(List.nil)
| `(labelmatcher| l{ $x }) => `([{key := $x, value := ""} : KeyValuePair])
--| `(labelmatcher| l{ $x, $xs,* }) => `([{key := $x, value := ""}])

macro_rules
| `(pql! $name) => `(InstantVector.selector [{key := "__name__", value:= $(name)}] 0)
| `(pql! $name $xs) => `(InstantVector.selector ([{key := "__name__", value:= $(name)}]++($xs)) 0)

#eval pql!"upz"l{}

#eval s!"f"