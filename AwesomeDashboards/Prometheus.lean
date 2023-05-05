import Lean

inductive MetricType
| counter
| gauge
| histogram
| untyped

-- https://prometheus.io/docs/practices/naming/#base-units
@[matchPattern]
inductive MetricUnit
| seconds
| bytes
| bool
| time
| unitless
| div (dividend devisor : MetricUnit)
deriving Repr, BEq, DecidableEq

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

structure ScrapeConfig where
targetLabels : List String
exporter : Exporter

structure Environment where
scrapeConfigs: List ScrapeConfig

inductive VectorMatching
| ignoring
| on

inductive AggregationSelector 

def name_label := "__name__"

structure KeyValuePair where
(key : String)
(value : String)

def KeyValuePair.toString (kvp : KeyValuePair) := kvp.key ++ "=\"" ++ kvp.value ++ "\""

def joinSep (s : List String) (sep : String) : String := match s with
| [] => ""
| x::[] => x
| x::xs => x ++ sep ++ (joinSep xs sep)

structure LabelMatchers where
  equal : List KeyValuePair

def LabelMatchers.empty : LabelMatchers := {equal := []}

def LabelMatchers.withEqualMatcher (m : LabelMatchers) (key : String) (value : String) := {
  m with equal := {key := key, value:= value} :: m.equal
}

def LabelMatchers.withEqualMatchers' (m : LabelMatchers) (x : KeyValuePair) := {
  m with equal := x :: m.equal
}

def LabelMatchers.withName (m : LabelMatchers) (value : String) := withEqualMatcher m name_label value

def LabelMatchers.toString (m : LabelMatchers) := "{" ++ joinSep (m.equal.map KeyValuePair.toString) ", " ++ "}"

inductive RangeVector
| selector (lms : LabelMatchers) (duration : Nat)

def RangeVector.to_string : RangeVector → String
  | (selector lms d) => s!"{lms.toString}[{d}s]"

inductive InstantVectorType
| scalar
| vector

open InstantVectorType

inductive InstantVector : InstantVectorType → Type
  | selector (lms : LabelMatchers) (offset : Nat) : InstantVector vector -- TODO: regex, negative, and proof for minimal label requirements
  | literal (v : Float) : InstantVector InstantVectorType.scalar
  | sub_vector (vector_matching : Option VectorMatching) (a b : InstantVector vector) : InstantVector vector
  | add_scalar_left (a : InstantVector scalar) (b : InstantVector vector) : InstantVector vector
  | sum (a : AggregationSelector) (v : InstantVector vector) : InstantVector vector
  | topk (a : AggregationSelector) (k : Nat) (v : InstantVector vector) : InstantVector vector
  | rate (r : RangeVector) : InstantVector vector
  | label_replace (v : InstantVector vector) (dst replacement src regex : String) : InstantVector vector
  | time : InstantVector scalar 

def is_name := λ (l : KeyValuePair) => l.key == name_label

def TypeSafeSelector (lms : LabelMatchers) (e : Exporter) : Bool := 
  Option.isSome $ e.metrics.find? (λ m =>
    (List.all (lms.equal.filter $ not ∘ is_name) (λ l => m.labels.contains l.key )) &&
    (List.all (lms.equal.filter $ is_name) (λ l => m.name = l.value ))
  )

def InstantVector.typesafe {t : InstantVectorType} (v : InstantVector t) (e : Exporter) : Bool := match v with
  | (InstantVector.selector lm offset) => TypeSafeSelector lm e
  | (InstantVector.sub_vector _ a b) => typesafe a e && typesafe b e
  | _ => true

structure TypesafeInstantVector (t : InstantVectorType) (e : Exporter) where
v : InstantVector t
h : InstantVector.typesafe v e := by simp

def InstantVector.toString {t : InstantVectorType} : InstantVector t → String
  | (selector lms offset) => (String.join $ List.map (λ l => l.value) (lms.equal.filter (λ l => l.key == name_label))) ++ "{" ++ joinSep (List.map KeyValuePair.toString (lms.equal.filter λ l => l.key != name_label)) ", " ++ "}"
  | time => "time()"
  | (label_replace v dst replace src regex) => "label_replace()"
  | (rate r) => s!"rate({r.to_string})"
  | (sub_vector vm a b) => s!"{a.toString} - {b.toString}"
  | _ => ""

instance {t : InstantVectorType} : ToString $ InstantVector t where
  toString := InstantVector.toString

instance {t : InstantVectorType} : Repr $ InstantVector t where
  reprPrec v _ := InstantVector.toString v

#eval InstantVector.rate (RangeVector.selector (LabelMatchers.empty.withEqualMatcher "instance" "test") 5)

def Option.get : (a : Option α) → a.isSome → α
  | some a, _ => a

def getNameLm? (lms : LabelMatchers) : Option String :=
  let name? := lms.equal.find? (λl => is_name l)
  match name? with
    | (Option.some name) => name.value
    | (Option.none) => Option.none

def getMetricDef? (e : Exporter) (lms : LabelMatchers) : Option Metric := match getNameLm? lms with
  | (Option.some name) => e.metrics.find? (λm => m.name = name)
  | (Option.none) => Option.none

def RangeVector.unitOf (e : Exporter) : RangeVector → Option MetricUnit
  | (selector lms d) => match getMetricDef? e lms with
    | (Option.some metric) => metric.unit
    | _ => Option.none

def unitOf {t : InstantVectorType} (e : Exporter) : InstantVector t → Option MetricUnit
  | (.time) => MetricUnit.time
  | (.selector lms offset) => match getMetricDef? e lms with
    | (Option.some metric) => metric.unit
    | _ => Option.none
  | (.label_replace v _ _ _ _) => unitOf e v
  | (.sub_vector _ a b) => if (unitOf e a) = (unitOf e b) then unitOf e a else Option.none
  | (.add_scalar_left  s v) => unitOf e v -- adding a scalar doesn't change the unit (however, it might no longer make sense)
  | (.rate r) => (r.unitOf e).map $ λx => MetricUnit.div x (MetricUnit.seconds)
  | (.literal f) => MetricUnit.unitless -- alternatively we could implement this function only vectors

open Lean
open Lean.Parser
open Lean.PrettyPrinter

def name : Parser := withAntiquot (mkAntiquot "name" `LX.text) {
  fn := fun c s => 
    let startPos := s.pos;
    let s := takeWhile1Fn (fun c => c.isAlphanum || "_".contains c) "Invalid name" c s;
    mkNodeToken `LX.text startPos c s
}

@[combinatorFormatter name] def name.formatter : Formatter := pure ()
@[combinatorParenthesizer name] def name.parenthesizer : Parenthesizer := pure ()

declare_syntax_cat labelmatcher_aux
syntax name "=" strLit : labelmatcher_aux
macro_rules
| `(labelmatcher_aux| $key:name=$value) => `({key := $(quote key[0].getAtomVal!), value:= $value})

-- 
declare_syntax_cat labelmatcher
-- The parser 
syntax "{" labelmatcher_aux,* "}" : labelmatcher
-- Make label matcher a valid term (not really neccessary)
syntax labelmatcher : term

-- How to translate label matchers into Lean terms
macro_rules
| `(labelmatcher| {}) => `(LabelMatchers.empty)
| `(labelmatcher| { $x }) => `(LabelMatchers.empty.withEqualMatchers' $x)
--| `(labelmatcher| { $x, $xs,* }) => `({$xs,*})

declare_syntax_cat rangevector
syntax name (labelmatcher) "[" numLit "]" : rangevector

macro_rules
| `(rangevector| $name:name $xs [ $range ] ) => `(RangeVector.selector ($(xs).withName $(quote name[0].getAtomVal!)) $range)

declare_syntax_cat instantvector
syntax name (labelmatcher)? : instantvector
syntax "time()" : instantvector
syntax "rate(" rangevector ")" : instantvector
syntax instantvector " - " instantvector : instantvector

macro_rules
| `(instantvector| time()) => `(InstantVector.time)
| `(instantvector| $name:name) => `(InstantVector.selector (LabelMatchers.empty.withName $(quote name[0].getAtomVal!)) 0)
| `(instantvector| $name:name $xs) => `(InstantVector.selector ($(xs).withName $(quote name[0].getAtomVal!)) 0)
| `(instantvector| $a-$b) => `(InstantVector.sub_vector Option.none $a $b)
| `(instantvector| rate($rv)) => `(InstantVector.rate $rv)

syntax "[pql|" instantvector "]" : term

macro_rules
| `([pql| $v:instantvector]) => `($v)



set_option pp.rawOnError true
#eval [pql| up{}]
#eval [pql| up{instance="localhost"}]
#eval [pql| up - up]
-- #eval [pql| up{test="abc", lan="def"}]
#eval [pql| time()]
#eval [pql| rate(up{}[7])]


#eval s!"f"

-- syntax:max can be used to change precendense

syntax "`[abc| " name "]" : term
macro_rules
| `(`[abc| $x ]) => pure $ quote ("<" ++ x[0].getAtomVal! ++ "/>")

#check `[abc| def]
