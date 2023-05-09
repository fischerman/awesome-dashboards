import Lean

inductive MetricType
| counter
| gauge
| histogram
| untyped
deriving Lean.FromJson, Lean.ToJson

-- https://prometheus.io/docs/practices/naming/#base-units
@[match_pattern]
inductive MetricUnit
| seconds
| bytes
| bool
| time
| unitless
| div (dividend devisor : MetricUnit)
deriving Repr, BEq, DecidableEq, Lean.FromJson, Lean.ToJson

structure Metric where
name : String
type : MetricType
labels : List String  -- TODO: some metrics have dynamic label keys
unit : MetricUnit
deriving Lean.FromJson, Lean.ToJson

def up : Metric := {
  name := "up"
  type := MetricType.gauge
  labels := ["instance", "job"]
  unit := MetricUnit.bool
}

structure Exporter where
metrics : List Metric
deriving Lean.FromJson, Lean.ToJson

-- The resulting labels of the scrape is a combination of target labels and metric labels. 
-- Prometheus has a config "honor_labels" which deals with conflict.
-- We do not implement this faithful.
structure ScrapeConfig where
targetLabels : List String
exporter : Exporter
deriving Lean.FromJson, Lean.ToJson

namespace ScrapeConfig

  def labels (s : ScrapeConfig) (name: String) : Option $ List String := 
    Option.map (fun v => s.targetLabels ++ v.labels ++ ["job", "instace"]) (s.exporter.metrics.find? (fun m => m.name == name))

end ScrapeConfig

structure Environment where
scrapeConfigs: List ScrapeConfig
deriving Lean.FromJson, Lean.ToJson

inductive VectorMatching
  | ignoring
  | on
  deriving Lean.FromJson, Lean.ToJson

inductive AggregationSelector 
  deriving Lean.FromJson, Lean.ToJson

def name_label := "__name__"

structure KeyValuePair where
(key : String)
(value : String)
deriving Lean.FromJson, Lean.ToJson

def KeyValuePair.toString (kvp : KeyValuePair) := kvp.key ++ "=\"" ++ kvp.value ++ "\""

def joinSep (s : List String) (sep : String) : String := match s with
| [] => ""
| x::[] => x
| x::xs => x ++ sep ++ (joinSep xs sep)

structure LabelMatchers where
  equal : List KeyValuePair
  deriving Lean.FromJson, Lean.ToJson

namespace LabelMatchers

  def empty : LabelMatchers := {equal := []}

  def withEqualMatcher (m : LabelMatchers) (key : String) (value : String) := {
    m with equal := {key := key, value:= value} :: m.equal
  }

  def withEqualMatchers' (m : LabelMatchers) (x : KeyValuePair) := {
    m with equal := x :: m.equal
  }

  def withName (m : LabelMatchers) (value : String) := withEqualMatcher m name_label value

  def toString (m : LabelMatchers) := "{" ++ joinSep (m.equal.map KeyValuePair.toString) ", " ++ "}"

  def getMatchedName (m : LabelMatchers) : Option String := Option.map (fun e => e.value) (m.equal.find? fun e => e.key == name_label)

end LabelMatchers

inductive RangeVector
  | selector (lms : LabelMatchers) (duration : Nat)
  deriving Lean.FromJson, Lean.ToJson

def RangeVector.to_string : RangeVector → String
  | (selector lms d) => s!"{lms.toString}[{d}s]"

inductive InstantVectorType
  | scalar
  | vector
  deriving Lean.FromJson, Lean.ToJson

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
  deriving Lean.ToJson

def is_name := λ (l : KeyValuePair) => l.key == name_label

-- A label matcher is safe when
-- - every label that is used exists in combination with all other labels

-- - currently requires that there is an equlity matcher on the name
-- - matches on the any occurence of the metric name in the scrape config. Only one must fit. TODO: Check that all definitions are working.
def typeSafeSelector (lms : LabelMatchers) (e : Environment) : Bool := match lms.getMatchedName with
| (.some v) => Option.isSome $ e.scrapeConfigs.find? (fun c => Option.isSome $ c.exporter.metrics.find? (λ m =>
    v == m.name &&
    (List.all (lms.equal.filter $ not ∘ is_name) (λ l => (m.labels ++ c.targetLabels ++ ["job", "instace"]).contains l.key ))
  ))
| .none => false

def InstantVector.typesafe {t : InstantVectorType} (v : InstantVector t) (e : Environment) : Bool := match v with
  | (InstantVector.selector lm offset) => typeSafeSelector lm e
  | (InstantVector.sub_vector _ a b) => typesafe a e && typesafe b e
  | _ => true

structure TypesafeInstantVector (t : InstantVectorType) (e : Environment) where
  v : InstantVector t
  h : InstantVector.typesafe v e := by simp

instance (t : InstantVectorType) (e : Environment) : Lean.ToJson $ TypesafeInstantVector t e where
  toJson := fun v => Lean.toJson v.v

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

def getMetricDef? (e : Environment) (lms : LabelMatchers) : Option Metric := match getNameLm? lms with
  | (Option.some name) => (e.scrapeConfigs.findSome? (fun c => c.exporter.metrics.find? (λm => m.name = name)))
  | (Option.none) => Option.none

def RangeVector.unitOf (e : Environment) : RangeVector → Option MetricUnit
  | (selector lms d) => match getMetricDef? e lms with
    | (Option.some metric) => metric.unit
    | _ => Option.none

def unitOf {t : InstantVectorType} (e : Environment) : InstantVector t → Option MetricUnit
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

@[combinator_formatter name] def name.formatter : Formatter := pure ()
@[combinator_parenthesizer name] def name.parenthesizer : Parenthesizer := pure ()

declare_syntax_cat labelmatcher_aux
syntax name "=" strLit : labelmatcher_aux
macro_rules
| `(labelmatcher_aux| $key:name=$value) => `({key := $(quote key.raw[0].getAtomVal), value:= $value})
syntax labelmatcher_aux : term

-- 
declare_syntax_cat labelmatcher
-- The parser 
syntax "{" labelmatcher_aux,* "}" : labelmatcher
-- Make label matcher a valid term (not really neccessary)
syntax labelmatcher : term

-- See https://github.com/leanprover/lean4/pull/1251/commits/9eff6572334a40f671928400614309455c76ef38#diff-52ef0c67eea613acf6c0b6284063fd7112cdd40750de38365c214abaef246db4R1854
-- TODO: Remove this workaround
open TSyntax.Compat

-- How to translate label matchers into Lean terms
macro_rules
| `(labelmatcher| {}) => `(LabelMatchers.empty)
| `(labelmatcher| { $x }) => `(LabelMatchers.empty.withEqualMatchers' $x)
-- | `(labelmatcher| { $xs,* }) => `($(Array.map (fun x => x) xs))

declare_syntax_cat rangevector
syntax name (labelmatcher) "[" numLit "]" : rangevector

macro_rules
| `(rangevector| $name:name $xs [ $range ] ) => `(RangeVector.selector ($(xs).withName $(quote name.raw[0].getAtomVal)) $range)

declare_syntax_cat instantvector
syntax name (labelmatcher)? : instantvector
syntax "time()" : instantvector
syntax "rate(" rangevector ")" : instantvector
syntax instantvector " - " instantvector : instantvector

macro_rules
| `(instantvector| time()) => `(InstantVector.time)
| `(instantvector| $name:name) => `(InstantVector.selector (LabelMatchers.empty.withName $(quote name.raw[0].getAtomVal)) 0)
| `(instantvector| $name:name $xs) => `(InstantVector.selector ($(xs).withName $(quote name.raw[0].getAtomVal)) 0)
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
