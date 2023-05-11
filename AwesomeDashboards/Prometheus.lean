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
help : String
deriving Lean.FromJson, Lean.ToJson

def up : Metric := {
  name := "up"
  type := MetricType.gauge
  labels := ["instance", "job"]
  unit := MetricUnit.bool
  help := "Whether the target is up"
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

inductive LabelMatcher
| equal (key : String) (value : String)
deriving Lean.FromJson, Lean.ToJson

namespace LabelMatcher

  def toString (kvp : LabelMatcher) := match kvp with
  | (.equal k v) => s!"{k}=\"{v}\""

  def getMatchedName (lms : List LabelMatcher) : Option String := match lms with
  | ⟨k, v⟩ :: xs => if k == name_label then .some v else getMatchedName xs
  | [] => .none

  def matches_name (lm : LabelMatcher) := match lm with
  | (.equal k _) => k == name_label

  def key (lm : LabelMatcher) := match lm with
  | (.equal k _) => k

  def value (lm : LabelMatcher) := match lm with
  | (.equal _ v) => v

end LabelMatcher

def joinSep (s : List String) (sep : String) : String := match s with
| [] => ""
| x::[] => x
| x::xs => x ++ sep ++ (joinSep xs sep)

inductive RangeVector
  | selector (lms : List LabelMatcher) (duration : Nat)
  deriving Lean.FromJson, Lean.ToJson

def RangeVector.to_string : RangeVector → String
  | (selector lms d) => s!"{(String.join $ List.map (λ l => l.value) (lms.filter LabelMatcher.matches_name)) ++ "{" ++ joinSep (List.map LabelMatcher.toString (lms.filter $ not ∘ LabelMatcher.matches_name)) ", " ++ "}"}[{d}s]"

inductive InstantVectorType
  | scalar
  | vector
  deriving Lean.FromJson, Lean.ToJson

open InstantVectorType

/--
  This has to be an indexed family because "InstantVector scalar" depends on "InstantVecot vector" and vice versa. 
  Moreover both types are valid "entrypoints" for a query.
-/
inductive InstantVector : InstantVectorType → Type
  | selector (lms : List LabelMatcher) (offset : Nat) : InstantVector vector -- TODO: regex, negative, and proof for minimal label requirements
  | literal (v : Float) : InstantVector InstantVectorType.scalar
  | sub_vector (vector_matching : Option VectorMatching) (a b : InstantVector vector) : InstantVector vector
  | add_scalar_left (a : InstantVector scalar) (b : InstantVector vector) : InstantVector vector
  | sum (a : AggregationSelector) (v : InstantVector vector) : InstantVector vector
  | topk (a : AggregationSelector) (k : Nat) (v : InstantVector vector) : InstantVector vector
  | rate (r : RangeVector) : InstantVector vector
  | label_replace (v : InstantVector vector) (dst replacement src regex : String) : InstantVector vector
  | time : InstantVector scalar 
  deriving Lean.ToJson

namespace InstantVector

  def toString {t : InstantVectorType} : InstantVector t → String
    | (selector lms offset) => (String.join $ List.map (λ l => l.value) (lms.filter LabelMatcher.matches_name)) ++ "{" ++ joinSep (List.map LabelMatcher.toString (lms.filter $ not ∘ LabelMatcher.matches_name)) ", " ++ "}"
    | time => "time()"
    | (label_replace v dst replace src regex) => "label_replace()"
    | (rate r) => s!"rate({r.to_string})"
    | (sub_vector vm a b) => s!"{a.toString} - {b.toString}"
    | _ => ""

  inductive Subterm : {t t' : InstantVectorType} →  InstantVector t → InstantVector t' → Prop where
    | sub_vector_left {t : InstantVectorType} {vm : Option VectorMatching} {a b : InstantVector vector} {x : InstantVector t} : Subterm x a → Subterm x (.sub_vector vm a b)
    | sub_vector_right {t : InstantVectorType} {vm : Option VectorMatching} {a b : InstantVector vector} {x : InstantVector t} : Subterm x b → Subterm x (.sub_vector vm a b)
    --| add_scalar_left_scalar : Subterm 
    | refl {t : InstantVectorType} (x : InstantVector t) : Subterm x x

  example : Subterm (.selector [] 0) (.sub_vector .none (.sub_vector .none (.selector [] 0) (.selector [] 1)) (.selector [] 1)) := by repeat constructor
end InstantVector

/--
A label matcher is safe when
- every label that is used exists in combination with all other labels

- currently requires that there is an equlity matcher on the name
- matches on the any occurence of the metric name in the scrape config. Only one must fit. TODO: Check that all definitions are working.
-/
def typeSafeSelector (lms : List LabelMatcher) (e : Environment) : Bool := match LabelMatcher.getMatchedName lms with
| (.some v) => Option.isSome $ e.scrapeConfigs.find? (fun c => Option.isSome $ c.exporter.metrics.find? (λ m =>
    v == m.name &&
    (List.all (lms.filter $ not ∘ LabelMatcher.matches_name) (λ l => (m.labels ++ c.targetLabels ++ ["job", "instace"]).contains l.key ))
  ))
| .none => false

def InstantVector.typesafe {t : InstantVectorType} (v : InstantVector t) (e : Environment) : Bool := match v with
  | (InstantVector.selector lm offset) => typeSafeSelector lm e
  | (InstantVector.sub_vector _ a b) => typesafe a e && typesafe b e
  | _ => true

theorem subterm_typesafe {e : Environment} {t t' : InstantVectorType} (v : InstantVector t) (v' : InstantVector t') (h : InstantVector.Subterm v v') (h' : v'.typesafe e) : v.typesafe e := by
  unfold InstantVector.typesafe
  sorry
  -- induction v'
    


structure TypesafeInstantVector (t : InstantVectorType) (e : Environment) where
  v : InstantVector t
  h : InstantVector.typesafe v e := by simp

namespace TypesafeInstantVector
  variable {t : InstantVectorType} {e : Environment}

  /--
    It isn't really sensible to retrieve a single help string from a query.
    There could multiple metrics or none at all.
    This is just for demonstration purposes. A more fruitful approach would be to generate a string from all components of the query.
  -/
  def helpString : {t : InstantVectorType} → TypesafeInstantVector t e → String
    | t, ⟨v, h⟩ => match v with
      | .selector lms _ => (match LabelMatcher.getMatchedName lms with
        | .some v => Option.getD (e.scrapeConfigs.findSome? (fun c => c.exporter.metrics.findSome? (fun m => if m.name == v then m.help else .none))) ""
        | .none => ""
      )
      | .label_replace v' _ _ _ _ => helpString ⟨v', sorry⟩ -- use subterm_typesafe here
      | _ => ""
      

end TypesafeInstantVector

instance (t : InstantVectorType) (e : Environment) : Lean.ToJson $ TypesafeInstantVector t e where
  toJson := fun v => Lean.toJson v.v

instance {t : InstantVectorType} : ToString $ InstantVector t where
  toString := InstantVector.toString

instance {t : InstantVectorType} : Repr $ InstantVector t where
  reprPrec v _ := InstantVector.toString v

#eval InstantVector.rate (RangeVector.selector [.equal "instance" "test"] 5)

def Option.get : (a : Option α) → a.isSome → α
  | some a, _ => a

def getMetricDef? (e : Environment) (lms : List LabelMatcher) : Option Metric := match LabelMatcher.getMatchedName lms with
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

declare_syntax_cat labelmatcher
syntax name "=" strLit : labelmatcher
macro_rules
| `(labelmatcher| $key:name=$value) => `(LabelMatcher.equal $(quote key.raw[0].getAtomVal) $value)

-- 
declare_syntax_cat labelmatchers
-- The parser 
syntax "{" labelmatcher,* "}" : labelmatchers
-- See https://github.com/leanprover/lean4/pull/1251/commits/9eff6572334a40f671928400614309455c76ef38#diff-52ef0c67eea613acf6c0b6284063fd7112cdd40750de38365c214abaef246db4R1854
-- TODO: Remove this workaround
open TSyntax.Compat
-- How to translate label matchers into Lean terms
macro_rules
| `(labelmatchers| { $xs,* }) => `([$xs,*])

declare_syntax_cat rangevector
syntax name (labelmatchers) "[" numLit "]" : rangevector

macro_rules
| `(rangevector| $name:name $xs [ $range ] ) => `(RangeVector.selector ((LabelMatcher.equal name_label $(quote name.raw[0].getAtomVal)) :: $(xs)) $range)

declare_syntax_cat instantvector
syntax name (labelmatchers)? : instantvector
syntax "time()" : instantvector
syntax "rate(" rangevector ")" : instantvector
syntax instantvector " - " instantvector : instantvector

macro_rules
| `(instantvector| time()) => `(InstantVector.time)
| `(instantvector| $name:name) => `(InstantVector.selector [LabelMatcher.equal name_label $(quote name.raw[0].getAtomVal)] 0)
| `(instantvector| $name:name $xs:labelmatchers) => `(InstantVector.selector ((LabelMatcher.equal name_label $(quote name.raw[0].getAtomVal)) :: $(xs)) 0)
| `(instantvector| $a-$b) => `(InstantVector.sub_vector Option.none $a $b)
| `(instantvector| rate($rv)) => `(InstantVector.rate $rv)

syntax "[pql|" instantvector "]" : term

macro_rules
| `([pql| $v:instantvector]) => `($v)



set_option pp.rawOnError true
#eval [pql| up{}]
#eval [pql| up{instance="localhost"}]
#eval [pql| up - up]
#eval [pql| up{test="abc", lan="def"} ]
#eval [pql| time()]
#eval [pql| rate(up{}[7])]

-- syntax:max can be used to change precendense
