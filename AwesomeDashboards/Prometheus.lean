import Lean

def joinSep (s : List String) (sep : String) : String := match s with
| [] => ""
| x::[] => x
| x::xs => x ++ sep ++ (joinSep xs sep)

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

def name_label := "__name__"

namespace ScrapeConfig

  def labels (s : ScrapeConfig) (name: String) : Option $ List String := 
    Option.map (fun v => s.targetLabels ++ v.labels ++ ["job", "instance", name_label]) (s.exporter.metrics.find? (fun m => m.name == name))

end ScrapeConfig

structure Environment where
scrapeConfigs: List ScrapeConfig
deriving Lean.FromJson, Lean.ToJson

inductive VectorMatching
  | ignoring
  | on
  deriving Lean.FromJson, Lean.ToJson

inductive AggregationSelector 
  | by (labels : List String)
  | without (labels : List String)
  deriving Lean.FromJson, Lean.ToJson

namespace AggregationSelector

  def toString (a : AggregationSelector) : String := match a with
  | (.by ls) => s!"by({joinSep ls ", "})"
  | (.without ls) => s!"without({joinSep ls ", "})"

end AggregationSelector

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
  This has to be an indexed family because "InstantVector scalar" depends on "InstantVecot vector" and vice versa (see scalar()). 
  Moreover both types are valid "entrypoints" for a query.
-/
inductive InstantVector : InstantVectorType → Type
  | selector (lms : List LabelMatcher) (offset : Nat) : InstantVector vector -- TODO: regex, negative, and proof for minimal label requirements
  | literal (v : Float) : InstantVector InstantVectorType.scalar
  | sub_vector (vector_matching : Option VectorMatching) (a b : InstantVector vector) : InstantVector vector
  | add_scalar_left (a : InstantVector scalar) (b : InstantVector vector) : InstantVector vector
  | sum (a : Option AggregationSelector) (v : InstantVector vector) : InstantVector vector
  | topk (a : Option AggregationSelector) (k : Nat) (v : InstantVector vector) : InstantVector vector
  | rate (r : RangeVector) : InstantVector vector
  | label_replace (v : InstantVector vector) (dst replacement src regex : String) : InstantVector vector
  | time : InstantVector scalar 
  deriving Lean.ToJson

namespace InstantVector

  def toString {t : InstantVectorType} : InstantVector t → String
    | (selector lms offset) => (String.join $ List.map (λ l => l.value) (lms.filter LabelMatcher.matches_name)) ++ "{" ++ joinSep (List.map LabelMatcher.toString (lms.filter $ not ∘ LabelMatcher.matches_name)) ", " ++ "}"
    | time => "time()"
    | (label_replace v dst replace src regex) => s!"label_replace({v.toString}, \"{dst}\", \"{replace}\", \"{src}\", \"{regex}\")"
    | (rate r) => s!"rate({r.to_string})"
    | (sub_vector vm a b) => s!"{a.toString} - {b.toString}"
    | (.sum a v) => 
        let a' := Option.getD (a.map (·.toString)) ""
        s!"sum {a'} ({v.toString})"
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
    (List.all (lms.filter $ not ∘ LabelMatcher.matches_name) (λ l => (m.labels ++ c.targetLabels ++ ["job", "instance"]).contains l.key ))
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
  
  def labels : {t : InstantVectorType} → TypesafeInstantVector t e → List String
  | t, ⟨v, h⟩ => match v with
    | .selector lms _ => (match LabelMatcher.getMatchedName lms with
        | .some name => Option.getD (e.scrapeConfigs.findSome? (fun c => c.labels name)) []
        | .none => []
      )
    | .literal v => []
    -- We want to use h as evidence to retrieve the labels
    | .add_scalar_left a b => labels ⟨b, sorry⟩
    /- label_replace might or might not create a new label. 
    The the current implementation we can't know whether the regex will match, so we always add the new label.
    If the label was already present then it is now present twice... 
    -/
    | .label_replace v dst repl src reg => dst :: labels ⟨v, sorry⟩ 
    /- Any operation on a range vectors will remove the label __name__ -/
    | _ => []

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
  | (.sum a v) => unitOf e v
  | (.topk a k v) => unitOf e v

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

-- Used in places where a list of labels is required, e.g. by-clause.
declare_syntax_cat labelref
-- Without the asterisks we get "invalid syntax node kind 'labelref.pseudo'"
-- TODO: Find a way to convert a list of label names to a list of strings without asterisks.
syntax "**" name : labelref
macro_rules
| `(labelref| ** $n) => `(term| $(quote n.raw[0].getAtomVal))

declare_syntax_cat instantvector
syntax name (labelmatchers)? : instantvector
syntax "time()" : instantvector
syntax "rate(" rangevector ")" : instantvector
syntax "label_replace(" instantvector "," strLit "," strLit "," strLit "," strLit ")" : instantvector
syntax instantvector " - " instantvector : instantvector
syntax "sum" "(" instantvector ")"  : instantvector
syntax "sum" "by" "(" labelref,* ")" "(" instantvector ")"  : instantvector
syntax "sum" "(" instantvector ")" "by" "(" labelref,* ")" : instantvector
syntax "sum" "without" "(" labelref,* ")" "(" instantvector ")" : instantvector
syntax "sum" "(" instantvector ")" "without" "(" labelref,* ")" : instantvector

macro_rules
| `(instantvector| time()) => `(InstantVector.time)
| `(instantvector| $name:name) => `(InstantVector.selector [LabelMatcher.equal name_label $(quote name.raw[0].getAtomVal)] 0)
| `(instantvector| $name:name $xs:labelmatchers) => `(InstantVector.selector ((LabelMatcher.equal name_label $(quote name.raw[0].getAtomVal)) :: $(xs)) 0)
| `(instantvector| $a-$b) => `(InstantVector.sub_vector Option.none $a $b)
| `(instantvector| rate($rv)) => `(InstantVector.rate $rv)
| `(instantvector| label_replace($v, $dst, $repl, $src, $reg)) => `(InstantVector.label_replace $v $dst $repl $src $reg)
| `(instantvector| sum($v)) => `(InstantVector.sum .none $v)
| `(instantvector| sum by($bs,*) ($v)) => `(InstantVector.sum (AggregationSelector.by [$bs,*]) ($v))
| `(instantvector| sum ($v) by($bs,*)) => `(InstantVector.sum (AggregationSelector.by [$bs,*]) ($v))
| `(instantvector| sum without($bs,*) ($v)) => `(InstantVector.sum (AggregationSelector.by [$bs,*]) ($v))
| `(instantvector| sum ($v) without($bs,*)) => `(InstantVector.sum (AggregationSelector.by [$bs,*]) ($v))

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
#eval [pql| label_replace(up, "new", "$1-xyz", "job", "(.*)") ]
#eval [pql| sum (up{})]
#eval [pql| sum by(**instance) (up)]
#eval [pql| sum (up) by(**instance)]
#eval [pql| sum without (**instance) (up)]
#eval [pql| sum (up) without (**instance)]

-- syntax:max can be used to change precendense

def up_templated_with_job (job: String) : InstantVector .vector := InstantVector.selector [.equal name_label job] 0

def bytes_receives_templated_with_rate_window (rate_window : Nat) : InstantVector .vector := InstantVector.rate $ RangeVector.selector [.equal name_label "node_network_receive_bytes_total"] rate_window

inductive TemplateVariableType
| label_value

-- TODO: Should we build up the variables as they are added (and merge if we combine them and proving that they don't collide) or define them upfront?
-- Then we always get the minimal set.
-- How do we build up the type? Is there a better inspiration than vector?
-- What data structure can be used to to build vars, that guarantees that each name is only of at most one type?
inductive TemplatedInstantVector (vars : String → Option TemplateVariableType) : InstantVectorType → Type where
| selector (selectors: List (String × String)) (h : ∀ h, h ∈ selectors → vars h.snd = .some .label_value) : TemplatedInstantVector vars vector
| add_vector (x y : TemplatedInstantVector vars vector) : TemplatedInstantVector vars vector

namespace TemplatedInstantVector
  def toString {vars : String → Option TemplateVariableType} {t: InstantVectorType} (v: TemplatedInstantVector vars t) : String := match v with
    | .selector sls h => "{" ++ joinSep (sls.map (λs => s.fst ++ "=\"$" ++ s.snd ++ "Ä")) "," ++ "}"
    | .add_vector x y => s!"{toString x} + {toString y}"

  instance {vars : String → Option TemplateVariableType} {t: InstantVectorType} : ToString (TemplatedInstantVector vars t) := {
    toString := (fun v => v.toString)
  }

  def noVar : String → Option TemplateVariableType := (fun _ => .none)
  def sampleVars : String → Option TemplateVariableType := (fun x => if x = "schedule" then .some .label_value else .none)

  #eval TemplatedInstantVector.add_vector (.selector [⟨"instance", "schedule"⟩] (by
    intro var
    intro h
    rfl
  ) : TemplatedInstantVector sampleVars .vector) (.selector [] sorry : TemplatedInstantVector sampleVars _)
end TemplatedInstantVector
