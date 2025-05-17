import Lean

set_option linter.unusedVariables false

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

/--
Units for time series.
The have no formal meaning in promql and must be derived from the system that emits the metric.

https://prometheus.io/docs/practices/naming/#base-units
-/
@[match_pattern]
inductive MetricUnit
| seconds
| bytes
| bool
| time
| unitless
/--
  div is the result of a devision where dividend and devisor have different units.
  The result of rate() is a div where the devisor unit is seconds.
 -/
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

/--
The label key that contains the metric name.
up{} is equvilant to {\_\_name\_\_="up"}

https://prometheus.io/docs/prometheus/latest/querying/basics/#instant-vector-selectors
-/
def name_label := "__name__"

namespace ScrapeConfig

  def labels (s : ScrapeConfig) (name: String) : Option $ List String :=
    Option.map (fun v => s.targetLabels ++ v.labels ++ ["job", "instance", name_label]) (s.exporter.metrics.find? (fun m => m.name == name))

end ScrapeConfig

structure Environment where
scrapeConfigs: List ScrapeConfig
deriving Lean.FromJson, Lean.ToJson

/--
VectorMatching defines on which labels vectors are matched.

TODO: Add actual label keys. Currently it only determines whether.
TODO: A vector matcher is optional. By default all labels must match.
-/
inductive VectorMatching
  | ignoring
  | on
  deriving Lean.FromJson, Lean.ToJson

/--
AggregationSelector defines what labels are kept during aggregation.
https://prometheus.io/docs/prometheus/latest/querying/operators/#aggregation-operators
-/
inductive AggregationSelector
  | by (labels : List String)
  | without (labels : List String)
  deriving Lean.FromJson, Lean.ToJson

namespace AggregationSelector

  def toString (a : AggregationSelector) : String := match a with
  | (.by ls) => s!"by({joinSep ls ", "})"
  | (.without ls) => s!"without({joinSep ls ", "})"

end AggregationSelector

/--
Label matcher is a single matcher on a time series.
https://prometheus.io/docs/prometheus/latest/querying/basics/#instant-vector-selectors

Currently on equality matchers are supported.
TODO: Support inequality and regex matchers.
-/
inductive LabelMatcher
| equal (key : String) (value : String)
deriving Lean.FromJson, Lean.ToJson

namespace LabelMatcher

  def toString (kvp : LabelMatcher) := match kvp with
  | (.equal k v) => s!"{k}=\"{v}\""

  /-- Returns the value of the metric name if it exists. Label matchers don't need to select a metric name. -/
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
  An instant vector is used to query Prometheus at a point in time.

  This has to be an indexed family because "InstantVector scalar" depends on "InstantVecot vector" and vice versa (see scalar()).
  Moreover both types are valid "entrypoints" for a query.

  TODO: Replace specific binary operator constructor with generic once. In most cases the particular operator doesn't matter and it should make proofs easier.
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

  /-- Subterms are terms that are contained in a another term. This is currently not exhaustive and does not contain range vectors. -/
  inductive Subterm : {t t' : InstantVectorType} →  InstantVector t → InstantVector t' → Prop where
    | refl {t : InstantVectorType} (x : InstantVector t) : Subterm x x
    | sub_vector_left {t : InstantVectorType} {vm : Option VectorMatching} {a b : InstantVector vector} {x : InstantVector t} : Subterm x a → Subterm x (.sub_vector vm a b)
    | sub_vector_right {t : InstantVectorType} {vm : Option VectorMatching} {a b : InstantVector vector} {x : InstantVector t} : Subterm x b → Subterm x (.sub_vector vm a b)
    | add_scalar_left_vector {x : InstantVector vector} {a : InstantVector scalar} : Subterm x (.add_scalar_left a x)
    | label_replace {dst replacement src regex : String} : Subterm x (.label_replace x dst replacement src regex)
    | sum {a : Option AggregationSelector} {v : InstantVector vector} : Subterm v (.sum a v)
    | topk (a : Option AggregationSelector) (k : Nat) (v : InstantVector vector) : Subterm v (.topk a k v)

  example : Subterm (.selector [] 0) (.sub_vector .none (.sub_vector .none (.selector [] 0) (.selector [] 1)) (.selector [] 1)) := by repeat constructor
end InstantVector

/--
A label matcher is safe when
- every label that is used exists in combination with all other labels

- currently requires that there is an equlity matcher on the name
- matches on any occurence of the metric name in the scrape config. Only one must fit. TODO: Check that all definitions are working.
-/
def typeSafeSelector (lms : List LabelMatcher) (e : Environment) : Bool := match LabelMatcher.getMatchedName lms with
| (.some v) => Option.isSome $ e.scrapeConfigs.find? (fun c => Option.isSome $ c.exporter.metrics.find? (λ m =>
    v == m.name &&
    (List.all (lms.filter $ not ∘ LabelMatcher.matches_name) (λ l => (m.labels ++ c.targetLabels ++ ["job", "instance"]).contains l.key ))
  ))
| .none => false

def RangeVector.typesafe (r : RangeVector) (e : Environment) : Bool := match r with
  | (.selector lms duration) => typeSafeSelector lms e

def InstantVector.typesafe {t : InstantVectorType} (v : InstantVector t) (e : Environment) : Bool := match v with
  | (InstantVector.selector lm offset) => typeSafeSelector lm e
  | (InstantVector.sub_vector _ a b) => typesafe a e && typesafe b e
  | (InstantVector.add_scalar_left a b) => typesafe b e
  | (InstantVector.label_replace v dst replacement src regex) => typesafe v e
  -- Short coming: The labels of sum don't have to exist.
  -- Instead of using the environment we need an intermediate state to track what labels are selected.
  | (InstantVector.sum a v) => typesafe v e
  | (InstantVector.topk a k v) => typesafe v e
  | (InstantVector.rate r) => RangeVector.typesafe r e
  | _ => false

/-- Any subterm of an type-safe instant vector is also type-safe with respect to the environment. -/
theorem subterm_typesafe {e : Environment} {t t' : InstantVectorType} (v : InstantVector t) (v' : InstantVector t') (h : v.Subterm v') (h' : v'.typesafe e) : v.typesafe e := by
  induction v'
  case literal =>
    cases h
    apply h'
  case selector lms offset =>
    cases h
    rewrite [<-h']
    eq_refl
  case sub_vector vm a b a_ih b_ih =>
    cases h with
    | sub_vector_left h'' =>
      apply a_ih
      apply h''
      unfold InstantVector.typesafe at h'
      rewrite [Bool.and_eq_true] at h'
      cases h'
      assumption
    | sub_vector_right h'' =>
      apply b_ih
      apply h''
      unfold InstantVector.typesafe at h'
      rewrite [Bool.and_eq_true] at h'
      cases h'
      assumption
    | refl =>
      assumption
  case time =>
    cases h
    assumption
  case label_replace v dst replacement src regex ih =>
    cases h
    case refl =>
      apply h'
    case label_replace =>
      apply h'
  case add_scalar_left a b iha ihb =>
    cases h
    case refl =>
      apply h'
    case add_scalar_left_vector =>
      apply h'
  case sum sel v' ih =>
    cases h
    case refl | sum =>
      apply h'
  case topk =>
    cases h
    case refl | topk =>
      apply h'
  case rate =>
    cases h
    case refl =>
      apply h'


structure TypesafeInstantVector (t : InstantVectorType) (e : Environment) where
  v : InstantVector t
  h : InstantVector.typesafe v e := by eq_refl

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
      | .label_replace v' dst replacement src regex => helpString ⟨v', subterm_typesafe v' (.label_replace v' dst replacement src regex) InstantVector.Subterm.label_replace h⟩
      | _ => ""

  /--
    Returns the keys of the labels of the query.
  -/
  def labels : {t : InstantVectorType} → TypesafeInstantVector t e → List String
  | t, ⟨v, h⟩ => match v with
    | .selector lms _ => (match LabelMatcher.getMatchedName lms with
        | .some name => Option.getD (e.scrapeConfigs.findSome? (fun c => c.labels name)) []
        | .none => []
      )
    | .literal v => []
    -- We want to use h as evidence to retrieve the labels
    | .add_scalar_left a b => labels ⟨b, subterm_typesafe b (.add_scalar_left a b) (InstantVector.Subterm.add_scalar_left_vector) h⟩
    /- label_replace might or might not create a new label.
    The the current implementation we can't know whether the regex will match, so we always add the new label.
    If the label was already present then it is now present twice...
    -/
    | .label_replace v dst repl src reg => dst :: labels ⟨v, subterm_typesafe v (.label_replace v dst repl src reg) (InstantVector.Subterm.label_replace) h⟩
    /- Any operation on a range vectors will remove the label \_\_name\_\_ -/
    | _ => []

end TypesafeInstantVector

instance (t : InstantVectorType) (e : Environment) : Lean.ToJson $ TypesafeInstantVector t e where
  toJson := fun v => Lean.toJson v.v

instance {t : InstantVectorType} : ToString $ InstantVector t where
  toString := InstantVector.toString

instance {t : InstantVectorType} : Repr $ InstantVector t where
  reprPrec v _ := InstantVector.toString v

#eval InstantVector.rate (RangeVector.selector [.equal "instance" "test"] 5)

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
   /-- A variable that can be used for a label value. -/
| label_value
  /-- A variable that can be used in a selector representing a list of labels pairs. -/
| key_value_pairs
deriving DecidableEq

def Vars := String → Option TemplateVariableType

/--
  Combines the maps a and b.
  Variables in a have precendence.
-/
def Vars.or (a b : Vars) : Vars := fun n => match a n with
  | (.some x) => .some x
  | .none => b n

-- Should this be a bool or a decidable prop?
-- def no_conflict (a b : Vars) : Bool := _
def no_conflict (a b : Vars) : Prop := ∀x, a x == b x ∨ a x == .none ∨ b x == .none

def dec_selector_value_bound (vars : Vars) (x : String × String) : Decidable (vars x.snd = some TemplateVariableType.label_value) :=
  match vars x.snd with
  | .none => isFalse (by
        intro
        contradiction
      )
  | .some .key_value_pairs => isFalse (by
        intro h
        cases h
      )
  | .some TemplateVariableType.label_value => isTrue rfl

instance (vars : Vars) (x : String × String) : Decidable (vars x.snd = some TemplateVariableType.label_value) := dec_selector_value_bound vars x

/--
  Each variable used in the selector is present in vars, i.e. bound.
  vars may contain variables not used in the selector.
-/
def selector_values_bound (vars : Vars) (selectors : List (String × String)) := ∀ h, h ∈ selectors → vars h.snd = .some .label_value

/--
  Whether the selector values are by is decidable.

  TODO: Instead of returning Decidable, we can return a wrapper that contains an error message when Decidable is false.
-/
def dec_selector_values_bound (vars : Vars) (selectors : List (String × String)) : Decidable (selector_values_bound vars selectors) :=
  match selectors with
  | [] => isTrue (by
      unfold selector_values_bound
      intro s hs
      cases hs
    )
  | x :: xs => if h : vars x.snd = .some .label_value then (by
    -- How do I split Decidable on a list into head and tail
    match dec_selector_values_bound vars xs with
    | isTrue z => {
      apply isTrue
      unfold selector_values_bound at z
      intro hh
      intro hhh
      cases hhh with
      | head => {
        exact h
      }
      | tail => {
        apply z
        trivial
      }
    }
    | isFalse z => {
      apply isFalse
      intro hh
      apply z
      unfold selector_values_bound
      intro hhh
      intro hhhh
      apply hh
      apply List.Mem.tail
      exact hhhh
    }
  ) else isFalse (by
    unfold selector_values_bound
    intro hh
    have hhh := hh x (List.Mem.head _)
    contradiction
  )

instance (vars : Vars) (selectors : List (String × String)) : Decidable (selector_values_bound vars selectors) := dec_selector_values_bound vars selectors

def LabelName := String
def LabelValue := String
/-- The name of a variable -/
def VarRef := String

/-- A version of promql that allows the use of variables. The variables can be of the wrong type or not defined at all, hence unsafe. -/
inductive UnsafeTemplatedInstantVector : InstantVectorType → Type where
/--
  value_vars contains eqality selectors where the values are variables.
  literal_equal_selectors contains equality matchers that don't use variables.
  key_value_vars contains variables whose content can contain any matchers.

  TODO: Should we allow selectors of different types in any order by having a top-level list which can contain all the possible selectors types?
-/
| selector (value_vars: List (LabelName × VarRef)) (literal_equal_selectors: List (LabelName × LabelValue)) (key_value_vars: List VarRef) : UnsafeTemplatedInstantVector vector
/-- For simplicity, we could have an operator enum instead of having each operator as a constructor. -/
| add_vector (x y : UnsafeTemplatedInstantVector vector) : UnsafeTemplatedInstantVector vector

-- TODO: Should we build up the variables as they are added (and merge if we combine them and proving that they don't collide) or define them upfront?
-- Then we always get the minimal set.
-- How do we build up the type? Is there a better inspiration than vector?
-- What data structure can be used to to build vars, that guarantees that each name is only of at most one type?
--
-- The COQ book has an example on simple lambda with type variables.
inductive TemplatedInstantVector (vars : Vars) : InstantVectorType → Type where
/-- key_value_vars may be unsafe. -/
| selector (selectors: List (String × String)) (h : selector_values_bound vars selectors) (literal_selector: List (String × String)) (key_value_vars: List String) /-(h₂: ∀ x, x ∈ key_value_vars → vars x = .some .key_value_pairs)-/ : TemplatedInstantVector vars vector
| add_vector (x y : TemplatedInstantVector vars vector) : TemplatedInstantVector vars vector

/--
  If a and b are not conflicting then we can add b to the vars of v.
  Basically, we can add unused variables.
-/
def vars_or_left {t : InstantVectorType} {a b : Vars} (v : TemplatedInstantVector a t) (h : no_conflict a b) : TemplatedInstantVector (a.or b) t := sorry

-- If a and b are not conflicting then we can add a to the vars of v.
def vars_or_right {t : InstantVectorType} {a b : Vars} (v : TemplatedInstantVector b t) (h : no_conflict a b) : TemplatedInstantVector (a.or b) t := sorry

namespace TemplatedInstantVector
  /--
    check returns a type-safe vector if all variables in x are bound by vars.
  -/
  def check {t : InstantVectorType} (vars : Vars) (x : UnsafeTemplatedInstantVector t) : Option (TemplatedInstantVector vars t) := match x with
  | .selector selectors lit key_values_vars => match dec_selector_values_bound vars selectors with
      | isTrue h => .some $ .selector selectors h lit key_values_vars
      | isFalse _ => .none
  | .add_vector x y => match check vars x, check vars y with
      | .none, _ => .none
      | _, .none => .none
      | .some a, .some b => .some $ .add_vector a b

  def noVar : Vars := (fun _ => .none)

  theorem x : selector_values_bound noVar [] := by
    unfold selector_values_bound
    intro vars h
    cases h

  -- TODO: Can decidecidable equality of options be derived?
  def dec_option {α : Type} [DecidableEq α] (a b : Option α) : Decidable (a = b) := match a, b with
  | .none, .some x => isFalse (by
      intro h
      cases h
    )
  | .some x, .none => isFalse (by
      intro h
      cases h
    )
  | .none, .none => .isTrue rfl
  | .some x, .some y => if h₁ : x = y
    then .isTrue (by
      rw [h₁]
    )
    else .isFalse (by
      intro h₂
      cases h₂
      apply h₁
      rfl
    )


  instance {α : Type} [DecidableEq α] (a b : Option α) : Decidable (a = b) := dec_option a b

  /--
    From a list of selectors where the values are variable references, this function constructs a map of all used variables and a proof the all variables are bound.
  -/
  def build_vars_for_selector (selectors : List (String × String)) : Option $ Σ' vars: Vars, selector_values_bound vars selectors := match selectors with
  | [] => .some ⟨noVar, x⟩
  | x :: xs => match build_vars_for_selector xs with
    | .none => .none
    | .some ⟨vars, h⟩ =>
      -- hh : vars already contains the variable that is used in X and has the correct type.
      if hh : vars x.snd = .some .label_value then
        have : selector_values_bound vars (x :: xs) := by
          unfold selector_values_bound
          intro s s_mem
          cases s_mem with
          | head => exact hh
          | tail _ s_mem => apply h s s_mem
        .some ⟨vars, this⟩
      else if vars x.snd = .none then (
      -- hhh : x references a variable that is not yet used in vars, so we need to add it
        -- with the addition of x all selector are still bound properly
        have : selector_values_bound (fun z => if x.snd = z then .some .label_value else vars z) (x :: xs) := by
          unfold selector_values_bound
          intro y y_in
          cases y_in with
          | head => simp
          | tail _ y_in_xs =>
            unfold selector_values_bound at h
            have hhhh := h y y_in_xs
            rw [←hhhh]
            simp
        .some ⟨fun y => if x.snd = y then .some .label_value else vars y, this⟩
      ) else .none

/--
ofUnsafe goes through x and constructs a map of use variables and the type-safe version of x.
If variables are colliding then .none is returned.
-/
  def ofUnsafe (x : UnsafeTemplatedInstantVector t) : Option (Σ vars : Vars, TemplatedInstantVector vars t) := match x with
  | .selector selectors literal_selectors key_value_vars => match build_vars_for_selector selectors with
    | .none => .none
    | .some ⟨vars, bound⟩ => .some ⟨vars, .selector selectors bound literal_selectors key_value_vars⟩
  | .add_vector a b => match ofUnsafe a, ofUnsafe b with
    | .none, _ => .none
    | _, .none => .none
    -- How do we decide whether aVars and bVars are compatible?
    -- aVars and bVars are functions, so their domain is infinite and no_conflict is undecidable.
    -- We probably have to deduce it from the usage in a' and b' or change Vars into a map.
    | .some ⟨aVars, a'⟩, some ⟨bVars, b'⟩ =>
        let vars' : Vars := aVars.or bVars
        .some ⟨vars', .add_vector (vars_or_left a' sorry) (vars_or_right b' sorry)⟩ -- TODO: Use theorems to combine variables. If variables collides returns .none.

  -- TODO: there are no unused variables produced by build_vars_for_selector
  --theorem build_vars_for_selector_optimal

  -- TODO: syntax for templated promQL

  def toString {vars : String → Option TemplateVariableType} {t: InstantVectorType} (v: TemplatedInstantVector vars t) : String := match v with
    | .selector sls _ lits _ => let sls' := sls.map (λs => s.fst ++ "=\"$" ++ s.snd ++ "\"")
        let lits' := lits.map (λl => l.fst ++ "=\"" ++ l.snd ++ "\"")
        "{" ++ (joinSep (sls' ++ lits') ", ") ++ "}"
    | .add_vector x y => s!"{toString x} + {toString y}"

  instance {vars : String → Option TemplateVariableType} {t: InstantVectorType} : ToString (TemplatedInstantVector vars t) := {
    toString := (fun v => v.toString)
  }

  #eval check (λ x => match x with
    | "instance" => .some .label_value
    | _ => .none
  ) (.add_vector (.selector [⟨"instance", "instance"⟩] [⟨"operation", "get"⟩] []) (.selector [⟨"instance", "instance"⟩] [⟨"operation", "list"⟩] []))
end TemplatedInstantVector
