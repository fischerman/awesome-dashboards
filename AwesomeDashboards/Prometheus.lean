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
deriving Repr, BEq, DecidableEq

abbrev Vars := List (String × TemplateVariableType)

namespace Vars

  def noVar : Vars := []

  def lookup : Vars → String → Option TemplateVariableType
    | [], _ => .none
    | (k, ty) :: xs, name => if k = name then .some ty else lookup xs name

  /--
    Combines the variable lists `a` and `b`.
    Variables in `a` have precedence.
  -/
  def or (a b : Vars) : Vars := a ++ b

  theorem lookup_or_left {a b : Vars} {name : String} {ty : TemplateVariableType}
      (h : a.lookup name = .some ty) : (a.or b).lookup name = .some ty := by
    induction a with
    | nil => cases h
    | cons hd tl ih =>
      cases hd with | mk k val =>
      dsimp [or, lookup] at h ⊢
      by_cases hk : k = name
      · rw [if_pos hk] at h ⊢
        exact h
      · rw [if_neg hk] at h ⊢
        exact ih h

  theorem lookup_or_of_none {a b : Vars} {name : String}
      (h : a.lookup name = .none) : (a.or b).lookup name = b.lookup name := by
    induction a with
    | nil => rfl
    | cons hd tl ih =>
      cases hd with | mk k val =>
      dsimp [or, lookup] at h ⊢
      by_cases hk : k = name
      · rw [if_pos hk] at h
        cases h
      · rw [if_neg hk] at h ⊢
        exact ih h

  theorem mem_keys_of_lookup_some {v : Vars} {name : String} {ty : TemplateVariableType}
      (h : v.lookup name = .some ty) : name ∈ v.map Prod.fst := by
    induction v with
    | nil => cases h
    | cons hd tl ih =>
      cases hd with | mk k val =>
      dsimp [lookup] at h
      by_cases hk : k = name
      · subst hk
        exact List.Mem.head _
      · rw [if_neg hk] at h
        exact List.Mem.tail _ (ih h)

end Vars

/--
  Two `Vars` lists have no conflict when every variable bound in `b` either maps to the
  same type in `a` or is not present in `a`.
-/
def no_conflict (a b : Vars) : Prop :=
  ∀ k, k ∈ b.map Prod.fst → a.lookup k = b.lookup k ∨ a.lookup k = .none

def dec_no_conflict_keys (a b : Vars) (keys : List String) :
    Decidable (∀ k, k ∈ keys → a.lookup k = b.lookup k ∨ a.lookup k = .none) :=
  match keys with
  | [] => isTrue (by intro k hk; cases hk)
  | x :: xs =>
    if h : a.lookup x = b.lookup x ∨ a.lookup x = .none then
      match dec_no_conflict_keys a b xs with
      | isTrue hxs => isTrue (by
          intro k hk
          cases hk with
          | head => exact h
          | tail _ hmem => exact hxs k hmem)
      | isFalse hxs => isFalse (by
          intro hall
          apply hxs
          intro k hk
          exact hall k (List.Mem.tail _ hk))
    else
      isFalse (by
        intro hall
        exact h (hall x (List.Mem.head _)))

instance (a b : Vars) : Decidable (no_conflict a b) :=
  dec_no_conflict_keys a b (b.map Prod.fst)

theorem Vars.lookup_or_right {a b : Vars} {name : String} {ty : TemplateVariableType}
    (hnc : no_conflict a b) (hb : b.lookup name = .some ty) :
    (a.or b).lookup name = .some ty := by
  have hmem : name ∈ b.map Prod.fst := Vars.mem_keys_of_lookup_some hb
  cases hnc name hmem with
  | inl heq =>
    rw [hb] at heq
    exact Vars.lookup_or_left heq
  | inr hnone =>
    rw [Vars.lookup_or_of_none hnone]
    exact hb

/--
  Each variable used in the selector is present in vars, i.e. bound.
  vars may contain variables not used in the selector.
-/
def selector_values_bound (vars : Vars) (selectors : List (String × String)) : Prop :=
  ∀ h, h ∈ selectors → vars.lookup h.snd = .some .label_value

def dec_selector_values_bound (vars : Vars) (selectors : List (String × String)) :
    Decidable (selector_values_bound vars selectors) :=
  match selectors with
  | [] => isTrue (by intro s hs; cases hs)
  | x :: xs =>
    if h : vars.lookup x.snd = .some .label_value then
      match dec_selector_values_bound vars xs with
      | isTrue z => isTrue (by
          intro hh hhh
          cases hhh with
          | head => exact h
          | tail _ hmem => exact z hh hmem)
      | isFalse z => isFalse (by
          intro hh
          apply z
          intro hhh hhhh
          exact hh hhh (List.Mem.tail _ hhhh))
    else
      isFalse (by
        intro hh
        exact h (hh x (List.Mem.head _)))

instance (vars : Vars) (selectors : List (String × String)) :
    Decidable (selector_values_bound vars selectors) :=
  dec_selector_values_bound vars selectors

/--
  Each key-value variable used in the selector is present in vars with type `.key_value_pairs`.
-/
def key_value_vars_bound (vars : Vars) (key_value_vars : List String) : Prop :=
  ∀ x, x ∈ key_value_vars → vars.lookup x = .some .key_value_pairs

def dec_key_value_vars_bound (vars : Vars) (key_value_vars : List String) :
    Decidable (key_value_vars_bound vars key_value_vars) :=
  match key_value_vars with
  | [] => isTrue (by intro s hs; cases hs)
  | x :: xs =>
    if h : vars.lookup x = .some .key_value_pairs then
      match dec_key_value_vars_bound vars xs with
      | isTrue z => isTrue (by
          intro hh hhh
          cases hhh with
          | head => exact h
          | tail _ hmem => exact z hh hmem)
      | isFalse z => isFalse (by
          intro hh
          apply z
          intro hhh hhhh
          exact hh hhh (List.Mem.tail _ hhhh))
    else
      isFalse (by
        intro hh
        exact h (hh x (List.Mem.head _)))

instance (vars : Vars) (key_value_vars : List String) :
    Decidable (key_value_vars_bound vars key_value_vars) :=
  dec_key_value_vars_bound vars key_value_vars

def LabelName := String
def LabelValue := String
/-- The name of a variable -/
def VarRef := String

/--
A version of promql that allows the use of variables.
The variables can be of the wrong type or not defined at all, hence unsafe.
-/
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

inductive TemplatedInstantVector (vars : Vars) : InstantVectorType → Type where
| selector
    (selectors : List (String × String))
    (literal_selector : List (String × String))
    (key_value_vars : List String)
    (h : selector_values_bound vars selectors := by decide)
    (h₂ : key_value_vars_bound vars key_value_vars := by decide) : TemplatedInstantVector vars vector
| add_vector (x y : TemplatedInstantVector vars vector) : TemplatedInstantVector vars vector

theorem selector_values_bound_or_left {a b : Vars} {selectors : List (String × String)}
    (h : selector_values_bound a selectors) : selector_values_bound (a.or b) selectors := by
  intro s hs
  exact Vars.lookup_or_left (h s hs)

theorem selector_values_bound_or_right {a b : Vars} {selectors : List (String × String)}
    (hnc : no_conflict a b) (h : selector_values_bound b selectors) :
    selector_values_bound (a.or b) selectors := by
  intro s hs
  exact Vars.lookup_or_right hnc (h s hs)

theorem key_value_vars_bound_or_left {a b : Vars} {kvs : List String}
    (h : key_value_vars_bound a kvs) : key_value_vars_bound (a.or b) kvs := by
  intro s hs
  exact Vars.lookup_or_left (h s hs)

theorem key_value_vars_bound_or_right {a b : Vars} {kvs : List String}
    (hnc : no_conflict a b) (h : key_value_vars_bound b kvs) :
    key_value_vars_bound (a.or b) kvs := by
  intro s hs
  exact Vars.lookup_or_right hnc (h s hs)

/--
  If `a` and `b` are not conflicting then we can add `b` to the vars of `v`.
  Basically, we can add unused variables.
-/
def vars_or_left {t : InstantVectorType} {a b : Vars} (v : TemplatedInstantVector a t) (_ : no_conflict a b) :
    TemplatedInstantVector (a.or b) t :=
  match v with
  | .selector selectors lit key_value_vars h h₂ =>
    .selector selectors lit key_value_vars (selector_values_bound_or_left h) (key_value_vars_bound_or_left h₂)
  | .add_vector x y =>
    .add_vector (vars_or_left x ‹no_conflict a b›) (vars_or_left y ‹no_conflict a b›)

/--
  If `a` and `b` are not conflicting then we can add `a` to the vars of `v`.
-/
def vars_or_right {t : InstantVectorType} {a b : Vars} (v : TemplatedInstantVector b t) (h : no_conflict a b) :
    TemplatedInstantVector (a.or b) t :=
  match v with
  | .selector selectors lit key_value_vars hs hkv =>
    .selector selectors lit key_value_vars (selector_values_bound_or_right h hs) (key_value_vars_bound_or_right h hkv)
  | .add_vector x y =>
    .add_vector (vars_or_right x h) (vars_or_right y h)

namespace TemplatedInstantVector
  /--
    check returns a type-safe vector if all variables in x are bound by vars.
  -/
  def check {t : InstantVectorType} (vars : Vars) (x : UnsafeTemplatedInstantVector t) :
      Option (TemplatedInstantVector vars t) := match x with
  | .selector selectors lit key_values_vars =>
      match dec_selector_values_bound vars selectors, dec_key_value_vars_bound vars key_values_vars with
      | isTrue h, isTrue h₂ => .some $ .selector selectors lit key_values_vars h h₂
      | _, _ => .none
  | .add_vector x y => match check vars x, check vars y with
      | .some a, .some b => .some $ .add_vector a b
      | _, _ => .none

  def noVar : Vars := Vars.noVar

  theorem empty_selector_values_bound (vars : Vars) : selector_values_bound vars [] := by
    intro _ h
    cases h

  theorem empty_key_value_vars_bound (vars : Vars) : key_value_vars_bound vars [] := by
    intro _ h
    cases h

  /--
    From a list of selectors where the values are variable references, this function constructs
    a list of all used variables and a proof that all variables are bound.
  -/
  def build_vars_for_selector (selectors : List (String × String)) :
      Option $ Σ' vars : Vars, selector_values_bound vars selectors :=
    match selectors with
    | [] => .some ⟨noVar, empty_selector_values_bound noVar⟩
    | x :: xs => match build_vars_for_selector xs with
      | .none => .none
      | .some ⟨vars, h⟩ =>
        if hh : vars.lookup x.snd = .some .label_value then
          have : selector_values_bound vars (x :: xs) := by
            intro s s_mem
            cases s_mem with
            | head => exact hh
            | tail _ s_mem => exact h s s_mem
          .some ⟨vars, this⟩
        else if vars.lookup x.snd = .none then
          have : selector_values_bound ((x.snd, .label_value) :: vars) (x :: xs) := by
            intro y y_in
            dsimp [Vars.lookup]
            cases y_in with
            | head => rw [if_pos rfl]
            | tail _ y_in_xs =>
              by_cases heq : x.snd = y.snd
              · rw [if_pos heq]
              · rw [if_neg heq]
                exact h y y_in_xs
          .some ⟨(x.snd, .label_value) :: vars, this⟩
        else .none

  /--
    From a list of key-value variable names, constructs a variable list and a proof that all
    variables are bound with `.key_value_pairs`.
  -/
  def build_vars_for_key_value_vars (kvs : List String) :
      Option $ Σ' vars : Vars, key_value_vars_bound vars kvs :=
    match kvs with
    | [] => .some ⟨noVar, empty_key_value_vars_bound noVar⟩
    | x :: xs => match build_vars_for_key_value_vars xs with
      | .none => .none
      | .some ⟨vars, h⟩ =>
        if hh : vars.lookup x = .some .key_value_pairs then
          have : key_value_vars_bound vars (x :: xs) := by
            intro s s_mem
            cases s_mem with
            | head => exact hh
            | tail _ s_mem => exact h s s_mem
          .some ⟨vars, this⟩
        else if vars.lookup x = .none then
          have : key_value_vars_bound ((x, .key_value_pairs) :: vars) (x :: xs) := by
            intro y y_in
            dsimp [Vars.lookup]
            cases y_in with
            | head => rw [if_pos rfl]
            | tail _ y_in_xs =>
              by_cases heq : x = y
              · rw [if_pos heq]
              · rw [if_neg heq]
                exact h y y_in_xs
          .some ⟨(x, .key_value_pairs) :: vars, this⟩
        else .none

  /--
    ofUnsafe goes through `x` and constructs a map of used variables and the type-safe version of `x`.
    If variables are colliding then `.none` is returned.
  -/
  def ofUnsafe (x : UnsafeTemplatedInstantVector t) :
      Option (Σ vars : Vars, TemplatedInstantVector vars t) := match x with
  | .selector selectors literal_selectors key_value_vars =>
    match build_vars_for_selector selectors, build_vars_for_key_value_vars key_value_vars with
    | .some ⟨sVars, hs⟩, .some ⟨kvVars, hkv⟩ =>
      if hnc : no_conflict sVars kvVars then
        .some ⟨sVars.or kvVars, .selector selectors literal_selectors key_value_vars (selector_values_bound_or_left hs) (key_value_vars_bound_or_right hnc hkv)⟩
      else
        .none
    | _, _ => .none
  | .add_vector a b => match ofUnsafe a, ofUnsafe b with
    | .some ⟨aVars, a'⟩, .some ⟨bVars, b'⟩ =>
      if hnc : no_conflict aVars bVars then
        .some ⟨aVars.or bVars, .add_vector (vars_or_left a' hnc) (vars_or_right b' hnc)⟩
      else
        .none
    | _, _ => .none

  -- TODO: syntax for templated promQL

  def toString {vars : Vars} {t : InstantVectorType} (v : TemplatedInstantVector vars t) : String := match v with
    | .selector sls lits kvs _ _ =>
        let sls' := sls.map (λ s => s.fst ++ "=\"$" ++ s.snd ++ "\"")
        let lits' := lits.map (λ l => l.fst ++ "=\"" ++ l.snd ++ "\"")
        let kvs' := kvs.map (λ kv => "$" ++ kv)
        "{" ++ (joinSep (sls' ++ lits' ++ kvs') ", ") ++ "}"
    | .add_vector x y => s!"{toString x} + {toString y}"

  instance {vars : Vars} {t : InstantVectorType} : ToString (TemplatedInstantVector vars t) := {
    toString := (fun v => v.toString)
  }

  #eval check [("instance", .label_value)]
    (.add_vector (.selector [⟨"instance", "instance"⟩] [⟨"operation", "get"⟩] []) (.selector [⟨"instance", "instance"⟩] [⟨"operation", "list"⟩] []))

  #eval (ofUnsafe (.add_vector
    (.selector [⟨"instance", "instance"⟩] [⟨"operation", "get"⟩] ["extra_filters"])
    (.selector [⟨"instance", "instance"⟩] [⟨"operation", "list"⟩] []))).map (fun ⟨vars, v⟩ => (vars, v.toString))

end TemplatedInstantVector
