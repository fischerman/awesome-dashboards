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
deriving Repr, BEq, DecidableEq, Lean.FromJson, Lean.ToJson

/--
Units for time series.
They have no formal meaning in PromQL and must be derived from the system that emits the metric.

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
  div is the result of a division where dividend and divisor have different units.
  The result of rate() is a div where the divisor unit is seconds.
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
-- We do not implement this faithfully.
structure ScrapeConfig where
  targetLabels : List String
  exporter : Exporter
deriving Lean.FromJson, Lean.ToJson

/--
The label key that contains the metric name.
up{} is equivalent to {\_\_name\_\_="up"}

https://prometheus.io/docs/prometheus/latest/querying/basics/#instant-vector-selectors
-/
def name_label := "__name__"

namespace ScrapeConfig

  def labels (s : ScrapeConfig) (name : String) : Option $ List String :=
    Option.map (fun v => s.targetLabels ++ v.labels ++ ["job", "instance", name_label])
      (s.exporter.metrics.find? (fun m => m.name == name))

end ScrapeConfig

structure Environment where
  scrapeConfigs : List ScrapeConfig
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

/-!
## Template Variables
-/

inductive TemplateVariableType
  /-- A variable that can be used for a label value. -/
  | label_value
  /-- A variable that can be used in a selector representing a list of label matchers. -/
  | key_value_pairs
  /-- A variable that can be used as a range duration (e.g. `$__rate_interval`). -/
  | duration
  deriving Repr, BEq, DecidableEq, Lean.FromJson, Lean.ToJson

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

/-!
## Mode-Parameterized PromQL AST (Pure PromQL vs. Grafana-Templated PromQL)
-/

/--
  Distinguishes pure PromQL (no template variables permitted by construction)
  from Grafana-templated PromQL (variables permitted in label values, durations,
  and key-value matcher slots).
-/
inductive PromQLMode
  | pure
  | templated
  deriving Repr, BEq, DecidableEq, Lean.FromJson, Lean.ToJson

inductive TemplatedValue
  | lit (value : String)
  | var (name : String)
  deriving Repr, BEq, DecidableEq, Lean.FromJson, Lean.ToJson

inductive TemplatedDuration
  | seconds (n : Nat)
  | var (name : String)
  deriving Repr, BEq, DecidableEq, Lean.FromJson, Lean.ToJson

abbrev LabelMatcherValue : PromQLMode → Type
  | .pure      => String
  | .templated => TemplatedValue

abbrev RangeDuration : PromQLMode → Type
  | .pure      => Nat
  | .templated => TemplatedDuration

def LabelMatcherValue.toString : {m : PromQLMode} → LabelMatcherValue m → String
  | .pure, s => s
  | .templated, .lit s => s
  | .templated, .var name => "$" ++ name

def LabelMatcherValue.asLiteral? : {m : PromQLMode} → LabelMatcherValue m → Option String
  | .pure, s => .some s
  | .templated, .lit s => .some s
  | .templated, .var _ => .none

def RangeDuration.toString : {m : PromQLMode} → RangeDuration m → String
  | .pure, d => s!"{d}s"
  | .templated, .seconds d => s!"{d}s"
  | .templated, .var name => "$" ++ name

inductive MatchOp
  | eq
  | neq
  | re
  | nre
  deriving Repr, BEq, DecidableEq, Lean.FromJson, Lean.ToJson

namespace MatchOp

  def toString : MatchOp → String
    | .eq  => "="
    | .neq => "!="
    | .re  => "=~"
    | .nre => "!~"

end MatchOp

/--
Label matcher is a single matcher on a time series (`=`, `!=`, `=~`, `!~`).
https://prometheus.io/docs/prometheus/latest/querying/basics/#instant-vector-selectors

In `.pure` mode, matchers only accept literal string values.
In `.templated` mode, label values may be `$var` references and `.keyValueVar` matchers are allowed.
-/
inductive LabelMatcher : PromQLMode → Type where
  | matcher {m : PromQLMode} (op : MatchOp) (key : String) (value : LabelMatcherValue m) : LabelMatcher m
  | keyValueVar (name : String) : LabelMatcher .templated

namespace LabelMatcher

  def equal {m : PromQLMode} (key : String) (value : LabelMatcherValue m) : LabelMatcher m :=
    .matcher .eq key value

  def notEqual {m : PromQLMode} (key : String) (value : LabelMatcherValue m) : LabelMatcher m :=
    .matcher .neq key value

  def regex {m : PromQLMode} (key : String) (value : LabelMatcherValue m) : LabelMatcher m :=
    .matcher .re key value

  def notRegex {m : PromQLMode} (key : String) (value : LabelMatcherValue m) : LabelMatcher m :=
    .matcher .nre key value

  def toString {m : PromQLMode} : LabelMatcher m → String
    | .matcher op k v => s!"{k}{op.toString}\"{LabelMatcherValue.toString v}\""
    | .keyValueVar name => "$" ++ name

  def matchedName? {m : PromQLMode} : LabelMatcher m → Option String
    | .matcher .eq k v => if k == name_label then LabelMatcherValue.asLiteral? v else .none
    | _ => .none

  /-- Returns the literal metric name if matched via `__name__="..."`. -/
  def getMatchedName {m : PromQLMode} (lms : List (LabelMatcher m)) : Option String :=
    lms.findSome? matchedName?

  def matches_name {m : PromQLMode} : LabelMatcher m → Bool
    | .matcher .eq k v => k == name_label && (LabelMatcherValue.asLiteral? v).isSome
    | _ => false

  def key? {m : PromQLMode} : LabelMatcher m → Option String
    | .matcher _ k _ => .some k
    | .keyValueVar _ => .none

  def metricNamePart {m : PromQLMode} : LabelMatcher m → String
    | .matcher _ _ v => LabelMatcherValue.toString v
    | .keyValueVar _ => ""

  def toTemplated : LabelMatcher .pure → LabelMatcher .templated
    | .matcher op k v => .matcher op k (.lit v)

end LabelMatcher

inductive RangeVector (m : PromQLMode) : Type where
  | selector (lms : List (LabelMatcher m)) (duration : RangeDuration m) : RangeVector m

namespace RangeVector

  def toString {m : PromQLMode} : RangeVector m → String
    | .selector lms d =>
        let metricName := String.join (lms.filter LabelMatcher.matches_name |>.map LabelMatcher.metricNamePart)
        let rest := joinSep (lms.filter (not ∘ LabelMatcher.matches_name) |>.map LabelMatcher.toString) ", "
        metricName ++ "{" ++ rest ++ "}[" ++ RangeDuration.toString d ++ "]"

  def toTemplated : RangeVector .pure → RangeVector .templated
    | .selector lms d => .selector (lms.map LabelMatcher.toTemplated) (.seconds d)

end RangeVector

inductive InstantVectorType
  | scalar
  | vector
  deriving Repr, BEq, DecidableEq, Lean.FromJson, Lean.ToJson

open InstantVectorType

/--
  An instant vector is used to query Prometheus at a point in time.

  Parameterized by `PromQLMode`:
  - `InstantVector .pure` (`PureInstantVector`) is pure PromQL with no template variables.
  - `InstantVector .templated` (`TemplatedInstantVector`) permits Grafana template variables
    in label values, range durations, and key-value matchers.

  TODO: Replace specific binary operator constructors with a generic binary operator enum.
-/
inductive InstantVector (m : PromQLMode) : InstantVectorType → Type where
  | selector (lms : List (LabelMatcher m)) (offset : Nat) : InstantVector m .vector
  | literal (v : Float) : InstantVector m .scalar
  | sub_vector (vector_matching : Option VectorMatching) (a b : InstantVector m .vector) : InstantVector m .vector
  | add_vector (vector_matching : Option VectorMatching) (a b : InstantVector m .vector) : InstantVector m .vector
  | add_scalar_left (a : InstantVector m .scalar) (b : InstantVector m .vector) : InstantVector m .vector
  | sum (a : Option AggregationSelector) (v : InstantVector m .vector) : InstantVector m .vector
  | topk (a : Option AggregationSelector) (k : Nat) (v : InstantVector m .vector) : InstantVector m .vector
  | rate (r : RangeVector m) : InstantVector m .vector
  | label_replace (v : InstantVector m .vector) (dst replacement src regex : String) : InstantVector m .vector
  | time : InstantVector m .scalar

abbrev PureInstantVector := InstantVector .pure
abbrev TemplatedInstantVector := InstantVector .templated

namespace InstantVector

  def toString {m : PromQLMode} {t : InstantVectorType} : InstantVector m t → String
    | .selector lms _ =>
        let metricName := String.join (lms.filter LabelMatcher.matches_name |>.map LabelMatcher.metricNamePart)
        let rest := joinSep (lms.filter (not ∘ LabelMatcher.matches_name) |>.map LabelMatcher.toString) ", "
        metricName ++ "{" ++ rest ++ "}"
    | .literal v => s!"{v}"
    | .time => "time()"
    | .label_replace v dst replace src regex =>
        s!"label_replace({v.toString}, \"{dst}\", \"{replace}\", \"{src}\", \"{regex}\")"
    | .rate r => s!"rate({r.toString})"
    | .sub_vector _ a b => s!"{a.toString} - {b.toString}"
    | .add_vector _ a b => s!"{a.toString} + {b.toString}"
    | .add_scalar_left a b => s!"{a.toString} + {b.toString}"
    | .sum a v =>
        match a with
        | .some sel => s!"sum {sel.toString} ({v.toString})"
        | .none => s!"sum({v.toString})"
    | .topk a k v =>
        match a with
        | .some sel => s!"topk {sel.toString} ({k}, {v.toString})"
        | .none => s!"topk({k}, {v.toString})"

  /-- Lifts a pure PromQL query into a templated PromQL query. -/
  def toTemplated {t : InstantVectorType} : PureInstantVector t → TemplatedInstantVector t
    | .selector lms offset => .selector (lms.map LabelMatcher.toTemplated) offset
    | .literal v => .literal v
    | .sub_vector vm a b => .sub_vector vm a.toTemplated b.toTemplated
    | .add_vector vm a b => .add_vector vm a.toTemplated b.toTemplated
    | .add_scalar_left a b => .add_scalar_left a.toTemplated b.toTemplated
    | .sum a v => .sum a v.toTemplated
    | .topk a k v => .topk a k v.toTemplated
    | .rate r => .rate r.toTemplated
    | .label_replace v dst repl src reg => .label_replace v.toTemplated dst repl src reg
    | .time => .time

end InstantVector

instance {t : InstantVectorType} : Coe (PureInstantVector t) (TemplatedInstantVector t) where
  coe := InstantVector.toTemplated

instance {m : PromQLMode} {t : InstantVectorType} : ToString (InstantVector m t) where
  toString := InstantVector.toString

instance {m : PromQLMode} {t : InstantVectorType} : Repr (InstantVector m t) where
  reprPrec v _ := InstantVector.toString v

instance {m : PromQLMode} {t : InstantVectorType} : Lean.ToJson (InstantVector m t) where
  toJson v := Lean.Json.str v.toString

/-- Concrete runtime values for Grafana template variables. -/
structure VarAssignment where
  labelValues   : String → Option String := fun _ => .none
  durations     : String → Option Nat := fun _ => .none
  keyValuePairs : String → Option (List (LabelMatcher .pure)) := fun _ => .none

def LabelMatcher.instantiate (env : VarAssignment) : LabelMatcher .templated → Option (List (LabelMatcher .pure))
  | .matcher op k (.lit s) => .some [.matcher op k s]
  | .matcher op k (.var name) => (env.labelValues name).map (fun s => [.matcher op k s])
  | .keyValueVar name => env.keyValuePairs name

def instantiateMatchers (env : VarAssignment) (lms : List (LabelMatcher .templated)) :
    Option (List (LabelMatcher .pure)) := do
  let nested ← lms.mapM (LabelMatcher.instantiate env)
  return nested.foldl (· ++ ·) []

def RangeVector.instantiate (env : VarAssignment) : RangeVector .templated → Option (RangeVector .pure)
  | .selector lms d => do
      let lms' ← instantiateMatchers env lms
      let d' ← match d with
        | .seconds n => .some n
        | .var name => env.durations name
      return .selector lms' d'

/-- Specializes a Grafana-templated query into a pure PromQL query given variable assignments. -/
def InstantVector.instantiate (env : VarAssignment) {t : InstantVectorType} :
    TemplatedInstantVector t → Option (PureInstantVector t)
  | .selector lms offset => do
      let lms' ← instantiateMatchers env lms
      return .selector lms' offset
  | .literal v => .some (.literal v)
  | .sub_vector vm a b => do
      let a' ← a.instantiate env
      let b' ← b.instantiate env
      return .sub_vector vm a' b'
  | .add_vector vm a b => do
      let a' ← a.instantiate env
      let b' ← b.instantiate env
      return .add_vector vm a' b'
  | .add_scalar_left a b => do
      let a' ← a.instantiate env
      let b' ← b.instantiate env
      return .add_scalar_left a' b'
  | .sum a v => do
      let v' ← v.instantiate env
      return .sum a v'
  | .topk a k v => do
      let v' ← v.instantiate env
      return .topk a k v'
  | .rate r => do
      let r' ← r.instantiate env
      return .rate r'
  | .label_replace v dst repl src reg => do
      let v' ← v.instantiate env
      return .label_replace v' dst repl src reg
  | .time => .some .time

/-!
## Query Introspection (Units, Help Strings, Labels, Used Variables)
-/

def getMetricDef? (e : Environment) {m : PromQLMode} (lms : List (LabelMatcher m)) : Option Metric :=
  match LabelMatcher.getMatchedName lms with
  | .some name => e.scrapeConfigs.findSome? (fun c => c.exporter.metrics.find? (fun metric => metric.name == name))
  | .none => .none

def RangeVector.unitOf (e : Environment) {m : PromQLMode} : RangeVector m → Option MetricUnit
  | .selector lms _ => (getMetricDef? e lms).map (·.unit)

def unitOf {m : PromQLMode} {t : InstantVectorType} (e : Environment) : InstantVector m t → Option MetricUnit
  | .time => .some MetricUnit.time
  | .selector lms _ => (getMetricDef? e lms).map (·.unit)
  | .label_replace v _ _ _ _ => unitOf e v
  | .sub_vector _ a b => if unitOf e a == unitOf e b then unitOf e a else .none
  | .add_vector _ a b => if unitOf e a == unitOf e b then unitOf e a else .none
  | .add_scalar_left _ v => unitOf e v
  | .rate r => (r.unitOf e).map (fun x => MetricUnit.div x MetricUnit.seconds)
  | .literal _ => .some MetricUnit.unitless
  | .sum _ v => unitOf e v
  | .topk _ _ v => unitOf e v

namespace InstantVector

  /--
    Retrieves a help string from the primary metric referenced by the query, if any.
  -/
  def helpString {m : PromQLMode} {t : InstantVectorType} (e : Environment) : InstantVector m t → String
    | .selector lms _ => Option.getD ((getMetricDef? e lms).map (·.help)) ""
    | .rate (.selector lms _) => Option.getD ((getMetricDef? e lms).map (·.help)) ""
    | .label_replace v _ _ _ _ => v.helpString e
    | .sum _ v => v.helpString e
    | .topk _ _ v => v.helpString e
    | _ => ""

  /--
    Returns the label keys produced by the query.
  -/
  def labels {m : PromQLMode} {t : InstantVectorType} (e : Environment) : InstantVector m t → List String
    | .selector lms _ =>
        match LabelMatcher.getMatchedName lms with
        | .some name => Option.getD (e.scrapeConfigs.findSome? (fun c => c.labels name)) []
        | .none => []
    | .literal _ => []
    | .add_scalar_left _ b => b.labels e
    | .label_replace v dst _ _ _ => dst :: v.labels e
    | _ => []

  def matchersUsedVars : List (LabelMatcher .templated) → Vars
    | [] => []
    | lm :: xs =>
        match lm with
        | .matcher _ _ (.lit _) => matchersUsedVars xs
        | .matcher _ _ (.var name) => (name, .label_value) :: matchersUsedVars xs
        | .keyValueVar name => (name, .key_value_pairs) :: matchersUsedVars xs

  private def rangeUsedVars : RangeVector .templated → Vars
    | .selector lms (.seconds _) => matchersUsedVars lms
    | .selector lms (.var name) => matchersUsedVars lms ++ [(name, .duration)]

  /-- Extracts all template variables referenced in a templated query. -/
  def usedVars {t : InstantVectorType} : TemplatedInstantVector t → Vars
    | .selector lms _ => matchersUsedVars lms
    | .literal _ | .time => []
    | .sub_vector _ a b => a.usedVars ++ b.usedVars
    | .add_vector _ a b => a.usedVars ++ b.usedVars
    | .add_scalar_left a b => a.usedVars ++ b.usedVars
    | .sum _ v => v.usedVars
    | .topk _ _ v => v.usedVars
    | .rate r => rangeUsedVars r
    | .label_replace v _ _ _ _ => v.usedVars

end InstantVector

/-- Structured validation errors (designed for both compile-time checks and IDE diagnostics/suggestions). -/
inductive ValidationError
  | missingMetricName
  | unknownMetric (name : String)
  | unknownLabel (metric : String) (label : String) (validLabels : List String)
  | unboundVar (name : String) (expected : TemplateVariableType)
  | varTypeMismatch (name : String) (expected : TemplateVariableType) (actual : TemplateVariableType)
  | rateOnNonCounter (metric : String) (actual : MetricType)
  | incompatibleUnits (left right : Option MetricUnit)
  deriving Repr, BEq, DecidableEq

abbrev Validator (m : PromQLMode) (t : InstantVectorType) :=
  InstantVector m t → Except ValidationError Unit

def nonNameKeys {m : PromQLMode} : List (LabelMatcher m) → List String
  | [] => []
  | lm :: xs =>
      if LabelMatcher.matches_name lm then
        nonNameKeys xs
      else
        match LabelMatcher.key? lm with
        | .some k => k :: nonNameKeys xs
        | .none => nonNameKeys xs

def findMatchingMetrics (metricName : String) : List ScrapeConfig → List (ScrapeConfig × Metric)
  | [] => []
  | c :: cs =>
      match c.exporter.metrics.find? (fun metric => metric.name == metricName) with
      | .some metric => (c, metric) :: findMatchingMetrics metricName cs
      | .none => findMatchingMetrics metricName cs

def validateSelectorEnv (e : Environment) {m : PromQLMode} (lms : List (LabelMatcher m)) :
    Except ValidationError Unit :=
  match LabelMatcher.getMatchedName lms with
  | .none => .error .missingMetricName
  | .some metricName =>
    match findMatchingMetrics metricName e.scrapeConfigs with
    | [] => .error (.unknownMetric metricName)
    | candidates =>
      let keys := nonNameKeys lms
      let ok := candidates.any fun (c, metric) =>
        let validLabels := metric.labels ++ c.targetLabels ++ ["job", "instance"]
        keys.all validLabels.contains
      if ok then
        .ok ()
      else
        match candidates with
        | (c, metric) :: _ =>
          let validLabels := metric.labels ++ c.targetLabels ++ ["job", "instance"]
          match keys.find? (fun k => !validLabels.contains k) with
          | .some badLabel => .error (.unknownLabel metricName badLabel validLabels)
          | .none => .error (.unknownMetric metricName)
        | [] => .error (.unknownMetric metricName)

/-- Validates that every metric name and explicit label key exists in `e : Environment`. -/
def validateEnv (e : Environment) {m : PromQLMode} {t : InstantVectorType} :
    Validator m t
  | .selector lms _ => validateSelectorEnv e lms
  | .rate (.selector lms _) => validateSelectorEnv e lms
  | .sub_vector _ a b => do validateEnv e a; validateEnv e b
  | .add_vector _ a b => do validateEnv e a; validateEnv e b
  | .add_scalar_left a b => do validateEnv e a; validateEnv e b
  | .label_replace v _ _ _ _ => validateEnv e v
  | .sum _ v => validateEnv e v
  | .topk _ _ v => validateEnv e v
  | .literal _ | .time => .ok ()

def validateVarBinding (vars : Vars) (name : String) (expected : TemplateVariableType) :
    Except ValidationError Unit :=
  match vars.lookup name with
  | .none => .error (.unboundVar name expected)
  | .some actual =>
    if actual == expected then .ok ()
    else .error (.varTypeMismatch name expected actual)

def validateMatchersVars (vars : Vars) : List (LabelMatcher .templated) → Except ValidationError Unit
  | [] => .ok ()
  | lm :: xs =>
      match lm with
      | .matcher _ _ (.lit _) => validateMatchersVars vars xs
      | .matcher _ _ (.var name) => do
          validateVarBinding vars name .label_value
          validateMatchersVars vars xs
      | .keyValueVar name => do
          validateVarBinding vars name .key_value_pairs
          validateMatchersVars vars xs

def validateRangeVars (vars : Vars) : RangeVector .templated → Except ValidationError Unit
  | .selector lms (.seconds _) => validateMatchersVars vars lms
  | .selector lms (.var name) => do
      validateMatchersVars vars lms
      validateVarBinding vars name .duration

/-- Validates that every `$var` reference in a templated query is bound in `vars` with the right type. -/
def validateVars (vars : Vars) {t : InstantVectorType} : Validator .templated t
  | .selector lms _ => validateMatchersVars vars lms
  | .rate r => validateRangeVars vars r
  | .sub_vector _ a b => do validateVars vars a; validateVars vars b
  | .add_vector _ a b => do validateVars vars a; validateVars vars b
  | .add_scalar_left a b => do validateVars vars a; validateVars vars b
  | .label_replace v _ _ _ _ => validateVars vars v
  | .sum _ v => validateVars vars v
  | .topk _ _ v => validateVars vars v
  | .literal _ | .time => .ok ()

/-- Validates that `rate()` is only applied to counters or histograms. -/
def validateRateOnCounter (e : Environment) {m : PromQLMode} {t : InstantVectorType} :
    Validator m t
  | .rate (.selector lms _) =>
      match getMetricDef? e lms with
      | .some metric =>
          match metric.type with
          | .counter | .histogram => .ok ()
          | other => .error (.rateOnNonCounter metric.name other)
      | .none => .ok ()
  | .sub_vector _ a b => do validateRateOnCounter e a; validateRateOnCounter e b
  | .add_vector _ a b => do validateRateOnCounter e a; validateRateOnCounter e b
  | .add_scalar_left a b => do validateRateOnCounter e a; validateRateOnCounter e b
  | .label_replace v _ _ _ _ => validateRateOnCounter e v
  | .sum _ v => validateRateOnCounter e v
  | .topk _ _ v => validateRateOnCounter e v
  | .selector _ _ | .literal _ | .time => .ok ()

/-- Validates that binary vector operations combine compatible units. -/
def validateUnits (e : Environment) {m : PromQLMode} {t : InstantVectorType} :
    Validator m t
  | .sub_vector _ a b => do
      validateUnits e a
      validateUnits e b
      let ua := unitOf e a
      let ub := unitOf e b
      if ua == ub then .ok () else .error (.incompatibleUnits ua ub)
  | .add_vector _ a b => do
      validateUnits e a
      validateUnits e b
      let ua := unitOf e a
      let ub := unitOf e b
      if ua == ub then .ok () else .error (.incompatibleUnits ua ub)
  | .add_scalar_left a b => do validateUnits e a; validateUnits e b
  | .label_replace v _ _ _ _ => validateUnits e v
  | .sum _ v => validateUnits e v
  | .topk _ _ v => validateUnits e v
  | .selector _ _ | .rate _ | .literal _ | .time => .ok ()

def runValidators {m : PromQLMode} {t : InstantVectorType} :
    List (Validator m t) → InstantVector m t → Except ValidationError Unit
  | [], _ => .ok ()
  | check :: rest, q =>
      match check q with
      | .ok () => runValidators rest q
      | .error err => .error err

def decExceptOk {ε : Type} (x : Except ε Unit) : Decidable (x = .ok ()) :=
  match x with
  | .ok () => isTrue rfl
  | .error _ => isFalse (by intro h; cases h)

instance {ε : Type} (x : Except ε Unit) : Decidable (x = .ok ()) := decExceptOk x

/-- A PromQL query verified at compile time against a composable list of validators. -/
structure ValidatedQuery (m : PromQLMode) (t : InstantVectorType) (validators : List (Validator m t)) where
  query : InstantVector m t
  valid : runValidators validators query = .ok () := by decide

def defaultPureValidators (e : Environment) {t : InstantVectorType} : List (Validator .pure t) :=
  [validateEnv e, validateRateOnCounter e, validateUnits e]

def defaultTemplatedValidators (e : Environment) (vars : Vars) {t : InstantVectorType} :
    List (Validator .templated t) :=
  [validateEnv e, validateVars vars, validateRateOnCounter e, validateUnits e]

abbrev CheckedPureQuery (e : Environment) (t : InstantVectorType) :=
  ValidatedQuery .pure t (defaultPureValidators e)

abbrev CheckedQuery (e : Environment) (vars : Vars) (t : InstantVectorType) :=
  ValidatedQuery .templated t (defaultTemplatedValidators e vars)

instance {m : PromQLMode} {t : InstantVectorType} {vs : List (Validator m t)} :
    Lean.ToJson (ValidatedQuery m t vs) where
  toJson q := Lean.toJson q.query

namespace ValidatedQuery

  def helpString {m : PromQLMode} {t : InstantVectorType} {vs : List (Validator m t)}
      (e : Environment) (q : ValidatedQuery m t vs) : String :=
    q.query.helpString e

  def labels {m : PromQLMode} {t : InstantVectorType} {vs : List (Validator m t)}
      (e : Environment) (q : ValidatedQuery m t vs) : List String :=
    q.query.labels e

  def unit {m : PromQLMode} {t : InstantVectorType} {vs : List (Validator m t)}
      (e : Environment) (q : ValidatedQuery m t vs) : Option MetricUnit :=
    unitOf e q.query

end ValidatedQuery

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

declare_syntax_cat matchop
syntax "="  : matchop
syntax "!=" : matchop
syntax "=~" : matchop
syntax "!~" : matchop

def expandMatchOp : TSyntax `matchop → MacroM (TSyntax `term)
  | `(matchop| =)  => `(MatchOp.eq)
  | `(matchop| !=) => `(MatchOp.neq)
  | `(matchop| =~) => `(MatchOp.re)
  | `(matchop| !~) => `(MatchOp.nre)
  | _              => Macro.throwUnsupported

-- Pure PromQL syntax (`[pql| ... ]`)

declare_syntax_cat labelmatcher
syntax name matchop strLit : labelmatcher
macro_rules
| `(labelmatcher| $key:name $op:matchop $value:str) => do
    let opTerm ← expandMatchOp op
    `(LabelMatcher.matcher (m := PromQLMode.pure) $opTerm $(quote key.raw[0].getAtomVal) $value)

declare_syntax_cat labelmatchers
syntax "{" labelmatcher,* "}" : labelmatchers
open TSyntax.Compat
macro_rules
| `(labelmatchers| { $xs,* }) => `([$xs,*])

declare_syntax_cat rangevector
syntax name (labelmatchers) "[" numLit "]" : rangevector
macro_rules
| `(rangevector| $name:name $xs [ $range ]) =>
    `(RangeVector.selector (m := PromQLMode.pure)
        ((LabelMatcher.equal (m := PromQLMode.pure) name_label $(quote name.raw[0].getAtomVal)) :: $(xs))
        $range)

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
syntax instantvector " + " instantvector : instantvector
syntax "sum" "(" instantvector ")" : instantvector
syntax "sum" "by" "(" labelref,* ")" "(" instantvector ")" : instantvector
syntax "sum" "(" instantvector ")" "by" "(" labelref,* ")" : instantvector
syntax "sum" "without" "(" labelref,* ")" "(" instantvector ")" : instantvector
syntax "sum" "(" instantvector ")" "without" "(" labelref,* ")" : instantvector
syntax "topk" "(" numLit "," instantvector ")" : instantvector

macro_rules
| `(instantvector| time()) => `(InstantVector.time (m := PromQLMode.pure))
| `(instantvector| $name:name) =>
    `(InstantVector.selector (m := PromQLMode.pure)
        [LabelMatcher.equal (m := PromQLMode.pure) name_label $(quote name.raw[0].getAtomVal)] 0)
| `(instantvector| $name:name $xs:labelmatchers) =>
    `(InstantVector.selector (m := PromQLMode.pure)
        ((LabelMatcher.equal (m := PromQLMode.pure) name_label $(quote name.raw[0].getAtomVal)) :: $(xs)) 0)
| `(instantvector| $a - $b) => `(InstantVector.sub_vector (m := PromQLMode.pure) Option.none $a $b)
| `(instantvector| $a + $b) => `(InstantVector.add_vector (m := PromQLMode.pure) Option.none $a $b)
| `(instantvector| rate($rv)) => `(InstantVector.rate (m := PromQLMode.pure) $rv)
| `(instantvector| label_replace($v, $dst, $repl, $src, $reg)) =>
    `(InstantVector.label_replace (m := PromQLMode.pure) $v $dst $repl $src $reg)
| `(instantvector| sum($v)) => `(InstantVector.sum (m := PromQLMode.pure) .none $v)
| `(instantvector| sum by($bs,*) ($v)) =>
    `(InstantVector.sum (m := PromQLMode.pure) (AggregationSelector.by [$bs,*]) ($v))
| `(instantvector| sum ($v) by($bs,*)) =>
    `(InstantVector.sum (m := PromQLMode.pure) (AggregationSelector.by [$bs,*]) ($v))
| `(instantvector| sum without($bs,*) ($v)) =>
    `(InstantVector.sum (m := PromQLMode.pure) (AggregationSelector.without [$bs,*]) ($v))
| `(instantvector| sum ($v) without($bs,*)) =>
    `(InstantVector.sum (m := PromQLMode.pure) (AggregationSelector.without [$bs,*]) ($v))
| `(instantvector| topk($k, $v)) =>
    `(InstantVector.topk (m := PromQLMode.pure) .none $k $v)

syntax "[pql|" instantvector "]" : term

macro_rules
| `([pql| $v:instantvector]) => `($v)

-- Grafana-templated PromQL syntax (`[tpql| ... ]`)
-- Label values support both Grafana's `"$var"` syntax and `@var`.
-- Key-value matchers (`@extra_filters`) and duration variables (`[@__rate_interval]`) use `@var`
-- because `$ident` is reserved by Lean 4's macro antiquotation parser.

declare_syntax_cat tlabelmatcher
syntax name matchop strLit : tlabelmatcher
syntax name matchop "@" name : tlabelmatcher
syntax "@" name : tlabelmatcher

macro_rules
| `(tlabelmatcher| $key:name $op:matchop $value:str) => do
    let opTerm ← expandMatchOp op
    let s := value.getString
    if s.startsWith "$" then
      `(LabelMatcher.matcher (m := PromQLMode.templated) $opTerm $(quote key.raw[0].getAtomVal) (TemplatedValue.var $(quote (s.drop 1))))
    else
      `(LabelMatcher.matcher (m := PromQLMode.templated) $opTerm $(quote key.raw[0].getAtomVal) (TemplatedValue.lit $value))
| `(tlabelmatcher| $key:name $op:matchop @$var:name) => do
    let opTerm ← expandMatchOp op
    `(LabelMatcher.matcher (m := PromQLMode.templated) $opTerm $(quote key.raw[0].getAtomVal) (TemplatedValue.var $(quote var.raw[0].getAtomVal)))
| `(tlabelmatcher| @$var:name) =>
    `(LabelMatcher.keyValueVar $(quote var.raw[0].getAtomVal))

declare_syntax_cat tlabelmatchers
syntax "{" tlabelmatcher,* "}" : tlabelmatchers
macro_rules
| `(tlabelmatchers| { $xs,* }) => `([$xs,*])

declare_syntax_cat trangevector
syntax name tlabelmatchers "[" numLit "]" : trangevector
syntax name tlabelmatchers "[" "@" name "]" : trangevector

macro_rules
| `(trangevector| $name:name $xs:tlabelmatchers [ $range:num ]) =>
    `(RangeVector.selector (m := PromQLMode.templated)
        ((LabelMatcher.equal (m := PromQLMode.templated) name_label (TemplatedValue.lit $(quote name.raw[0].getAtomVal))) :: $(xs))
        (TemplatedDuration.seconds $range))
| `(trangevector| $name:name $xs:tlabelmatchers [ @$var:name ]) =>
    `(RangeVector.selector (m := PromQLMode.templated)
        ((LabelMatcher.equal (m := PromQLMode.templated) name_label (TemplatedValue.lit $(quote name.raw[0].getAtomVal))) :: $(xs))
        (TemplatedDuration.var $(quote var.raw[0].getAtomVal)))

declare_syntax_cat tinstantvector
syntax name (tlabelmatchers)? : tinstantvector
syntax "time()" : tinstantvector
syntax "rate(" trangevector ")" : tinstantvector
syntax "label_replace(" tinstantvector "," strLit "," strLit "," strLit "," strLit ")" : tinstantvector
syntax tinstantvector " - " tinstantvector : tinstantvector
syntax tinstantvector " + " tinstantvector : tinstantvector
syntax "sum" "(" tinstantvector ")" : tinstantvector
syntax "sum" "by" "(" labelref,* ")" "(" tinstantvector ")" : tinstantvector
syntax "sum" "(" tinstantvector ")" "by" "(" labelref,* ")" : tinstantvector
syntax "sum" "without" "(" labelref,* ")" "(" tinstantvector ")" : tinstantvector
syntax "sum" "(" tinstantvector ")" "without" "(" labelref,* ")" : tinstantvector
syntax "topk" "(" numLit "," tinstantvector ")" : tinstantvector

macro_rules
| `(tinstantvector| time()) => `(InstantVector.time (m := PromQLMode.templated))
| `(tinstantvector| $name:name) =>
    `(InstantVector.selector (m := PromQLMode.templated)
        [LabelMatcher.equal (m := PromQLMode.templated) name_label (TemplatedValue.lit $(quote name.raw[0].getAtomVal))] 0)
| `(tinstantvector| $name:name $xs:tlabelmatchers) =>
    `(InstantVector.selector (m := PromQLMode.templated)
        ((LabelMatcher.equal (m := PromQLMode.templated) name_label (TemplatedValue.lit $(quote name.raw[0].getAtomVal))) :: $(xs)) 0)
| `(tinstantvector| $a - $b) => `(InstantVector.sub_vector (m := PromQLMode.templated) Option.none $a $b)
| `(tinstantvector| $a + $b) => `(InstantVector.add_vector (m := PromQLMode.templated) Option.none $a $b)
| `(tinstantvector| rate($rv)) => `(InstantVector.rate (m := PromQLMode.templated) $rv)
| `(tinstantvector| label_replace($v, $dst, $repl, $src, $reg)) =>
    `(InstantVector.label_replace (m := PromQLMode.templated) $v $dst $repl $src $reg)
| `(tinstantvector| sum($v)) => `(InstantVector.sum (m := PromQLMode.templated) .none $v)
| `(tinstantvector| sum by($bs,*) ($v)) =>
    `(InstantVector.sum (m := PromQLMode.templated) (AggregationSelector.by [$bs,*]) ($v))
| `(tinstantvector| sum ($v) by($bs,*)) =>
    `(InstantVector.sum (m := PromQLMode.templated) (AggregationSelector.by [$bs,*]) ($v))
| `(tinstantvector| sum without($bs,*) ($v)) =>
    `(InstantVector.sum (m := PromQLMode.templated) (AggregationSelector.without [$bs,*]) ($v))
| `(tinstantvector| sum ($v) without($bs,*)) =>
    `(InstantVector.sum (m := PromQLMode.templated) (AggregationSelector.without [$bs,*]) ($v))
| `(tinstantvector| topk($k, $v)) =>
    `(InstantVector.topk (m := PromQLMode.templated) .none $k $v)

syntax "[tpql|" tinstantvector "]" : term

macro_rules
| `([tpql| $v:tinstantvector]) => `($v)

set_option pp.rawOnError true
#eval [pql| up{}]
#eval [pql| up{instance="localhost"}]
#eval [pql| up - up]
#eval [pql| up{test="abc", lan="def"}]
#eval [pql| time()]
#eval [pql| rate(up{}[7])]
#eval [pql| label_replace(up, "new", "$1-xyz", "job", "(.*)")]
#eval [pql| sum (up{})]
#eval [pql| sum by(**instance) (up)]
#eval [pql| sum (up) by(**instance)]
#eval [pql| sum without (**instance) (up)]
#eval [pql| sum (up) without (**instance)]
#eval [tpql| rate(node_network_receive_bytes_total{device="$device", @extra_filters}[@__rate_interval])]

def up_templated_with_job (job : String) : PureInstantVector .vector :=
  InstantVector.selector [.equal name_label job] 0

def bytes_receives_templated_with_rate_window (rate_window : Nat) : PureInstantVector .vector :=
  InstantVector.rate $ RangeVector.selector [.equal name_label "node_network_receive_bytes_total"] rate_window
