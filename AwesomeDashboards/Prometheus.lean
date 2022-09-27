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
| time
| unitless
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
  | sub_vector (vector_matching : Option VectorMatching) (a b : InstantVector vector) : InstantVector scalar
  | add_scalar_left (a : InstantVector scalar) (b : InstantVector vector) : InstantVector vector
  | sum (a : AggregationSelector) (v : InstantVector vector) : InstantVector vector
  | topk (a : AggregationSelector) (k : Nat) (v : InstantVector vector) : InstantVector vector
  | rate (r : RangeVector) : InstantVector vector
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
  | (InstantVector.sub_vector _ a b) => typesafe a e && typesafe b e
  | _ => true

def InstantVector.withLiteralMatch {t : InstantVectorType} (key: String) (value: String) : InstantVector t → InstantVector t
  | (InstantVector.selector lm offset) => InstantVector.selector (lm++[{key:=key, value:=value}]) offset
  | x => x

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

def Exporter.unitOf (e : Exporter) (name : string) : Option MetricUnit := sorry

def Option.get : (a : Option α) → a.isSome → α
  | some a, _ => a

def getNameLm? (lm : List KeyValuePair) : Option String :=
  let name? := lm.find? (λl => is_name l)
  match name? with
    | (Option.some name) => name.value
    | (Option.none) => Option.none

def InstantVector.getName? {lm} {offset} (v : InstantVector vector) (h : v = InstantVector.selector lm offset) : Option String := 
  getNameLm? lm

def getMetricDef? (e : Exporter) (lm : List KeyValuePair) : Option Metric := match getNameLm? lm with
  | (Option.some name) => e.metrics.find? (λm => m.name = name)
  | (Option.none) => Option.none

def unitOf {t : InstantVectorType} (e : Exporter) : InstantVector t → Option MetricUnit
  | (InstantVector.time) => MetricUnit.time
  | (InstantVector.selector lm offset) => match getMetricDef? e lm with
    | (Option.some metric) => metric.unit
    | _ => Option.none
  | (InstantVector.label_replace v _ _ _ _) => unitOf e v
  | (InstantVector.sub_vector _ a b) => if (unitOf e a) = (unitOf e b) then unitOf e a else Option.none
  | (InstantVector.add_scalar_left  s v) => unitOf e v -- adding a scalar doesn't change the unit (however, it might no longer make sense)
  | (InstantVector.rate r) => MetricUnit.seconds -- we need to implement unitOf for range vectors and allow units to be rates
  | (InstantVector.literal f) => MetricUnit.unitless -- alternatively we could implement this function only vectors

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
| `(labelmatcher_aux| $key:name=$value) => `(InstantVector.selector [{key := $(quote key[0].getAtomVal!), value:= $value}] 0)

-- 
declare_syntax_cat labelmatcher
-- The parser 
syntax "{" labelmatcher_aux,* "}" : labelmatcher
-- Make label matcher a valid term (not really neccessary)
syntax labelmatcher : term

-- How to translate label matchers into Lean terms
macro_rules
| `(labelmatcher| {}) => `(InstantVector.selector (@List.nil KeyValuePair) 0)
| `(labelmatcher| { $x }) => `($x)
--| `(labelmatcher| { $x, $xs,* }) => `({$xs,*})

syntax "[pql|" name (labelmatcher)? "]" : term
syntax "[pql|" "time()" "]" : term

set_option trace.Elab.definition true in
macro_rules
| `([pql| time()]) => `(InstantVector.time)
| `([pql| $name]) => `(InstantVector.selector [{key := "__name__", value:= $(quote name[0].getAtomVal!)}] 0)
| `([pql| $name $xs]) => `(InstantVector.withLiteralMatch "__name__" $(quote name[0].getAtomVal!)  $xs)

#eval InstantVector.selector ([{key := "__name__", value:= "f"}]++[{key := "$x", value := ""}]) 0
set_option pp.rawOnError true
#eval [pql| up{}]
#eval [pql| up{instance="localhost"}]
-- #eval [pql| up{test="abc", lan="def"}]
#eval [pql| time()]


#eval s!"f"

-- syntax:max can be used to change precendense

syntax "`[abc| " name "]" : term
macro_rules
| `(`[abc| $x ]) => pure $ quote ("<" ++ x[0].getAtomVal! ++ "/>")

#check `[abc| def]
