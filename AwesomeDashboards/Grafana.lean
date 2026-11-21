import Lean.Data.Json
import AwesomeDashboards.Dashboard

structure GrafanaPanelGridPos where
  x : Nat
  y : Nat
  w : Nat
  h : Nat
  deriving Lean.ToJson

structure GrafanaPanelDatasource where
  type : String
  uid : String
  deriving Lean.ToJson

def Datasource.toGrafana (ds : Datasource) : GrafanaPanelDatasource :=
  { type := ds.type, uid := ds.uid }

structure GrafanaPanelTarget where
  datasource : GrafanaPanelDatasource
  expr : String
  refId : String
  format : Option String := none
  legendFormat : Option String := .none
  instant : Option Bool := none
  range : Option Bool := none
  deriving Lean.ToJson

structure FieldConfigDefaults where
  unit : String
  deriving Lean.ToJson

inductive FieldConfigOverrideMatcher
  | byName (name : String)

instance : Lean.ToJson FieldConfigOverrideMatcher where
  toJson := fun t => match t with
    | .byName n => Lean.Json.mkObj [⟨"id", Lean.Json.str "byName"⟩, ⟨"options", Lean.Json.str n⟩]

inductive FieldConfigOverrideProperty
  | unit (value : String)

instance : Lean.ToJson FieldConfigOverrideProperty where
  toJson := fun t => match t with
    | .unit v => Lean.Json.mkObj [⟨"id", Lean.Json.str "unit"⟩, ⟨"value", Lean.Json.str v⟩]

structure FieldConfigOverride where
  matcher : FieldConfigOverrideMatcher
  properties : List FieldConfigOverrideProperty
  deriving Lean.ToJson

structure FieldConfig where
  defaults : FieldConfigDefaults
  overrides : List FieldConfigOverride
  deriving Lean.ToJson

structure GrafanaPanelSeriesToColumnsOptions where
  byField : String
  deriving Lean.ToJson, Lean.FromJson

instance : Lean.ToJson $ Std.HashMap String Bool where
  toJson := fun m => Lean.Json.mkObj $ m.toList.map (fun e => ⟨e.1, e.2⟩)

instance : Lean.ToJson $ Std.HashMap String String where
  toJson := fun m => Lean.Json.mkObj $ m.toList.map (fun e => ⟨e.1, e.2⟩)

structure GrafanaPanelOrganizeOptions where
  excludeByName : Std.HashMap String Bool
  renameByName : Std.HashMap String String
  deriving Lean.ToJson

inductive GrafanaPanelTransformation
  | seriesToColumns (options : GrafanaPanelSeriesToColumnsOptions)
  | organize (options : GrafanaPanelOrganizeOptions)

instance : Lean.ToJson GrafanaPanelTransformation where
  toJson := fun t => match t with
    | .seriesToColumns o => Lean.Json.mkObj [⟨"id", Lean.Json.str "seriesToColumns"⟩, ⟨"options", Lean.toJson o⟩]
    | .organize o => Lean.Json.mkObj [⟨"id", Lean.Json.str "organize"⟩, ⟨"options", Lean.toJson o⟩]

structure GrafanaPanel where
  id : Nat
  type : String
  title : String
  description : Option String := .none
  gridPos : GrafanaPanelGridPos
  datasource : GrafanaPanelDatasource
  fieldConfig : FieldConfig
  targets : Option $ List GrafanaPanelTarget
  transformations : Option $ List GrafanaPanelTransformation := .none
  deriving Lean.ToJson

inductive GrafanaTemplatingRefresh
  /-- 1: On dashboard load -/
  | onDashboardLoad
  /-- 2: On time range change -/
  | onTimeRangeChange

instance : Lean.ToJson GrafanaTemplatingRefresh where
  toJson := fun t => match t with
    | .onDashboardLoad => 1
    | .onTimeRangeChange => 2

inductive GrafanaTemplatingQueryType
  /-- 1: Label values (`label_values(label)`) -/
  | labelValues
  /-- 3: Classic PromQL query (`query_result(...)`) -/
  | query

instance : Lean.ToJson GrafanaTemplatingQueryType where
  toJson := fun t => match t with
    | .labelValues => 1
    | .query => 3

structure GrafanaTemplatingQuery where
  qryType : GrafanaTemplatingQueryType
  query : String
  refId : String := "PrometheusVariableQueryEditor-VariableQuery"
  deriving Lean.ToJson

inductive GrafanaTemplatingVariable
  | query
      (name : String)
      (datasource : GrafanaPanelDatasource)
      (query : GrafanaTemplatingQuery)
      (definition : String)
      (description : String := "")
      (label : Option String := .none)
      (includeAll : Bool := false)
      (multi : Bool := false)
      (regex : String := "")
      (refresh : GrafanaTemplatingRefresh := .onDashboardLoad)
  | interval
      (name : String)
      (query : String := "1m,5m,15m,1h")
      (description : String := "")
      (label : Option String := .none)
      (auto : Bool := true)
      (autoCount : Nat := 30)
      (autoMin : String := "10s")
  | textbox
      (name : String)
      (query : String := "")
      (description : String := "")
      (label : Option String := .none)

instance : Lean.ToJson GrafanaTemplatingVariable where
  toJson
    | .query name ds query definition description label includeAll multi regex refresh =>
        Lean.Json.mkObj [
          ⟨"type", Lean.Json.str "query"⟩,
          ⟨"name", Lean.Json.str name⟩,
          ⟨"label", Lean.toJson label⟩,
          ⟨"description", Lean.Json.str description⟩,
          ⟨"datasource", Lean.toJson ds⟩,
          ⟨"query", Lean.toJson query⟩,
          ⟨"definition", Lean.Json.str definition⟩,
          ⟨"includeAll", Lean.toJson includeAll⟩,
          ⟨"multi", Lean.toJson multi⟩,
          ⟨"regex", Lean.Json.str regex⟩,
          ⟨"refresh", Lean.toJson refresh⟩
        ]
    | .interval name query description label auto autoCount autoMin =>
        Lean.Json.mkObj [
          ⟨"type", Lean.Json.str "interval"⟩,
          ⟨"name", Lean.Json.str name⟩,
          ⟨"label", Lean.toJson label⟩,
          ⟨"description", Lean.Json.str description⟩,
          ⟨"query", Lean.Json.str query⟩,
          ⟨"auto", Lean.toJson auto⟩,
          ⟨"auto_count", Lean.toJson autoCount⟩,
          ⟨"auto_min", Lean.Json.str autoMin⟩
        ]
    | .textbox name query description label =>
        Lean.Json.mkObj [
          ⟨"type", Lean.Json.str "textbox"⟩,
          ⟨"name", Lean.Json.str name⟩,
          ⟨"label", Lean.toJson label⟩,
          ⟨"description", Lean.Json.str description⟩,
          ⟨"query", Lean.Json.str query⟩
        ]

structure GrafanaTemplating where
  list : List GrafanaTemplatingVariable
  deriving Lean.ToJson

structure GrafanaTimeRange where
  fromTime : String
  toTime : String

instance : Lean.ToJson GrafanaTimeRange where
  toJson tr := Lean.Json.mkObj [
    ⟨"from", Lean.Json.str tr.fromTime⟩,
    ⟨"to", Lean.Json.str tr.toTime⟩
  ]

structure GrafanaDashboard where
  id : Option Nat := .none
  uid : Option String := .none
  title : String
  description : Option String := .none
  tags : List String := []
  schemaVersion : Nat := 38
  panels : List GrafanaPanel
  templating : GrafanaTemplating := { list := [] }
  time : Option GrafanaTimeRange := .none
  deriving Lean.ToJson

def metricUnitToGrafanaUnit (u : MetricUnit) : String := match u with
  | MetricUnit.bytes => "decbytes"
  | MetricUnit.div MetricUnit.bytes MetricUnit.seconds => "Bps"
  | MetricUnit.seconds => "s"
  | MetricUnit.time => "dateTimeAsIso"
  | _ => "locale"

/-- Converts a 0-based target index into a Grafana refId ("A", "B", ...). -/
def refIdOfIndex (i : Nat) : String :=
  String.mk [Char.ofNat (65 + i)]

/-- mapI is like map except that the map function is also passed the index of the element. -/
def mapI (f : α → Nat → β) : Nat → List α → List β
  | _, []    => []
  | n, a::as => f a n :: mapI f (n+1) as

private def firstNonEmpty (xs : List String) : Option String :=
  xs.find? (fun s => !s.isEmpty)

def panelToGrafanaPanel {e : Environment} {vars : Vars}
    (ds : GrafanaPanelDatasource) (panelId : Nat) (p : @Panel e vars) (pos : GrafanaPanelGridPos) : GrafanaPanel :=
  match p with
  | Panel.graph g =>
    let derivedDesc := firstNonEmpty (g.targets.map (fun t => t.promql.helpString e))
    let derivedUnit := g.targets.findSome? (fun t => t.promql.unit e)
    {
      id := panelId,
      type := "timeseries",
      title := g.title,
      gridPos := pos,
      description := g.description.orElse (fun _ => derivedDesc),
      datasource := ds,
      targets := some $ mapI (fun t i => {
        datasource := ds,
        expr := t.promql.query.toString,
        refId := refIdOfIndex i,
        legendFormat := t.legendFormat
      }) 0 g.targets,
      fieldConfig := {
        defaults := {
          unit := match derivedUnit with
            | some m => metricUnitToGrafanaUnit m
            | none => "none"
        },
        overrides := []
      }
    }
  | Panel.table t =>
    {
      id := panelId,
      type := "table",
      datasource := ds,
      title := t.name,
      description := t.description,
      gridPos := pos,
      targets := some $ mapI (fun c i =>
        { datasource := ds, expr := c.promql.query.toString, refId := refIdOfIndex i, format := "table", instant := true, range := false }
      ) 0 t.columns,
      transformations := .some [
        .seriesToColumns { byField := t.joinLabel },
        .organize {
          excludeByName := Std.HashMap.ofList $ (
            if t.columns.length = 1 then
              [⟨"Time", true⟩]
            else
              mapI (fun _ i => ⟨s!"Time {i+1}", true⟩) 0 t.columns
          ) ++ (
            if h : t.columns.length = 1 then
              let h' : 0 < List.length t.columns := by simp [h]
              let c := t.columns[0]
              List.map (⟨·, true⟩) $ (c.promql.labels e).filter (not ∘ (c.additionalLabels.contains ·))
            else
              List.foldl (· ++ ·) [] $ mapI (fun c i => List.map (⟨s!"{·} {i+1}", true⟩) $ (c.promql.labels e).filter (not ∘ (c.additionalLabels.contains ·))) 0 t.columns
          ),
          renameByName := Std.HashMap.ofList (
            if h : t.columns.length = 1 then
              let h' : 0 < List.length t.columns := by simp [h]
              [⟨"Value", (t.columns[0]).name⟩]
            else
              mapI (fun c i => ⟨s!"Value #{refIdOfIndex i}", c.name⟩) 0 t.columns
          )
        }
      ],
      fieldConfig := {
        defaults := {
          unit := "none"
        },
        overrides := t.columns.map (fun c =>
          { matcher := .byName c.name, properties := [.unit $ metricUnitToGrafanaUnit $ Option.getD (c.promql.unit e) .unitless] })
      }
    }

def panelsToGrafanaPanels {e : Environment} {vars : Vars}
    (ds : GrafanaPanelDatasource) (nextId : Nat) (ps : List $ @Panel e vars)
    (x y w h : Nat) : List GrafanaPanel × Nat :=
  match ps with
  | [] => ([], nextId)
  | p :: rest =>
    let gp := panelToGrafanaPanel ds nextId p { x := x, y := y, w := w, h := h }
    let (restPanels, finalId) := panelsToGrafanaPanels ds (nextId + 1) rest (x + w) y w h
    (gp :: restPanels, finalId)

def rowsToGrafanaPanels {e : Environment} {vars : Vars}
    (ds : GrafanaPanelDatasource) (nextId : Nat) (rows : List $ Row e vars) (y : Nat) : List GrafanaPanel :=
  match rows with
  | [] => []
  | r :: rs =>
    match r.panels with
    | [] => rowsToGrafanaPanels ds nextId rs y
    | ps =>
      let w := 24 / ps.length
      let (rowPanels, nextId') := panelsToGrafanaPanels ds nextId ps 0 y w r.height
      rowPanels ++ rowsToGrafanaPanels ds nextId' rs (y + r.height)

/-- Built-in Grafana global variables (like `$__rate_interval`, `$__interval`) start with `"__"`. -/
def isBuiltinGrafanaVar (name : String) : Bool :=
  name.startsWith "__"

private def dedupVars (seen : List String) : Vars → Vars
  | [] => []
  | (name, ty) :: rest =>
    if seen.contains name then
      dedupVars seen rest
    else
      (name, ty) :: dedupVars (name :: seen) rest

def varToGrafanaTemplatingVariable (ds : GrafanaPanelDatasource) (name : String) (ty : TemplateVariableType) :
    Option GrafanaTemplatingVariable :=
  if isBuiltinGrafanaVar name then
    .none
  else
    match ty with
    | .label_value =>
      let expr := s!"label_values({name})"
      .some (.query name ds { qryType := .labelValues, query := expr } expr)
    | .duration =>
      .some (.interval name)
    | .key_value_pairs =>
      .some (.textbox name)

def varsToGrafanaTemplating (ds : GrafanaPanelDatasource) (vars : Vars) : GrafanaTemplating :=
  let uniqueVars := dedupVars [] vars
  { list := uniqueVars.filterMap (fun (name, ty) => varToGrafanaTemplatingVariable ds name ty) }

def dashboardToGrafana {e : Environment} {vars : Vars} (d : @Dashboard e vars) : GrafanaDashboard :=
  let ds := d.datasource.toGrafana
  {
    id := none,
    uid := d.uid,
    title := d.name,
    description := d.description,
    tags := d.tags,
    schemaVersion := 38,
    panels := rowsToGrafanaPanels ds 1 d.panels 0,
    templating := varsToGrafanaTemplating ds vars,
    time := d.time.map (fun tr => { fromTime := tr.fromTime, toTime := tr.toTime })
  }

namespace Unpositioned

structure UnpositionedGrafanaPanel where
  type : String
  title : String
  description : Option String := .none
  datasource : GrafanaPanelDatasource
  fieldConfig : FieldConfig
  targets : Option $ List GrafanaPanelTarget
  transformations : Option $ List GrafanaPanelTransformation := .none

namespace UnpositionedGrafanaPanel
  def position (p : UnpositionedGrafanaPanel) (panelId : Nat) (pos : GrafanaPanelGridPos) : GrafanaPanel := {
    id := panelId,
    type := p.type,
    title := p.title,
    description := p.description,
    datasource := p.datasource,
    fieldConfig := p.fieldConfig,
    targets := p.targets,
    gridPos := pos,
    transformations := p.transformations,
  }
end UnpositionedGrafanaPanel

structure UnpositionedGrafanaDashboard where
  id : Option Nat := .none
  uid : Option String := .none
  title : String
  tags : List String := []
  schemaVersion : Nat := 38
  panels : List UnpositionedGrafanaPanel

namespace GrafanaDashboard
  def positionPanels (ps : List UnpositionedGrafanaPanel) (nextId : Nat) (x : Nat) : List GrafanaPanel := match ps with
  | p :: rest =>
      p.position nextId { h := 10, w := 12, x := x % 24, y := (x / 24) * 10 } :: positionPanels rest (nextId + 1) (x + 12)
  | [] => []

  def toPositioned (d : UnpositionedGrafanaDashboard) : GrafanaDashboard := {
    id := d.id,
    uid := d.uid,
    title := d.title,
    tags := d.tags,
    schemaVersion := d.schemaVersion,
    panels := positionPanels d.panels 1 0
  }
end GrafanaDashboard

end Unpositioned
