import Lean.Data.Json
import AwesomeDashboards.Dashboard

structure GrafanaPanelGridPos where
  x: Nat
  y: Nat
  w: Nat
  h: Nat
  deriving  Lean.ToJson

structure GrafanaPanelDatasource where
  type: String
  uid: String
  deriving  Lean.ToJson

structure GrafanaPanelTarget where
  datasource: GrafanaPanelDatasource
  expr: String
  refId: String
  format: Option String := none
  legendFormat: Option String := .none
  instant: Option Bool := none
  range: Option Bool := none
deriving  Lean.ToJson

structure FieldConfigDefaults where
  unit: String
deriving  Lean.ToJson

inductive FieldConfigOverrideMatcher
| byName (name: String)

instance : Lean.ToJson FieldConfigOverrideMatcher where
  toJson := fun t => match t with
  | .byName n => Lean.Json.mkObj [⟨"id", Lean.Json.str "byName"⟩, ⟨"options", Lean.Json.str n⟩]

inductive FieldConfigOverrideProperty
| unit (value: String)

instance : Lean.ToJson FieldConfigOverrideProperty where
  toJson := fun t => match t with
  | .unit v => Lean.Json.mkObj [⟨"id", Lean.Json.str "unit"⟩, ⟨"value", Lean.Json.str v⟩]

structure FieldConfigOverride where
  matcher: FieldConfigOverrideMatcher
  properties: List FieldConfigOverrideProperty
deriving  Lean.ToJson

structure FieldConfig where
  defaults: FieldConfigDefaults
  overrides: List FieldConfigOverride
deriving  Lean.ToJson

structure GrafanaPanelSeriesToColumnsOptions where
  byField: String
  deriving Lean.ToJson, Lean.FromJson

instance : Lean.ToJson $ Std.HashMap String Bool where
  toJson := fun m => Lean.Json.mkObj $ m.toList.map (fun e => ⟨e.1, e.2⟩)

instance : Lean.ToJson $ Std.HashMap String String where
  toJson := fun m => Lean.Json.mkObj $ m.toList.map (fun e => ⟨e.1, e.2⟩)

structure GrafanaPanelOrganizeOptions where
  excludeByName: Std.HashMap String Bool
  renameByName: Std.HashMap String String
  deriving Lean.ToJson

inductive GrafanaPanelTransformation
| seriesToColumns (options: GrafanaPanelSeriesToColumnsOptions)
| organize (options: GrafanaPanelOrganizeOptions)

instance : Lean.ToJson GrafanaPanelTransformation where
  toJson := fun t => match t with
  | .seriesToColumns o => Lean.Json.mkObj [⟨"id", Lean.Json.str "seriesToColumns"⟩, ⟨"options", Lean.toJson o⟩]
  | .organize o => Lean.Json.mkObj [⟨"id", Lean.Json.str "organize"⟩, ⟨"options", Lean.toJson o⟩]

structure GrafanaPanel where
  type: String
  title: String
  description: Option String := .none
  gridPos: GrafanaPanelGridPos
  context: String
  datasource: GrafanaPanelDatasource
  fieldConfig: FieldConfig
  targets: Option $ List GrafanaPanelTarget
  transformations : Option $ List GrafanaPanelTransformation := .none
  deriving Lean.ToJson

inductive GrafanaTemplatingRefresh
/-- 1 -/
| onDashboardLoad
/-- 2 -/
| onTimeRangeChange

instance : Lean.ToJson GrafanaTemplatingRefresh where
  toJson := fun t => match t with
  | .onDashboardLoad => 1
  | .onTimeRangeChange => 2

inductive GrafanaTemplatingQueryType
| query

instance : Lean.ToJson GrafanaTemplatingQueryType where
  toJson := fun t => match t with
  | .query => 3

structure GrafanaTemplatingQuery where
  qryType: GrafanaTemplatingQueryType
  deriving Lean.ToJson


/-- Only  -/
structure GrafanaTemplatingVariable where
  datasource: GrafanaPanelDatasource
  description: String
  /-- Whether an option is to include all. -/
  includeAll: Bool
  /-- Whether multiple values can be selected at the same time. -/
  multi: Bool
  /-- Optional display name. -/
  label: Option String
  name: String
  regex: String
  query: GrafanaTemplatingQuery
  refresh: GrafanaTemplatingRefresh
  deriving Lean.ToJson

structure GrafanaTemplating where
  list: List GrafanaTemplatingVariable
  deriving Lean.ToJson

structure GrafanaDashboard where
  id: Option String
  uuid: Option String
  title: String
  tags: List String
  schemaVersion: Nat
  panels: List GrafanaPanel
  templating: Option GrafanaTemplating := .none
  deriving  Lean.ToJson

def myDatasource : GrafanaPanelDatasource := { type := "prometheus", uid := "OoN46punz" }

def myGrafanaDashboard : GrafanaDashboard := {
  id := Option.none, uuid := "lBf76pX7k", title := "My Dashboard", tags := [], schemaVersion := 36, panels := [
    {
      type := "text",
      datasource := { type := "prometheus", uid := "OoN46punz" },
      context := "Hello world",
      title := "",
      gridPos := { x := 0, y := 0, w := 12, h := 5 },
      targets := none,
      fieldConfig := { defaults := { unit := "none" }, overrides := []}
    },
    {
      type := "timeseries",
      datasource := { type := "prometheus", uid := "OoN46punz" },
      context := "Hello world",
      title := "",
      gridPos := { x := 0, y := 12, w := 12, h := 5 } ,
      targets := some [{
        datasource := myDatasource
        expr := "up{}",
        refId := "A",
      }],
      fieldConfig := { defaults := { unit := "none" }, overrides := []}
    }
  ] }

def metricUnitToGrafanaUnit (u : MetricUnit) : String := match u with
  | (MetricUnit.bytes) => "decbytes"
  | (MetricUnit.div MetricUnit.bytes MetricUnit.seconds) => "Bps"
  | (MetricUnit.seconds) => "s"
  | _ => "locale"

/-- mapI is like map except that the map function is also passed the index of the element. -/
def mapI (f : α → Nat → β) : Nat → List α → List β
  | _, []    => []
  | n, a::as => f a n :: mapI f (n+1) as

def unsafeTemplatedInstantVectorToGrafanaExpr {t: InstantVectorType} (v: UnsafeTemplatedInstantVector t) : String := match v with
| .selector value_vars literal_equal_selectors key_value_vars =>
    let sls' := value_vars.map (λs => s.fst ++ "=\"$" ++ s.snd ++ "\"")
    let lits' := literal_equal_selectors.map (λl => l.fst ++ "=\"" ++ l.snd ++ "\"")
    let key_values_vars' := key_value_vars.map (λkvs => "$" ++ kvs)
    "{" ++ (joinSep (sls' ++ lits' ++ key_values_vars') ", ") ++ "}"
| .add_vector x y => s!"{unsafeTemplatedInstantVectorToGrafanaExpr x} + {unsafeTemplatedInstantVectorToGrafanaExpr y}"

def panelToGrafanaPanel {e : Environment} (p : @Panel e) (pos : GrafanaPanelGridPos) : GrafanaPanel := match p with
| Panel.graph g =>  {
  type := "timeseries",
  title := "",
  gridPos := pos,
  context := "Hello world",
  description := match g.promql with
    | .inl promql => .some promql.helpString
    | .inr _ => .none
  datasource := myDatasource,
  targets := some [
    {
      datasource := myDatasource,
      expr := match g.promql with
        | .inl promql => promql.v.toString
        | .inr promql => unsafeTemplatedInstantVectorToGrafanaExpr promql
      refId := "A",
      legendFormat := g.legendFormat
    }
  ],
  fieldConfig := {
    defaults := {
      unit := match g.promql with
        | .inl promql => match unitOf e promql.v with
          | (some m) => metricUnitToGrafanaUnit m
          | _ => "none"
        | .inr _ => "none"
    },
    overrides := []
  }
}
| Panel.table t => {
      type := "table",
      datasource := { type := "prometheus", uid := "OoN46punz" },
      context := "Hello table",
      title := t.name,
      gridPos := pos,
      targets := some $ mapI (fun c i =>
        { datasource := myDatasource, expr := c.promql.v.toString, refId := String.mk [Char.ofNat (65+i)], format := "table", instant := true, range := false }
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
            if h: t.columns.length = 1 then
              let h' : 0 < List.length t.columns := by simp[h];
              let c := t.columns[0]
              List.map (⟨·, true⟩) $ c.promql.labels.filter (not ∘ (c.additionalLabels.contains ·))
            else
              List.foldl (·++·) [] $ mapI (fun c i => List.map (⟨s!"{·} {i+1}", true⟩) $ c.promql.labels.filter (not ∘ (c.additionalLabels.contains ·))) 0 t.columns
          ),
          renameByName := Std.HashMap.ofList (
            if h: t.columns.length = 1 then
              let h' : 0 < List.length t.columns := by simp[h];
              [⟨"Value", (t.columns[0]).name⟩]
            else
              mapI (fun c i => ⟨s!"Value #{String.mk [Char.ofNat (65+i)]}", c.name⟩) 0 t.columns
          )
        }
      ],
      fieldConfig := {
        defaults := {
          unit := "none"
        },
        overrides := t.columns.map (fun c => { matcher := (.byName c.name), properties := [.unit $ metricUnitToGrafanaUnit $ Option.getD (unitOf e c.promql.v) .unitless]})
      }
    }

def panelsToGrafanaPanels {e : Environment} (ps : List $ @Panel e) (x : Nat) (y : Nat) (w : Nat) (h : Nat) : List GrafanaPanel := match ps with
| (p :: ps) => panelToGrafanaPanel p { x := x, y := y, h := h, w := w } :: panelsToGrafanaPanels ps (x+w) y w h
| [] => []

def rowsToGrafanaPanels {e : Environment} (ps : List $ Row e) (y : Nat) : List GrafanaPanel := match ps with
| (r :: rs) => (rowsToGrafanaPanels rs (y+10)) ++ (panelsToGrafanaPanels r.panels 0 y (24 / r.panels.length) r.height)
| [] => []

def dashboardToGrafana {e : Environment} (d : @Dashboard e) : GrafanaDashboard := {
  id := none,
  uuid := "lBf76pX7k",
  title := d.name,
  tags := [],
  schemaVersion := 36,
  panels := rowsToGrafanaPanels d.panels 0
}

namespace Unpositioned

structure UnpositionedGrafanaPanel where
  type: String
  title: String
  description: Option String := .none
  context: String
  datasource: GrafanaPanelDatasource
  fieldConfig: FieldConfig
  targets: Option $ List GrafanaPanelTarget
  transformations : Option $ List GrafanaPanelTransformation := .none

namespace UnpositionedGrafanaPanel
  def position (p: UnpositionedGrafanaPanel) (pos: GrafanaPanelGridPos) : GrafanaPanel := {
    type := p.type,
    title := p.title,
    description := p.description,
    context := p.context,
    datasource := p.datasource,
    fieldConfig := p.fieldConfig,
    targets := p.targets,
    gridPos := pos,
    transformations := p.transformations,
  }
end UnpositionedGrafanaPanel

structure UnpositionedGrafanaDashboard where
  id: Option String
  uuid: Option String
  title: String
  tags: List String
  schemaVersion: Nat
  panels: List UnpositionedGrafanaPanel

namespace GrafanaDashboard
  def positionPanels (ps : List UnpositionedGrafanaPanel) (x : Nat) : List GrafanaPanel := match ps with
  | (p :: ps) => p.position {h := 10, w := 12, x := x / 12, y := x % 12} :: positionPanels ps (x+12)
  | [] => []

  def toPositioned (d: UnpositionedGrafanaDashboard) : GrafanaDashboard := {
    id := d.id,
    uuid := d.uuid,
    title := d.title,
    tags := d.tags,
    schemaVersion := d.schemaVersion,
    panels := positionPanels d.panels 0
  }
end GrafanaDashboard

end Unpositioned
