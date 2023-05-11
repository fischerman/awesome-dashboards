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
  instant: Option Bool := none
  range: Option Bool := none
deriving  Lean.ToJson

structure FieldConfigDefaults where
  unit: String
deriving  Lean.ToJson

structure FieldConfig where
  defaults: FieldConfigDefaults
deriving  Lean.ToJson

structure GrafanaPanelSeriesToColumnsOptions where
  byField: String
  deriving Lean.ToJson, Lean.FromJson

instance : Lean.ToJson $ Lean.HashMap String Bool where
  toJson := fun m => Lean.Json.mkObj $ m.toList.map (fun e => ⟨e.1, e.2⟩)

instance : Lean.ToJson $ Lean.HashMap String String where
  toJson := fun m => Lean.Json.mkObj $ m.toList.map (fun e => ⟨e.1, e.2⟩)

structure GrafanaPanelOrganizeOptions where
  excludeByName: Lean.HashMap String Bool
  renameByName: Lean.HashMap String String
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


structure GrafanaDashboard where
  id: Option String
  uuid: Option String
  title: String
  tags: List String
  schemaVersion: Nat
  panels: List GrafanaPanel
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
      fieldConfig := { defaults := { unit := "none" }}
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
      fieldConfig := { defaults := { unit := "none" }}
    }
  ] }

def metricUnitToGrafanaUnit (u : MetricUnit) : String := match u with
  | (MetricUnit.bytes) => "decbytes"
  | (MetricUnit.div MetricUnit.bytes MetricUnit.seconds) => "Bps"
  | _ => "none"

def mapI (f : α → Nat → β) : Nat → List α → List β
  | _, []    => []
  | n, a::as => f a n :: mapI f (n+1) as

def panelToGrafanaPanel {e : Environment} (p : @Panel e) (h : Nat) : GrafanaPanel := match p with
| Panel.graph g =>  { 
  type := "timeseries", 
  title := "",
  gridPos := {
    w := 24,
    h := 10,
    x := 0,
    y := h,
  },
  context := "Hello world",
  description := g.promql.helpString,
  datasource := myDatasource,
  targets := some [
    { datasource := myDatasource, expr := g.promql.v.toString, refId := "A" }
  ],
  fieldConfig := {
    defaults := {
      unit := match unitOf e g.promql.v with
        | (some m) => metricUnitToGrafanaUnit m
        | _ => "none"
    }
  }
}
| Panel.table t => {
      type := "table", 
      datasource := { type := "prometheus", uid := "OoN46punz" }, 
      context := "Hello table", 
      title := t.name, 
      gridPos := {
        w := 24,
        h := 10,
        x := 0,
        y := h,
      },
      targets := some $ mapI (fun c i => 
        { datasource := myDatasource, expr := c.v.toString, refId := String.mk [Char.ofNat (65+i)], format := "table", instant := true, range := false }
      ) 0 t.columns,
      transformations := .some [
        .seriesToColumns { byField := t.joinLabel },
        .organize { 
          excludeByName := Lean.HashMap.ofList [],
          renameByName := Lean.HashMap.ofList (
            if h: t.columns.length = 1 then 
              let h' : 0 < List.length t.columns := by simp[h];
              [⟨"Value", (t.columns[0]'h').name⟩] 
            else 
              mapI (fun c i => ⟨s!"Value #{String.mk [Char.ofNat (65+i)]}", c.name⟩) 0 t.columns
          )
        }
      ],
      fieldConfig := {
        defaults := {
          unit := "none"
        }
      }
    }

def panelsToGrafanaPanels {e : Environment} (ps : List $ @Panel e) (h : Nat) : List GrafanaPanel := match ps with
| (p :: ps) => panelToGrafanaPanel p h :: panelsToGrafanaPanels ps (h+10)
| [] => []

def dashboardToGrafana {e : Environment} (d : @Dashboard e) : GrafanaDashboard := {
  id := none,
  uuid := "lBf76pX7k",
  title := d.name,
  tags := [],
  schemaVersion := 36,
  panels := panelsToGrafanaPanels d.panels 0
}
