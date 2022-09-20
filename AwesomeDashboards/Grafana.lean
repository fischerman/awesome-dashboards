import Lean.Data.Json
import AwesomeDashboards.Dashboard

structure GrafanaPanelGridPos where
  x: Nat
  y: Nat
  w: Nat
  h: Nat
  deriving Lean.FromJson, Lean.ToJson

structure GrafanaPanelDatasource where
  type: String
  uid: String
  deriving Lean.FromJson, Lean.ToJson

structure GrafanaPanelTarget where
  datasource: GrafanaPanelDatasource
  expr: String
  refId: String
  deriving Lean.FromJson, Lean.ToJson

structure GrafanaPanel where
  type: String
  title: String
  gridPos: GrafanaPanelGridPos
  context: String
  datasource: GrafanaPanelDatasource
  targets: Option $ List GrafanaPanelTarget
  deriving Lean.FromJson, Lean.ToJson


structure GrafanaDashboard where
  id: Option String
  uuid: Option String
  title: String
  tags: List String
  schemaVersion: Nat
  panels: List GrafanaPanel
  deriving Lean.FromJson, Lean.ToJson

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
      }]
    }
  ] }

def panelToGrafanaPanel {e : Exporter} (p : @Panel e) (h : Nat) : GrafanaPanel := match p with
| Panel.graph g =>  { 
  type := "timeseries", 
  title := "",
  gridPos := {
    w := 12,
    h := 10,
    x := 0,
    y := h,
  },
  context := "Hello world",
  datasource := myDatasource,
  targets := some [
    { datasource := myDatasource, expr := g.promql.v.toString, refId := "A" }
  ]
}
| Panel.table t => {
      type := "text", 
      datasource := { type := "prometheus", uid := "OoN46punz" }, 
      context := "Hello table", 
      title := "", 
      gridPos := {
        w := 12,
        h := 10,
        x := 0,
        y := h,
      },
      targets := none,
    }

def panelsToGrafanaPanels {e : Exporter} (ps : List $ @Panel e) (h : Nat) : List GrafanaPanel := match ps with
| (p :: ps) => panelToGrafanaPanel p h :: panelsToGrafanaPanels ps (h+10)
| [] => []

def dashboardToGrafana {e : Exporter} (d : @Dashboard e) : GrafanaDashboard := {
  id := none,
  uuid := "lBf76pX7k",
  title := d.name,
  tags := [],
  schemaVersion := 36,
  panels := panelsToGrafanaPanels d.panels 0
}
