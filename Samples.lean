import AwesomeDashboards.Grafana

open Unpositioned

def datasource : GrafanaPanelDatasource := {
  type := "prometheus",
  uid := "OoN46punz"
}

def a := Unpositioned.GrafanaDashboard.toPositioned {
  id := "",
  uuid := "",
  title := "Example A",
  tags := [],
  schemaVersion := 36,
  panels := [
    {
      type := "timeseries",
      title := "Panel A",
      context := "",
      datasource := datasource,
      fieldConfig := {defaults := {}}
    }
  ]
}
