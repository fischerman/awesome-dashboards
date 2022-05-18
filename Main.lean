import LeanPlayground
import LeanPlayground.Grafana
import Lean.Data.Json

def myDashboard : Dashboard := { name := "My Dashboard", panels := [
    (Panel.graph { bottomY := 0, topY := 0, promql := "up" }),
    (Panel.table { name := "", columns := [
      { name := "Uptime", data := (ColumnValueSource.PrometheusValueColumn $ InstantVector.selector [{key := "__name__", value := "node_boot_time_seconds"}] 0), index_label := "instance" },
      { name := "Free disk space", data := (ColumnValueSource.PrometheusValueColumn $ InstantVector.selector [{key := "__name__", value := "node_filesystem_avail_bytes"}] 0), index_label := "instance" }
    ] })
  ] }

def main : IO Unit := do
  IO.print $ Lean.toJson $ dashboardToGrafana myDashboard
  return
