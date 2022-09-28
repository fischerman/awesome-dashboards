import AwesomeDashboards.Grafana
import AwesomeDashboards.NodeExporter
import AwesomeDashboards.Prometheus
import Lean.Data.Json

def myDashboard : Dashboard node_exporter := { name := "My Dashboard", panels := [
    (Panel.graph { bottomY := 0, topY := 0, promql := { v := [pql| node_filesystem_avail_bytes{}] } }),
    (Panel.graph { bottomY := 0, topY := 0, promql := { v := [pql| node_filesystem_avail_bytes] } }),
    (Panel.table { name := "", columns := [
      { name := "Uptime", data := (ColumnValueSource.PrometheusValueColumn $ InstantVector.selector [{key := "__name__", value := "node_boot_time_seconds"}] 0), index_label := "instance" },
      { name := "Free disk space", data := (ColumnValueSource.PrometheusValueColumn $ InstantVector.selector [{key := "__name__", value := "node_filesystem_avail_bytes"}] 0), index_label := "instance" }
    ] })
  ] }

def main : IO Unit := do
  IO.print $ Lean.toJson $ dashboardToGrafana myDashboard
  return
