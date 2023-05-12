import AwesomeDashboards.Grafana
import AwesomeDashboards.NodeExporter
import AwesomeDashboards.Prometheus
import AwesomeDashboards.Widget
import Lean
import Lean.Data.Json

open Lean.Widget

def myPrometheusEnv : Environment := {
  scrapeConfigs := [{ targetLabels := [], exporter := node_exporter }]
}

def myDashboard : Dashboard myPrometheusEnv := { name := "My Dashboard", panels := [
    (Panel.graph { promql := { v := [pql| node_filesystem_avail_bytes{} ] } }),
    (Panel.graph { promql := { v := [pql| process_cpu_seconds_total{} ]}}),
    (Panel.graph { promql := { v := [pql| rate(node_network_receive_bytes_total{device="eth0"}[120]) ]}}),
    (Panel.table { name := "My table", joinLabel := "device", columns := [
      { name := "Free bytes", v := [pql| node_filesystem_avail_bytes{} ] },
      { name := "Free files", v := [pql| node_filesystem_files_free{} ] }
    ] })
]}

#eval (⟨[pql| label_replace(node_filesystem_avail_bytes{}, "new", "$1-xyz", "job", "(.*)") ], by simp⟩ : TypesafeInstantVector .vector myPrometheusEnv).labels

def main : IO Unit := do
  IO.print $ Lean.toJson $ dashboardToGrafana myDashboard
  return

#widget dashboardWidget (Lean.toJson myDashboard)
