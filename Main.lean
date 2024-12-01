import AwesomeDashboards.Grafana
import AwesomeDashboards.NodeExporter
import AwesomeDashboards.Prometheus
import AwesomeDashboards.Widget
--import AwesomeDashboards.Lsp
import Lean
import Lean.Data.Json

open Lean.Widget

def myPrometheusEnv : Environment := {
  scrapeConfigs := [{ targetLabels := ["env"], exporter := node_exporter }]
}

def myDashboard : Dashboard myPrometheusEnv := { name := "My Dashboard", panels := [{
  panels := [
    (.graph { promql := .inl { v := [pql| node_filesystem_avail_bytes{} ]}, legendFormat := "{{device}}"}),
    (.graph { promql := .inl { v := [pql| process_cpu_seconds_total{} ]}}),
    (.graph { promql := .inr (.selector [⟨"instance", "instance"⟩] [⟨"__name__", "node_info"⟩] [])})
  ]
} , {
  height := 6,
  panels := [
    (.graph { promql := .inl { v := [pql| rate(node_network_receive_bytes_total{device="eth0"}[120]) ]}}),
    (.table { name := "My table", joinLabel := "device", columns := [
      { name := "Free bytes", promql := { v := [pql| node_filesystem_avail_bytes{} ]}, additionalLabels := ["fstype"] },
      { name := "Free files", promql := { v := [pql| node_filesystem_files_free{} ]} }
    ] })
  ]
}]}

-- #eval (⟨[pql| label_replace(node_filesystem_avail_bytes{}, "new", "$1-xyz", "job", "(.*)") ], by eq_refl⟩ : TypesafeInstantVector .vector myPrometheusEnv).labels

def main : IO Unit := do
  IO.print $ Lean.toJson $ dashboardToGrafana myDashboard
  return
