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

def myDatasource : Datasource := {
  type := "prometheus"
  uid := "OoN46punz"
}

def myVars : Vars := [
  ("instance", .label_value),
  ("__rate_interval", .duration)
]

def myDashboard : Dashboard myPrometheusEnv myVars := {
  name := "My Dashboard",
  uid := some "lBf76pX7k",
  tags := ["node-exporter"],
  datasource := myDatasource,
  time := some { fromTime := "now-1h", toTime := "now" },
  panels := [{
    height := 8,
    panels := [
      (.graph (GraphPanel.single
        { query := [pql| node_filesystem_avail_bytes{} ] }
        (title := "Filesystem Available")
        (legendFormat := some "{{device}}"))),
      (.graph (GraphPanel.single
        { query := [pql| process_cpu_seconds_total{} ] }
        (title := "Process CPU Time"))),
      (.graph {
        title := "Network Receive Rate",
        targets := [
          { promql := { query := [tpql| rate(node_network_receive_bytes_total{instance="$instance"}[@__rate_interval]) ] }, legendFormat := some "{{device}} (by instance)" },
          { promql := { query := [pql| rate(node_network_receive_bytes_total{device="eth0"}[5m]) ] }, legendFormat := some "eth0 (5m)" }
        ]
      })
    ]
  }, {
    height := 6,
    panels := [
      (.graph (GraphPanel.single
        { query := [pql| rate(node_network_receive_bytes_total{device="eth0"}[120s]) ] }
        (title := "eth0 Receive Rate"))),
      (.table {
        name := "Filesystem Summary",
        joinLabel := "device",
        columns := [
          { name := "Free bytes", promql := { query := [pql| node_filesystem_avail_bytes{} ] }, additionalLabels := ["fstype"] },
          { name := "Free files", promql := { query := [pql| node_filesystem_files_free{} ] } }
        ]
      })
    ]
  }]
}

-- #eval (⟨[pql| label_replace(node_filesystem_avail_bytes{}, "new", "$1-xyz", "job", "(.*)") ], by decide⟩ : CheckedPureQuery myPrometheusEnv .vector).labels myPrometheusEnv

def main : IO Unit := do
  IO.print $ Lean.toJson $ dashboardToGrafana myDashboard
  return
