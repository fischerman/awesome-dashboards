import AwesomeDashboards.Grafana
import AwesomeDashboards.NodeExporter
import AwesomeDashboards.Prometheus
import Lean.Data.Json

def myDashboard : Dashboard node_exporter := { name := "My Dashboard", panels := [
    (Panel.graph { promql := { v := [pql| node_filesystem_avail_bytes{}] } }),
    (Panel.graph { promql := { v := [pql| process_cpu_seconds_total] } }),
    (Panel.graph { promql := { v := [pql| rate(node_network_receive_bytes_total{device="eth0"}[120])] } }),
    (Panel.table { name := "", columns := [
      { name := "Uptime", data := (ColumnValueSource.PrometheusValueColumn [pql| node_boot_time_seconds]), index_label := "instance" },
      { name := "Free disk space", data := (ColumnValueSource.PrometheusValueColumn [pql| node_filesystem_avail_bytes]), index_label := "instance" }
    ] })
  ] }

def main : IO Unit := do
  IO.print $ Lean.toJson $ dashboardToGrafana myDashboard
  return
