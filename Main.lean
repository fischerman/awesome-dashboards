import AwesomeDashboards.Grafana
import AwesomeDashboards.NodeExporter
import AwesomeDashboards.Prometheus
import Lean.Data.Json

def myPrometheusEnv : Environment := {
  scrapeConfigs := [{ targetLabels := ["job", "instance"], exporter := node_exporter }]
}

def myDashboard : Dashboard myPrometheusEnv := { name := "My Dashboard", panels := [
    (Panel.graph { promql := { v := [pql| node_filesystem_avail_bytes{fstype="f"} ] } }),
    (Panel.graph { promql := { v := [pql| process_cpu_seconds_total{job="linux"} ]}}),
    (Panel.graph { promql := { v := [pql| rate(node_network_receive_bytes_total{device="eth0"}[120])] } })
]}

def main : IO Unit := do
  IO.print $ Lean.toJson $ dashboardToGrafana myDashboard
  return
