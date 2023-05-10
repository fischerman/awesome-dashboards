import AwesomeDashboards.Prometheus

def node_boot_time_seconds : Metric := {
  name := "node_boot_time_seconds"
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.seconds
}

def node_filesystem_avail_bytes : Metric := {
  name := "node_filesystem_avail_bytes"
  type := MetricType.gauge
  labels := ["device", "fstype", "mountpoint"]
  unit := MetricUnit.bytes
}

def process_cpu_seconds_total : Metric := {
  name := "process_cpu_seconds_total"
  type := MetricType.counter
  labels := []
  unit := MetricUnit.seconds
}

def node_network_receive_bytes_total : Metric := {
  name := "node_network_receive_bytes_total"
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.bytes
}

def node_exporter : Exporter := {
  metrics := [node_boot_time_seconds, node_filesystem_avail_bytes, process_cpu_seconds_total, node_network_receive_bytes_total]
}

def lm : List LabelMatcher := [.equal "__name__" "node_filesystem_avail_bytes"]
def v := InstantVector.selector lm 0

namespace x
def myPrometheusEnv : Environment := {
  scrapeConfigs := [{ targetLabels := ["job", "instance"], exporter := node_exporter }]
}

#eval InstantVector.typesafe v myPrometheusEnv

example : InstantVector.typesafe (InstantVector.selector lm 0) myPrometheusEnv := by simp

def avail_bytes : InstantVector InstantVectorType.vector := [pql| node_filesystem_avail_bytes-node_filesystem_avail_bytes]
#eval unitOf myPrometheusEnv avail_bytes
#eval unitOf myPrometheusEnv [pql| time()]
#eval RangeVector.unitOf myPrometheusEnv $ RangeVector.selector [.equal "__name__" "node_network_receive_bytes_total"] 5
#eval unitOf myPrometheusEnv [pql| rate(node_network_receive_bytes_total{}[5])]
#eval [pql| rate(node_network_receive_bytes_total{device="vda"}[120])]

end x
