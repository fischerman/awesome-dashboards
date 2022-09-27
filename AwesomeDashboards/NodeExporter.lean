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

def node_exporter : Exporter := {
  metrics := [node_boot_time_seconds, node_filesystem_avail_bytes]
}

def lm : List KeyValuePair := [{key := "__name__", value := "node_filesystem_avail_bytes"}]
def v := InstantVector.selector lm 0

#eval InstantVector.typesafe v node_exporter
#eval List.map (λ l => l.key) (lm.filter $ is_name)
#eval List.all (lm.filter $ is_name) (λ l => "node_filesystem_avail_bytes" = l.key )

example : InstantVector.typesafe (InstantVector.selector [{key := "__name__", value := "node_filesystem_avail_bytes"}] 0) node_exporter := by simp

def avail_bytes : InstantVector InstantVectorType.vector := [pql| node_filesystem_avail_bytes{}]
#eval unitOf node_exporter avail_bytes
