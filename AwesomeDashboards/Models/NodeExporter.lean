import AwesomeDashboards.Dashboard

namespace AwesomeDashboards.Models.NodeExporter


def go_memstats_heap_objects : Metric := {
  name := "go_memstats_heap_objects"
  help := "Number of allocated objects."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_entropy_available_bits : Metric := {
  name := "node_entropy_available_bits"
  help := "Bits of available entropy."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_filefd_maximum : Metric := {
  name := "node_filefd_maximum"
  help := "File descriptor statistics: maximum."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_power_supply_energy_full : Metric := {
  name := "node_power_supply_energy_full"
  help := "energy_full value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def node_timex_pps_jitter_total : Metric := {
  name := "node_timex_pps_jitter_total"
  help := "Pulse per second count of jitter limit exceeded events."
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def go_info : Metric := {
  name := "go_info"
  help := "Information about the Go environment."
  type := MetricType.gauge
  labels := ["version"]
  unit := MetricUnit.unitless
}

def node_hwmon_pwm : Metric := {
  name := "node_hwmon_pwm"
  help := "Hardware monitor pwm element "
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_memory_Committed_AS_bytes : Metric := {
  name := "node_memory_Committed_AS_bytes"
  help := "Memory information field Committed_AS_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_network_flags : Metric := {
  name := "node_network_flags"
  help := "Network device property: flags"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_hwmon_pwm_enable : Metric := {
  name := "node_hwmon_pwm_enable"
  help := "Hardware monitor pwm element enable"
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_memory_HugePages_Rsvd : Metric := {
  name := "node_memory_HugePages_Rsvd"
  help := "Memory information field HugePages_Rsvd."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Cached_bytes : Metric := {
  name := "node_memory_Cached_bytes"
  help := "Memory information field Cached_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_FileHugePages_bytes : Metric := {
  name := "node_memory_FileHugePages_bytes"
  help := "Memory information field FileHugePages_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_timex_sync_status : Metric := {
  name := "node_timex_sync_status"
  help := "Is clock synchronized to a reliable server (1 = yes, 0 = no)."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_TCP_mem_bytes : Metric := {
  name := "node_sockstat_TCP_mem_bytes"
  help := "Number of TCP sockets in state mem_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_cooling_device_cur_state : Metric := {
  name := "node_cooling_device_cur_state"
  help := "Current throttle state of the cooling device"
  type := MetricType.gauge
  labels := ["name", "type"]
  unit := MetricUnit.unitless
}

def node_disk_filesystem_info : Metric := {
  name := "node_disk_filesystem_info"
  help := "Info about disk filesystem."
  type := MetricType.gauge
  labels := ["device", "type", "usage", "uuid", "version"]
  unit := MetricUnit.unitless
}

def node_netstat_TcpExt_SyncookiesFailed : Metric := {
  name := "node_netstat_TcpExt_SyncookiesFailed"
  help := "Statistic TcpExtSyncookiesFailed."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_network_carrier_up_changes_total : Metric := {
  name := "node_network_carrier_up_changes_total"
  help := "Network device property: carrier_up_changes_total"
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_schedstat_running_seconds_total : Metric := {
  name := "node_schedstat_running_seconds_total"
  help := "Number of seconds CPU spent running a process."
  type := MetricType.counter
  labels := ["cpu"]
  unit := MetricUnit.unitless
}

def node_cpu_package_throttles_total : Metric := {
  name := "node_cpu_package_throttles_total"
  help := "Number of times this CPU package has been throttled."
  type := MetricType.counter
  labels := ["package"]
  unit := MetricUnit.unitless
}

def node_memory_VmallocTotal_bytes : Metric := {
  name := "node_memory_VmallocTotal_bytes"
  help := "Memory information field VmallocTotal_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Tcp_ActiveOpens : Metric := {
  name := "node_netstat_Tcp_ActiveOpens"
  help := "Statistic TcpActiveOpens."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_UDP_inuse : Metric := {
  name := "node_sockstat_UDP_inuse"
  help := "Number of UDP sockets in state inuse."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_gc_duration_seconds : Metric := {
  name := "go_gc_duration_seconds"
  help := "A summary of the pause duration of garbage collection cycles."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_discards_merged_total : Metric := {
  name := "node_disk_discards_merged_total"
  help := "The total number of discards merged."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_hwmon_in_volts : Metric := {
  name := "node_hwmon_in_volts"
  help := "Hardware monitor for voltage (input)"
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_network_iface_link_mode : Metric := {
  name := "node_network_iface_link_mode"
  help := "Network device property: iface_link_mode"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_udp_queues : Metric := {
  name := "node_udp_queues"
  help := "Number of allocated memory in the kernel for UDP datagrams in bytes."
  type := MetricType.gauge
  labels := ["ip", "queue"]
  unit := MetricUnit.unitless
}

def node_context_switches_total : Metric := {
  name := "node_context_switches_total"
  help := "Total number of context switches."
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_network_name_assign_type : Metric := {
  name := "node_network_name_assign_type"
  help := "Network device property: name_assign_type"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_network_receive_packets_total : Metric := {
  name := "node_network_receive_packets_total"
  help := "Network device statistic receive_packets."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_disk_read_time_seconds_total : Metric := {
  name := "node_disk_read_time_seconds_total"
  help := "The total number of seconds spent by all reads."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_memory_Shmem_bytes : Metric := {
  name := "node_memory_Shmem_bytes"
  help := "Memory information field Shmem_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Zswap_bytes : Metric := {
  name := "node_memory_Zswap_bytes"
  help := "Memory information field Zswap_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_network_speed_bytes : Metric := {
  name := "node_network_speed_bytes"
  help := "Network device property: speed_bytes"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_timex_tick_seconds : Metric := {
  name := "node_timex_tick_seconds"
  help := "Seconds between clock ticks."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_heap_released_bytes : Metric := {
  name := "go_memstats_heap_released_bytes"
  help := "Number of heap bytes released to OS."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_boot_time_seconds : Metric := {
  name := "node_boot_time_seconds"
  help := "Node boot time, in unixtime."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_network_iface_link : Metric := {
  name := "node_network_iface_link"
  help := "Network device property: iface_link"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_pressure_cpu_waiting_seconds_total : Metric := {
  name := "node_pressure_cpu_waiting_seconds_total"
  help := "Total time in seconds that processes have waited for CPU time"
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_UDP_mem_bytes : Metric := {
  name := "node_sockstat_UDP_mem_bytes"
  help := "Number of UDP sockets in state mem_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_cpu_scaling_frequency_hertz : Metric := {
  name := "node_cpu_scaling_frequency_hertz"
  help := "Current scaled CPU thread frequency in hertz."
  type := MetricType.gauge
  labels := ["cpu"]
  unit := MetricUnit.unitless
}

def node_cpu_scaling_frequency_min_hertz : Metric := {
  name := "node_cpu_scaling_frequency_min_hertz"
  help := "Minimum scaled CPU thread frequency in hertz."
  type := MetricType.gauge
  labels := ["cpu"]
  unit := MetricUnit.unitless
}

def node_netstat_Udp6_InErrors : Metric := {
  name := "node_netstat_Udp6_InErrors"
  help := "Statistic Udp6InErrors."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_network_transmit_errs_total : Metric := {
  name := "node_network_transmit_errs_total"
  help := "Network device statistic transmit_errs."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_memory_DirectMap1G_bytes : Metric := {
  name := "node_memory_DirectMap1G_bytes"
  help := "Memory information field DirectMap1G_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Slab_bytes : Metric := {
  name := "node_memory_Slab_bytes"
  help := "Memory information field Slab_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_network_net_dev_group : Metric := {
  name := "node_network_net_dev_group"
  help := "Network device property: net_dev_group"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_network_receive_errs_total : Metric := {
  name := "node_network_receive_errs_total"
  help := "Network device statistic receive_errs."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_nf_conntrack_stat_search_restart : Metric := {
  name := "node_nf_conntrack_stat_search_restart"
  help := "Number of conntrack table lookups which had to be restarted due to hashtable resizes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_procs_blocked : Metric := {
  name := "node_procs_blocked"
  help := "Number of processes blocked waiting for I/O to complete."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_UDPLITE6_inuse : Metric := {
  name := "node_sockstat_UDPLITE6_inuse"
  help := "Number of UDPLITE6 sockets in state inuse."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_other_sys_bytes : Metric := {
  name := "go_memstats_other_sys_bytes"
  help := "Number of bytes used for other system allocations."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_io_now : Metric := {
  name := "node_disk_io_now"
  help := "The number of I/Os currently in progress."
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_memory_DirectMap4k_bytes : Metric := {
  name := "node_memory_DirectMap4k_bytes"
  help := "Memory information field DirectMap4k_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Mlocked_bytes : Metric := {
  name := "node_memory_Mlocked_bytes"
  help := "Memory information field Mlocked_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Tcp_PassiveOpens : Metric := {
  name := "node_netstat_Tcp_PassiveOpens"
  help := "Statistic TcpPassiveOpens."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_vmstat_pgpgin : Metric := {
  name := "node_vmstat_pgpgin"
  help := "/proc/vmstat information field pgpgin."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_cpu_frequency_min_hertz : Metric := {
  name := "node_cpu_frequency_min_hertz"
  help := "Minimum cpu thread frequency in hertz."
  type := MetricType.gauge
  labels := ["cpu"]
  unit := MetricUnit.unitless
}

def node_sockstat_TCP_mem : Metric := {
  name := "node_sockstat_TCP_mem"
  help := "Number of TCP sockets in state mem."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_softnet_dropped_total : Metric := {
  name := "node_softnet_dropped_total"
  help := "Number of dropped packets"
  type := MetricType.counter
  labels := ["cpu"]
  unit := MetricUnit.unitless
}

def node_timex_pps_error_total : Metric := {
  name := "node_timex_pps_error_total"
  help := "Pulse per second count of calibration errors."
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_FilePmdMapped_bytes : Metric := {
  name := "node_memory_FilePmdMapped_bytes"
  help := "Memory information field FilePmdMapped_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Udp_InErrors : Metric := {
  name := "node_netstat_Udp_InErrors"
  help := "Statistic UdpInErrors."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_uname_info : Metric := {
  name := "node_uname_info"
  help := "Labeled system information as provided by the uname system call."
  type := MetricType.gauge
  labels := ["domainname", "machine", "nodename", "release", "sysname", "version"]
  unit := MetricUnit.unitless
}

def promhttp_metric_handler_requests_total : Metric := {
  name := "promhttp_metric_handler_requests_total"
  help := "Total number of scrapes by HTTP status code."
  type := MetricType.counter
  labels := ["code"]
  unit := MetricUnit.unitless
}

def node_hwmon_chip_names : Metric := {
  name := "node_hwmon_chip_names"
  help := "Annotation metric for human-readable chip names"
  type := MetricType.gauge
  labels := ["chip", "chip_name"]
  unit := MetricUnit.unitless
}

def node_netstat_Tcp_InErrs : Metric := {
  name := "node_netstat_Tcp_InErrs"
  help := "Statistic TcpInErrs."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_nf_conntrack_stat_drop : Metric := {
  name := "node_nf_conntrack_stat_drop"
  help := "Number of packets dropped due to conntrack failure."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_time_seconds : Metric := {
  name := "node_time_seconds"
  help := "System time in seconds since epoch (1970)."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_cpu_frequency_max_hertz : Metric := {
  name := "node_cpu_frequency_max_hertz"
  help := "Maximum cpu thread frequency in hertz."
  type := MetricType.gauge
  labels := ["cpu"]
  unit := MetricUnit.unitless
}

def node_hwmon_fan_rpm : Metric := {
  name := "node_hwmon_fan_rpm"
  help := "Hardware monitor for fan revolutions per minute (input)"
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_hwmon_in_max_volts : Metric := {
  name := "node_hwmon_in_max_volts"
  help := "Hardware monitor for voltage (max)"
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_load15 : Metric := {
  name := "node_load15"
  help := "15m load average."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_TcpExt_TCPSynRetrans : Metric := {
  name := "node_netstat_TcpExt_TCPSynRetrans"
  help := "Statistic TcpExtTCPSynRetrans."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_ata_write_cache_enabled : Metric := {
  name := "node_disk_ata_write_cache_enabled"
  help := "ATA disk has its write cache enabled."
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_load1 : Metric := {
  name := "node_load1"
  help := "1m load average."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Icmp_InErrors : Metric := {
  name := "node_netstat_Icmp_InErrors"
  help := "Statistic IcmpInErrors."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def process_max_fds : Metric := {
  name := "process_max_fds"
  help := "Maximum number of open file descriptors."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_SecPageTables_bytes : Metric := {
  name := "node_memory_SecPageTables_bytes"
  help := "Memory information field SecPageTables_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_network_carrier : Metric := {
  name := "node_network_carrier"
  help := "Network device property: carrier"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_timex_loop_time_constant : Metric := {
  name := "node_timex_loop_time_constant"
  help := "Phase-locked loop time constant."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def process_cpu_seconds_total : Metric := {
  name := "process_cpu_seconds_total"
  help := "Total user and system CPU time spent in seconds."
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def promhttp_metric_handler_requests_in_flight : Metric := {
  name := "promhttp_metric_handler_requests_in_flight"
  help := "Current number of scrapes being served."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_SReclaimable_bytes : Metric := {
  name := "node_memory_SReclaimable_bytes"
  help := "Memory information field SReclaimable_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_ShmemHugePages_bytes : Metric := {
  name := "node_memory_ShmemHugePages_bytes"
  help := "Memory information field ShmemHugePages_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Icmp6_InErrors : Metric := {
  name := "node_netstat_Icmp6_InErrors"
  help := "Statistic Icmp6InErrors."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def go_threads : Metric := {
  name := "go_threads"
  help := "Number of OS threads created."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_writes_merged_total : Metric := {
  name := "node_disk_writes_merged_total"
  help := "The number of writes merged."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_hwmon_curr_amps : Metric := {
  name := "node_hwmon_curr_amps"
  help := "Hardware monitor for current (input)"
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_hwmon_temp_crit_celsius : Metric := {
  name := "node_hwmon_temp_crit_celsius"
  help := "Hardware monitor for temperature (crit)"
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_memory_PageTables_bytes : Metric := {
  name := "node_memory_PageTables_bytes"
  help := "Memory information field PageTables_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Tcp_CurrEstab : Metric := {
  name := "node_netstat_Tcp_CurrEstab"
  help := "Statistic TcpCurrEstab."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_timex_offset_seconds : Metric := {
  name := "node_timex_offset_seconds"
  help := "Time offset in between local system and reference clock."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_vmstat_pswpout : Metric := {
  name := "node_vmstat_pswpout"
  help := "/proc/vmstat information field pswpout."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_heap_alloc_bytes : Metric := {
  name := "go_memstats_heap_alloc_bytes"
  help := "Number of heap bytes allocated and still in use."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Percpu_bytes : Metric := {
  name := "node_memory_Percpu_bytes"
  help := "Memory information field Percpu_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_UdpLite6_InErrors : Metric := {
  name := "node_netstat_UdpLite6_InErrors"
  help := "Statistic UdpLite6InErrors."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_power_supply_voltage_min_design : Metric := {
  name := "node_power_supply_voltage_min_design"
  help := "voltage_min_design value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def node_scrape_collector_success : Metric := {
  name := "node_scrape_collector_success"
  help := "node_exporter: Whether a collector succeeded."
  type := MetricType.gauge
  labels := ["collector"]
  unit := MetricUnit.unitless
}

def node_power_supply_energy_watthour : Metric := {
  name := "node_power_supply_energy_watthour"
  help := "energy_watthour value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def go_memstats_heap_idle_bytes : Metric := {
  name := "go_memstats_heap_idle_bytes"
  help := "Number of heap bytes waiting to be used."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_read_bytes_total : Metric := {
  name := "node_disk_read_bytes_total"
  help := "The total number of bytes read successfully."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_load5 : Metric := {
  name := "node_load5"
  help := "5m load average."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_network_iface_id : Metric := {
  name := "node_network_iface_id"
  help := "Network device property: iface_id"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_nf_conntrack_stat_insert_failed : Metric := {
  name := "node_nf_conntrack_stat_insert_failed"
  help := "Number of entries for which list insertion was attempted but failed."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_nvme_info : Metric := {
  name := "node_nvme_info"
  help := "Non-numeric data from /sys/class/nvme/<device>, value is always 1."
  type := MetricType.gauge
  labels := ["device", "firmware_revision", "model", "serial", "state"]
  unit := MetricUnit.unitless
}

def node_vmstat_oom_kill : Metric := {
  name := "node_vmstat_oom_kill"
  help := "/proc/vmstat information field oom_kill."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_hwmon_in_min_volts : Metric := {
  name := "node_hwmon_in_min_volts"
  help := "Hardware monitor for voltage (min)"
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_hwmon_temp_crit_alarm_celsius : Metric := {
  name := "node_hwmon_temp_crit_alarm_celsius"
  help := "Hardware monitor for temperature (crit_alarm)"
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_network_device_id : Metric := {
  name := "node_network_device_id"
  help := "Network device property: device_id"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_network_dormant : Metric := {
  name := "node_network_dormant"
  help := "Network device property: dormant"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_network_transmit_packets_total : Metric := {
  name := "node_network_transmit_packets_total"
  help := "Network device statistic transmit_packets."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def go_memstats_alloc_bytes : Metric := {
  name := "go_memstats_alloc_bytes"
  help := "Number of bytes allocated and still in use."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_arp_entries : Metric := {
  name := "node_arp_entries"
  help := "ARP entries by device"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_disk_written_bytes_total : Metric := {
  name := "node_disk_written_bytes_total"
  help := "The total number of bytes written successfully."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_netstat_Udp6_RcvbufErrors : Metric := {
  name := "node_netstat_Udp6_RcvbufErrors"
  help := "Statistic Udp6RcvbufErrors."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_time_zone_offset_seconds : Metric := {
  name := "node_time_zone_offset_seconds"
  help := "System time zone offset in seconds."
  type := MetricType.gauge
  labels := ["time_zone"]
  unit := MetricUnit.unitless
}

def node_timex_maxerror_seconds : Metric := {
  name := "node_timex_maxerror_seconds"
  help := "Maximum error in seconds."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_stack_inuse_bytes : Metric := {
  name := "go_memstats_stack_inuse_bytes"
  help := "Number of bytes in use by the stack allocator."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Active_bytes : Metric := {
  name := "node_memory_Active_bytes"
  help := "Memory information field Active_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_network_receive_multicast_total : Metric := {
  name := "node_network_receive_multicast_total"
  help := "Network device statistic receive_multicast."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_network_transmit_fifo_total : Metric := {
  name := "node_network_transmit_fifo_total"
  help := "Network device statistic transmit_fifo."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_sockstat_UDP6_inuse : Metric := {
  name := "node_sockstat_UDP6_inuse"
  help := "Number of UDP6 sockets in state inuse."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_discard_time_seconds_total : Metric := {
  name := "node_disk_discard_time_seconds_total"
  help := "This is the total number of seconds spent by all discards."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_network_up : Metric := {
  name := "node_network_up"
  help := "Value is 1 if operstate is 'up', 0 otherwise."
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_power_supply_present : Metric := {
  name := "node_power_supply_present"
  help := "present value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def node_sockstat_sockets_used : Metric := {
  name := "node_sockstat_sockets_used"
  help := "Number of IPv4 sockets in use."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_sys_bytes : Metric := {
  name := "go_memstats_sys_bytes"
  help := "Number of bytes obtained from system."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_writes_completed_total : Metric := {
  name := "node_disk_writes_completed_total"
  help := "The total number of writes completed successfully."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_memory_KernelStack_bytes : Metric := {
  name := "node_memory_KernelStack_bytes"
  help := "Memory information field KernelStack_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_UdpLite_InErrors : Metric := {
  name := "node_netstat_UdpLite_InErrors"
  help := "Statistic UdpLiteInErrors."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_network_receive_compressed_total : Metric := {
  name := "node_network_receive_compressed_total"
  help := "Network device statistic receive_compressed."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_forks_total : Metric := {
  name := "node_forks_total"
  help := "Total number of forks."
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_AnonPages_bytes : Metric := {
  name := "node_memory_AnonPages_bytes"
  help := "Memory information field AnonPages_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Ip6_OutOctets : Metric := {
  name := "node_netstat_Ip6_OutOctets"
  help := "Statistic Ip6OutOctets."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_SUnreclaim_bytes : Metric := {
  name := "node_memory_SUnreclaim_bytes"
  help := "Memory information field SUnreclaim_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_SwapFree_bytes : Metric := {
  name := "node_memory_SwapFree_bytes"
  help := "Memory information field SwapFree_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Icmp_InMsgs : Metric := {
  name := "node_netstat_Icmp_InMsgs"
  help := "Statistic IcmpInMsgs."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_heap_inuse_bytes : Metric := {
  name := "go_memstats_heap_inuse_bytes"
  help := "Number of heap bytes that are in use."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_mcache_inuse_bytes : Metric := {
  name := "go_memstats_mcache_inuse_bytes"
  help := "Number of bytes in use by mcache structures."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_cpu_scaling_frequency_max_hertz : Metric := {
  name := "node_cpu_scaling_frequency_max_hertz"
  help := "Maximum scaled CPU thread frequency in hertz."
  type := MetricType.gauge
  labels := ["cpu"]
  unit := MetricUnit.unitless
}

def node_disk_write_time_seconds_total : Metric := {
  name := "node_disk_write_time_seconds_total"
  help := "This is the total number of seconds spent by all writes."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_memory_HardwareCorrupted_bytes : Metric := {
  name := "node_memory_HardwareCorrupted_bytes"
  help := "Memory information field HardwareCorrupted_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_nf_conntrack_stat_invalid : Metric := {
  name := "node_nf_conntrack_stat_invalid"
  help := "Number of packets seen which can not be tracked."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_power_supply_power_watt : Metric := {
  name := "node_power_supply_power_watt"
  help := "power_watt value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def node_schedstat_timeslices_total : Metric := {
  name := "node_schedstat_timeslices_total"
  help := "Number of timeslices executed by CPU."
  type := MetricType.counter
  labels := ["cpu"]
  unit := MetricUnit.unitless
}

def node_network_protocol_type : Metric := {
  name := "node_network_protocol_type"
  help := "Network device property: protocol_type"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_network_receive_fifo_total : Metric := {
  name := "node_network_receive_fifo_total"
  help := "Network device statistic receive_fifo."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_network_transmit_bytes_total : Metric := {
  name := "node_network_transmit_bytes_total"
  help := "Network device statistic transmit_bytes."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_network_transmit_compressed_total : Metric := {
  name := "node_network_transmit_compressed_total"
  help := "Network device statistic transmit_compressed."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_nf_conntrack_stat_insert : Metric := {
  name := "node_nf_conntrack_stat_insert"
  help := "Number of entries inserted into the list."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_FRAG6_inuse : Metric := {
  name := "node_sockstat_FRAG6_inuse"
  help := "Number of FRAG6 sockets in state inuse."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_time_clocksource_available_info : Metric := {
  name := "node_time_clocksource_available_info"
  help := "Available clocksources read from '/sys/devices/system/clocksource'."
  type := MetricType.gauge
  labels := ["clocksource", "device"]
  unit := MetricUnit.unitless
}

def process_virtual_memory_max_bytes : Metric := {
  name := "process_virtual_memory_max_bytes"
  help := "Maximum amount of virtual memory available in bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_timex_pps_frequency_hertz : Metric := {
  name := "node_timex_pps_frequency_hertz"
  help := "Pulse per second frequency."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_timex_pps_shift_seconds : Metric := {
  name := "node_timex_pps_shift_seconds"
  help := "Pulse per second interval duration."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_mspan_inuse_bytes : Metric := {
  name := "go_memstats_mspan_inuse_bytes"
  help := "Number of bytes in use by mspan structures."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Inactive_file_bytes : Metric := {
  name := "node_memory_Inactive_file_bytes"
  help := "Memory information field Inactive_file_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Tcp_OutRsts : Metric := {
  name := "node_netstat_Tcp_OutRsts"
  help := "Statistic TcpOutRsts."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_power_supply_cyclecount : Metric := {
  name := "node_power_supply_cyclecount"
  help := "cyclecount value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def node_procs_running : Metric := {
  name := "node_procs_running"
  help := "Number of processes in runnable state."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_ata_write_cache : Metric := {
  name := "node_disk_ata_write_cache"
  help := "ATA disk has a write cache."
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_memory_Hugetlb_bytes : Metric := {
  name := "node_memory_Hugetlb_bytes"
  help := "Memory information field Hugetlb_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_TcpExt_ListenDrops : Metric := {
  name := "node_netstat_TcpExt_ListenDrops"
  help := "Statistic TcpExtListenDrops."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_power_supply_online : Metric := {
  name := "node_power_supply_online"
  help := "online value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def node_timex_frequency_adjustment_ratio : Metric := {
  name := "node_timex_frequency_adjustment_ratio"
  help := "Local clock frequency adjustment."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def promhttp_metric_handler_errors_total : Metric := {
  name := "promhttp_metric_handler_errors_total"
  help := "Total number of internal errors encountered by the promhttp metric handler."
  type := MetricType.counter
  labels := ["cause"]
  unit := MetricUnit.unitless
}

def node_memory_CommitLimit_bytes : Metric := {
  name := "node_memory_CommitLimit_bytes"
  help := "Memory information field CommitLimit_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_MemTotal_bytes : Metric := {
  name := "node_memory_MemTotal_bytes"
  help := "Memory information field MemTotal_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_nf_conntrack_entries_limit : Metric := {
  name := "node_nf_conntrack_entries_limit"
  help := "Maximum size of connection tracking table."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_vmstat_pgpgout : Metric := {
  name := "node_vmstat_pgpgout"
  help := "/proc/vmstat information field pgpgout."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_vmstat_pswpin : Metric := {
  name := "node_vmstat_pswpin"
  help := "/proc/vmstat information field pswpin."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_DirectMap2M_bytes : Metric := {
  name := "node_memory_DirectMap2M_bytes"
  help := "Memory information field DirectMap2M_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_power_supply_info : Metric := {
  name := "node_power_supply_info"
  help := "info of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply", "type"]
  unit := MetricUnit.unitless
}

def node_scrape_collector_duration_seconds : Metric := {
  name := "node_scrape_collector_duration_seconds"
  help := "node_exporter: Duration of a collector scrape."
  type := MetricType.gauge
  labels := ["collector"]
  unit := MetricUnit.unitless
}

def process_start_time_seconds : Metric := {
  name := "process_start_time_seconds"
  help := "Start time of the process since unix epoch in seconds."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_discards_completed_total : Metric := {
  name := "node_disk_discards_completed_total"
  help := "The total number of discards completed successfully."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_disk_io_time_seconds_total : Metric := {
  name := "node_disk_io_time_seconds_total"
  help := "Total seconds spent doing I/Os."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_disk_io_time_weighted_seconds_total : Metric := {
  name := "node_disk_io_time_weighted_seconds_total"
  help := "The weighted # of seconds spent doing I/Os."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_selinux_enabled : Metric := {
  name := "node_selinux_enabled"
  help := "SELinux is enabled, 1 is true, 0 is false"
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_TCP6_inuse : Metric := {
  name := "node_sockstat_TCP6_inuse"
  help := "Number of TCP6 sockets in state inuse."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Tcp_RetransSegs : Metric := {
  name := "node_netstat_Tcp_RetransSegs"
  help := "Statistic TcpRetransSegs."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_os_info : Metric := {
  name := "node_os_info"
  help := "A metric with a constant '1' value labeled by build_id, id, id_like, image_id, image_version, name, pretty_name, variant, variant_id, version, version_codename, version_id."
  type := MetricType.gauge
  labels := ["build_id", "id", "id_like", "image_id", "image_version", "name", "pretty_name", "variant", "variant_id", "version", "version_codename", "version_id"]
  unit := MetricUnit.unitless
}

def go_memstats_alloc_bytes_total : Metric := {
  name := "go_memstats_alloc_bytes_total"
  help := "Total number of bytes allocated, even if freed."
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_filesystem_avail_bytes : Metric := {
  name := "node_filesystem_avail_bytes"
  help := "Filesystem space available to non-root users in bytes."
  type := MetricType.gauge
  labels := ["device", "fstype", "mountpoint"]
  unit := MetricUnit.unitless
}

def node_hwmon_temp_max_celsius : Metric := {
  name := "node_hwmon_temp_max_celsius"
  help := "Hardware monitor for temperature (max)"
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_memory_Unevictable_bytes : Metric := {
  name := "node_memory_Unevictable_bytes"
  help := "Memory information field Unevictable_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Writeback_bytes : Metric := {
  name := "node_memory_Writeback_bytes"
  help := "Memory information field Writeback_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_info : Metric := {
  name := "node_disk_info"
  help := "Info of /sys/block/<block_device>."
  type := MetricType.gauge
  labels := ["device", "major", "minor", "model", "path", "revision", "serial", "wwn"]
  unit := MetricUnit.unitless
}

def node_filesystem_device_error : Metric := {
  name := "node_filesystem_device_error"
  help := "Whether an error occurred while getting statistics for the given device."
  type := MetricType.gauge
  labels := ["device", "fstype", "mountpoint"]
  unit := MetricUnit.unitless
}

def node_netstat_Ip_Forwarding : Metric := {
  name := "node_netstat_Ip_Forwarding"
  help := "Statistic IpForwarding."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_IpExt_OutOctets : Metric := {
  name := "node_netstat_IpExt_OutOctets"
  help := "Statistic IpExtOutOctets."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_network_transmit_drop_total : Metric := {
  name := "node_network_transmit_drop_total"
  help := "Network device statistic transmit_drop."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_nf_conntrack_stat_found : Metric := {
  name := "node_nf_conntrack_stat_found"
  help := "Number of searched entries which were successful."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_RAW_inuse : Metric := {
  name := "node_sockstat_RAW_inuse"
  help := "Number of RAW sockets in state inuse."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def process_virtual_memory_bytes : Metric := {
  name := "process_virtual_memory_bytes"
  help := "Virtual memory size in bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_discarded_sectors_total : Metric := {
  name := "node_disk_discarded_sectors_total"
  help := "The total number of sectors discarded successfully."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_filesystem_readonly : Metric := {
  name := "node_filesystem_readonly"
  help := "Filesystem read-only status."
  type := MetricType.gauge
  labels := ["device", "fstype", "mountpoint"]
  unit := MetricUnit.unitless
}

def node_netstat_TcpExt_SyncookiesSent : Metric := {
  name := "node_netstat_TcpExt_SyncookiesSent"
  help := "Statistic TcpExtSyncookiesSent."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_network_receive_nohandler_total : Metric := {
  name := "node_network_receive_nohandler_total"
  help := "Network device statistic receive_nohandler."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_vmstat_pgfault : Metric := {
  name := "node_vmstat_pgfault"
  help := "/proc/vmstat information field pgfault."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_flush_requests_time_seconds_total : Metric := {
  name := "node_disk_flush_requests_time_seconds_total"
  help := "This is the total number of seconds spent by all flush requests."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_memory_HugePages_Free : Metric := {
  name := "node_memory_HugePages_Free"
  help := "Memory information field HugePages_Free."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_VmallocChunk_bytes : Metric := {
  name := "node_memory_VmallocChunk_bytes"
  help := "Memory information field VmallocChunk_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_network_carrier_changes_total : Metric := {
  name := "node_network_carrier_changes_total"
  help := "Network device property: carrier_changes_total"
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def process_resident_memory_bytes : Metric := {
  name := "process_resident_memory_bytes"
  help := "Resident memory size in bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_mcache_sys_bytes : Metric := {
  name := "go_memstats_mcache_sys_bytes"
  help := "Number of bytes used for mcache structures obtained from system."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_RAW6_inuse : Metric := {
  name := "node_sockstat_RAW6_inuse"
  help := "Number of RAW6 sockets in state inuse."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Dirty_bytes : Metric := {
  name := "node_memory_Dirty_bytes"
  help := "Memory information field Dirty_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_SwapCached_bytes : Metric := {
  name := "node_memory_SwapCached_bytes"
  help := "Memory information field SwapCached_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_power_supply_current_max : Metric := {
  name := "node_power_supply_current_max"
  help := "current_max value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def go_memstats_last_gc_time_seconds : Metric := {
  name := "go_memstats_last_gc_time_seconds"
  help := "Number of seconds since 1970 of last garbage collection."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Mapped_bytes : Metric := {
  name := "node_memory_Mapped_bytes"
  help := "Memory information field Mapped_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Zswapped_bytes : Metric := {
  name := "node_memory_Zswapped_bytes"
  help := "Memory information field Zswapped_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_pressure_io_stalled_seconds_total : Metric := {
  name := "node_pressure_io_stalled_seconds_total"
  help := "Total time in seconds no process could make progress due to IO congestion"
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_softnet_processed_total : Metric := {
  name := "node_softnet_processed_total"
  help := "Number of processed packets"
  type := MetricType.counter
  labels := ["cpu"]
  unit := MetricUnit.unitless
}

def go_memstats_stack_sys_bytes : Metric := {
  name := "go_memstats_stack_sys_bytes"
  help := "Number of bytes obtained from system for stack allocator."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_cooling_device_max_state : Metric := {
  name := "node_cooling_device_max_state"
  help := "Maximum throttle state of the cooling device"
  type := MetricType.gauge
  labels := ["name", "type"]
  unit := MetricUnit.unitless
}

def node_filesystem_free_bytes : Metric := {
  name := "node_filesystem_free_bytes"
  help := "Filesystem free space in bytes."
  type := MetricType.gauge
  labels := ["device", "fstype", "mountpoint"]
  unit := MetricUnit.unitless
}

def node_pressure_memory_stalled_seconds_total : Metric := {
  name := "node_pressure_memory_stalled_seconds_total"
  help := "Total time in seconds no process could make progress due to memory congestion"
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_schedstat_waiting_seconds_total : Metric := {
  name := "node_schedstat_waiting_seconds_total"
  help := "Number of seconds spent by processing waiting for this CPU."
  type := MetricType.counter
  labels := ["cpu"]
  unit := MetricUnit.unitless
}

def node_netstat_TcpExt_ListenOverflows : Metric := {
  name := "node_netstat_TcpExt_ListenOverflows"
  help := "Statistic TcpExtListenOverflows."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Udp_RcvbufErrors : Metric := {
  name := "node_netstat_Udp_RcvbufErrors"
  help := "Statistic UdpRcvbufErrors."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_network_mtu_bytes : Metric := {
  name := "node_network_mtu_bytes"
  help := "Network device property: mtu_bytes"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_exporter_build_info : Metric := {
  name := "node_exporter_build_info"
  help := "A metric with a constant '1' value labeled by version, revision, branch, and goversion from which node_exporter was built."
  type := MetricType.gauge
  labels := ["branch", "goversion", "revision", "version"]
  unit := MetricUnit.unitless
}

def node_filesystem_files : Metric := {
  name := "node_filesystem_files"
  help := "Filesystem total file nodes."
  type := MetricType.gauge
  labels := ["device", "fstype", "mountpoint"]
  unit := MetricUnit.unitless
}

def node_hwmon_temp_min_celsius : Metric := {
  name := "node_hwmon_temp_min_celsius"
  help := "Hardware monitor for temperature (min)"
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_memory_KReclaimable_bytes : Metric := {
  name := "node_memory_KReclaimable_bytes"
  help := "Memory information field KReclaimable_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_NFS_Unstable_bytes : Metric := {
  name := "node_memory_NFS_Unstable_bytes"
  help := "Memory information field NFS_Unstable_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def process_open_fds : Metric := {
  name := "process_open_fds"
  help := "Number of open file descriptors."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_dmi_info : Metric := {
  name := "node_dmi_info"
  help := "A metric with a constant '1' value labeled by bios_date, bios_release, bios_vendor, bios_version, board_asset_tag, board_name, board_serial, board_vendor, board_version, chassis_asset_tag, chassis_serial, chassis_vendor, chassis_version, product_family, product_name, product_serial, product_sku, product_uuid, product_version, system_vendor if provided by DMI."
  type := MetricType.gauge
  labels := ["bios_date", "bios_release", "bios_vendor", "bios_version", "board_asset_tag", "board_name", "board_vendor", "board_version", "chassis_asset_tag", "chassis_vendor", "chassis_version", "product_family", "product_name", "product_sku", "product_version", "system_vendor"]
  unit := MetricUnit.unitless
}

def node_netstat_Udp_InDatagrams : Metric := {
  name := "node_netstat_Udp_InDatagrams"
  help := "Statistic UdpInDatagrams."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_TCP_inuse : Metric := {
  name := "node_sockstat_TCP_inuse"
  help := "Number of TCP sockets in state inuse."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Inactive_bytes : Metric := {
  name := "node_memory_Inactive_bytes"
  help := "Memory information field Inactive_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_nf_conntrack_stat_ignore : Metric := {
  name := "node_nf_conntrack_stat_ignore"
  help := "Number of packets seen which are already connected to a conntrack entry."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_FRAG6_memory : Metric := {
  name := "node_sockstat_FRAG6_memory"
  help := "Number of FRAG6 sockets in state memory."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_TCP_alloc : Metric := {
  name := "node_sockstat_TCP_alloc"
  help := "Number of TCP sockets in state alloc."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_timex_pps_jitter_seconds : Metric := {
  name := "node_timex_pps_jitter_seconds"
  help := "Pulse per second jitter."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Udp6_InDatagrams : Metric := {
  name := "node_netstat_Udp6_InDatagrams"
  help := "Statistic Udp6InDatagrams."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_network_receive_bytes_total : Metric := {
  name := "node_network_receive_bytes_total"
  help := "Network device statistic receive_bytes."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_power_supply_voltage_volt : Metric := {
  name := "node_power_supply_voltage_volt"
  help := "voltage_volt value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def node_timex_estimated_error_seconds : Metric := {
  name := "node_timex_estimated_error_seconds"
  help := "Estimated error in seconds."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_next_gc_bytes : Metric := {
  name := "go_memstats_next_gc_bytes"
  help := "Number of heap bytes when next garbage collection will take place."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_flush_requests_total : Metric := {
  name := "node_disk_flush_requests_total"
  help := "The total number of flush requests completed successfully"
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_intr_total : Metric := {
  name := "node_intr_total"
  help := "Total number of interrupts serviced."
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_HugePages_Surp : Metric := {
  name := "node_memory_HugePages_Surp"
  help := "Memory information field HugePages_Surp."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_TCP_orphan : Metric := {
  name := "node_sockstat_TCP_orphan"
  help := "Number of TCP sockets in state orphan."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_cpu_guest_seconds_total : Metric := {
  name := "node_cpu_guest_seconds_total"
  help := "Seconds the CPUs spent in guests (VMs) for each mode."
  type := MetricType.counter
  labels := ["cpu", "mode"]
  unit := MetricUnit.unitless
}

def node_netstat_Ip6_InOctets : Metric := {
  name := "node_netstat_Ip6_InOctets"
  help := "Statistic Ip6InOctets."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_network_receive_frame_total : Metric := {
  name := "node_network_receive_frame_total"
  help := "Network device statistic receive_frame."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_nf_conntrack_entries : Metric := {
  name := "node_nf_conntrack_entries"
  help := "Number of currently allocated flow entries for connection tracking."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_pressure_io_waiting_seconds_total : Metric := {
  name := "node_pressure_io_waiting_seconds_total"
  help := "Total time in seconds that processes have waited due to IO congestion"
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_filesystem_files_free : Metric := {
  name := "node_filesystem_files_free"
  help := "Filesystem total free file nodes."
  type := MetricType.gauge
  labels := ["device", "fstype", "mountpoint"]
  unit := MetricUnit.unitless
}

def node_memory_Buffers_bytes : Metric := {
  name := "node_memory_Buffers_bytes"
  help := "Memory information field Buffers_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_CmaFree_bytes : Metric := {
  name := "node_memory_CmaFree_bytes"
  help := "Memory information field CmaFree_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Icmp_OutMsgs : Metric := {
  name := "node_netstat_Icmp_OutMsgs"
  help := "Statistic IcmpOutMsgs."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Udp_NoPorts : Metric := {
  name := "node_netstat_Udp_NoPorts"
  help := "Statistic UdpNoPorts."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_softnet_times_squeezed_total : Metric := {
  name := "node_softnet_times_squeezed_total"
  help := "Number of times processing packets ran out of quota"
  type := MetricType.counter
  labels := ["cpu"]
  unit := MetricUnit.unitless
}

def node_textfile_scrape_error : Metric := {
  name := "node_textfile_scrape_error"
  help := "1 if there was an error opening or reading a file, 0 otherwise"
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_reads_merged_total : Metric := {
  name := "node_disk_reads_merged_total"
  help := "The total number of reads merged."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_hwmon_curr_max_amps : Metric := {
  name := "node_hwmon_curr_max_amps"
  help := "Hardware monitor for current (max)"
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_netstat_Udp6_OutDatagrams : Metric := {
  name := "node_netstat_Udp6_OutDatagrams"
  help := "Statistic Udp6OutDatagrams."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_network_info : Metric := {
  name := "node_network_info"
  help := "Non-numeric data from /sys/class/net/<iface>, value is always 1."
  type := MetricType.gauge
  labels := ["address", "broadcast", "device", "duplex", "ifalias", "operstate"]
  unit := MetricUnit.unitless
}

def node_power_supply_voltage_min : Metric := {
  name := "node_power_supply_voltage_min"
  help := "voltage_min value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def node_disk_reads_completed_total : Metric := {
  name := "node_disk_reads_completed_total"
  help := "The total number of reads completed successfully."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_filesystem_size_bytes : Metric := {
  name := "node_filesystem_size_bytes"
  help := "Filesystem size in bytes."
  type := MetricType.gauge
  labels := ["device", "fstype", "mountpoint"]
  unit := MetricUnit.unitless
}

def node_netstat_TcpExt_TCPTimeouts : Metric := {
  name := "node_netstat_TcpExt_TCPTimeouts"
  help := "Statistic TcpExtTCPTimeouts."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_power_supply_energy_full_design : Metric := {
  name := "node_power_supply_energy_full_design"
  help := "energy_full_design value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def node_timex_pps_stability_exceeded_total : Metric := {
  name := "node_timex_pps_stability_exceeded_total"
  help := "Pulse per second count of stability limit exceeded events."
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_filefd_allocated : Metric := {
  name := "node_filefd_allocated"
  help := "File descriptor statistics: allocated."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Hugepagesize_bytes : Metric := {
  name := "node_memory_Hugepagesize_bytes"
  help := "Memory information field Hugepagesize_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_TcpExt_SyncookiesRecv : Metric := {
  name := "node_netstat_TcpExt_SyncookiesRecv"
  help := "Statistic TcpExtSyncookiesRecv."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Tcp_OutSegs : Metric := {
  name := "node_netstat_Tcp_OutSegs"
  help := "Statistic TcpOutSegs."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Udp6_SndbufErrors : Metric := {
  name := "node_netstat_Udp6_SndbufErrors"
  help := "Statistic Udp6SndbufErrors."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_frees_total : Metric := {
  name := "go_memstats_frees_total"
  help := "Total number of frees."
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_pressure_memory_waiting_seconds_total : Metric := {
  name := "node_pressure_memory_waiting_seconds_total"
  help := "Total time in seconds that processes have waited for memory"
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_FRAG_memory : Metric := {
  name := "node_sockstat_FRAG_memory"
  help := "Number of FRAG sockets in state memory."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_UDPLITE_inuse : Metric := {
  name := "node_sockstat_UDPLITE_inuse"
  help := "Number of UDPLITE sockets in state inuse."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_thermal_zone_temp : Metric := {
  name := "node_thermal_zone_temp"
  help := "Zone temperature in Celsius"
  type := MetricType.gauge
  labels := ["type", "zone"]
  unit := MetricUnit.unitless
}

def node_memory_AnonHugePages_bytes : Metric := {
  name := "node_memory_AnonHugePages_bytes"
  help := "Memory information field AnonHugePages_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_CmaTotal_bytes : Metric := {
  name := "node_memory_CmaTotal_bytes"
  help := "Memory information field CmaTotal_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_VmallocUsed_bytes : Metric := {
  name := "node_memory_VmallocUsed_bytes"
  help := "Memory information field VmallocUsed_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_network_address_assign_type : Metric := {
  name := "node_network_address_assign_type"
  help := "Network device property: address_assign_type"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_power_supply_voltage_max : Metric := {
  name := "node_power_supply_voltage_max"
  help := "voltage_max value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def node_power_supply_current_ampere : Metric := {
  name := "node_power_supply_current_ampere"
  help := "current_ampere value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def node_sockstat_FRAG_inuse : Metric := {
  name := "node_sockstat_FRAG_inuse"
  help := "Number of FRAG sockets in state inuse."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_timex_status : Metric := {
  name := "node_timex_status"
  help := "Value of the status array bits."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_lookups_total : Metric := {
  name := "go_memstats_lookups_total"
  help := "Total number of pointer lookups."
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Icmp6_InMsgs : Metric := {
  name := "node_netstat_Icmp6_InMsgs"
  help := "Statistic Icmp6InMsgs."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Udp6_NoPorts : Metric := {
  name := "node_netstat_Udp6_NoPorts"
  help := "Statistic Udp6NoPorts."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_network_carrier_down_changes_total : Metric := {
  name := "node_network_carrier_down_changes_total"
  help := "Network device property: carrier_down_changes_total"
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_network_transmit_carrier_total : Metric := {
  name := "node_network_transmit_carrier_total"
  help := "Network device statistic transmit_carrier."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_timex_tai_offset_seconds : Metric := {
  name := "node_timex_tai_offset_seconds"
  help := "International Atomic Time (TAI) offset."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_goroutines : Metric := {
  name := "go_goroutines"
  help := "Number of goroutines that currently exist."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_buck_hash_sys_bytes : Metric := {
  name := "go_memstats_buck_hash_sys_bytes"
  help := "Number of bytes used by the profiling bucket hash table."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_mspan_sys_bytes : Metric := {
  name := "go_memstats_mspan_sys_bytes"
  help := "Number of bytes used for mspan structures obtained from system."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_WritebackTmp_bytes : Metric := {
  name := "node_memory_WritebackTmp_bytes"
  help := "Memory information field WritebackTmp_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Icmp6_OutMsgs : Metric := {
  name := "node_netstat_Icmp6_OutMsgs"
  help := "Statistic Icmp6OutMsgs."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_hwmon_sensor_label : Metric := {
  name := "node_hwmon_sensor_label"
  help := "Label for given chip and sensor"
  type := MetricType.gauge
  labels := ["chip", "label", "sensor"]
  unit := MetricUnit.unitless
}

def node_memory_Active_file_bytes : Metric := {
  name := "node_memory_Active_file_bytes"
  help := "Memory information field Active_file_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Bounce_bytes : Metric := {
  name := "node_memory_Bounce_bytes"
  help := "Memory information field Bounce_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_hwmon_temp_celsius : Metric := {
  name := "node_hwmon_temp_celsius"
  help := "Hardware monitor for temperature (input)"
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_memory_MemAvailable_bytes : Metric := {
  name := "node_memory_MemAvailable_bytes"
  help := "Memory information field MemAvailable_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_SwapTotal_bytes : Metric := {
  name := "node_memory_SwapTotal_bytes"
  help := "Memory information field SwapTotal_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_timex_pps_calibration_total : Metric := {
  name := "node_timex_pps_calibration_total"
  help := "Pulse per second count of calibration intervals."
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_vmstat_pgmajfault : Metric := {
  name := "node_vmstat_pgmajfault"
  help := "/proc/vmstat information field pgmajfault."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Udp_OutDatagrams : Metric := {
  name := "node_netstat_Udp_OutDatagrams"
  help := "Statistic UdpOutDatagrams."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_network_transmit_queue_length : Metric := {
  name := "node_network_transmit_queue_length"
  help := "Network device property: transmit_queue_length"
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_sockstat_TCP_tw : Metric := {
  name := "node_sockstat_TCP_tw"
  help := "Number of TCP sockets in state tw."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_device_mapper_info : Metric := {
  name := "node_disk_device_mapper_info"
  help := "Info about disk device mapper."
  type := MetricType.gauge
  labels := ["device", "lv_layer", "lv_name", "name", "uuid", "vg_name"]
  unit := MetricUnit.unitless
}

def node_entropy_pool_size_bits : Metric := {
  name := "node_entropy_pool_size_bits"
  help := "Bits of entropy pool."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Active_anon_bytes : Metric := {
  name := "node_memory_Active_anon_bytes"
  help := "Memory information field Active_anon_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_MemFree_bytes : Metric := {
  name := "node_memory_MemFree_bytes"
  help := "Memory information field MemFree_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Tcp_InSegs : Metric := {
  name := "node_netstat_Tcp_InSegs"
  help := "Statistic TcpInSegs."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_sockstat_UDP_mem : Metric := {
  name := "node_sockstat_UDP_mem"
  help := "Number of UDP sockets in state mem."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_IpExt_InOctets : Metric := {
  name := "node_netstat_IpExt_InOctets"
  help := "Statistic IpExtInOctets."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_netstat_Udp_SndbufErrors : Metric := {
  name := "node_netstat_Udp_SndbufErrors"
  help := "Statistic UdpSndbufErrors."
  type := MetricType.untyped
  labels := []
  unit := MetricUnit.unitless
}

def node_network_receive_drop_total : Metric := {
  name := "node_network_receive_drop_total"
  help := "Network device statistic receive_drop."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def go_memstats_heap_sys_bytes : Metric := {
  name := "go_memstats_heap_sys_bytes"
  help := "Number of heap bytes obtained from system."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_cpu_seconds_total : Metric := {
  name := "node_cpu_seconds_total"
  help := "Seconds the CPUs spent in each mode."
  type := MetricType.counter
  labels := ["cpu", "mode"]
  unit := MetricUnit.unitless
}

def node_hwmon_temp_alarm : Metric := {
  name := "node_hwmon_temp_alarm"
  help := "Hardware sensor alarm status (temp)"
  type := MetricType.gauge
  labels := ["chip", "sensor"]
  unit := MetricUnit.unitless
}

def node_memory_HugePages_Total : Metric := {
  name := "node_memory_HugePages_Total"
  help := "Memory information field HugePages_Total."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_memory_Inactive_anon_bytes : Metric := {
  name := "node_memory_Inactive_anon_bytes"
  help := "Memory information field Inactive_anon_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_network_transmit_colls_total : Metric := {
  name := "node_network_transmit_colls_total"
  help := "Network device statistic transmit_colls."
  type := MetricType.counter
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_nf_conntrack_stat_early_drop : Metric := {
  name := "node_nf_conntrack_stat_early_drop"
  help := "Number of dropped conntrack entries to make room for new ones, if maximum table size was reached."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_power_supply_capacity : Metric := {
  name := "node_power_supply_capacity"
  help := "capacity value of /sys/class/power_supply/<power_supply>."
  type := MetricType.gauge
  labels := ["power_supply"]
  unit := MetricUnit.unitless
}

def node_timex_pps_stability_hertz : Metric := {
  name := "node_timex_pps_stability_hertz"
  help := "Pulse per second stability, average of recent frequency changes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_gc_sys_bytes : Metric := {
  name := "go_memstats_gc_sys_bytes"
  help := "Number of bytes used for garbage collection system metadata."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def node_cpu_core_throttles_total : Metric := {
  name := "node_cpu_core_throttles_total"
  help := "Number of times this CPU core has been throttled."
  type := MetricType.counter
  labels := ["core", "package"]
  unit := MetricUnit.unitless
}

def node_memory_ShmemPmdMapped_bytes : Metric := {
  name := "node_memory_ShmemPmdMapped_bytes"
  help := "Memory information field ShmemPmdMapped_bytes."
  type := MetricType.gauge
  labels := []
  unit := MetricUnit.unitless
}

def go_memstats_mallocs_total : Metric := {
  name := "go_memstats_mallocs_total"
  help := "Total number of mallocs."
  type := MetricType.counter
  labels := []
  unit := MetricUnit.unitless
}

def node_disk_ata_rotation_rate_rpm : Metric := {
  name := "node_disk_ata_rotation_rate_rpm"
  help := "ATA disk rotation rate in RPMs (0 for SSDs)."
  type := MetricType.gauge
  labels := ["device"]
  unit := MetricUnit.unitless
}

def node_time_clocksource_current_info : Metric := {
  name := "node_time_clocksource_current_info"
  help := "Current clocksource read from '/sys/devices/system/clocksource'."
  type := MetricType.gauge
  labels := ["clocksource", "device"]
  unit := MetricUnit.unitless
}
def NodeExporter : Exporter := {
  metrics := [go_memstats_heap_objects, node_entropy_available_bits, node_filefd_maximum, node_power_supply_energy_full, node_timex_pps_jitter_total, go_info, node_hwmon_pwm, node_memory_Committed_AS_bytes, node_network_flags, node_hwmon_pwm_enable, node_memory_HugePages_Rsvd, node_memory_Cached_bytes, node_memory_FileHugePages_bytes, node_timex_sync_status, node_sockstat_TCP_mem_bytes, node_cooling_device_cur_state, node_disk_filesystem_info, node_netstat_TcpExt_SyncookiesFailed, node_network_carrier_up_changes_total, node_schedstat_running_seconds_total, node_cpu_package_throttles_total, node_memory_VmallocTotal_bytes, node_netstat_Tcp_ActiveOpens, node_sockstat_UDP_inuse, go_gc_duration_seconds, node_disk_discards_merged_total, node_hwmon_in_volts, node_network_iface_link_mode, node_udp_queues, node_context_switches_total, node_network_name_assign_type, node_network_receive_packets_total, node_disk_read_time_seconds_total, node_memory_Shmem_bytes, node_memory_Zswap_bytes, node_network_speed_bytes, node_timex_tick_seconds, go_memstats_heap_released_bytes, node_boot_time_seconds, node_network_iface_link, node_pressure_cpu_waiting_seconds_total, node_sockstat_UDP_mem_bytes, node_cpu_scaling_frequency_hertz, node_cpu_scaling_frequency_min_hertz, node_netstat_Udp6_InErrors, node_network_transmit_errs_total, node_memory_DirectMap1G_bytes, node_memory_Slab_bytes, node_network_net_dev_group, node_network_receive_errs_total, node_nf_conntrack_stat_search_restart, node_procs_blocked, node_sockstat_UDPLITE6_inuse, go_memstats_other_sys_bytes, node_disk_io_now, node_memory_DirectMap4k_bytes, node_memory_Mlocked_bytes, node_netstat_Tcp_PassiveOpens, node_vmstat_pgpgin, node_cpu_frequency_min_hertz, node_sockstat_TCP_mem, node_softnet_dropped_total, node_timex_pps_error_total, node_memory_FilePmdMapped_bytes, node_netstat_Udp_InErrors, node_uname_info, promhttp_metric_handler_requests_total, node_hwmon_chip_names, node_netstat_Tcp_InErrs, node_nf_conntrack_stat_drop, node_time_seconds, node_cpu_frequency_max_hertz, node_hwmon_fan_rpm, node_hwmon_in_max_volts, node_load15, node_netstat_TcpExt_TCPSynRetrans, node_disk_ata_write_cache_enabled, node_load1, node_netstat_Icmp_InErrors, process_max_fds, node_memory_SecPageTables_bytes, node_network_carrier, node_timex_loop_time_constant, process_cpu_seconds_total, promhttp_metric_handler_requests_in_flight, node_memory_SReclaimable_bytes, node_memory_ShmemHugePages_bytes, node_netstat_Icmp6_InErrors, go_threads, node_disk_writes_merged_total, node_hwmon_curr_amps, node_hwmon_temp_crit_celsius, node_memory_PageTables_bytes, node_netstat_Tcp_CurrEstab, node_timex_offset_seconds, node_vmstat_pswpout, go_memstats_heap_alloc_bytes, node_memory_Percpu_bytes, node_netstat_UdpLite6_InErrors, node_power_supply_voltage_min_design, node_scrape_collector_success, node_power_supply_energy_watthour, go_memstats_heap_idle_bytes, node_disk_read_bytes_total, node_load5, node_network_iface_id, node_nf_conntrack_stat_insert_failed, node_nvme_info, node_vmstat_oom_kill, node_hwmon_in_min_volts, node_hwmon_temp_crit_alarm_celsius, node_network_device_id, node_network_dormant, node_network_transmit_packets_total, go_memstats_alloc_bytes, node_arp_entries, node_disk_written_bytes_total, node_netstat_Udp6_RcvbufErrors, node_time_zone_offset_seconds, node_timex_maxerror_seconds, go_memstats_stack_inuse_bytes, node_memory_Active_bytes, node_network_receive_multicast_total, node_network_transmit_fifo_total, node_sockstat_UDP6_inuse, node_disk_discard_time_seconds_total, node_network_up, node_power_supply_present, node_sockstat_sockets_used, go_memstats_sys_bytes, node_disk_writes_completed_total, node_memory_KernelStack_bytes, node_netstat_UdpLite_InErrors, node_network_receive_compressed_total, node_forks_total, node_memory_AnonPages_bytes, node_netstat_Ip6_OutOctets, node_memory_SUnreclaim_bytes, node_memory_SwapFree_bytes, node_netstat_Icmp_InMsgs, go_memstats_heap_inuse_bytes, go_memstats_mcache_inuse_bytes, node_cpu_scaling_frequency_max_hertz, node_disk_write_time_seconds_total, node_memory_HardwareCorrupted_bytes, node_nf_conntrack_stat_invalid, node_power_supply_power_watt, node_schedstat_timeslices_total, node_network_protocol_type, node_network_receive_fifo_total, node_network_transmit_bytes_total, node_network_transmit_compressed_total, node_nf_conntrack_stat_insert, node_sockstat_FRAG6_inuse, node_time_clocksource_available_info, process_virtual_memory_max_bytes, node_timex_pps_frequency_hertz, node_timex_pps_shift_seconds, go_memstats_mspan_inuse_bytes, node_memory_Inactive_file_bytes, node_netstat_Tcp_OutRsts, node_power_supply_cyclecount, node_procs_running, node_disk_ata_write_cache, node_memory_Hugetlb_bytes, node_netstat_TcpExt_ListenDrops, node_power_supply_online, node_timex_frequency_adjustment_ratio, promhttp_metric_handler_errors_total, node_memory_CommitLimit_bytes, node_memory_MemTotal_bytes, node_nf_conntrack_entries_limit, node_vmstat_pgpgout, node_vmstat_pswpin, node_memory_DirectMap2M_bytes, node_power_supply_info, node_scrape_collector_duration_seconds, process_start_time_seconds, node_disk_discards_completed_total, node_disk_io_time_seconds_total, node_disk_io_time_weighted_seconds_total, node_selinux_enabled, node_sockstat_TCP6_inuse, node_netstat_Tcp_RetransSegs, node_os_info, go_memstats_alloc_bytes_total, node_filesystem_avail_bytes, node_hwmon_temp_max_celsius, node_memory_Unevictable_bytes, node_memory_Writeback_bytes, node_disk_info, node_filesystem_device_error, node_netstat_Ip_Forwarding, node_netstat_IpExt_OutOctets, node_network_transmit_drop_total, node_nf_conntrack_stat_found, node_sockstat_RAW_inuse, process_virtual_memory_bytes, node_disk_discarded_sectors_total, node_filesystem_readonly, node_netstat_TcpExt_SyncookiesSent, node_network_receive_nohandler_total, node_vmstat_pgfault, node_disk_flush_requests_time_seconds_total, node_memory_HugePages_Free, node_memory_VmallocChunk_bytes, node_network_carrier_changes_total, process_resident_memory_bytes, go_memstats_mcache_sys_bytes, node_sockstat_RAW6_inuse, node_memory_Dirty_bytes, node_memory_SwapCached_bytes, node_power_supply_current_max, go_memstats_last_gc_time_seconds, node_memory_Mapped_bytes, node_memory_Zswapped_bytes, node_pressure_io_stalled_seconds_total, node_softnet_processed_total, go_memstats_stack_sys_bytes, node_cooling_device_max_state, node_filesystem_free_bytes, node_pressure_memory_stalled_seconds_total, node_schedstat_waiting_seconds_total, node_netstat_TcpExt_ListenOverflows, node_netstat_Udp_RcvbufErrors, node_network_mtu_bytes, node_exporter_build_info, node_filesystem_files, node_hwmon_temp_min_celsius, node_memory_KReclaimable_bytes, node_memory_NFS_Unstable_bytes, process_open_fds, node_dmi_info, node_netstat_Udp_InDatagrams, node_sockstat_TCP_inuse, node_memory_Inactive_bytes, node_nf_conntrack_stat_ignore, node_sockstat_FRAG6_memory, node_sockstat_TCP_alloc, node_timex_pps_jitter_seconds, node_netstat_Udp6_InDatagrams, node_network_receive_bytes_total, node_power_supply_voltage_volt, node_timex_estimated_error_seconds, go_memstats_next_gc_bytes, node_disk_flush_requests_total, node_intr_total, node_memory_HugePages_Surp, node_sockstat_TCP_orphan, node_cpu_guest_seconds_total, node_netstat_Ip6_InOctets, node_network_receive_frame_total, node_nf_conntrack_entries, node_pressure_io_waiting_seconds_total, node_filesystem_files_free, node_memory_Buffers_bytes, node_memory_CmaFree_bytes, node_netstat_Icmp_OutMsgs, node_netstat_Udp_NoPorts, node_softnet_times_squeezed_total, node_textfile_scrape_error, node_disk_reads_merged_total, node_hwmon_curr_max_amps, node_netstat_Udp6_OutDatagrams, node_network_info, node_power_supply_voltage_min, node_disk_reads_completed_total, node_filesystem_size_bytes, node_netstat_TcpExt_TCPTimeouts, node_power_supply_energy_full_design, node_timex_pps_stability_exceeded_total, node_filefd_allocated, node_memory_Hugepagesize_bytes, node_netstat_TcpExt_SyncookiesRecv, node_netstat_Tcp_OutSegs, node_netstat_Udp6_SndbufErrors, go_memstats_frees_total, node_pressure_memory_waiting_seconds_total, node_sockstat_FRAG_memory, node_sockstat_UDPLITE_inuse, node_thermal_zone_temp, node_memory_AnonHugePages_bytes, node_memory_CmaTotal_bytes, node_memory_VmallocUsed_bytes, node_network_address_assign_type, node_power_supply_voltage_max, node_power_supply_current_ampere, node_sockstat_FRAG_inuse, node_timex_status, go_memstats_lookups_total, node_netstat_Icmp6_InMsgs, node_netstat_Udp6_NoPorts, node_network_carrier_down_changes_total, node_network_transmit_carrier_total, node_timex_tai_offset_seconds, go_goroutines, go_memstats_buck_hash_sys_bytes, go_memstats_mspan_sys_bytes, node_memory_WritebackTmp_bytes, node_netstat_Icmp6_OutMsgs, node_hwmon_sensor_label, node_memory_Active_file_bytes, node_memory_Bounce_bytes, node_hwmon_temp_celsius, node_memory_MemAvailable_bytes, node_memory_SwapTotal_bytes, node_timex_pps_calibration_total, node_vmstat_pgmajfault, node_netstat_Udp_OutDatagrams, node_network_transmit_queue_length, node_sockstat_TCP_tw, node_disk_device_mapper_info, node_entropy_pool_size_bits, node_memory_Active_anon_bytes, node_memory_MemFree_bytes, node_netstat_Tcp_InSegs, node_sockstat_UDP_mem, node_netstat_IpExt_InOctets, node_netstat_Udp_SndbufErrors, node_network_receive_drop_total, go_memstats_heap_sys_bytes, node_cpu_seconds_total, node_hwmon_temp_alarm, node_memory_HugePages_Total, node_memory_Inactive_anon_bytes, node_network_transmit_colls_total, node_nf_conntrack_stat_early_drop, node_power_supply_capacity, node_timex_pps_stability_hertz, go_memstats_gc_sys_bytes, node_cpu_core_throttles_total, node_memory_ShmemPmdMapped_bytes, go_memstats_mallocs_total, node_disk_ata_rotation_rate_rpm, node_time_clocksource_current_info]
}

end AwesomeDashboards.Models.NodeExporter
