# Awsome dashboards (**Prototype**)

Start a local Grafana instance with a configured dashboard provider. See `run-grafana.sh` for an example.

Generate the dashboard.

```
lake build && ./build/bin/dashboard > grafana/dashboards/dashboard.json
```

## References

- https://github.com/grafana/dashboard-spec/tree/master/specs/7.0
- https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md
