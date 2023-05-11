# Awsome dashboards (**Prototype**)

Start a local Grafana instance with a configured dashboard provider. See `run-grafana.sh` for an example.

Generate the dashboard.

```
lake build && ./build/bin/dashboard > grafana/dashboards/dashboard.json
```

## References

- https://github.com/grafana/dashboard-spec/tree/master/specs/7.0
- https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md
- [PromQL syntax in yaac format](https://github.com/prometheus/prometheus/blob/2aacd807b3ec6ddd90ae55f3a42f4cffed561ea9/promql/generated_parser.y)
    - We could [extract BNF from yaac](https://github.com/prometheus/prometheus/blob/2aacd807b3ec6ddd90ae55f3a42f4cffed561ea9/promql/generated_parser.y), generate a parser from PromQL syntax in Lean and prove that we generate only syntactically valid PromQL queries.
