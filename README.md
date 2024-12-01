# Awesome dashboards 

**The state of this project is prototype. Most features have not been fully implemented.**

Features:
- **Produces Grafana dashboards:** Maintaining a dashboard through the UI can be repetitive and error-prone. 
- **Compiler checked PromQL:** PromQL queries are checked during compilation. Many aspects of a dashboard panel can be automatically set based on the query.
- **Models for exporters and targets:** A model (e.g. for node-exporter) contains all metrics, incl. types, units, help strings, and possible label names. Queries can be checked against this model.
- The units of panels can be automatically set based on the query. E.g. if a metric is in bytes and you wrap it into `rate()` the unit of the panel will be bytes/second.
- Easily create a topk queries over time laveraging Grafana variables.
- **Extensible:** awesome-dashboards is written in Lean 4. Also dashboards are written directly in Lean. You have the full power of a dependently-typed language at your fingertips.
- **Opt out:** If you no longer want to use this library, you can simply maintain your dashboard in Grafana.
- **Dashboard preview in the IDE:** Lean 4 supports widgets in the IDE. This can be used to preview the dashboard.

## Development

Requirements:
- [elan](https://github.com/leanprover/elan)

Start a local Grafana instance with a configured dashboard provider. See `./hack/run-grafana.sh` for an example.

Generate the dashboard.

```
lake build && ./build/bin/dashboard > grafana/dashboards/dashboard.json
```

## References

- https://github.com/grafana/dashboard-spec/tree/master/specs/7.0
- https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md
- [PromQL syntax in yaac format](https://github.com/prometheus/prometheus/blob/2aacd807b3ec6ddd90ae55f3a42f4cffed561ea9/promql/generated_parser.y)
    - We could [extract BNF from yaac](https://github.com/prometheus/prometheus/blob/2aacd807b3ec6ddd90ae55f3a42f4cffed561ea9/promql/generated_parser.y), generate a parser from PromQL syntax in Lean and prove that we generate only syntactically valid PromQL queries.
