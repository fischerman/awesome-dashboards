package main

import (
	"fmt"
	"os"
	"text/template"

	dto "github.com/prometheus/client_model/go"
	"github.com/prometheus/common/expfmt"

	_ "embed"
)

//go:embed model.tmpl
var tmpl string

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type tmplContext struct {
	MetricFamilies []*dto.MetricFamily
	Namespace      string
}

func main() {
	if len(os.Args) != 3 {
		panic(fmt.Errorf("usage: <metrics file> <namespace>"))
	}

	t, err := template.New("model").Funcs(template.FuncMap{
		"type": func(t *dto.MetricType) string {
			switch *t {
			case dto.MetricType_COUNTER:
				return "counter"
			case dto.MetricType_GAUGE:
				return "gauge"
			case dto.MetricType_HISTOGRAM:
				return "histogram"
			case dto.MetricType_UNTYPED:
				return "untyped"
			}
			return "untyped"
		},
		"labels": func(metrics []*dto.Metric) []string {
			labels := []string{}
			for _, label := range metrics[0].GetLabel() {
				labels = append(labels, *label.Name)
			}
			return labels
		},
	}).Parse(tmpl)
	check(err)

	f, err := os.Open(os.Args[1])
	check(err)

	parser := &expfmt.TextParser{}
	families, err := parser.TextToMetricFamilies(f)
	check(err)

	ctx := tmplContext{
		MetricFamilies: []*dto.MetricFamily{},
		Namespace:      os.Args[2],
	}

	for _, v := range families {
		ctx.MetricFamilies = append(ctx.MetricFamilies, v)
	}

	err = t.Execute(os.Stdout, ctx)
	check(err)
}
