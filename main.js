fetch('https://prometheus.demo.do.prometheus.io/api/v1/query_range?query=scrape_duration_seconds&start=2021-12-21T08:10:30.781Z&end=2021-12-21T08:16:00.781Z&step=15s')
.then((res) => {
    return res.json()
}).then((body) => {
    console.log(body)
    const labels = body.data.result[0].values.map(x => x[0]);
      const data = {
        labels: labels,
        datasets: body.data.result.map(ts => {
            return {
                label: ts.metric.instance,
                backgroundColor: 'rgb(255, 99, 132)',
                borderColor: 'rgb(255, 99, 132)',
                data: ts.values.map(v => parseFloat(v[1])),
            }
        })
      };
      const config = {
        type: 'line',
        data: data,
        options: {}
      };
      const myChart = new Chart(
        document.getElementById('myChart'),
        config
      );
}, (err) => {
    console.error(err)
})