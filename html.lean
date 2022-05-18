def tableToHTML (g : TablePanel) : IO String := do
  let i ← (g.columns.mapM (λc => match c.data with
    | (ColumnValueSource.PrometheusLabelColumn v) => prometheusQueryInstant v
    | (ColumnValueSource.PrometheusValueColumn v) => prometheusQueryInstant v
  ))
  let rows ← pure $ (i.get 0 sorry).map λ cell => [cell]
  -- TODO: match rows of column
  return s!"
    <div>
      <h3>Table</h3>
      <table>
        <thead>
          <tr>
            {
              String.join $ g.columns.map $ λ c => "<th>" ++ c.name ++ "</th>"
            }
          </tr>
        </thead>
        <tbody>
          {
            String.join $ rows.map $ λ r => "<tr>" ++ (
              String.join $ r.map $ λ cell => "<td>" ++ cell.value.toString ++ "</td>"
            ) ++ "</tr>"
          }
        </tbody>
      </table>
    </div>
  "

-- TODO: generate some IR without IO
def dashboardToHTML (d : Dashboard) (start endd : Nat) : IO String := do
  let panels ← (d.panels.mapM (λp => match p with
    | (Panel.graph g) => graphToHTML g
    | (Panel.table t) => tableToHTML t
  ))
  return s!"
    <html>
      <head>
        <title>{d.name} - Awesome dashboard</title>
        <script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js\"></script>
        <script src=\"https://cdn.jsdelivr.net/npm/chart.js@3.6.2/dist/chart.js\"></script>
      </head>
      <body>
        <h1>{d.name}</h1>
        {String.join panels}
        <script src=\"./main.js\"></script>
      </body>
    </html>
  "
