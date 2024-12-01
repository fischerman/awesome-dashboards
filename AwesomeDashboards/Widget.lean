import Lean
open Lean Widget

--@[widget]
def dashboardWidget : UserWidgetDefinition where
  name := "Hello"
  javascript := "
    import * as React from 'react';
    export default function(props) {
      return React.createElement('p', {}, `
Name: ${props.name}
Panels: ${props.panels.length}
      `)
    }"
