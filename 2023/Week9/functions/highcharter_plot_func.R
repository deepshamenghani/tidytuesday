highcharter_plot_func <-
function(data) {
  data |> 
    hchart(type = "column", hcaes(x = language, y = perc, color = colorvalue))  |> 
    hc_plotOptions(column = list(stacking = "normal")) |> 
    hc_yAxis(max = 100, min = 0, labels = list(format = "{value}%"),
                showFirstLabel = FALSE, title = "") |> 
    hc_tooltip(
      useHTML = TRUE,                              
      formatter = JS(
        "
        function(){
          outHTML = '<b>' + this.point.language + '<br>' + this.point.countrylist + '<br>' + this.point.label + '<br>' + Math.round(this.y) + '%'
          return(outHTML)
        }
  
        "
      ),
      shape = "callout", 
      borderWidth = 0 
    ) |> 
    hc_xAxis(
      title = ""
    )
}
