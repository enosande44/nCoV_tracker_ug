library(shiny)
library(ggplot2)
library(rgdal)
library(rgeos)

function(input, output) {
  
  
  
  output$travel <- renderValueBox({
    valueBox(
      2661, "High risk travelers identified", color = "blue"
    )
  })
  output$follow <- renderValueBox({
    valueBox(
      980, "Individuals under follow-up", color = "green"
    )
  })
  output$tested <- renderValueBox({
    valueBox(
      2629, "Cumulative number of individuals tested"
    )
  })
  output$confirmed <- renderValueBox({
    valueBox(
      48, "Confirmed cases"
    )
  })
  output$ugMap <- renderPlot({
    plot(ug_map, col = as.character(ug_map@data$colors))
    legend("bottomright", cex = 0.7, legend = levels(ug_map$cut), fill = colors, 
           title = "Suspect Case No.")
  })
  output$trend <- renderPlotly(
    plot_ly(counts, x=~Var1, y=~Freq, type="bar")
  )
    

}
