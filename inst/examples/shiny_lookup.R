library(shiny)
library(rhandsontable)
library(metricsgraphics)

ui = shinyUI(fluidPage(

  titlePanel("Lookup"),
  actionButton("save", "Save"),
  rHandsontableOutput("hot")

))

server = function(input, output) {
  values = reactiveValues()

  load = reactive({
    out = read.csv("inst/examples/mtcars.csv")

    values[["hot"]] = out

    out
  })

  save = reactive({
    input$save

    write.csv(values[["hot"]], "inst/examples/mtcars.csv")
  })

  output$hot = renderRHandsontable({
    if (!is.null(load()) && !is.null(values[["hot"]]))
      rhandsontable(values[["hot"]]) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
}

shinyApp(ui = ui, server = server)
