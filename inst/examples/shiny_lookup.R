library(shiny)
library(rhandsontable)
library(metricsgraphics)

ui = shinyUI(fluidPage(

  titlePanel("Lookup"),
  # actionButton("saveBtn", "Save"),
  rHandsontableOutput("hot")

))

server = function(input, output) {
  fname = tempfile()

#   # relies on the actionButton to save
#   values = list()
#   setHot = function(x) values[["hot"]] <<- x

  # saves after each update
  values = reactiveValues()
  setHot = function(x) values[["hot"]] = x

  observe({
    input$saveBtn

    if (!is.null(values[["hot"]])) {
      write.csv(values[["hot"]], fname)
      print(fname)
    }
  })

  output$hot = renderRHandsontable({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
      setHot(DF)
      rhandsontable(DF) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    } else {
      DF = read.csv("inst/examples/mtcars.csv", stringsAsFactors = FALSE)
      setHot(DF)
      rhandsontable(DF) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })
}

shinyApp(ui = ui, server = server)
