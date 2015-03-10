library(shiny)
library(rhandsontable)

ui = shinyUI(fluidPage(

  titlePanel("Handsontable"),

  sidebarLayout(
    sidebarPanel(
      helpText("Handsontable demo output. Column add/delete does work ",
               "for tables with defined column properties, including type."),
      radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
    ),
    mainPanel(
      rHandsontableOutput("hot")
    )
  )
))

server = function(input, output) {
  output$hot <- renderRHandsontable({
    if (is.null(input$hot)) {
      DF = data.frame(val = 1:10, bool = TRUE, nm = LETTERS[1:10],
                      dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                      stringsAsFactors = F)
    } else {
      DF = hot_to_r(input$hot)
    }
    rhandsontable(DF, useTypes = as.logical(input$useType))
  })
}

shinyApp(ui = ui, server = server)
