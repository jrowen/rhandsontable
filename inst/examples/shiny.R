library(shiny)
library(rHandsontable)

ui = shinyUI(fluidPage(

  titlePanel("Handsontable"),

  sidebarLayout(
    sidebarPanel(
      helpText("Handsontable demo output.")
    ),
    mainPanel(
      rHandsontableOutput("hot")
    )
  )
))

server = function(input, output) {
  output$hot <- renderRHandsontable({
    DF = data.frame(val = 1:10, bool = TRUE, nm = LETTERS[1:10],
                    dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                    stringsAsFactors = F)
    rHandsontable(DF)
  })
}

shinyApp(ui = ui, server = server)
