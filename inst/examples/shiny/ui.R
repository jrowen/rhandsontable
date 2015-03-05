library(rHandsontable)

shinyUI(fluidPage(

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
