library(rhandsontable)

shinyUI(fluidPage(
  titlePanel("Handsontable"),
  sidebarLayout(
    sidebarPanel(
      helpText("Handsontable demo output. Column add/delete does work ",
               "for tables with defined column properties, including type."),
      radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
    ),
    mainPanel(
      rHandsontableOutput("hot", width = 350)
    )
  )
))
