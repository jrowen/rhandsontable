library(rhandsontable)

shinyUI(fluidPage(
  titlePanel("Handsontable"),
  sidebarLayout(
    sidebarPanel(
      helpText("Change a cell comment using the right-click menu. ",
               "Note that deleting a comment does not currently fire ",
               "a callback."),
      actionButton("exportData", "Export Data"),
      actionButton("exportComments", "Export Comments")
    ),
    mainPanel(
      rHandsontableOutput("hot")
    )
  )
))
