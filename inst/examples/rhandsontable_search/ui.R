library(rhandsontable)

shinyUI(fluidPage(
  titlePanel("Edit Data File"),
  helpText("Changes to the table will be automatically saved to the source file."),
  
  # uncomment line below to use action button to commit changes
  fluidRow(
    column(width=2, textInput("searchField", "Search")),
    column(width=2, uiOutput("saveText"), actionButton("saveBtn", "Save"))
  ),
  rHandsontableOutput("hot")
))
