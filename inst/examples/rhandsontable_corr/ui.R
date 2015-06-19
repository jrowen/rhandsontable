library(shiny)
library(rhandsontable)
library(metricsgraphics)

shinyUI(fluidPage(
  titlePanel("Stock Correlations"),
  helpText("Click a cell in the table to see a plot of the raw data."),
  fluidRow(
    column(4,
           rHandsontableOutput("hot")
    ),
    column(6,
           metricsgraphicsOutput("plot")
    )
  )
))
