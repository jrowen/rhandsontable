library(shiny)
library(rhandsontable)
library(ggplot2)

shinyUI(fluidPage(
  titlePanel("Efficient Frontier"),
  fluidRow(
    column(3,
           rHandsontableOutput("hot_retvol")
    ),
    column(3,
           rHandsontableOutput("hot_corr")
    )
  ),
  fluidRow(
    column(6,
           plotOutput("plot")
    )
  )
))
