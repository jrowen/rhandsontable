library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)

ui = dashboardPage(
  dashboardHeader(title = "rhandsontable Example"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "table",
              fluidRow(box(rHandsontableOutput("hot", height = 400)),
                       box(rHandsontableOutput("hot2", width = 200))),
              fluidRow(box(rHandsontableOutput("hot3")))
      )
    )
  )
)

server = function(input, output) {
  output$hot = renderRHandsontable({
    rhandsontable(do.call(cbind, lapply(1:20, function(i) data.table(rnorm(10000)))))
  })

  output$hot2 = renderRHandsontable({
    rhandsontable(do.call(cbind, lapply(1:3, function(i) data.table(rnorm(5)))))
  })

  output$hot3 = renderRHandsontable({
    rhandsontable(do.call(cbind, lapply(1:3, function(i) data.table(rnorm(5)))),
                  stretchH = "all")
  })
}

shinyApp(ui, server)
