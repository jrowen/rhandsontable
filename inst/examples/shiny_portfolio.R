library(shiny)
library(rhandsontable)
library(metricsgraphics)
library(data.table)

returns = data.table(expand.grid(
  Date = seq(from = as.Date("2014-01-01"),
             to = as.Date("2014-12-31"),
             by = "days"),
  Name = LETTERS[1:20]))
returns[, `:=`(Return = rnorm(.N, mean = 0.005, sd = 0.01))]
setkey(returns, Name)

port = data.table(Name = LETTERS[1:20],
                  Position = ifelse(rnorm(20) > 0, "Long", "Short"),
                  Weight = sample(c(rep(0, 10), runif(10)), 20))
port[, `:=`(Weight = Weight / sum(Weight)), by = Position]
port[Position == "Long", `:=`(Weight = Weight * 1.3)]
port[Position == "Short", `:=`(Weight = Weight * 0.3)]
setkey(port, Name)

ui = shinyUI(fluidPage(

  titlePanel("Portfolio"),

  fluidRow(
    column(4,
           rHandsontableOutput("hot")
    ),
    column(6,
           metricsgraphicsOutput("plot")
    )
  )
))

server = function(input, output) {
  values = reactiveValues(hot = port)

  calc = reactive({
    port = values[["hot"]]
    perf = returns[port]
    perf = perf[, list(Return = ifelse(Position == "Long", 1, -1) *
                         Weight * Return), by = Date][order(Date)]
    perf[, `:=`(CumulRet = cumprod(1 + Return) - 1)]

    list(Perf = perf)
  })

  output$hot = renderRHandsontable({
    DF = NULL
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
      # TODO: use changes to adj other weights proportionaly
      values[["hot"]] = DF
    } else if (!is.null(values[["hot"]])) {
      DF = values[["hot"]]
    }

    if (!is.null(DF))
      rhandsontable(DF) %>%
      hot_col(col = "Position", type = "dropdown",
              source = c("Long", "Short")) %>%
      hot_col(col = "Name", readOnly = TRUE) %>%
      hot_cols(columnSorting = list(column = which(names(DF) == "Name"),
                                    sortOrder = TRUE))
  })

  output$plot = renderMetricsgraphics({
    if (!is.null(calc())) {
      calc()$Perf %>%
        mjs_plot(x = Date, y = CumulRet) %>%
        mjs_labs(x = "Date", y = "Return") %>%
        mjs_axis_x(xax_format = "date")
    }
  })
}

shinyApp(ui = ui, server = server)
