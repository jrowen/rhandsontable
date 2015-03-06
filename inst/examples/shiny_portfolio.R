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
  values = reactiveValues()

  calc = reactive({
    # load initial values
    rets = NULL
    port = data.table(Name = LETTERS[1:20],
                      Position = ifelse(rnorm(20) > 0, "Long", "Short"),
                      Weight = runif(20))
    port[, `:=`(Weight = Weight / sum(Weight)), by = Position]

    values[["port"]] = port

    setkey(port, Name)
    perf = returns[port]
    perf = perf[, list(Return = ifelse(Position == "Long", 1, -1) *
                         Weight * Return), by = Date][order(Date)]
    perf[, `:=`(CumulRet = cumprod(1 + Return) - 1)]

    list(Perf = perf)
  })

  output$hot = renderRHandsontable({
    if (!is.null(values[["port"]]))
      rhandsontable(values[["port"]])
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
