library(shiny)
library(rhandsontable)
# devtools::install_github("hrbrmstr/metricsgraphics")
library(metricsgraphics)
library(data.table)

ui = shinyUI(fluidPage(

  titlePanel("Correlations"),
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

server = function(input, output) {

  data = reactive({
    dts = seq(from = as.Date("2014-01-01"),
              to = as.Date("2014-12-31"),
              by = "days")
    rets = rbindlist(lapply(LETTERS[1:5], function(x) {
      data.table(Name = x,
                 Date = dts,
                 Return = rnorm(length(dts),
                                mean = runif(1, min = -0.02, max = 0.02),
                                sd = runif(1, min = 0, max = 0.05)))
    }))

    rets
  })

  calc = reactive({
    cor(dcast.data.table(data(), Date ~ Name, value.var = "Return")[
      , !"Date", with = FALSE])
  })

  output$hot = renderRHandsontable({
    MAT = calc()
    diag(MAT) = 1
    MAT[upper.tri(MAT)] = MAT[lower.tri(MAT)]
    rhandsontable(MAT, readOnly = TRUE, selectCallback = TRUE) %>%
      hot_cols(renderer = gsub("\n", "", "
                               function (instance, td, row, col, prop, value, cellProperties) {
                                Handsontable.renderers.TextRenderer.apply(this, arguments);
                                if (row == col) {
                                  td.style.background = 'lightgrey';
                                } else if (col > row) {
                                  td.style.background = 'grey';
                                  td.style.color = 'grey';
                                } else if (value < -0.75) {
                                  td.style.background = 'pink';
                                } else if (value > 0.75) {
                                  td.style.background = 'lightgreen';
                                }
                               }"))
  })

  output$plot = renderMetricsgraphics({
    if (!is.null(input$hot_select)) {
      x_val = colnames(calc())[input$hot_select$select$c + 1]
      y_val = colnames(calc())[input$hot_select$select$r + 1]

      DT = data()[Name %in% c(x_val, y_val)]
      DT = dcast.data.table(DT, Date ~ Name, value.var = "Return")

      DT[, list(x = get(x_val), y = get(y_val))] %>%
        mjs_plot(x = x, y = y) %>%
        mjs_point(color_accessor = "Date") %>%
        mjs_labs(x_label = x_val, y_label = y_val)
    }
  })
}

shinyApp(ui = ui, server = server)
