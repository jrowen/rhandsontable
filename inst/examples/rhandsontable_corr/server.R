library(shiny)
library(rhandsontable)
library(metricsgraphics)
library(data.table)
library(reshape2)
library(quantmod)

tkrs = c("MSFT", "CAT", "AXP", "DIS", "MMM")

# quantmod::getSymbols(tkrs, from = "2012-06-01", auto.assign=TRUE)
# returns = Reduce(function(x, y) merge(x, y), lapply(tkrs, get))
# returns = returns[, names(returns)[grepl("Close", names(returns))]]
# returns = data.table(Date = time(returns), coredata(returns))
# returns = melt(returns, id.vars = "Date", variable.name = "Name",
#                value.name = "Price")[order(Name, Date)]
# returns[, `:=`(Name = gsub(".Close", "", Name))]
# returns[, `:=`(Return = c(NA, Price[-1] / head(Price, -1) - 1)), by = Name]
# saveRDS(returns, "returns.rds")
returns = readRDS("returns.rds")
setkey(returns, Name)

shinyServer(function(input, output, session) {
  calc = reactive({
    cor(dcast.data.table(returns, Date ~ Name, value.var = "Return")[
      , !"Date", with = FALSE], use = "pairwise.complete.obs")
  })

  output$hot = renderRHandsontable({
    MAT = calc()
    diag(MAT) = 1
    MAT[upper.tri(MAT)] = MAT[lower.tri(MAT)]
    rhandsontable(MAT, readOnly = TRUE, selectCallback = TRUE) %>%
      hot_cols(renderer = "function (instance, td, row, col, prop, value, cellProperties) {
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
  }")
  })

  output$plot = renderMetricsgraphics({
    if (!is.null(input$hot_select)) {
      x_val = colnames(calc())[input$hot_select$select$c]
      y_val = colnames(calc())[input$hot_select$select$r]

      DT = returns[Name %in% c(x_val, y_val)]
      DT = dcast.data.table(DT, Date ~ Name, value.var = "Return")

      DT[, list(x = get(x_val), y = get(y_val))] %>%
        mjs_plot(x = x, y = y) %>%
        mjs_point(color_accessor = "Date") %>%
        mjs_labs(x_label = x_val, y_label = y_val)
    }
  })
})
