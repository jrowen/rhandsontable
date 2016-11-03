library(rhandsontable)

shinyServer(function(input, output, session) {
  # this caching step is no longer necessary
  # it was left as an example
  values = reactiveValues()

  data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF = data.frame(val = 1:10, bool = TRUE, nm = LETTERS[1:10],
                        dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                        stringsAsFactors = F)
      else
        DF = values[["DF"]]
    }

    values[["DF"]] = DF
    DF
  })

  output$hot <- renderRHandsontable({
    DF = data()
    if (!is.null(DF))
      rhandsontable(DF, useTypes = as.logical(input$useType), stretchH = "all")
  })
})
