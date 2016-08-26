library(rhandsontable)

shinyServer(function(input, output, session) {
  fname = tempfile()

  observe({
    # remove button and isolate to update file automatically
    # after each table change
    input$saveBtn
    hot = isolate(input$hot)
    if (!is.null(hot)) {
      write.csv(hot, fname)
      print(fname)
    }
  })

  output$hot = renderRHandsontable({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      DF = read.csv("mtcars.csv", stringsAsFactors = FALSE)
    }

    setHot(DF)
    rhandsontable(DF) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
})
