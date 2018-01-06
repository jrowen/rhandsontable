library(rhandsontable)

shinyServer(function(input, output, session) {
  fname = tempfile(fileext = ".csv")
  
  observe({
    # remove button and isolate to update file automatically
    # after each table change
    input$saveBtn
    hot = isolate(input$hot)
    if (!is.null(hot)) {
      write.csv(hot_to_r(input$hot), fname)
      print(fname)
    }
  })
  
  output$hot = renderRHandsontable({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      DF = read.csv("life_expectancy.csv", stringsAsFactors = FALSE)
    }
    
    rhandsontable(DF, search = TRUE) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  output$saveText<- renderUI({
    HTML("<label>Save data</label>")  
  })
})
