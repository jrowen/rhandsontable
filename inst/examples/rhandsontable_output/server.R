library(rhandsontable)

shinyServer(function(input, output, session) {
  output$hot <- renderRHandsontable({
    if (is.null(input$hot)) {
      DF = data.frame(val = 1:10, bool = TRUE, nm = LETTERS[1:10],
                      dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                      stringsAsFactors = F)
    } else {
      DF = hot_to_r(input$hot)
    }
    rhandsontable(DF, useTypes = as.logical(input$useType))
  })
})
