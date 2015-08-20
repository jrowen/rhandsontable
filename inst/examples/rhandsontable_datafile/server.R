library(rhandsontable)

shinyServer(function(input, output, session) {
  fname = tempfile()

  # uncomment lines below if action button is used to commit changes
  # values = list()
  # setHot = function(x) values[["hot"]] <<- x

  # comment lines below if action button is used to commit changes
  values = reactiveValues()
  setHot = function(x) values[["hot"]] = x

  observe({
    input$saveBtn

    if (!is.null(values[["hot"]])) {
      write.csv(values[["hot"]], fname)
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
