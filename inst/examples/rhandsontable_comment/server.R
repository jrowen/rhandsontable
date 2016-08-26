library(rhandsontable)

shinyServer(function(input, output, session) {
  values = reactiveValues()

  data = reactive({
    if (is.null(input$hot)) {
      MAT = matrix(rnorm(50), nrow = 10, dimnames = list(LETTERS[1:10],
                                                         letters[1:5]))
      vals = rep(NA, 50)
      vals[sample(1:50, 5)] = paste0(LETTERS[1:5], letters[1:5])
      CMTS = matrix(vals, nrow = 10, ncol = 5, dimnames = list(LETTERS[1:10],
                                                               letters[1:5]))
    } else {
      MAT = hot_to_r(input$hot)
    }

    MAT
  })

  comments = reactive({
    if (is.null(input$hot_comment)) {
      vals = rep(NA, 50)
      vals[sample(1:50, 5)] = paste0(LETTERS[1:5], letters[1:5])
      CMTS = matrix(vals, nrow = 10, ncol = 5, dimnames = list(LETTERS[1:10],
                                                               letters[1:5]))
    } else {
      new = input$hot_comment$comment

      CMTS = values[["CMTS"]]
      CMTS[new$r, new$c] = new$val
    }

    values[["CMTS"]] = CMTS
    CMTS
  })

  observe({
    hot = data()
    if (input$exportData != 0) {
      if (!is.null(hot)) {
        write.csv(hot, "data.csv")
      }
    }
  })

  observe({
    if (input$exportComments != 0) {
      if (!is.null(values[["CMTS"]])) {
        write.csv(values[["CMTS"]], "comments.csv")
      }
    }
  })

  output$hot <- renderRHandsontable({
    MAT = data()
    CMTS = comments()

    if (!is.null(MAT) && !is.null(CMTS)) {
      hot = rhandsontable(MAT, comments = CMTS)

    hot
    }
  })
})
