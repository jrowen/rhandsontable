library(rhandsontable)
library(shiny)

app <- shinyApp(
  ui = fluidPage(
    numericInput("myindex", "Highlight", min = 0, max = 9, value = 1),
    rHandsontableOutput("hot", width = 350)
  ),
  server = function(input, output) {
    data = reactive({
      if (!is.null(input$hot)) {
        DF = hot_to_r(input$hot)
      } else {
        DF = data.frame(val = 1:10, bool = TRUE, nm = LETTERS[1:10],
                        dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                        stringsAsFactors = F)
      }
      DF
    })

    output$hot <- renderRHandsontable({
      DF = data()
      myindex = input$myindex
      if (!is.null(DF)) {
        rhandsontable(DF, myindex = myindex) %>%
          hot_cols(renderer = "function(instance, td, row, col, prop, value, cellProperties) {
            Handsontable.TextCell.renderer.apply(this, arguments);
            if (instance.params && instance.params.myindex == row) td.style.background = 'lightblue';
          }
        ")
      }
    })
  }
)

runApp(app)
