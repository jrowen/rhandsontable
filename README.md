[![Build Status](https://travis-ci.org/jrowen/rhandsontable.svg?branch=master)](https://travis-ci.org/jrowen/rhandsontable)

An [`htmlwidgets`](http://www.htmlwidgets.org/) implementation of [Handsontable.js](http://http://handsontable.com/).  Per the website:

*Handsontable is a minimalist Excel-like data grid editor for HTML & JavaScript*

This library was inspired by the [`shinyTable`](https://github.com/trestletech/shinyTable) package.  Most of the original functionality was preserved, and the `htmlwidgets` framework made it possible to leverage even more of the Handsontable.js functionality.

**See the [vignette](http://rpubs.com/jrowen/intro_rhandsontable) for detailed examples.**

To install use
```R
devtools::install_github("jrowen/rhandsontable")
```

A simple example
```R
library(rhandsontable)

DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
                small = letters[1:10],
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = F)

rhandsontable(DF, rowHeaders = NULL) %>%
  hot_col(col = "big", type = "dropdown", source = LETTERS) %>%
  hot_col(col = "small", type = "autocomplete", source = letters,
          strict = FALSE)
```

A heatmap example
```R
MAT = matrix(rnorm(50), nrow = 10, dimnames = list(LETTERS[1:10],
                                                   letters[1:5]))

rhandsontable(MAT) %>%
  hot_heatmap()
```

A simple `shiny` example
```R
library(shiny)
library(rhandsontable)

ui = shinyUI(fluidPage(

  titlePanel("Handsontable"),

  sidebarLayout(
    sidebarPanel(
      helpText("Handsontable demo output. Column add/delete does work ",
               "for tables with defined column properties, including type."),
      radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
    ),
    mainPanel(
      rHandsontableOutput("hot")
    )
  )
))

server = function(input, output) {
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
}

shinyApp(ui = ui, server = server)
```
