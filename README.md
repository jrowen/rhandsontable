[![Build Status](https://travis-ci.org/jrowen/rhandsontable.svg?branch=master)](https://travis-ci.org/jrowen/rhandsontable)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rhandsontable)](http://cran.r-project.org/package=rhandsontable)

**See the [project website](http://jrowen.github.io/rhandsontable/) for more details and live examples.**

An [`htmlwidgets`](http://www.htmlwidgets.org/) implementation of [Handsontable.js](http://http://handsontable.com/).  Per the website:

*Handsontable is a minimalist Excel-like data grid editor for HTML & JavaScript*

This library was inspired by the [`shinyTable`](https://github.com/trestletech/shinyTable) package.  Most of the original functionality was preserved, and the `htmlwidgets` framework made it possible to leverage even more of the Handsontable.js functionality.

**See the [vignette](http://rpubs.com/jrowen/intro_rhandsontable) for detailed examples and links to shiny apps.**

To install from CRAN use
```R
install.packages("rhandsontable")
```
For the latest development version use
```R
devtools::install_github("jrowen/rhandsontable")
```

A simple example
```R
library(rhandsontable)

DF = data.frame(int = 1:10,
                numeric = rnorm(10),
                logical = TRUE,
                character = LETTERS[1:10],
                fact = factor(letters[1:10]),
                date = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = FALSE)

# add a sparkline chart
DF$chart = sapply(1:10, function(x) jsonlite::toJSON(list(values=rnorm(10))))
                                                    
rhandsontable(DF, rowHeaders = NULL) %>%
  hot_col("chart", renderer = htmlwidgets::JS("renderSparkline"))
```
![alt tag](https://raw.github.com/jrowen/rhandsontable/master/inst/examples/images/rhandsontable_readme.png "A simple example")

A more involved `shiny` example
```R
shiny::runGitHub("rhandsontable", "jrowen", subdir = "inst/examples/rhandsontable_corr")
```
