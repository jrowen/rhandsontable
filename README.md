[![Build Status](https://travis-ci.org/jrowen/rhandsontable.svg?branch=master)](https://travis-ci.org/jrowen/rhandsontable)

An [`htmlwidgets`](http://www.htmlwidgets.org/) implementation of [Handsontable.js](http://http://handsontable.com/).  Per the website:

*Handsontable is a minimalist Excel-like data grid editor for HTML & JavaScript*

This library was inspired by the [`shinyTable`](https://github.com/trestletech/shinyTable) package.  Most of the original functionality was preserved, and the `htmlwidgets` framework made it possible to leverage even more of the Handsontable.js functionality.

**See the [vignette](http://rpubs.com/jrowen/intro_rhandsontable) for detailed examples and links to shiny apps.**

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

rhandsontable(DF, rowHeaders = NULL)
```
![A simple example](https://github.com/jrowen/rhandsontable/tree/master/inst/examples/images/rhandsontable_readme.png)
