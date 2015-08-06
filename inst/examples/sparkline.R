library(rhandsontable)

DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
                small = factor(letters[1:10]),
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = FALSE)

DF$chart = sapply(1:10,
                  function(x) jsonlite::toJSON(list(values=rnorm(10),
                                                    options = list(type = "bar"))))
rhandsontable(DF, rowHeaders = NULL) %>%
  hot_col("chart", renderer = htmlwidgets::JS("renderSparkline"))
