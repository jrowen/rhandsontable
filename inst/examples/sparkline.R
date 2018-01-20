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


spk_df <- data.frame(
  value = 1:2,
  factor = c("A", "B"),
  chart = sapply(
    list(
      list(
        values=c(3,3,3,-3,-3,-3),
        options=list(type="bar", zeroAxis=FALSE)
      ),
      list(
        values=runif(10, -3, 3),
        options=list(type="bar", zeroAxis=FALSE, barColor="purple", negBarColor="yellow")
      )
    ),
    jsonlite::toJSON
  ),
  stringsAsFactors = FALSE
)

rhandsontable(spk_df, rowHeaders = NULL, width = 550, height = 300) %>%
  hot_col("chart", renderer = htmlwidgets::JS("renderSparkline"))
