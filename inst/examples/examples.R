library(rhandsontable)

# DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
#                 small = letters[1:10],
#                 dt = seq(from = Sys.Date(), by = "days", length.out = 10),
#                 stringsAsFactors = F)
#
# rhandsontable(DF) %>%
#   hot_col(col = "big", type = "dropdown", source = LETTERS) %>%
#   hot_col(col = "small", type = "autocomplete", source = letters,
#           strict = FALSE)
#
# rhandsontable(DF) %>%
#   hot_col("small", "password")
#
# rhandsontable(DF) %>%
#   hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
#   hot_rows(fixedRowsTop = 1)
#
# MAT = matrix(rnorm(50), nrow = 10)
#
# rhandsontable(MAT) %>%
#   hot_heatmap(col = seq_len(ncol(MAT)))
#

# Not working

rhandsontable(DF) %>%
  hot_table(highlightCol = TRUE, highlightRow = TRUE)
