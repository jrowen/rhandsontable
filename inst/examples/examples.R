library(rhandsontable)

DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
                small = letters[1:10],
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = F)

rhandsontable(DF, rowHeaders = NULL) %>%
  hot_col(col = "big", type = "dropdown", source = LETTERS) %>%
  hot_col(col = "small", type = "autocomplete", source = letters,
          strict = FALSE)

rhandsontable(DF, readOnly = TRUE) %>%
  hot_col("small", "password") %>%
  hot_cell(1, 1, "Test comment")

rhandsontable(DF) %>%
  hot_cols(columnSorting = TRUE)

rhandsontable(DF) %>%
  hot_table(highlightCol = TRUE, highlightRow = TRUE)

MAT = matrix(rnorm(50), nrow = 10, dimnames = list(LETTERS[1:10],
                                                   letters[1:5]))

rhandsontable(MAT) %>%
  hot_heatmap(cols = seq_len(ncol(MAT)),
              color_scale = c("#ED6D47", "#17F556"))

rhandsontable(MAT, width = 300, height = 150) %>%
  hot_cols(colWidths = 100, fixedColumnsLeft = 1) %>%
  hot_rows(rowHeights = 50, fixedRowsTop = 1)

rhandsontable(MAT) %>%
  hot_table(customBorders = TRUE)

rhandsontable(MAT) %>%
  hot_table(groups = jsonlite::toJSON(list(list(cols = c(0, 1)),
                                           list(rows = c(0, 1)))))

rhandsontable(MAT) %>%
  hot_table(customBorders = jsonlite::toJSON(list(list(
    range = list(from = list(row = 1, col = 1),
                 to = list(row = 2, col = 2)),
    top = list(width = 2, color = "red"),
    left = list(width = 2, color = "red"),
    bottom = list(width = 2, color = "red"),
    right = list(width = 2, color = "red"))), auto_unbox = TRUE))

# try to update any cell to 0
rhandsontable(MAT * 10) %>%
  hot_cols(validator = gsub("\n", "", "
    function (value, callback) {
      setTimeout(function(){
        if (value != 0) {
          callback(true);
        }
        else {
          callback(false);
        }
      }, 1000)
    }"),
           allowInvalid = FALSE)

# add conditional formatting to a correlation matrix
MAT = matrix(runif(100, -1, 1), nrow = 10,
             dimnames = list(LETTERS[1:10], LETTERS[1:10]))
diag(MAT) = 1
MAT[upper.tri(MAT)] = MAT[lower.tri(MAT)]
rhandsontable(MAT, readOnly = TRUE) %>%
  hot_cols(renderer = gsub("\n", "", "
    function (instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      if (row == col) {
        td.style.background = 'lightgrey';
      } else if (col > row) {
        td.style.background = 'grey';
        td.style.color = 'grey';
      } else if (value < -0.75) {
        td.style.background = 'pink';
      } else if (value > 0.75) {
        td.style.background = 'lightgreen';
      }
    }"))
