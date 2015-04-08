library(rhandsontable)

DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
                small = letters[1:10],
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = F)

# table with dropdowns
rhandsontable(DF, rowHeaders = NULL) %>%
  hot_col(col = "big", type = "dropdown", source = LETTERS) %>%
  hot_col(col = "small", type = "autocomplete", source = letters,
          strict = FALSE)

# read-only table
rhandsontable(DF, readOnly = TRUE) %>%
  hot_col("small", "password") %>%
  hot_cell(1, 1, "Test comment")

# column sorting enables
rhandsontable(DF) %>%
  hot_cols(columnSorting = TRUE)

# highlight selected row and column
rhandsontable(DF) %>%
  hot_table(highlightCol = TRUE, highlightRow = TRUE)

# export data to csv
rhandsontable(DF) %>%
  hot_table(exportToCsv = TRUE)

MAT = matrix(rnorm(50), nrow = 10, dimnames = list(LETTERS[1:10],
                                                   letters[1:5]))

# turn the table into a heatmap
rhandsontable(MAT) %>%
  hot_heatmap()

# add fixed rows and columns
rhandsontable(MAT, width = 300, height = 150) %>%
  hot_cols(colWidths = 100, fixedColumnsLeft = 1) %>%
  hot_rows(rowHeights = 50, fixedRowsTop = 1)

# group rows and columns
rhandsontable(MAT) %>%
  hot_table(groups = jsonlite::toJSON(list(list(cols = c(0, 1)),
                                           list(rows = c(0, 1)))))

# add custom borders
rhandsontable(MAT) %>%
  hot_table(customBorders = jsonlite::toJSON(list(list(
    range = list(from = list(row = 1, col = 1),
                 to = list(row = 2, col = 2)),
    top = list(width = 2, color = "red"),
    left = list(width = 2, color = "red"),
    bottom = list(width = 2, color = "red"),
    right = list(width = 2, color = "red"))), auto_unbox = TRUE))

# add numeric validation
rhandsontable(MAT * 10) %>%
  hot_validate_numeric(col = 1, min = -50, max = 50, exclude = 40)

rhandsontable(MAT * 10) %>%
  hot_validate_numeric(col = 1, choices = c(10, 20, 40))

# add character validation
rhandsontable(DF) %>%
  hot_validate_character(col = "big", choices = LETTERS[1:10])

# custom validation; try to update any cell to 0
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

# add conditional formatting to a triangular matrix
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
