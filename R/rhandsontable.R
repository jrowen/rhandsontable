#' Handsontable widget
#'
#' Create a \href{http://handsontable.com}{Handsontable.js} widget.
#'
#' @param data a \code{data.table}, \code{data.frame} or \code{matrix}
#' @param colHeaders a vector of column names. If missing \code{colnames}
#'  will be used. Setting to \code{NULL} will omit.
#' @param rowHeaders a vector of row names. If missing \code{rownames}
#'  will be used. Setting to \code{NULL} will omit.
#' @param useTypes logical specifying whether column classes should be mapped to
#'  equivalent Javascript types
#' @param readOnly logical specifying whether the table is editable
#' @param selectCallback logical enabling the afterSelect event to return data.
#'  This can be used with shiny to tie updates to a selected table cell.
#' @param width numeric table width
#' @param height numeric table height
#' @param ... passed to hot_table
#' @examples
#' library(rhandsontable)
#' DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
#'                 small = letters[1:10],
#'                 dt = seq(from = Sys.Date(), by = "days", length.out = 10),
#'                 stringsAsFactors = FALSE)
#'
#' rhandsontable(DF, rowHeaders = NULL)
#' @export
rhandsontable <- function(data, colHeaders, rowHeaders, useTypes = TRUE,
                          readOnly = NULL, selectCallback = FALSE,
                          width = NULL, height = NULL, ...) {
  if (missing(colHeaders))
    colHeaders = colnames(data)
  if (missing(rowHeaders))
    rowHeaders = rownames(data)

  if (!useTypes) {
    data = as.matrix(data, rownames.force = TRUE)
    cols = NULL
  }

  rClass = class(data)
  if ("matrix" %in% rClass) {
    rColClasses = class(data[1, 1])
  } else {
    rColClasses = lapply(data, class)
  }

  if(useTypes) {
    # get column data types
    col_typs = get_col_types(data)

    # format date for display
    dt_inds = which(col_typs == "date")
    if (length(dt_inds) > 0L) {
      for (i in dt_inds)
        data[, i] = as.character(data[, i], format = DATE_FORMAT)
    }

    cols = lapply(col_typs, function(type) {
      res = list(type = type)
      res$readOnly = readOnly
      res
    })
  }

  x = list(
    data = jsonlite::toJSON(data, na = "string", rownames = FALSE),
    rClass = rClass,
    rColClasses = rColClasses,
    selectCallback = selectCallback,
    colHeaders = colHeaders,
    rowHeaders = rowHeaders,
    columns = cols,
    width = width,
    height = height
  )

  # create widget
  hot = htmlwidgets::createWidget(
    name = 'rhandsontable',
    x,
    width = width,
    height = height,
    package = 'rhandsontable',
    sizingPolicy = htmlwidgets::sizingPolicy(
      padding = 5,
      defaultHeight = 0,
      defaultWidth = 0
    )
  )

  if (!is.null(readOnly)) {
    for (x in hot$x$colHeaders)
      hot = hot %>% hot_col(x, readOnly = readOnly)
  }

  hot = hot %>% hot_table(allowColEdit = ("matrix" %in% rClass), ...)

  hot
}

#' Handsontable widget
#'
#' Configure multiple columns.
#'
#' @param hot rhandsontable object
#' @param columns a list of column settings
#' @param colWidths a scalar or numeric vector of column widths
#' @param columnSorting logical enabling row sorting. Sorting only alters the
#'  table presentation and the original dataset row order is maintained.
#' @param manualColumnMove logical enabling column drag-and-drop reordering
#' @param manualColumnResize logical enabline column width resizing
#' @param fixedColumnsLeft a numeric vector indicating which columns should be
#'  frozen on the left
#' @param ... passed to hot_col
#' @examples
#' library(rhandsontable)
#' DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
#'                 small = letters[1:10],
#'                 dt = seq(from = Sys.Date(), by = "days", length.out = 10),
#'                 stringsAsFactors = FALSE)
#'
#' rhandsontable(DF) %>%
#'   hot_cols(columnSorting = TRUE)
#' @export
hot_cols = function(hot, columns = NULL, colWidths = NULL,
                    columnSorting = NULL, manualColumnMove = NULL,
                    manualColumnResize = NULL, fixedColumnsLeft = NULL, ...) {
  # overwrite original settings
  if (!is.null(columns)) hot$x$columns = columns

  if (!is.null(colWidths)) hot$x$colWidths = colWidths

  if (!is.null(columnSorting)) hot$x$columnSorting = columnSorting
  if (!is.null(manualColumnMove)) hot$x$manualColumnMove = manualColumnMove
  if (!is.null(manualColumnResize)) hot$x$manualColumnResize = manualColumnResize

  if (!is.null(fixedColumnsLeft)) hot$x$fixedColumnsLeft = fixedColumnsLeft

  for (x in hot$x$colHeaders)
    hot = hot %>% hot_col(x, ...)

  hot
}

#' Handsontable widget
#'
#' Configure single column.
#'
#' @param hot rhandsontable object
#' @param col numeric column index
#' @param type character specify the data type. Options include:
#'  numeric, date, checkbox, select, dropdown, autocomplete, password,
#'  and handsontable (not implemented yet)
#' @param format characer specifying column format. See Cell Types at
#'  \href{http://handsontable.com}{Handsontable.js} for the formatting
#'  options for each data type
#' @param source a vector of choices for select, dropdown and autocomplete
#'  column types
#' @param strict logical specifying whether values not in the \code{source}
#'  vector will be accepted
#' @param readOnly logical making the table read-only
#' @param validator character defining a Javascript function to be used
#'  to validate user input. See \code{hot_validate_numeric} and
#'  \code{hot_validate_character} for pre-build validators.
#' @param allowInvalid logical specifying whether invalid data will be
#'  accepted. Invalid data cells will be color red.
#' @param halign character defining the horizontal alignment. Possible
#'  values are htLeft, htCenter, htRight and htJustify
#' @param valign character defining the vertical alignment. Possible
#'  values are htTop, htMiddle, htBottom
#' @param renderer character defining a Javascript function to be used
#'  to format column cells. Can be used to implement conditional formatting.
#' @examples
#' library(rhandsontable)
#' DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
#'                 small = letters[1:10],
#'                 dt = seq(from = Sys.Date(), by = "days", length.out = 10),
#'                 stringsAsFactors = FALSE)
#'
#' rhandsontable(DF, rowHeaders = NULL) %>%
#'   hot_col(col = "big", type = "dropdown", source = LETTERS) %>%
#'   hot_col(col = "small", type = "autocomplete", source = letters,
#'           strict = FALSE)
#' @export
hot_col = function(hot, col, type = NULL, format = NULL, source = NULL,
                   strict = NULL,
                   readOnly = NULL, validator = NULL, allowInvalid = NULL,
                   halign = NULL, valign = NULL,
                   renderer = NULL) {
  cols = hot$x$columns
  if (is.null(cols)) {
    # create a columns list
    warning("rhandsontable column types were previously not defined but are ",
            "now being set to 'text' to support column properties")
    cols = lapply(hot$x$colHeaders, function(x) {
      list(type = "text")
    })
  }

  if (is.character(col)) col = which(hot$x$colHeaders == col)

  if (!is.null(type)) cols[[col]]$type = type
  if (!is.null(format)) cols[[col]]$format = format
  if (!is.null(source)) cols[[col]]$source = source
  if (!is.null(strict)) cols[[col]]$strict = strict
  if (!is.null(readOnly)) cols[[col]]$readOnly = readOnly

  if (!is.null(validator)) cols[[col]]$validator = JS(validator)
  if (!is.null(allowInvalid)) cols[[col]]$allowInvalid = allowInvalid
  if (!is.null(renderer)) cols[[col]]$renderer = JS(renderer)

  className = c(halign, valign)
  if (!is.null(className)) {
    cols[[col]]$className = className
  }

  hot$x$columns = cols
  hot
}

#' Handsontable widget
#'
#' Add numeric validation to a column
#'
#' @param hot rhandsontable object
#' @param cols numeric vector column index
#' @param min minimum value to accept
#' @param max maximum value to accept
#' @param choices a vector of acceptable numeric choices. It will be evaluated
#'  after min and max if specified.
#' @param exclude a vector or unacceptable numeric values
#' @param allowInvalid logical specifying whether invalid data will be
#'  accepted. Invalid data cells will be color red.
#' @examples
#' library(rhandsontable)
#' MAT = matrix(rnorm(50), nrow = 10, dimnames = list(LETTERS[1:10],
#'              letters[1:5]))
#'
#' rhandsontable(MAT * 10) %>%
#'   hot_validate_numeric(col = 1, min = -50, max = 50, exclude = 40)
#'
#' rhandsontable(MAT * 10) %>%
#'   hot_validate_numeric(col = 1, choices = c(10, 20, 40))
#' @export
hot_validate_numeric = function(hot, cols, min = NULL, max = NULL,
                                choices = NULL, exclude = NULL,
                                allowInvalid = FALSE) {
  f = "function (value, callback) {
         setTimeout(function(){
           if (isNaN(parseFloat(value))) {
             callback(false);
           }
           %exclude
           %min
           %max
           %choices
           callback(true);
         }, 500)
       }"

  if (!is.null(exclude))
    ex_str = paste0("if ([",
                    paste0(paste0("'", exclude, "'"), collapse = ","),
                    "].indexOf(value) > -1) { callback(false); }")
  else
    ex_str = ""
  f = gsub("%exclude", ex_str, f)

  if (!is.null(min))
    min_str = paste0("if (value < ", min, ") { callback(false); }")
  else
    min_str = ""
  f = gsub("%min", min_str, f)

  if (!is.null(max))
    max_str = paste0("if (value > ", max, ") { callback(false); }")
  else
    max_str = ""
  f = gsub("%max", max_str, f)

  if (!is.null(choices))
    chcs_str = paste0("if ([",
                      paste0(paste0("'", choices, "'"), collapse = ","),
                      "].indexOf(value) == -1) { callback(false); }")
  else
    chcs_str = ""
  f = gsub("%choices", chcs_str, f)

  for (x in cols)
    hot = hot %>% hot_col(x, validator = f,
                          allowInvalid = allowInvalid)

  hot
}

#' Handsontable widget
#'
#' Add numeric validation to a column
#'
#' @param hot rhandsontable object
#' @param cols numeric vector column index
#' @param choices a vector of acceptable numeric choices. It will be evaluated
#'  after min and max if specified.
#' @param allowInvalid logical specifying whether invalid data will be
#'  accepted. Invalid data cells will be color red.
#' @examples
#' library(rhandsontable)
#' DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
#'                 small = letters[1:10],
#'                 dt = seq(from = Sys.Date(), by = "days", length.out = 10),
#'                 stringsAsFactors = FALSE)
#'
#' rhandsontable(DF) %>%
#'   hot_validate_character(col = "big", choices = LETTERS[1:10])
#' @export
hot_validate_character = function(hot, cols, choices,
                                  allowInvalid = FALSE) {
  f = "function (value, callback) {
         setTimeout(function() {
           if (typeof(value) != 'string') {
             callback(false);
           }
           %choices
           callback(false);
         }, 500)
       }"

  ch_str = paste0("if ([",
                  paste0(paste0("'", choices, "'"), collapse = ","),
                  "].indexOf(value) > -1) { callback(true); }")
  f = gsub("%choices", ch_str, f)

  for (x in cols)
    hot = hot %>% hot_col(x, validator = f,
                          allowInvalid = allowInvalid)

  hot
}

#' Handsontable widget
#'
#' Configure rows.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot rhandsontable object
#' @param rowHeights a scalar or numeric vector of row heights
#' @param fixedRowsTop a numeric vector indicating which rows should be
#'  frozen on the top
#' @examples
#' library(rhandsontable)
#' MAT = matrix(rnorm(50), nrow = 10, dimnames = list(LETTERS[1:10],
#'              letters[1:5]))
#'
#' rhandsontable(MAT, width = 300, height = 150) %>%
#' hot_cols(colWidths = 100, fixedColumnsLeft = 1) %>%
#'   hot_rows(rowHeights = 50, fixedRowsTop = 1)
#' @export
hot_rows = function(hot, rowHeights = NULL, fixedRowsTop = NULL) {
  if (!is.null(rowHeights)) hot$x$rowHeights = rowHeights
  if (!is.null(fixedRowsTop)) hot$x$fixedRowsTop = fixedRowsTop
  hot
}

#' Handsontable widget
#'
#' Configure single cell.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot rhandsontable object
#' @param row numeric row index
#' @param col numeric column index
#' @param comment character comment to add to cell
#' @examples
#' library(rhandsontable)
#' DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
#'                 small = letters[1:10],
#'                 dt = seq(from = Sys.Date(), by = "days", length.out = 10),
#'                 stringsAsFactors = FALSE)
#'
#' rhandsontable(DF, readOnly = TRUE) %>%
#'   hot_cell(1, 1, "Test comment")
#' @export
hot_cell = function(hot, row, col, comment = NULL) {
  cell = list(row = row, col = col, comment = comment)

  hot$x$cell = c(hot$x$cell, list(cell))

  if (is.null(hot$x$comments))
    hot = hot %>% hot_table(comments = TRUE, contextMenu = TRUE)

  hot
}

#' Handsontable widget
#'
#' Configure table.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot rhandsontable object
#' @param contextMenu logical enabling the right-click menu
#' @param allowRowEdit logical enabling right-click row options
#' @param allowColEdit logical enabling right-click column options
#' @param customBorders json object. See
#'  \href{http://handsontable.com/demo/custom_borders.html}{Custom borders} for details.
#' @param groups json object. See
#'  \href{http://handsontable.com/demo/grouping.html}{Grouping & ungrouping of rows and columns} for details.
#' @param highlightRow logical enabling row highlighting for the selected
#'  cell
#' @param highlightCol logical enabling column highlighting for the
#'  selected cell
#' @param comments logical enabling comments in the table, including the
#'  corresponding options in the right-click menu. User comments are not
#'  currently returned to R.
#' @param exportToCsv logical adding a context menu option to export the table
#'  data to a csv file
#' @param csvFileName character csv file name
#' @param ... passed to \href{http://handsontable.com}{Handsontable.js} constructor
#' @examples
#' library(rhandsontable)
#' DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
#'                 small = letters[1:10],
#'                 dt = seq(from = Sys.Date(), by = "days", length.out = 10),
#'                 stringsAsFactors = FALSE)
#'
#' rhandsontable(DF) %>%
#' hot_table(highlightCol = TRUE, highlightRow = TRUE,
#'           allowRowEdit = FALSE, allowColEdit = FALSE)
#' @export
hot_table = function(hot, contextMenu = TRUE,
                     allowRowEdit = TRUE, allowColEdit = TRUE,
                     customBorders = NULL, groups = NULL, highlightRow = NULL,
                     highlightCol = NULL, comments = NULL,
                     exportToCsv = NULL, csvFileName = "download.csv",
                     ...) {
  if (!is.null(contextMenu)) hot$x$contextMenu = contextMenu
  if (!is.null(allowRowEdit)) hot$x$allowRowEdit = allowRowEdit
  if (!is.null(allowColEdit)) hot$x$allowColEdit = allowColEdit
  if (!is.null(customBorders)) hot$x$customBorders = customBorders
  if (!is.null(groups)) hot$x$groups = groups
  if (!is.null(comments)) hot$x$comments = comments
  if (!is.null(exportToCsv)) hot$x$exportToCsv = exportToCsv
  if (!is.null(csvFileName)) hot$x$csvFileName = csvFileName

  if ((!is.null(highlightRow) && highlightRow) ||
        (!is.null(highlightCol) && highlightCol))
    hot$x$ishighlight = TRUE
  if (!is.null(highlightRow) && highlightRow)
    hot$x$currentRowClassName = "currentRow"
  if (!is.null(highlightCol) && highlightCol)
    hot$x$currentColClassName = "currentCol"

  if (!is.null(list(...)))
    hot$x = c(hot$x, list(...))

  hot
}

#' Handsontable widget
#'
#' Add heatmap to table.  See
#' \href{http://handsontable.com/demo/heatmaps.html}{Heatmaps for values in a column}
#' for details.
#'
#' @param hot rhandsontable object
#' @param cols numeric vector of columns to include in the heatmap. If missing
#'  all columns are used.
#' @param color_scale character vector that includes the lower and upper
#'  colors
#' @param renderer character defining a Javascript function to be used
#'  to determine the cell colors. If missing,
#'  \code{rhandsontable:::renderer_heatmap} is used.
#' @examples
#' MAT = matrix(rnorm(50), nrow = 10, dimnames = list(LETTERS[1:10],
#'              letters[1:5]))
#'
#'rhandsontable(MAT) %>%
#'  hot_heatmap()
#' @export
hot_heatmap = function(hot, cols, color_scale = c("#ED6D47", "#17F556"),
                       renderer = NULL) {
  if (is.null(renderer)) {
    renderer = renderer_heatmap(color_scale)
  }

  if (missing(cols))
    cols = seq_along(hot$x$colHeaders)
  for (x in hot$x$colHeaders[cols])
    hot = hot %>% hot_col(x, renderer = renderer)

  hot
}

# Used by hot_heatmap
renderer_heatmap = function(color_scale) {
  renderer = gsub("\n", "", "
      function (instance, td, row, col, prop, value, cellProperties) {

        Handsontable.renderers.TextRenderer.apply(this, arguments);
        heatmapScale  = chroma.scale(['%s1', '%s2']);

        if (instance.heatmap[col]) {
          mn = instance.heatmap[col].min;
          mx = instance.heatmap[col].max;
          pt = (parseInt(value, 10) - mn) / (mx - mn);

          td.style.backgroundColor = heatmapScale(pt).hex();
        }
      }
      ")
  renderer = gsub("%s1", color_scale[1], renderer)
  renderer = gsub("%s2", color_scale[2], renderer)
  renderer
}

#' Handsontable widget
#'
#' Shiny bindings for rhandsontable
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'  \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'  string and have \code{"px"} appended.
#' @export
rHandsontableOutput <- function(outputId, width = NA, height = NA){
  htmlwidgets::shinyWidgetOutput(outputId, 'rhandsontable', width, height,
                                 package = 'rhandsontable')
}

#' Handsontable widget
#'
#' Shiny bindings for rhandsontable
#'
#' @param expr An expression that generates threejs graphics.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'  is useful if you want to save an expression in a variable.
#' @export
renderRHandsontable <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, rHandsontableOutput, env, quoted = TRUE)
}

#' Handsontable widget
#'
#' Convert handsontable data to R object. Can be used in a \code{shiny} app
#'  to convert the input json to an R dataset.
#'
#' @param ... passed to \code{rhandsontable:::toR}
#' @export
hot_to_r = function(...) {
  do.call(toR, ...)
}
