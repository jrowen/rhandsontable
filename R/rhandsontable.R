#' See \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param data
#' @param rownames
#' @param contextMenu
#' @param width
#' @param height
#'
#' @export
rhandsontable <- function(data, rownames = NULL, contextMenu = TRUE,
                          readOnly = FALSE, width = NULL, height = NULL) {

  # get column data types
  col_typs = get_col_types(data)
  cols = jsonlite::toJSON(data.frame(type = col_typs,
                                     readOnly = readOnly))

  # format date for display
  dt_inds = which(col_typs == "date")
  if (length(dt_inds) > 0L) {
    for (i in dt_inds)
      data[, i] = as.character(data[, i], format = "%m/%d/%Y")
  }

  # forward options using x
  x = list(
    data = jsonlite::toJSON(data),
    colHeaders = names(data),
    rowHeaders = rownames,
    contextMenu = contextMenu,
    columns = cols
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'rhandsontable',
    x,
    width = width,
    height = height,
    package = 'rhandsontable'
  )
}

#' Configure column parameters.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot rhandsontable object
#' @param columns
#' @param colWidths scalar, vector or JS()
#' @param columnSorting
#' @param manualColumnMove
#' @param manualColumnResize
#' @param fixedColumnsLeft
#' @param halign htLeft, htCenter, htRight, htJustify
#' @param valign htTop, htMiddle, htBottom
#' @export
hot_cols = function(hot, columns = NULL, colWidths = NULL,
                    columnSorting = FALSE, manualColumnMove = FALSE,
                    manualColumnResize = FALSE, fixedColumnsLeft = NULL,
                    halign = NULL, valign = NULL) {
  # overwrite original settings
  if (!is.null(columns)) hot$x$columns = columns

  hot$x$colWidths = colWidths

  hot$x$columnSorting = columnSorting
  hot$x$manualColumnMove = manualColumnMove
  hot$x$manualColumnResize = manualColumnResize

  hot$x$fixedColumnsLeft = fixedColumnsLeft

  className = c(halign, valign)
  if (!is.null(className)) {
    cols = jsonlite::fromJSON(hot$x$columns)
    cols$className = className
    hot$x$columns = jsonlite::toJSON(cols, auto_unbox = TRUE)
  }

  hot
}

#' Configure single column.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot rhandsontable object
#' @param col
#' @param type
#' @param format
#' @param source
#' @param strict
#' @param allowInvalcol
#' @param readOnly
#' @param validator regex expression or JS()
#' @param halign htLeft, htCenter, htRight, htJustify
#' @param valign htTop, htMiddle, htBottom
#' @param renderer
#' @export
hot_col = function(hot, col, type, format = NULL, source = NULL,
                   strict = NULL, allowInvalcol = NULL,
                   readOnly = FALSE, validator = NULL,
                   halign = NULL, valign = NULL,
                   renderer = NULL) {
  cols = jsonlite::fromJSON(hot$x$columns, simplifyVector = FALSE)

  if (is.character(col)) col = which(hot$x$colHeaders == col)

  cols[[col]]$type = type
  cols[[col]]$format = format
  cols[[col]]$source = source
  cols[[col]]$strict = strict
  cols[[col]]$allowInvalcol = allowInvalcol
  cols[[col]]$readOnly = readOnly
  cols[[col]]$validator = validator

  className = c(halign, valign)
  if (!is.null(className)) {
    cols[[col]]$className = className
  }

  hot$x$columns = jsonlite::toJSON(cols, auto_unbox = TRUE)
  hot
}

#' Configure rows.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot rhandsontable object
#' @param rowHeights scalar, vector or JS()
#' @param fixedRowsTop
#' @export
hot_rows = function(hot, rowHeights = NULL, fixedRowsTop = NULL) {
  hot$x$rowHeights = rowHeights
  hot$x$fixedRowsTop = fixedRowsTop
  hot
}

#' Configure single cell.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot rhandsontable object
#' @param row
#' @param col
#' @param comment
#' @export
hot_cell = function(hot, row, col, comment = NULL) {
  hot$x$cell = jsonlite::toJSON(data.frame(row = row, col = col,
                                           comment = comment),
                                auto_unbox = TRUE)
  hot
}

#' Configure table.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot rhandsontable object
#' @param customBorders json See
#' \href{http://handsontable.com/demo/custom_borders.html}{Custom borders} for details.
#' @param contextMenu
#' @param groups json See
#' \href{http://handsontable.com/demo/grouping.html}{Grouping & ungrouping of rows and columns} for details.
#' @param highlightRow
#' @param highlightCol
#' @param wordWrap
#' @export
hot_table = function(hot, customBorders = NULL, contextMenu = TRUE,
                     groups = NULL, highlightRow = FALSE,
                     highlightCol = FALSE, wordWrap = TRUE) {
  hot$x$customBorders = customBorders
  hot$x$contextMenu = contextMenu
  hot$x$wordWrap = wordWrap

  if (highlightRow || highlightCol) hot$x$ishighlight = TRUE
  if (highlightRow) hot$x$currentRowClassName = "currentRow"
  if (highlightCol) hot$x$currentColClassName = "currentCol"

  hot
}

#' Add heatmap to table.  See
#' \href{http://handsontable.com/demo/heatmaps.html}{Heatmaps for values in a column}
#' for details.
#'
#' @param hot rhandsontable object
#' @param col
#' @param color_scale
#' @export
hot_heatmap = function(hot, col, color_scale) {
  cols = jsonlite::fromJSON(hot$x$columns, simplifyVector = FALSE)

  if (is.character(col)) col = which(hot$x$colHeaders == col)

  for (id in col) {
    cols[[id]]$renderer = "heatmapRenderer"
  }

  hot$x$isheatmap = TRUE
  hot$x$color_scale = color_scale

  hot$x$columns = jsonlite::toJSON(cols, auto_unbox = TRUE)
  hot
}

#' Add conditional formatting to column.  See
#' \href{http://handsontable.com/demo/conditional.html}{Conditional formatting}
#' for details.
#'
#' @param hot rhandsontable object
#' @param col
#' @param vals
#' @param styles
#' @export
hot_condformat = function(hot, col, vals, styles) {
  hot$x$condformat$vals = vals
  hot$x$condformat$styles = styles

  hot_col(hot = hot, col = col, renderer = "condformatRenderer")
}

#' Widget output function for use in Shiny
#'
#' @import htmlwidgets
#'
#' @export
rHandsontableOutput <- function(outputId, width = '100%', height = '100px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'rhandsontable', width, height,
                                 package = 'rhandsontable')
}

#' Widget render function for use in Shiny
#'
#' @import htmlwidgets
#'
#' @export
renderRHandsontable <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, rHandsontableOutput, env, quoted = TRUE)
}
