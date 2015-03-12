#' See \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param data
#' @param colHeaders
#' @param rowHeaders
#' @param useTypes
#' @param readOnly
#' @param width
#' @param height
#' @export
rhandsontable <- function(data, colHeaders = NULL, rowHeaders = NULL, useTypes = TRUE,
                          readOnly = NULL, width = NULL, height = NULL) {
  if (is.null(colHeaders))
    colHeaders = colnames(data)
  if (is.null(rowHeaders))
    rowHeaders = rownames(data)

  if (!useTypes) {
    data = as.matrix(data, rownames.force = TRUE)
    cols = NULL
  }

  rClass = class(data)
  if (rClass == "matrix") {
    rColClasses = class(data[1, 1])
  } else {
    rColClasses = sapply(data, class)
  }

  if(useTypes) {
    # get column data types
    col_typs = get_col_types(data)
    cols = list(type = col_typs)
    cols$readOnly = readOnly

    # format date for display
    dt_inds = which(col_typs == "date")
    if (length(dt_inds) > 0L) {
      for (i in dt_inds)
        data[, i] = as.character(data[, i], format = DATE_FORMAT)
    }

    cols = jsonlite::toJSON(data.frame(do.call(cbind, cols)))
  }

  # removes _row from jsonlite::toJSON
  rownames(data) = NULL

  x = list(
    data = jsonlite::toJSON(data, na = "string"),
    rClass = rClass,
    rColClasses = rColClasses,
    colHeaders = colHeaders,
    rowHeaders = rowHeaders,
    columns = cols,
    width = width,
    height = height
  )

  # create widget
  htmlwidgets::createWidget(
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
#' @param ... passed to hot_col
#' @export
hot_cols = function(hot, columns = NULL, colWidths = NULL,
                    columnSorting = FALSE, manualColumnMove = FALSE,
                    manualColumnResize = FALSE, fixedColumnsLeft = NULL,
                    halign = NULL, valign = NULL, ...) {
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

  for (x in hot$x$colHeaders)
    hot = hot %>% hot_col(x, ...)

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
hot_col = function(hot, col, type = NULL, format = NULL, source = NULL,
                   strict = NULL, allowInvalcol = NULL,
                   readOnly = FALSE, validator = NULL,
                   halign = NULL, valign = NULL,
                   renderer = NULL) {
  cols = jsonlite::fromJSON(hot$x$columns, simplifyVector = FALSE)

  if (is.character(col)) col = which(hot$x$colHeaders == col)

  if (!is.null(type)) cols[[col]]$type = type
  cols[[col]]$format = format
  cols[[col]]$source = source
  cols[[col]]$strict = strict
  cols[[col]]$allowInvalcol = allowInvalcol
  cols[[col]]$readOnly = readOnly

  # jsonlite::toJSON doesn't currently handle JS
  if (!is.null(validator))
    hot$x$colValidator[[as.character(col - 1)]] = JS(validator)
  if (!is.null(renderer))
    hot$x$colRenderer[[as.character(col - 1)]] = JS(renderer)
  # if (!is.null(validator)) cols[[col]] = JS(validator)
  # if (!is.null(renderer)) cols[[col]] = JS(renderer)

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
#' @param contextMenu
#' @export
hot_table = function(hot, customBorders = NULL, contextMenu = TRUE,
                     groups = NULL, highlightRow = FALSE,
                     highlightCol = FALSE, wordWrap = TRUE) {
  hot$x$customBorders = customBorders
  hot$x$contextMenu = contextMenu
  hot$x$wordWrap = wordWrap
  hot$x$groups = groups

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
#' @param cols
#' @param color_scale
#' @param render
#' @export
hot_heatmap = function(hot, cols, color_scale, renderer = NULL) {
  if (is.null(renderer)) {
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
  }

  for (x in hot$x$colHeaders)
    hot = hot %>% hot_col(x, renderer = renderer)

  hot
}

#' Widget output function for use in Shiny
#'
#' @import htmlwidgets
#'
#' @export
rHandsontableOutput <- function(outputId, width = NA, height = NA){
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

#' Convert handsontable data to R object
#'
#' @param ...
#'
#' @export
hot_to_r = function(...) {
  do.call(toR, ...)
}
