#' See \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param data
#' @param colHeaders
#' @param rowHeaders
#' @param useTypes
#' @param readOnly
#' @param contextMenu
#' @param width
#' @param height
#' @export
rhandsontable <- function(data, colHeaders, rowHeaders, useTypes = TRUE,
                          readOnly = NULL, contextMenu = TRUE,
                          width = NULL, height = NULL) {
  if (missing(colHeaders))
    colHeaders = colnames(data)
  if (missing(rowHeaders))
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

  if (!is.null(contextMenu))
    hot = hot %>% hot_table(contextMenu = contextMenu)

  hot
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
#' @param ... passed to hot_col
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

#' Configure single column.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot rhandsontable object
#' @param col
#' @param type
#' @param format
#' @param source
#' @param strict
#' @param readOnly
#' @param validator
#' @param allowInvalid
#' @param halign htLeft, htCenter, htRight, htJustify
#' @param valign htTop, htMiddle, htBottom
#' @param renderer
#' @export
hot_col = function(hot, col, type = NULL, format = NULL, source = NULL,
                   strict = NULL,
                   readOnly = NULL, validator = NULL, allowInvalid = NULL,
                   halign = NULL, valign = NULL,
                   renderer = NULL) {
  cols = jsonlite::fromJSON(hot$x$columns, simplifyVector = FALSE)

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

  hot$x$columns = jsonlite::toJSON(cols, auto_unbox = TRUE,
                                   force = TRUE)
  hot
}

#' Add numeric validation to a column
#'
#' @param hot rhandsontable object
#' @param col
#' @param min
#' @param max
#' @param choices
#' @param exclude
#' @param allowInvalid
#' @export
hot_validate_numeric = function(hot, col, min = NULL, max = NULL,
                                choices = NULL, exclude = NULL,
                                allowInvalid = FALSE) {
  f = "function (value, callback) {
         setTimeout(function(){
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

  hot %>% hot_col(col, validator = gsub("\n", "", f),
                  allowInvalid = allowInvalid)
}

#' Add numeric validation to a column
#'
#' @param hot rhandsontable object
#' @param col
#' @param choices
#' @param allowInvalid
#' @export
hot_validate_character = function(hot, col, choices,
                                  allowInvalid = FALSE) {
  f = "function (value, callback) {
         setTimeout(function(){
           %choices
           callback(false);
         }, 500)
       }"

  ch_str = paste0("if ([",
                  paste0(paste0("'", choices, "'"), collapse = ","),
                  "].indexOf(value) > -1) { callback(true); }")
  f = gsub("%choices%", ch_str, f)

  hot %>% hot_col(col, validator = gsub("\n", "", f),
                  allowInvalid = allowInvalid)
}

#' Configure rows.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot rhandsontable object
#' @param rowHeights scalar, vector or JS()
#' @param fixedRowsTop
#' @export
hot_rows = function(hot, rowHeights = NULL, fixedRowsTop = NULL) {
  if (!is.null(rowHeights)) hot$x$rowHeights = rowHeights
  if (!is.null(fixedRowsTop)) hot$x$fixedRowsTop = fixedRowsTop
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
  cell = list(row = row, col = col, comment = comment)

  hot$x$cell = jsonlite::toJSON(c(hot$x$cell, list(cell)),
                                auto_unbox = TRUE)

  if (is.null(hot$x$comments))
    hot = hot %>% hot_table(comments = TRUE, contextMenu = TRUE)

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
#' @param comments
#' @export
hot_table = function(hot, customBorders = NULL, contextMenu = NULL,
                     groups = NULL, highlightRow = NULL,
                     highlightCol = NULL, wordWrap = NULL,
                     comments = NULL) {
  if (!is.null(customBorders)) hot$x$customBorders = customBorders
  if (!is.null(contextMenu)) hot$x$contextMenu = contextMenu
  if (!is.null(wordWrap)) hot$x$wordWrap = wordWrap
  if (!is.null(groups)) hot$x$groups = groups
  if (!is.null(comments)) hot$x$comments = comments

  if ((!is.null(highlightRow) && highlightRow) ||
        (!is.null(highlightCol) && highlightCol))
    hot$x$ishighlight = TRUE
  if (!is.null(highlightRow) && highlightRow) hot$x$currentRowClassName = "currentRow"
  if (!is.null(highlightCol) && highlightCol) hot$x$currentColClassName = "currentCol"

  hot
}

#' Add heatmap to table.  See
#' \href{http://handsontable.com/demo/heatmaps.html}{Heatmaps for values in a column}
#' for details.
#'
#' @param hot rhandsontable object
#' @param cols
#' @param color_scale
#' @param renderer
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

  if (missing(cols))
    cols = seq_along(hot$x$colHeaders)
  for (x in hot$x$colHeaders[cols])
    hot = hot %>% hot_col(x, renderer = renderer)

  hot
}

#' Widget output function for use in Shiny
#'
#' @import htmlwidgets
#' @param outputId
#' @param width
#' @param height
#' @export
rHandsontableOutput <- function(outputId, width = NA, height = NA){
  htmlwidgets::shinyWidgetOutput(outputId, 'rhandsontable', width, height,
                                 package = 'rhandsontable')
}

#' Widget render function for use in Shiny
#'
#' @import htmlwidgets
#' @param expr
#' @param env
#' @param quoted
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
