#' See \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param data a \code{data.table}, \code{data.frame} or \code{matrix}
#' @param colHeaders a vector of column names. If missing \code{colnames}
#'  will be used. Setting to \code{NULL} will omit.
#' @param rowHeaders a vector of row names. If missing \code{rownames}
#'  will be used. Setting to \code{NULL} will omit.
#' @param useTypes logical specifying whether column classes should be mapped to
#'  equivalent Javascript types
#' @param readOnly logical specifying whether the table is editable
#' @param contextMenu passed to \code{hot_table}
#' @param width numeric table width
#' @param height numeric table height
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

#' Configure multiple columns.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
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
#' @export
hot_col = function(hot, col, type = NULL, format = NULL, source = NULL,
                   strict = NULL,
                   readOnly = NULL, validator = NULL, allowInvalid = NULL,
                   halign = NULL, valign = NULL,
                   renderer = NULL) {
  cols = hot$x$columns

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

#' Add numeric validation to a column
#'
#' @param hot rhandsontable object
#' @param col numeric column index
#' @param min minimum value to accept
#' @param max maximum value to accept
#' @param choices a vector of acceptable numeric choices. It will be evaluated
#'  after min and max if specified.
#' @param exclude a vector or unacceptable numeric values
#' @param allowInvalid logical specifying whether invalid data will be
#'  accepted. Invalid data cells will be color red.
#' @export
hot_validate_numeric = function(hot, col, min = NULL, max = NULL,
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

  hot %>% hot_col(col, validator = gsub("\n", "", f),
                  allowInvalid = allowInvalid)
}

#' Add numeric validation to a column
#'
#' @param hot rhandsontable object
#' @param col numeric column index
#' @param choices a vector of acceptable numeric choices. It will be evaluated
#'  after min and max if specified.
#' @param allowInvalid logical specifying whether invalid data will be
#'  accepted. Invalid data cells will be color red.
#' @export
hot_validate_character = function(hot, col, choices,
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

  hot %>% hot_col(col, validator = gsub("\n", "", f),
                  allowInvalid = allowInvalid)
}

#' Configure rows.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot rhandsontable object
#' @param rowHeights a scalar or numeric vector of row heights
#' @param fixedRowsTop a numeric vector indicating which rows should be
#'  frozen on the top
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
#' @param row numeric row index
#' @param col numeric column index
#' @param comment character comment to add to cell
#' @export
hot_cell = function(hot, row, col, comment = NULL) {
  cell = list(row = row, col = col, comment = comment)

  hot$x$cell = c(hot$x$cell, list(cell))

  if (is.null(hot$x$comments))
    hot = hot %>% hot_table(comments = TRUE, contextMenu = TRUE)

  hot
}

#' Configure table.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot rhandsontable object
#' @param customBorders json object. See
#'  \href{http://handsontable.com/demo/custom_borders.html}{Custom borders} for details.
#' @param contextMenu logical enabling the right-click menu
#' @param groups json object. See
#'  \href{http://handsontable.com/demo/grouping.html}{Grouping & ungrouping of rows and columns} for details.
#' @param highlightRow logical enabling row highlighting for the selected
#'  cell
#' @param highlightCol logical enabling column highlighting for the
#'  selected cell
#' @param wordWrap logical enabling word wrap for the table
#' @param comments logical enabling comments in the table, including the
#'  corresponding options in the right-click menu. User comments are not
#'  currently returned to R.
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
#' @param cols numeric vector of columns to include in the heatmap. If missing
#'  all columns are used.
#' @param color_scale character vector that includes the lower and upper
#'  colors
#' @param renderer character defining a Javascript function to be used
#'  to determine the cell colors. If missing,
#'  \code{rhandsontable:::renderer_heatmap} is used.
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

#' Convert handsontable data to R object. Can be used in a \code{shiny} app
#'  to convert the input json to an R dataset.
#'
#' @param ... passed to \code{rhandsontable:::toR}
#'
#' @export
hot_to_r = function(...) {
  do.call(toR, ...)
}
