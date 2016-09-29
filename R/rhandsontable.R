#' Handsontable widget
#'
#' Create a \href{http://handsontable.com}{Handsontable.js} widget.
#'
#' For full documentation on the package, visit \url{http://jrowen.github.io/rhandsontable/}
#' @param data a \code{data.table}, \code{data.frame} or \code{matrix}
#' @param colHeaders a vector of column names. If missing \code{colnames}
#'  will be used. Setting to \code{NULL} will omit.
#' @param rowHeaders a vector of row names. If missing \code{rownames}
#'  will be used. Setting to \code{NULL} will omit.
#' @param comments matrix or data.frame of comments; NA values are ignored
#' @param useTypes logical specifying whether column classes should be mapped to
#'  equivalent Javascript types.  Note that
#'  Handsontable does not support column add/remove when column types
#'  are defined (i.e. useTypes == TRUE in rhandsontable).
#' @param readOnly logical specifying whether the table is editable
#' @param selectCallback logical enabling the afterSelect event to return data.
#'  This can be used with shiny to tie updates to a selected table cell.
#' @param width numeric table width
#' @param height numeric table height
#' @param digits numeric passed to \code{jsonlite::toJSON}
#' @param debug numeric Javascript log level
#' @param ... passed to \code{hot_table} and to the \code{params} property of the widget
#' @examples
#' library(rhandsontable)
#' DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
#'                 small = letters[1:10],
#'                 dt = seq(from = Sys.Date(), by = "days", length.out = 10),
#'                 stringsAsFactors = FALSE)
#'
#' rhandsontable(DF, rowHeaders = NULL)
#' @seealso \code{\link{hot_table}}, \code{\link{hot_cols}}, \code{\link{hot_rows}}, \code{\link{hot_cell}}
#' @export
rhandsontable <- function(data, colHeaders, rowHeaders, comments = NULL,
                          useTypes = TRUE, readOnly = NULL,
                          selectCallback = FALSE,
                          width = NULL, height = NULL, digits = 4,
                          debug = NULL, ...) {
  rColHeaders = colnames(data)
  rRowHeaders = rownames(data)

  if (missing(colHeaders))
    colHeaders = colnames(data)
  if (missing(rowHeaders))
    rowHeaders = rownames(data)

  rClass = class(data)
  if ("matrix" %in% rClass) {
    rColClasses = class(data[1, 1])
  } else {
    rColClasses = lapply(data, class)
    rColClasses[grepl("factor", rColClasses)] = "factor"
  }

  if (!useTypes) {
    data = do.call(cbind, lapply(data, function(x) {
      if (class(x) == "Date")
        as.character(x, format = "%m/%d/%Y")
      else
        as.character(x)
    }))
    data = as.matrix(data, rownames.force = TRUE)
    cols = NULL
  } else {
    # get column data types
    col_typs = get_col_types(data)

    # format date for display
    dt_inds = which(col_typs == "date")
    if (length(dt_inds) > 0L) {
      for (i in dt_inds)
        data[, i] = as.character(data[, i], format = "%m/%d/%Y")
    }

    cols = lapply(seq_along(col_typs), function(i) {
      type = col_typs[i]
      if (type == "factor") {
#         data_fact = data.frame(level = levels(data[, i]),
#                                label = labels(data[, i]))
        res = list(type = "dropdown",
                   source = levels(data[, i]),
                   allowInvalid = FALSE
#                    handsontable = list(
#                      colHeaders = FALSE, #c("Label", "Level"),
#                      data = levels(data[, i]) #jsonlite::toJSON(data_fact, na = "string",
#                                               #rownames = FALSE)
#                    )
        )
      } else if (type == "numeric") {
        res = list(type = "numeric",
                   format = "0.00")
      } else if (type == "integer") {
        res = list(type = "numeric",
                   format = "0")
      } else if (type == "date") {
        res = list(type = "date",
                   correctFormat = TRUE,
                   dateFormat = "MM/DD/YYYY")
      } else {
        res = list(type = type)
      }
      res$readOnly = readOnly
      res$renderer = JS("customRenderer")
      res$default = NA
      res
    })
  }

  x = list(
    data = jsonlite::toJSON(data, na = "string", rownames = FALSE,
                            digits = digits),
    rClass = rClass,
    rColClasses = rColClasses,
    rColnames = as.list(colnames(data)),
    rColHeaders = rColHeaders,
    rRowHeaders = rRowHeaders,
    rDataDim = dim(data),
    selectCallback = selectCallback,
    colHeaders = colHeaders,
    rowHeaders = rowHeaders,
    columns = cols,
    width = width,
    height = height,
    debug = ifelse(is.null(debug) || is.na(debug) || !is.numeric(debug), 0, debug)
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
      defaultHeight = "100%",
      defaultWidth = "100%"
    )
  )

  if (!is.null(readOnly)) {
    for (x in hot$x$colHeaders)
      hot = hot %>% hot_col(x, readOnly = readOnly)
  }

  hot = hot %>% hot_table(enableComments = !is.null(comments), ...)

  if (!is.null(comments)) {
    inds = as.data.frame(which(!is.na(comments), arr.ind = TRUE))
    for (i in 1:nrow(inds))
      hot = hot %>%
        hot_cell(inds$row[i], inds$col[i],
                 comment = comments[inds$row[i], inds$col[i]])
    #hot$x$rComments = jsonlite::toJSON(comments)
  }

  hot
}

#' Handsontable widget
#'
#' Configure table.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot rhandsontable object
#' @param contextMenu logical enabling the right-click menu
#' @param stretchH character describing column stretching. Options are 'all', 'right',
#'  and 'none'. See \href{http://docs.handsontable.com/0.16.1/demo-stretching.html}{Column stretching} for details.
#' @param customBorders json object. See
#'  \href{http://handsontable.com/demo/custom_borders.html}{Custom borders} for details.
#' @param highlightRow logical enabling row highlighting for the selected
#'  cell
#' @param highlightCol logical enabling column highlighting for the
#'  selected cell
#' @param enableComments logical enabling comments in the table
#' @param overflow character setting the css overflow behavior. Options are
#'  auto (default), hidden and visible
#' @param rowHeaderWidth numeric width (in px) for the rowHeader column
#' @param ... passed to \href{http://handsontable.com}{Handsontable.js} constructor
#' @examples
#' library(rhandsontable)
#' DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
#'                 small = letters[1:10],
#'                 dt = seq(from = Sys.Date(), by = "days", length.out = 10),
#'                 stringsAsFactors = FALSE)
#'
#' rhandsontable(DF) %>%
#' hot_table(highlightCol = TRUE, highlightRow = TRUE)
#' @seealso \code{\link{rhandsontable}}
#' @export
hot_table = function(hot, contextMenu = TRUE, stretchH = "none",
                     customBorders = NULL, highlightRow = NULL,
                     highlightCol = NULL, enableComments = FALSE,
                     overflow = NULL, rowHeaderWidth = NULL, ...) {
  if (!is.null(stretchH)) hot$x$stretchH = stretchH
  if (!is.null(customBorders)) hot$x$customBorders = customBorders
  if (!is.null(enableComments)) hot$x$comments = enableComments
  if (!is.null(overflow)) hot$x$overflow = overflow
  if (!is.null(rowHeaderWidth)) hot$x$rowHeaderWidth = rowHeaderWidth

  if ((!is.null(highlightRow) && highlightRow) ||
      (!is.null(highlightCol) && highlightCol))
    hot$x$ishighlight = TRUE
  if (!is.null(highlightRow) && highlightRow)
    hot$x$currentRowClassName = "currentRow"
  if (!is.null(highlightCol) && highlightCol)
    hot$x$currentColClassName = "currentCol"

  if (!is.null(contextMenu) && contextMenu)
    hot = hot %>%
      hot_context_menu(allowComments = enableComments,
                       allowCustomBorders = !is.null(customBorders),
                       allowColEdit = is.null(hot$x$columns), ...)
  else
    hot$x$contextMenu = FALSE

  if (!is.null(list(...)))
    hot$x = c(hot$x, list(...))

  hot
}

#' Handsontable widget
#'
#' Configure the options for the right-click context menu.  See
#'  \href{http://docs.handsontable.com/0.16.1/demo-context-menu.html}{Context Menu} and
#'  \href{http://swisnl.github.io/jQuery-contextMenu/docs.html}{jquery contextMenu}
#'  for details.
#'
#' @param hot rhandsontable object
#' @param allowRowEdit logical enabling row editing
#' @param allowColEdit logical enabling column editing. Note that
#'  Handsontable does not support column add/remove when column types
#'  are defined (i.e. useTypes == TRUE in rhandsontable).
#' @param allowReadOnly logical enabling read-only toggle
#' @param allowComments logical enabling comments
#' @param allowCustomBorders logical enabling custom borders
#' @param customOpts list
#' @param ... ignored
#' @examples
#' library(rhandsontable)
#' DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
#'                 small = letters[1:10],
#'                 dt = seq(from = Sys.Date(), by = "days", length.out = 10),
#'                 stringsAsFactors = FALSE)
#'
#' rhandsontable(DF) %>%
#'   hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
#' @export
hot_context_menu = function(hot, allowRowEdit = TRUE, allowColEdit = TRUE,
                            allowReadOnly = FALSE, allowComments = FALSE,
                            allowCustomBorders = FALSE,
                            customOpts = NULL, ...) {
  if (!is.null(hot$x$contextMenu) && is.logical(hot$x$contextMenu) &&
      !hot$x$contextMenu)
    warning("The context menu was disabled but will be re-enabled (hot_context_menu)")

  if (!is.null(hot$x$colums) && allowColEdit)
    warning("Handsontable.js does not support column add/delete when column types ",
            "are defined.  Set useTypes = FALSE in rhandsontable to enable column ",
            "edits.")

  if (is.null(hot$x$contextMenu$items))
    opts = list()
  else
    opts = hot$x$contextMenu$items

  add_opts = function(new, old, val = list()) {
    new_ = lapply(new, function(x) {
      if (grepl("^hsep", x) && !is.null(val))
        return(list(name = "---------"))
      else
        return(val)
    })
    names(new_) = new
    if (length(old) > 0) {
      modifyList(old, new_)
    } else {
      new_
    }
  }
  remove_opts = function(new) {
    add_opts(new, opts, val = NULL)
  }

  if (!is.null(allowRowEdit) && allowRowEdit)
    opts =  add_opts(c("hsep1", "row_above", "row_below", "remove_row"), opts)
  else
    opts =  remove_opts(c("hsep1", "row_above", "row_below", "remove_row"))

  if (!is.null(allowColEdit) && allowColEdit)
    opts = add_opts(c("hsep2", "col_left", "col_right", "remove_col"), opts)
  else
    opts =  remove_opts(c("hsep2", "col_left", "col_right", "remove_col"))

  opts = add_opts(c("hsep3", "undo", "redo"), opts)

  opts = add_opts(c("hsep4", "alignment"), opts)

  if (!is.null(allowReadOnly) && allowReadOnly)
    opts = add_opts(c("hsep5", "make_read_only"), opts)
  else
    opts =  remove_opts(c("hsep5", "make_read_only"))

  if (!is.null(allowComments) && allowComments)
    opts = add_opts(c("hsep6", "commentsAddEdit", "commentsRemove"), opts)
  else
    opts =  remove_opts(c("hsep6", "commentsAddEdit", "commentsRemove"))

  if (!is.null(allowCustomBorders) && allowCustomBorders)
    opts = add_opts(c("hsep7", "borders"), opts)
  else
    opts =  remove_opts(c("hsep7", "borders"))

  sep_ct = 20
  if (!is.null(customOpts)) {
    opts[[paste0("hsep", sep_ct)]] = list(name = "---------")
    sep_ct = sep_ct + 1
    opts = modifyList(opts, customOpts)
  }

  if (grepl("^hsep", names(opts)[1]))
    opts = opts[-1]
  if (grepl("^hsep", names(opts)[length(opts)]))
    opts = opts[-length(opts)]

  hot$x$contextMenu = list(items = opts)

  hot
}

#' Handsontable widget
#'
#' Configure multiple columns.
#'
#' @param hot rhandsontable object
#' @param colWidths a scalar or numeric vector of column widths
#' @param columnSorting logical enabling row sorting. Sorting only alters the
#'  table presentation and the original dataset row order is maintained.
#'  The sorting will be done when a user click on column name
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
#' @seealso \code{\link{hot_col}}, \code{\link{hot_rows}}, \code{\link{hot_cell}}
#' @export
hot_cols = function(hot, colWidths = NULL, columnSorting = NULL,
                    manualColumnMove = NULL, manualColumnResize = NULL,
                    fixedColumnsLeft = NULL, ...) {
  if (!is.null(colWidths)) hot$x$colWidths = colWidths

  if (!is.null(columnSorting)) hot$x$columnSorting = columnSorting
  if (!is.null(manualColumnMove)) hot$x$manualColumnMove = manualColumnMove
  if (!is.null(manualColumnResize)) hot$x$manualColumnResize = manualColumnResize

  if (!is.null(fixedColumnsLeft)) hot$x$fixedColumnsLeft = fixedColumnsLeft

  for (i in seq_len(length(hot$x$columns)))
    hot = hot %>% hot_col(i, ...)

  hot
}

#' Handsontable widget
#'
#' Configure single column.
#'
#' @param hot rhandsontable object
#' @param col vector of column names or indices
#' @param type character specify the data type. Options include:
#'  numeric, date, checkbox, select, dropdown, autocomplete, password,
#'  and handsontable (not implemented yet)
#' @param format characer specifying column format. See Cell Types at
#'  \href{http://handsontable.com}{Handsontable.js} for the formatting
#'  options for each data type. Numeric columns are formatted using
#'  \href{http://numeraljs.com}{Numeral.js}.
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
#' @param copyable logical defining whether data in a cell can be copied using
#'  Ctrl + C
#' @param dateFormat character defining the date format. See
#'  \href{https://github.com/moment/moment}{Moment.js} for details.
#' @param default default column value for new rows (NA if not specified; shiny only)
#' @param language locale passed to \href{http://numbrojs.com}{Numbro.js};
#'  default is 'en-US'.
#' @param ... passed to handsontable
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
#' @seealso \code{\link{hot_cols}}, \code{\link{hot_rows}}, \code{\link{hot_cell}}
#' @export
hot_col = function(hot, col, type = NULL, format = NULL, source = NULL,
                   strict = NULL, readOnly = NULL, validator = NULL,
                   allowInvalid = NULL, halign = NULL, valign = NULL,
                   renderer = NULL, copyable = NULL, dateFormat = NULL,
                   default = NULL, language = NULL, ...) {
  cols = hot$x$columns
  if (is.null(cols)) {
    # create a columns list
    warning("rhandsontable column types were previously not defined but are ",
            "now being set to 'text' to support column properties")
    cols = lapply(hot$x$colHeaders, function(x) {
      list(type = "text")
    })
  }

  for (i in col) {
    if (is.character(i)) i = which(hot$x$colHeaders == i)

    if (!is.null(type)) cols[[i]]$type = type
    if (!is.null(format)) cols[[i]]$format = format
    if (!is.null(dateFormat)) cols[[i]]$dateFormat = dateFormat
    if (!is.null(source)) cols[[i]]$source = source
    if (!is.null(strict)) cols[[i]]$strict = strict
    if (!is.null(readOnly)) cols[[i]]$readOnly = readOnly
    if (!is.null(copyable)) cols[[i]]$copyable = copyable
    if (!is.null(default)) cols[[i]]$default = default
    if (!is.null(language)) cols[[i]]$language = language

    if (!is.null(validator)) cols[[i]]$validator = JS(validator)
    if (!is.null(allowInvalid)) cols[[i]]$allowInvalid = allowInvalid
    if (!is.null(renderer)) cols[[i]]$renderer = JS(renderer)

    if (!is.null(list(...)))
      cols[[i]] = c(cols[[i]], list(...))

    className = c(halign, valign)
    if (!is.null(className)) {
      cols[[i]]$className = className
    }
  }

  hot$x$columns = cols
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
#' @seealso \code{\link{hot_cols}}, \code{\link{hot_cell}}
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
#' @seealso \code{\link{hot_cols}}, \code{\link{hot_rows}}
#' @export
hot_cell = function(hot, row, col, comment = NULL) {
  cell = list(row = row - 1, col = col - 1, comment = comment)

  hot$x$cell = c(hot$x$cell, list(cell))

  hot = hot %>% hot_table(enableComments = TRUE)
#   if (is.null(hot$x$rComments)) {
#     cmts = matrix(nrow = hot$x$rDataDim[1], ncol = hot$x$rDataDim[2])
#   } else {
#     cmts = jsonlite::fromJSON(hot$x$rComments)
#   }
#   cmts[row, col] = comment
#   hot$x$rComments = jsonlite::toJSON(cmts)

  hot
}

#' Handsontable widget
#'
#' Add numeric validation to a column
#'
#' @param hot rhandsontable object
#' @param cols vector of column names or indices
#' @param min minimum value to accept
#' @param max maximum value to accept
#' @param choices a vector of acceptable numeric choices. It will be evaluated
#'  after min and max if specified.
#' @param exclude a vector of unacceptable numeric values
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
#' @seealso \code{\link{hot_validate_character}}
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
           return callback(true);
         }, 500)
       }"

  if (!is.null(exclude))
    ex_str = paste0("if ([",
                    paste0(paste0("'", exclude, "'"), collapse = ","),
                    "].indexOf(value) > -1) { return callback(false); }")
  else
    ex_str = ""
  f = gsub("%exclude", ex_str, f)

  if (!is.null(min))
    min_str = paste0("if (value < ", min, ") { return callback(false); }")
  else
    min_str = ""
  f = gsub("%min", min_str, f)

  if (!is.null(max))
    max_str = paste0("if (value > ", max, ") { return callback(false); }")
  else
    max_str = ""
  f = gsub("%max", max_str, f)

  if (!is.null(choices))
    chcs_str = paste0("if ([",
                      paste0(paste0("'", choices, "'"), collapse = ","),
                      "].indexOf(value) == -1) { return callback(false); }")
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
#' @param cols vector of column names or indices
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
#' @seealso \code{\link{hot_validate_numeric}}
#' @export
hot_validate_character = function(hot, cols, choices,
                                  allowInvalid = FALSE) {
  f = "function (value, callback) {
         setTimeout(function() {
           if (typeof(value) != 'string') {
             return callback(false);
           }
           %choices
           return callback(false);
         }, 500)
       }"

  ch_str = paste0("if ([",
                  paste0(paste0("'", choices, "'"), collapse = ","),
                  "].indexOf(value) > -1) { return callback(true); }")
  f = gsub("%choices", ch_str, f)

  for (x in cols)
    hot = hot %>% hot_col(x, validator = f,
                          allowInvalid = allowInvalid)

  hot
}

#' Handsontable widget
#'
#' Add heatmap to table.  See
#' \href{http://docs.handsontable.com/0.16.1/demo-chromajs.html}{Heatmaps for values in a column}
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
  hot$x$isHeatmap = TRUE

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
#' @param width,height must be a valid CSS unit in pixels
#'  or a number, which will be coerced to a string and have \code{"px"} appended.
#' @seealso \code{\link{renderRHandsontable}}
#' @export
rHandsontableOutput <- function(outputId, width = "100%", height = "100%"){
  htmlwidgets::shinyWidgetOutput(outputId, 'rhandsontable', width, height,
                                 package = 'rhandsontable')
}

#' Handsontable widget
#'
#' Shiny bindings for rhandsontable
#'
#' @param expr an expression that generates an rhandsontable.
#' @param env the environment in which to evaluate \code{expr}.
#' @param quoted is \code{expr} a quoted expression (with \code{quote()})? This
#'  is useful if you want to save an expression in a variable.
#' @seealso \code{\link{rHandsontableOutput}}, \code{\link{hot_to_r}}
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
#' @seealso \code{\link{rHandsontableOutput}}
#' @export
hot_to_r = function(...) {
  do.call(toR, ...)
}
