#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
#' @import jsonlite
rHandsontable <- function(data, rownames = NULL, width = NULL, height = NULL) {

  col_typs = get_col_types(data)
  cols = jsonlite::toJSON(data.frame(type = col_typs))

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
    contextMenu = TRUE,
    columns = cols
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'rHandsontable',
    x,
    width = width,
    height = height,
    package = 'rHandsontable'
  )
}

#' Widget output function for use in Shiny
#'
#' @export
#' @import htmlwidgets
rHandsontableOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'rHandsontable', width, height,
                                 package = 'rHandsontable')
}

#' Widget render function for use in Shiny
#'
#' @export
#' @import htmlwidgets
renderRHandsontable <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, rHandsontableOutput, env, quoted = TRUE)
}
