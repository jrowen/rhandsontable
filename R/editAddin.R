#' Edit a Data Frame.
#'
#' Interactively edit a \code{data.frame} or \code{data.table}. The resulting
#' code will be emitted as a call to reload the data from a temp RDS file.
#'
#' This addin can be used to interactively edit.
#' The intended way to use this is as follows:
#'
#' 1. Highlight a symbol naming a \code{data.frame} or \code{data.table}
#'    in your R session, e.g. \code{mtcars}.
#' 2. Execute this addin, to interactively edit it.
#'
#' When you're done, the code performing this operation will be emitted
#' at the cursor position.
#'
#' This function borrows heavily from \href{rstudio/addinexamples/subsetAddin}{https://github.com/rstudio/addinexamples/blob/master/R/subsetAddin.R}
#'
#' @export
editAddin <- function() {

  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text

  # create a temp file
  fname = gsub("\\\\", "/", tempfile())

  # Generate UI for the gadget.
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Edit a data.frame"),
    miniUI::miniContentPanel(
      stableColumnLayout(
        shiny::textInput("data", "Data", value = defaultData),
        shiny::radioButtons("outType", "Output type",
                            choices = c("Update original data" = "update",
                                        "Print updates to script (no update)" = "print")
        )
      ),
      shiny::uiOutput("pending"),
      rHandsontableOutput("hot")
    )
  )

  # Server code for the gadget.
  server <- function(input, output, session) {
    values = shiny::reactiveValues()
    setHot = function(x) values[["hot"]] = x

    reactiveData <- shiny::reactive({

      # Collect inputs.
      dataString <- input$data

      # Check to see if there is data called 'data',
      # and access it if possible.
      if (!nzchar(dataString))
        return(errorMessage("data", "No dataset available."))

      if (!exists(dataString, envir = .GlobalEnv))
        return(errorMessage("data", paste("No dataset named '", dataString, "' available.")))

      data <- get(dataString, envir = .GlobalEnv)

      data
    })

    output$pending <- shiny::renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        htmltools::h4(style = "color: #AA7732;", data$message)
    })

    output$hot <- renderRHandsontable({
      data <- reactiveData()
      if (isErrorMessage(data))
        return(NULL)

      if (is.null(input$hot))
        DF = data
      else
        DF = hot_to_r(input$hot)

      setHot(DF)
      rhandsontable(DF) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    # Listen for 'done'.
    shiny::observeEvent(input$done, {

      if (input$outType == "print") {
        rslt <- capture.output(dput(values[["hot"]]))
        rstudioapi::insertText(Inf, paste0(input$data, " = ",
                                           paste(rslt, collapse = "\n")))
      } else {
        if (nzchar(input$data) && !is.null(values[["hot"]])) {
          saveRDS(values[["hot"]], fname)
          code <- paste(input$data, " = readRDS('", fname, "')", sep = "")
          rstudioapi::sendToConsole(code)
        }
      }

      invisible(shiny::stopApp())
    })
  }

  # Use a modal dialog as a viewr.
  viewer <- shiny::dialogViewer("Edit", width = 1000, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)

}

# these functions come from rstudio/addinexamples
stableColumnLayout <- function(...) {
  dots <- list(...)
  n <- length(dots)
  width <- 12 / n
  class <- sprintf("col-xs-%s col-md-%s", width, width)
  shiny::fluidRow(
    lapply(dots, function(el) {
      shiny::div(class = class, el)
    })
  )
}

isErrorMessage <- function(object) {
  inherits(object, "error_message")
}

errorMessage <- function(type, message) {
  structure(
    list(type = type, message = message),
    class = "error_message"
  )
}
