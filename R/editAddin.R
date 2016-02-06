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
  fname = tempfile()

  # Generate UI for the gadget.
  ui <- miniPage(
    gadgetTitleBar("Edit a data.frame"),
    miniContentPanel(
      stableColumnLayout(
        textInput("data", "Data", value = defaultData)
      ),
      uiOutput("pending"),
      rHandsontableOutput("output")
    )
  )

  # Server code for the gadget.
  server <- function(input, output, session) {
    values = reactiveValues()
    setHot = function(x) values[["hot"]] = x

    reactiveData <- reactive({

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

    output$pending <- renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        h4(style = "color: #AA7732;", data$message)
    })

    output$output <- renderRHandsontable({
      data <- reactiveData()
      if (isErrorMessage(data))
        return(NULL)

      DF = hot_to_r(input$hot)

      setHot(DF)
      rhandsontable(DF) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    # Listen for 'done'.
    observeEvent(input$done, {

      # Emit a call to reload a rds
      if (nzchar(input$data)) {
        code <- paste("subset(", input$data, ", ", input$subset, ")", sep = "")
        rstudioapi::insertText(text = code)
      }

      if (nzchar(input$data) && !is.null(values[["hot"]])) {
        saveRDS(values[["hot"]], fname)
        code <- paste(input$data, " = readRDS(", fname, ")", sep = "")
        rstudioapi::insertText(text = code)
      }

      invisible(stopApp())
    })
  }

  # Use a modal dialog as a viewr.
  viewer <- dialogViewer("Edit", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)

}
