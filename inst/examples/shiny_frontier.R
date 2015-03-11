library(shiny)
library(rhandsontable)
library(lpSolve)
library(quadprog)
library(ggplot2)

ui = shinyUI(fluidPage(

  titlePanel("Efficient Frontier"),

  fluidRow(
    column(3,
           rHandsontableOutput("hot_retvol")
    ),
    column(3,
           rHandsontableOutput("hot_corr")
    )
  ),
  fluidRow(
    column(6,
           plotOutput("plot")
    )
  )
))

server = function(input, output) {
  values = reactiveValues(
    hot_corr = matrix(
      c(1, 0.1, 0.3, 0.1, 1, 0.5, 0.3, 0.5, 1), nrow = 3,
      dimnames = list(LETTERS[1:3], LETTERS[1:3])),
    hot_retvol = data.frame(Return = c(0.05, 0.07, 0.03),
                            Vol = c(0.1, 0.15, 0.07),
                            row.names = LETTERS[1:3]))

  calc = reactive({
    # load initial values
    vol = values[["hot_retvol"]]$Vol
    ret = values[["hot_retvol"]]$Return
    corr = values[["hot_corr"]]

    cov = diag(vol) %*% corr %*% diag(vol)

    # min wt
    n = length(ret)
    mat = diag(n)
    dir = rep(">=", n)
    rhs = rep(0, n)

    # max wt
    mat = rbind(mat, -diag(n))
    dir = c(dir, rep(">=", n))
    rhs = c(rhs, rep(-1, n))

    # sum wt
    mat = rbind(rep(1, n), mat)
    dir = c("=", dir)
    rhs = c(1, rhs)
    meq = 1

    max_ret = lp(direction = "max",
                 objective.in = ret,
                 const.mat = mat,
                 const.dir = dir,
                 const.rhs = rhs)
    min_rsk = solve.QP(Dmat = cov,
                       dvec = rep(0, n),
                       Amat = t(mat),
                       bvec = rhs,
                       meq = meq,
                       factorized = FALSE)

    frntr.mat = rbind(ret, mat)
    frntr.meq = meq + 1
    frntr = lapply(
      seq(t(min_rsk$solution) %*% ret, t(max_ret$solution) %*% ret,
          length.out = 100), function(x) {
            sol = solve.QP(Dmat = cov,
                           dvec = rep(0, n),
                           Amat = t(frntr.mat),
                           bvec = c(x, rhs),
                           meq = frntr.meq,
                           factorized = FALSE)
            data.frame(Return = x,
                       Risk = sqrt(t(sol$solution) %*% cov %*% sol$solution))
          })
    frntr = do.call(rbind, frntr)

    list(Covariance = cov,
         `Max Return` = max_ret$solution,
         `Min Risk` = min_rsk$solution,
         Frontier = frntr)
  })

  output$hot_retvol = renderRHandsontable({
    if (!is.null(input$hot_retvol)) {
      DF = hot_to_r(input$hot_retvol)
      values[["hot_retvol"]] = DF
      rhandsontable(DF)
    } else if (!is.null(values[["hot_retvol"]])) {
      DF = values[["hot_retvol"]]
      rhandsontable(DF)
    }
  })

  output$hot_corr = renderRHandsontable({
    if (!is.null(input$hot_corr)) {
      DF = hot_to_r(input$hot_corr)
      DF[upper.tri(DF)] = DF[lower.tri(DF)]
      values[["hot_corr"]] = DF
      DF[upper.tri(DF)] = NA
      rhandsontable(DF)
    } else if (!is.null(values[["hot_corr"]])) {
      DF = values[["hot_corr"]]
      DF[upper.tri(DF)] = NA
      rhandsontable(DF)
    }
  })

  output$plot = renderPlot({
    if (!is.null(calc())) {
      DF = calc()$Frontier
      DF$Sharpe = DF$Return / DF$Risk
      ggplot(calc()$Frontier) +
        geom_line(aes(x = Risk, y = Return)) +
        geom_point(data = DF[which.max(DF$Sharpe),],
                   aes(x = Risk, y = Return), color = "red", size = 4)
    }
  })
}

shinyApp(ui = ui, server = server)
