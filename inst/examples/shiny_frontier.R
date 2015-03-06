library(shiny)
library(rhandsontable)
library(lpSolve)
library(quadprog)
library(metricsgraphics)

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
           metricsgraphicsOutput("plot")
    )
  )
))

server = function(input, output) {
  values = reactiveValues()

  calc = reactive({
    # load initial values
    vol = c(0.1, 0.15, 0.07)
    ret = c(0.05, 0.07, 0.03)
    corr = matrix(c(1, 0.1, 0.3, 0.1, 1, 0.5, 0.3, 0.5, 1), nrow = 3)
    names(ret) <- names(vol) <- colnames(corr) <-
      rownames(corr) <- c(LETTERS[1:3])

    values[["ret_vol"]] = data.frame(Return = ret,
                                     Vol = vol)
    values[["corr"]] = corr

    cov = diag(vol) %*% corr %*% diag(vol)
    colnames(cov) <- rownames(cov) <- colnames(corr)

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
    if (!is.null(values[["ret_vol"]]))
      rhandsontable(values[["ret_vol"]],
                    rownames = names(values[["ret"]]))
  })

  output$hot_corr = renderRHandsontable({
    if (!is.null(values[["corr"]]))
      rhandsontable(as.data.frame(values[["corr"]]),
                    rownames = colnames(values[["corr"]]))
  })

  output$plot = renderMetricsgraphics({
    if (!is.null(calc())) {
      calc()$Frontier %>%
        mjs_plot(x = Risk, y = Return) %>%
        mjs_labs(x = "Risk", y = "Return")
    }
  })
}

shinyApp(ui = ui, server = server)
