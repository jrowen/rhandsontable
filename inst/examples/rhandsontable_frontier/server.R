library(shiny)
library(rhandsontable)
library(lpSolve)
library(quadprog)
library(data.table)
library(ggplot2)

tkrs = c("MSFT", "CAT", "AXP", "DIS", "MMM")

# quantmod::getSymbols(tkrs, from = "2012-06-01", auto.assign=TRUE)
# returns = Reduce(function(x, y) merge(x, y), lapply(tkrs, get))
# returns = returns[, names(returns)[grepl("Close", names(returns))]]
# returns = data.table(Date = time(returns), coredata(returns))
# returns = melt(returns, id.vars = "Date", variable.name = "Name",
#                value.name = "Price")[order(Name, Date)]
# returns[, `:=`(Name = gsub(".Close", "", Name))]
# returns[, `:=`(Return = c(NA, Price[-1] / head(Price, -1) - 1)), by = Name]
# saveRDS(returns, "returns.rds")
returns = readRDS("returns.rds")
setkey(returns, Name)

shinyServer(function(input, output, session) {
  values = reactiveValues(
    hot_corr = cor(dcast.data.table(returns, Date ~ Name, value.var = "Return")[
      , !"Date", with = FALSE], use = "pairwise.complete.obs"),
    hot_retvol = returns[, list(Return = mean(Return, na.rm = TRUE),
                                Vol = sd(Return, na.rm = TRUE)), by = Name])

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
      head(seq(t(min_rsk$solution) %*% ret, t(max_ret$solution) %*% ret,
               length.out = 100), -1), function(x) {
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
      DF$Return = (1 + DF$Return) ^ 252 - 1
      DF$Risk = DF$Risk * sqrt(252)
      DF$Sharpe = DF$Return / DF$Risk
      ggplot(DF) +
        geom_line(aes(x = Risk, y = Return)) +
        geom_point(data = DF[which.max(DF$Sharpe),],
                   aes(x = Risk, y = Return), color = "red", size = 4) +
        scale_x_continuous(label = scales::percent) +
        scale_y_continuous(label = scales::percent) +
        theme_bw()
    }
  })
})
