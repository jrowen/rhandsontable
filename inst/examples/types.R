library(rhandsontable)

DF = data.frame(val = 1:10,
                bool = TRUE,
                big = LETTERS[1:10],
                small = factor(letters[1:10]),
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = FALSE)

rhandsontable(DF)

DF2 = data.frame(val = c(NA, 2:10),
                 bool = c(NA, rep(TRUE, 9)),
                 big = c(NA, LETTERS[1:9]),
                 small = c(NA, factor(letters[1:9])),
                 dt = c(NA, seq(from = Sys.Date(), by = "days", length.out = 9)),
                 dt_ch = c(NA, as.character(seq(from = Sys.Date(), by = "days", length.out = 9))),
                 stringsAsFactors = FALSE)

rhandsontable(DF2)
