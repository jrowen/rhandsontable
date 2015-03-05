library(rHandsontable)

DF = data.frame(val = 1:10, bool = TRUE, nm = LETTERS[1:10],
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = F)
rHandsontable(DF)
