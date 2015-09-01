library(rhandsontable)

DF = data.frame(val = 1:10,
                bool = TRUE,
                big = LETTERS[1:10],
                small = factor(letters[1:10]),
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = FALSE)

rhandsontable(DF) %>%
  hot_context_menu(
    customOpts = list(
      search = list(name = "Search",
                    callback = htmlwidgets::JS(
                      "function (key, options) {
                         var srch = prompt('Search criteria');

                         instance.hot.search.query(srch);
                         instance.hot.render();
                       }"))))
