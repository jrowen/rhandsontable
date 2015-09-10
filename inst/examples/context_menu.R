library(rhandsontable)

DF = data.frame(val = 1:10,
                bool = TRUE,
                big = LETTERS[1:10],
                small = factor(letters[1:10]),
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = FALSE)

rhandsontable(DF, search = TRUE) %>%
  hot_context_menu(
    customOpts = list(
      search = list(name = "Search",
                    callback = htmlwidgets::JS(
                      "function (key, options) {
                         var srch = prompt('Search criteria');

                         this.search.query(srch);
                         this.render();
                       }"))))

rhandsontable(DF) %>%
  hot_context_menu(
    customOpts = list(
      csv = list(name = "Download to CSV",
                    callback = htmlwidgets::JS(
                      "function (key, options) {
                         var csv = csvString(this);

                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }"))))
