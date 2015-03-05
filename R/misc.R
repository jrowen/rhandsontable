get_col_types = function(data) {
  if (is.matrix(data))  {
    types = typeof(data)
  } else if (is.data.frame(data)){
    types = as.character(lapply(data, class))
  } else{
    stop("Unsupported object type: ", class(data), " Can't extract column types.")
  }

  types <- sapply(types, function(type) {
    switch(type,
           integer="text",
           double="text",
           numeric="text",
           character="text",
           logical="checkbox",
           factor="text",
           Date="date",
           "text")
  })

  as.character(types)
}
