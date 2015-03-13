DATE_FORMAT = "%m/%d/%Y"

# Map R classes to handsontable.js types
get_col_types = function(data) {
  if (is.matrix(data))  {
    types = rep(typeof(data), ncol(data))
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

# Convert handsontable to R object
toR = function(data, changes, params, ...) {
  rClass = params$rClass
  rColClasses = unlist(params$rColClasses)
  colHeaders = unlist(params$colHeaders)
  rowHeaders = unlist(params$rowHeaders)

  out = jsonlite::fromJSON(data)

  # pre-conversion updates
  if (changes$event == "afterCreateRow") {
    # rename to numeric index
    rowHeaders = seq_len(nrow(out))
  } else if (changes$event == "afterRemoveRow") {
    inds = seq(changes$ind + 1, 1, length.out = changes$ct)
    rowHeaders = rowHeaders[-inds]
  } else if (changes$event == "afterCreateCol") {
    # find count of existing columns matching pattern
    ind_ct = length(which(grepl("V[0-9]{1,}", colHeaders)))
    # create new column names
    new_cols = paste0("V", changes$ct + ind_ct)
    # insert into vector
    inds = seq(changes$ind + 1, 1, length.out = changes$ct)
    colHeaders = c(colHeaders, new_cols)[
      order(c(seq_along(colHeaders), inds - 0.5))]
  } else if (changes$event == "afterRemoveCol") {
    # colHeaders already reflects removal
    if (rClass != "matrix") {
      inds = seq(changes$ind + 1, 1, length.out = changes$ct)
      rColClasses = rColClasses[-inds]
    }
  }

  # convert
  if (params$rClass[1] == "matrix") {
    class(out) = params$rColClasses
  } else if ("data.frame" %in% rClass) {
    out = colClasses(as.data.frame(out, stringsAsFactors = FALSE),
                     rColClasses)
  } else {
    stop("Conversion not implemented: ", rClass)
  }

  # post-conversion updates
  if (changes$event == "afterCreateRow") {
    # default logical NA in data.frame to FALSE
    if (rClass != "matrix") {
      inds_logical = which(rColClasses == "logical")
      for (i in inds_logical)
        out[[i]] = ifelse(is.na(out[[i]]), FALSE, out[[i]])
    }
  }

  colnames(out) = colHeaders
  rownames(out) = rowHeaders

  out
}

# Coerces data.frame columns to the specified classes
# see http://stackoverflow.com/questions/9214819/supply-a-vector-to-classes-of-dataframe
colClasses <- function(d, colClasses) {
  colClasses <- rep(colClasses, len=length(d))
  for(i in seq_along(d))
    d[[i]] = switch(colClasses[i],
                   Date = as.Date(d[[i]], origin='1970-01-01',
                                  format = DATE_FORMAT),
                   POSIXct = as.POSIXct(d[[i]], origin='1970-01-01',
                                        format = DATE_FORMAT),
                   as(d[[i]], colClasses[i]))
  d
}
