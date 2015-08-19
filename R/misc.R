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
    if (grepl("factor", type)) return("factor")
  
    switch(type,
           integer="integer",
           double="numeric",
           numeric="numeric",
           character="text",
           logical="checkbox",
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

  out = data

  # copy/paste may add rows without firing an afterCreateRow event
  if (length(out) != length(rowHeaders))
    changes$event = "afterCreateRow"

  # pre-conversion updates; afterCreateCol moved to end of function
  if (changes$event == "afterCreateRow") {
    # rename to numeric index
    rowHeaders = genRowHeaders(length(out))
  } else if (changes$event == "afterRemoveRow") {
    inds = seq(changes$ind + 1, 1, length.out = changes$ct)
    rowHeaders = rowHeaders[-inds]
  } else if (changes$event == "afterRemoveCol") {
    # colHeaders already reflects removal
    if (!("matrix" %in% rClass)) {
      inds = seq(changes$ind + 1, 1, length.out = changes$ct)
      rColClasses = rColClasses[-inds]
    }
  }

  # convert
  if ("matrix" %in% rClass) {
    nr = length(out)
    out = unlist(out, recursive = FALSE)
    # replace NULL with NA
    out = unlist(lapply(out, function(x) if (is.null(x)) NA else x))
    out = matrix(out, nrow = nr, byrow = TRUE)
    class(out) = params$rColClasses
  } else if ("data.frame" %in% rClass) {
    nr = length(out)
    out = unlist(out, recursive = FALSE)
    # replace NULL with NA
    out = unlist(lapply(out, function(x) if (is.null(x)) NA else x))
    out = matrix(out, nrow = nr, byrow = TRUE)
    out = colClasses(as.data.frame(out, stringsAsFactors = FALSE),
                     rColClasses, params$columns)
  } else {
    stop("Conversion not implemented: ", rClass)
  }

  # post-conversion updates
  if (changes$event == "afterCreateRow") {
    # default logical NA in data.frame to FALSE
    if (!("matrix" %in% rClass)) {
      inds_logical = which(rColClasses == "logical")
      for (i in inds_logical)
        out[[i]] = ifelse(is.na(out[[i]]), FALSE, out[[i]])
    }
  }

  # copy/paste may add cols without firing an afterCreateCol event so check
  #   header length;
  if (ncol(out) != length(colHeaders))
    colHeaders = genColHeaders(changes, colHeaders)

  colnames(out) = colHeaders
  rownames(out) = rowHeaders

  out
}

# Coerces data.frame columns to the specified classes
# see http://stackoverflow.com/questions/9214819/supply-a-vector-to-classes-of-dataframe
#' @importFrom methods as
colClasses <- function(d, colClasses, cols) {
  colClasses <- rep(colClasses, len=length(d))
  for(i in seq_along(d))
    d[[i]] = switch(
      colClasses[i],
      Date = as.Date(d[[i]], origin='1970-01-01',
                     format = DATE_FORMAT),
      POSIXct = as.POSIXct(d[[i]], origin='1970-01-01',
                           format = DATE_FORMAT),
      factor = factor(d[[i]],
                      levels = c(unlist(cols[[i]]$source),
                                 unique(d[[i]][!(d[[i]] %in% unlist(cols[[i]]$source))])),
                      ordered = TRUE),
      json = jsonlite::toJSON(d[[i]]),
      as(d[[i]], colClasses[i]))
  d
}

genColHeaders <- function(changes, colHeaders) {
  ind_ct = length(which(grepl("V[0-9]{1,}", colHeaders)))
  # create new column names
  new_cols = paste0("V", changes$ct + ind_ct)
  # insert into vector
  inds = seq(changes$ind + 1, 1, length.out = changes$ct)
  c(colHeaders, new_cols)[order(c(seq_along(colHeaders), inds - 0.5))]
}

genRowHeaders <- function(ct) {
  seq_len(ct)
}
