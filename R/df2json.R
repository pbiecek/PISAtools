escapeIfNotNumber <- function(x) {
  if (is.null(dim(x))) {
    sapply(x, function(v) ifelse (class(v) == "numeric", v, paste0('"', v, '"')))
  } else {
    apply(x, 1:2, function(v) ifelse (class(v) == "numeric", v, paste0('"', v, '"')))
  }
}

df2json <- function(df) {
  vnam <- paste0('"', colnames(df), '":')
  rows <- apply(df, 1, function(x) paste0(vnam, escapeIfNotNumber(x), collapse= ", "))
  rows <- paste0("{", rows, "}")
  paste0("[", paste(rows, collapse=",\n"), "]")
}

