escapeIfNotNumber <- function(x) {
  if (is.null(dim(x)) || min(dim(x))==1) {
    sapply(x, function(v) ifelse (class(v) == "numeric", v, paste0('"', v, '"')))
  } else {
    apply(x, 1:2, function(v) ifelse (class(v) == "numeric", v, paste0('"', v, '"')))
  }
}

df2json <- function(df) {
  vnam <- paste0('"', colnames(df), '":')
  rows <- sapply(1:nrow(df), function(i) paste0(vnam, escapeIfNotNumber(df[i,]), collapse= ", "))
  rows <- paste0("{", rows, "}")
  paste0("[", paste(rows, collapse=",\n"), "]")
}

