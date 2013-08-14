getISCED <- function(..., base) {
  base <- sapply(strsplit(base, split="[<>]"), function(x) {
    if (is.na(x[1])) return(NA)
    if (substr(x[1], 1, 7) == "Did Not") return ("ISCED level 0")
    x[length(x)]
  })
  addList <- rev(list(...))  
  if (length(addList) > 0) {
    for (i in seq_along(addList)) 
      base <- ifelse(addList[[i]] == "Yes" & !is.na(addList[[i]]), names(addList)[i], base)
  }
  base
}
