extract.data <- function(txt.file, sas.file){
  x <- parse.SAScii(sas.file)
  dat <- read.fwf(txt.file,widths=abs(x$width))
  names(dat) <- x$varname
  dat
}
extract.vardict <- function(sas.file){
  lines <- readLines(sas.file)
  lines <- paste(lines, collapse="\n")
  chunks <- strsplit(lines, split=";[ \n]*[Ll][Aa][Bb][Ee][Ll][ \n]*", useBytes=TRUE)[[1]]
  chunks <- strsplit(chunks[2], split=";[ \n]*[Ff][Oo][Rr][Mm][Aa][Tt][ \n]*", useBytes=TRUE)[[1]][1]
  tab <- read.table(textConnection(chunks), sep="=", stringsAsFactors=FALSE)
  tab[,1] <- gsub(gsub(tab[,1], pattern="^ +", replacement=""), pattern=" +$", replacement="")
  tab[,2] <- gsub(gsub(tab[,2], pattern="^ +", replacement=""), pattern=" +$", replacement="")
  tab2 <- tab[,2]
  names(tab2) <- tab[,1]
  tab2
}
extract.dict <- function(sas.file){
  lines <- readLines(sas.file)
  lines <- paste(lines, collapse="\n")
  chunks <- strsplit(lines, split=";[ \n]*[Vv][Aa][Ll][Uu][Ee][ \n]*", useBytes=TRUE)[[1]]
  chunks <- chunks[2:(length(chunks)-1)]
  res <- list()
  for (i in seq_along(chunks)) {
    vn <- gsub(strsplit(chunks[i], split="[ \n]", useBytes=TRUE)[[1]][1], pattern="\\$", replacement="")
    tmp <- gsub(chunks[i], pattern="^[^\n ]*[\n ]*", replacement="")
    tab <- read.table(textConnection(tmp), sep="=", stringsAsFactors=FALSE)
    tab2 <- tab[,2]
    names(tab2) <- tab[,1]
    res[[vn]] <- tab2
  }
  res
}
