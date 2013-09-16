getExtremeSchools <- function(values, weights, schoolid, cntid, quant=0.2, below=quant<0.5) {
  schoolmeans <- unclass(by(cbind(values, weights),factor(paste0(cntid, ":", schoolid)), function(x) {
    weighted.mean(x[,1], x[,2], na.rm=TRUE)
  }))
  schoolweights <- unclass(by(cbind(values, weights),factor(paste0(cntid, ":", schoolid)), function(x) {
    sum(x[,2], na.rm=TRUE)
  }))
  require(Hmisc)
  quants <- wtd.quantile(schoolmeans, schoolweights, quant)
  if (below) {
    selected <- which(schoolmeans <= quants[1])
  } else {
    selected <- which(schoolmeans >= quants[1])
  }
  names(selected)
}

getStudentsFromExtremeSchools <- function(values, weights, schoolid, cntid, quant=0.2, below=quant<0.5) {
  cntschool <- getExtremeSchools(values, weights, schoolid, cntid, quant, below)
  which(paste0(cntid, ":", schoolid)  %in% cntschool)
}
