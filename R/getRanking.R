# weights are single col
getWeightedAverages <- function(values, groups, weights = 1, sort = NA) {
  require(plyr)
  require(reshape2)
  df <- data.frame(values, weights, groups)
  mdf <- melt(df, id=c("groups","weights"), measure=colnames(df)[1:(ncol(df)-2)])
  rres <- ddply(mdf, .(groups, variable), summarize, avg = weighted.mean(value, weights, na.rm=T)) 
  tdf <- acast(rres, groups~variable, value.var="avg")
  if (!is.na(sort)) tdf <- tdf[order(tdf[,sort], decreasing=TRUE),,drop=FALSE]
  tdf
}

getRanking <- function(values, groups, weights = 1, sort = NA) {
  tdf <- getWeightedAverages(values, groups, weights)
  tdf <- nrow(tdf) + 1 - apply(tdf, 2, rank)
  if (!is.na(sort)) tdf <- tdf[order(tdf[,sort]),,drop=FALSE]
  tdf
}

