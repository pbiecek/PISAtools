# weights are single col
getWeightedAverages <- function(values, groups, weights = 1) {
  require(plyr)
  require(reshape2)
  df <- cbind(values, weights, groups)
  mdf <- melt(df, id=c("groups","weights"), measure=colnames(df)[1:(ncol(df)-2)])
  rres <- ddply(mdf, .(groups, variable), summarize, avg = weighted.mean(value, weights, na.rm=T)) 
  acast(rres, groups~variable, value.var="avg")
}

getRanking <- function(values, groups, weights = 1) {
  apply(getWeightedAverages(values, groups, weights), 2, rank)
}

