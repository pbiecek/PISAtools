# weights are single col
getWeightedAverages <- function(values, groups, weights = 1) {
  unclass(by(cbind(values, weights), groups, 
             function(x) apply(x[,-ncol(x)], 2, 
                               function(y) weighted.mean(y, x[,ncol(x)]))))
}

getRanking <- function(values, groups, weights = 1) {
  apply(getWeightedAverages(values, groups, weights), 2, rank)
}

