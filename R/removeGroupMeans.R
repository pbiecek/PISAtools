removeGroupMeans <- function(values, groupsID, weights = 1) {
  names(values) <- groupsID
  gmeans <- by(cbind(values, weights), groupsID, function(x) weighted.mean(x[,1], x[,2], na.rm=TRUE))
  values - gmeans[as.character(groupsID)]
}
