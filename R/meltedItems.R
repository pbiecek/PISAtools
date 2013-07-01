getMeltedItems <- function(scoredItem = scoredItem2009) {
  require(reshape2)
  measure.var <- grep(colnames(scoredItem), pattern="[A-Za-z][0-9][0-9][0-9]Q[0-9][0-9]")
  measure.var <- colnames(scoredItem)[measure.var]
  meltedScoredItems <- melt(scoredItem, id.var=c("CNT","COUNTRY","SCHOOLID", "STIDSTD", "BOOKID"), measure.var=measure.var, variable.name="itemName")
  meltedScoredItems <- meltedScoredItems[meltedScoredItems$value != "7",]
  meltedScoredItems
}
