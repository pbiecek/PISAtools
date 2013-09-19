\name{calculateItemsPerformaceInGroups}
\alias{calculateItemsPerformaceInGroups}
\title{Calculate Item Performance In Groups}
\usage{
  calculateItemsPerformaceInGroups(itemNames, dataset, grVal, cntVal = "CNT", weightVal = "W_FSTUWT", naLevels = c("7", "8")) 
}
\description{
   Item average performance in groups for every country
}
\author{
 Przemyslaw Biecek
}
\examples{
library(PISA2009lite)

weights <- student2003[,c("CNT", "SCHOOLID", "STIDSTD", "ST03Q01", "W_FSTUWT", paste("W_FSTR", 1:80, sep=""))]
colnames(weights) <- toupper(colnames(weights))
colnames(scoredItem2003) <- toupper(colnames(scoredItem2003))
scoredItem2003$SCHOOLID <- as.character(scoredItem2003$SCHOOLID)
scoredItem2003$STIDSTD <- as.character(scoredItem2003$STIDSTD)

item2003weights <- merge(scoredItem2003, weights, by=c("CNT", "SCHOOLID", "STIDSTD"))

names2003 <- colnames(item2003weights)[11:145]
names2003 <- gsub(names2003, pattern="[BD]$", replacement="T")

res <- calculateItemsPerformaceInGroups(names2003, item2003weights, grVal = "ST03Q01", cntVal="CNT")

}
\keyword{datasets}
