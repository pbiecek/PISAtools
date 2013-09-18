\name{calculateItemsPerformace}
\alias{calculateItemsPerformace}
\title{Calculate Item Performance}
\usage{
calculateItemsPerformace(itemNames, dataset, naLevels = c("7", "8"), cntVal = "CNT", weightVal = "W_FSTUWT") 
}
\description{
   Item average performance for every country
}
\author{
 Dataset created by OECD, transformed by Przemyslaw Biecek
}
\examples{
library(PISA2009lite)

weights <- student2003[,c("CNT", "SCHOOLID", "STIDSTD",  "W_FSTUWT", paste("W_FSTR", 1:80, sep=""))]
colnames(weights) <- toupper(colnames(weights))
colnames(scoredItem2003) <- toupper(colnames(scoredItem2003))
scoredItem2003$SCHOOLID <- as.character(scoredItem2003$SCHOOLID)
scoredItem2003$STIDSTD <- as.character(scoredItem2003$STIDSTD)

item2003weights <- merge(scoredItem2003, weights, by=c("CNT", "SCHOOLID", "STIDSTD"))

names2003 <- colnames(item2003weights)
names2003 <- gsub(names2003, pattern="[BD]$", replacement="T")

calculateItemsPerformace(names2003, item2003weights, cntVal="CNT")

}
\keyword{datasets}
