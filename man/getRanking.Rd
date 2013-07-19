\name{getRanking}
\alias{getRanking}
\alias{getWeightedAverages}
\title{Country ranking}
\description{
  The getRanking(values, groups, weights = 1, sort = NA) function generates ranking of countries
  values should correspond to one of performance variables, like PV1MATH [it might a table with different characteristics in different collumns]
  groups should correspond to the country ids like CNT
  weights - here you can supply weights
  sort - if it's an integer you can select a column and rows will be sorted according to given collumn
}
\references{
  http://pisa2009.acer.edu.au/downloads.php
}
\author{
 Dataset created by OECD, transformed by Przemyslaw Biecek
}
\examples{
library(PISA2009lite)
getWeightedAverages(student2009[,"PV1MATH"], student2009$CNT, student2009$W_FSTUWT)

getRanking(student2009[,c("PV1MATH", "PV1READ")], student2009$CNT, student2009$W_FSTUWT, sort=2)
}
\keyword{datasets}
