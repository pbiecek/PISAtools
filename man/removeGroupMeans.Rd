\name{removeGroupMeans}
\alias{removeGroupMeans}
\title{Remove group's (school's) weighted means}
\usage{
  removeGroupMeans(values, groupsID, weights = 1)
}
\description{
  This function calculate group wise averages and substract it from values.
}
\references{
}
\author{
 Przemyslaw Biecek
}
\examples{
  library(PISA2009lite)
  res <- removeGroupMeans(student2009$PV1MATH, student2009$SCHOOLID, student2009$W_FSTUWT)
  head(res)
}
\keyword{datasets}
