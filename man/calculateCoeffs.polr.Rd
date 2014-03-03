\name{calculateCoeffs.polr}
\alias{calculateCoeffs.polr}
\title{Calculate coefficients and standard errors based on weights only for polr model}
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
