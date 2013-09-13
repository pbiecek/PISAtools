\name{weighted.bagplot}
\alias{weighted.bagplot}
\alias{compareBagPlots}
\title{Compare two bagplots}
\usage{
  weighted.bagplot(x, y, weights, resolution = 10) 

  compareBagPlots(bp.selected, bp.reference, col.selected="#55555555", col.reference="#ffffff", ...)
}
\description{
  This function compares two bagplots.
}
\references{
}
\author{
 Przemyslaw Biecek
}
\examples{
  library(PISA2009lite)
  sel <- student2009[student2009$OECD == "OECD",]
  rPV <- removeGroupMeans(sel$PV1MATH, factor(paste(sel$CNT, sel$SCHOOLID)), sel$W_FSTUWT)
  rES <- removeGroupMeans(sel$ESCS, factor(paste(sel$CNT, sel$SCHOOLID)), sel$W_FSTUWT)
  rBagPlot <- weighted.bagplot(rPV, rES, sel$W_FSTUWT)

  sel <- student2009[student2009$CNT == "Poland",]
  pPV <- removeGroupMeans(sel$PV1MATH, factor(paste(sel$CNT, sel$SCHOOLID)), sel$W_FSTUWT)
  pES <- removeGroupMeans(sel$ESCS, factor(paste(sel$CNT, sel$SCHOOLID)), sel$W_FSTUWT)
  pBagPlot <- weighted.bagplot(pPV, pES, sel$W_FSTUWT)

  compareBagPlots(rBagPlot, pBagPlot, 
        xlim=c(-100,100), ylim=c(-1,1), xlab="PV1MATH diff", ylab="ESCS diff")
}
\keyword{datasets}
