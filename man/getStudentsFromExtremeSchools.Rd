\name{getStudentsFromExtremeSchools}
\alias{getStudentsFromExtremeSchools}
\alias{getExtremeSchools}
\title{Find schools or students from schools with most extreme average of given variable (like ESCS or PV1MATH)}
\usage{
  getExtremeSchools(values, weights, schoolid, cntid, quant=0.2, below=quant<0.5)

  getStudentsFromExtremeSchools(values, weights, schoolid, cntid, quant=0.2, below=quant<0.5)
}
\description{
  Find schools or students from schools with most extreme average of given variable (like ESCS or PV1MATH)
}
\references{
}
\author{
 Przemyslaw Biecek
}
\examples{
  library(PISA2009lite)
  sel <- student2009[student2009$OECD == "OECD",]
  # weathiest top10pct of schools
  head(getExtremeSchools(sel$ESCS, sel$W_FSTUWT, sel$SCHOOLID, sel$FAKECNT, 0.9))
  # students from weathiest top10pct of schools
  wsel <- sel[getStudentsFromExtremeSchools(sel$ESCS, sel$W_FSTUWT, sel$SCHOOLID, sel$FAKECNT, 0.1),]
 head(wsel[,1:20])

}
\keyword{datasets}
