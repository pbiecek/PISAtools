\name{calculateCoeffs.polr}
\alias{calculateCoeffs.polr}
\title{Calculate coefficients and standard errors based on weights only for polr model}
\usage{
  calculateCoeffs.polr(model, datas, method="probit", weigths, fweigth)
}
\description{
  This function run polr model for diifferent weighting schemas, and then calculate the overall coefficients and standard errors
}
\references{
}
\author{
 Przemyslaw Biecek
}
\examples{
  weigths <- paste0("SPFWT",1:80)
  fweigth <- "SPFWT0"
  datas <- na.omit(pol[,c("I_Q06A", "PVLIT1", "PVNUM1", "GENDER_R", "AGE_R", "PARED", "EDCAT4", fweigth, weigths)])
  model <- I_Q06A ~ PVLIT1 + PVNUM1 + GENDER_R + AGE_R + PARED + EDCAT4
  
  calculateCoeffs.polr(model, datas, method="probit", weigths, fweigth) 
  
s
}
\keyword{datasets}
