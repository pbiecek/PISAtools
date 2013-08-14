\name{getISCED}
\alias{getISCED}
\title{Calcualtion of ISCED level of parent}
\description{
 ISCED is coded by several variables. This function combines them int one variable.
}
\usage{
  getISCED(..., base)
}
\author{
 Przemyslaw Biecek
}
\examples{
library(PISA2009lite)
iscedFather <- 
  getISCED("ISCED level 6" = student2009$ST15Q01, 
         "ISCED level 5A" = student2009$ST15Q02, 
         "ISCED level 5B" = student2009$ST15Q03, 
         "ISCED level 4" = student2009$ST15Q04, 
         base = as.character(student2009$ST14Q01))
         
table(iscedFather)

iscedMother <- 
  getISCED("ISCED level 6" = student2009$ST11Q01, 
         "ISCED level 5A" = student2009$ST11Q02, 
         "ISCED level 5B" = student2009$ST11Q03, 
         "ISCED level 4" = student2009$ST11Q04, 
         base = as.character(student2009$ST10Q01))

table(iscedMother)
}
\keyword{datasets}
