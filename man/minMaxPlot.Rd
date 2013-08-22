\name{minMaxPlot}
\alias{minMaxPlot}
\title{Plot for joint distribution of min/max of pairs (pairs like parental education, occulation status).}
\description{
Features like occupation or edducation are related to parent. Since most students have two parents one can ask how the lower/higer education/occupation status of these two affect the pupil performance. Here is where the minMaxPlot can be habdy.
}
\usage{
	minMaxPlot(factor1, factor2, variable, FUN = mean, ..., normalizeCnts = "none", vnames=c("lower", "higher", "average"))
}
\author{
 Przemyslaw Biecek
}
\examples{
library(PISA2009lite)
library(PISAtools)
sstudent2009 <- student2009[student2009$CNT == "Switzerland", ]

iscedFather <- 
  getISCED("ISCED level 6" = sstudent2009$ST15Q01, 
         "ISCED level 5A" = sstudent2009$ST15Q02, 
         "ISCED level 5B" = sstudent2009$ST15Q03, 
         "ISCED level 4" = sstudent2009$ST15Q04, 
         base = as.character(sstudent2009$ST14Q01))

iscedMother <- 
  getISCED("ISCED level 6" = sstudent2009$ST11Q01, 
         "ISCED level 5A" = sstudent2009$ST11Q02, 
         "ISCED level 5B" = sstudent2009$ST11Q03, 
         "ISCED level 4" = sstudent2009$ST11Q04, 
         base = as.character(sstudent2009$ST10Q01))

levord <- c("ISCED level 0", "ISCED level 1", "ISCED level 2", "ISCED level 3B, 3C", "ISCED level 3A", "ISCED level 4", "ISCED level 5B", "ISCED level 5A", "ISCED level 6")

iscedMother <- factor(iscedMother, levels = levord)
iscedFather <- factor(iscedFather, levels = levord)

iscMin <- factor(pmin(as.numeric(iscedMother), as.numeric(iscedFather)), labels = levord[sort(unique(pmax(as.numeric(iscedMother), as.numeric(iscedFather))))])
iscMax <- factor(pmax(as.numeric(iscedMother), as.numeric(iscedFather)), labels = levord[sort(unique(pmax(as.numeric(iscedMother), as.numeric(iscedFather))))])

minMaxPlot(iscMin, iscMax, sstudent2009[,c("PV1READ", "W_FSTUWT")], FUN=function(x) weighted.mean(x[,1], w=x[,2], na.rm=TRUE))

}
\keyword{datasets}
