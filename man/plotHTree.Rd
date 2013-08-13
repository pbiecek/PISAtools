\name{plotHTree}
\alias{plotHTree}
\title{Performance vs. two level structured variable}
\description{
 Some variables, like occupations have natural hirarchical structure.

 Function plotHTree() allows to plot the performance against this structure.
}
\usage{
	plotHTree(level1, level2, labels, sizes=rep(1, length(level2)), mar=c(2,5,20,2), plotit=rep(TRUE, length(level2))) 
}
\author{
 Przemyslaw Biecek
}
\examples{
 library(PISA2009lite)
 # two and one digits out of parent occupation
 father <- substr(as.character(student2009$ST13Q01),1,2)
 mother <- substr(as.character(student2009$ST09Q01),1,2)
 father1 <- substr(as.character(student2009$ST13Q01),1,1)
 mother1 <- substr(as.character(student2009$ST09Q01),1,1)

 # names of groups
 groups <- sort(na.omit(unique(c(father, mother))))
 groups1 <- sort(na.omit(unique(c(father1, mother1))))

# sizes of two digits groups
sizes <- unclass(by(cbind(father, mother, student2009[,c("PV1MATH", "W_FSTUWT")]), 
           student2009$CNT, 
           function(x) {
             prop.table(sapply(groups, function(group) {
               sum(x[which(x[,1] == group | x[,2] == group),4], na.rm=TRUE)
             }))
           }))

# performance in two digits groups
perfMATH <- unclass(by(cbind(father, mother, student2009[,c("PV1MATH", "W_FSTUWT")]), 
           student2009$CNT, function(x) {
             sapply(groups, function(group) {
               inds <- which(x[,1] == group | x[,2] == group)
               weighted.mean(x[inds, 3], x[inds, 4], na.rm=TRUE)
             }) }))
# performance in one digit groups
perfMATH1 <- unclass(by(cbind(father1, mother1, student2009[,c("PV1MATH", "W_FSTUWT")]), 
           student2009$CNT, function(x) {
             sapply(groups1, function(group) {
               inds <- which(x[,1] == group | x[,2] == group)
               weighted.mean(x[inds, 3], x[inds, 4], na.rm=TRUE)
             }) }))

 plotHTree(level2 = perfMATH[["Poland"]], 
        level1 = perfMATH1[["Poland"]], 
 		sizes = sizes[["Poland"]] * 10, 
 		labels = ISCO88[paste(groups, "00", sep="")]) 
}
\keyword{datasets}
