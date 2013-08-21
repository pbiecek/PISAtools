slashPlot <- function(factor1, factor2, variable, FUN = mean, ..., normalizeCnts = "row", symbols=LETTERS) {
  tabs <- unclass(by(variable, list(factor1, factor2), FUN, ...))
  cnts <- unclass(by(variable, list(factor1, factor2), length))
  cnts <- switch(normalizeCnts,
              row = t(apply(cnts, 1, function(x) x/max(x, na.rm=TRUE))),
                 col = apply(cnts, 2, function(x) x/max(x, na.rm=TRUE)),
                 all = cnts/max(cnts, na.rm=TRUE),
                 cnts)
  vecs <- unclass(by(variable, factor1, FUN, ...))
  
  plot(rep(0,length(vecs)), vecs, pch=16, las=1, xlim=range(tabs - matrix(vecs, nrow(tabs), ncol(tabs)), na.rm=TRUE), ylim=range(tabs, na.rm=TRUE), 
       yaxt="n", bty="n", xlab="", ylab="", type="n")
  abline(v=0)
  for (i in 1:nrow(tabs)) {
    for (j in i:ncol(tabs)) {
      lines(c(0, tabs[i,j] - vecs[i]), c(vecs[i], tabs[i,j]), col="#00000044")
      points( tabs[i,j] - vecs[i], tabs[i,j], pch=symbols[j],  cex=cnts[i,j])
    }
  }
  axis(4, vecs,  names(vecs), las=1)
  axis(2,las=1)
  legend("topleft", colnames(tabs), pch=symbols[1:ncol(tabs)], bty="n", cex=0.8)
}
