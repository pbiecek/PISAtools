minMaxPlot <- function(factor1, factor2, variable, FUN = mean, ..., normalizeCnts = "row", symbols=LETTERS) {
  tabs <- unclass(by(variable, list(factor1, factor2), FUN, ...))
  cnts <- unclass(by(variable, list(factor1, factor2), function(x) if (is.null(dim(x))) length(x) else dim(x)[1] ))
  cnts <- switch(normalizeCnts,
              row = t(apply(cnts, 1, function(x) x/max(x, na.rm=TRUE))),
                 col = apply(cnts, 2, function(x) x/max(x, na.rm=TRUE)),
                 all = cnts/max(cnts, na.rm=TRUE),
                 cnts)
  vecs <- unclass(by(variable, factor1, FUN, ...))
  
  plot(tabs - matrix(vecs, nrow(tabs), ncol(tabs)), tabs, las=1, yaxt="n", bty="n", xlab="", ylab="", type="n")
  abline(v=0)
  for (i in 1:nrow(tabs)) {
    for (j in 1:ncol(tabs)) {
      if (!is.na(tabs[i,j])) {
        lines(c(0, tabs[i,j] - vecs[i]), c(vecs[i], tabs[i,j]), col="#00000044")
        points( tabs[i,j] - vecs[i], tabs[i,j], pch=symbols[j],  cex=cnts[i,j])
      }
    }
  }
  axis(4, vecs,  names(vecs), las=1)
  axis(2,las=1)
  legend("topleft", colnames(tabs), pch=symbols[1:ncol(tabs)], bty="n", cex=0.8)
}
