weighted.bagplot <- function(x, y, weights, resolution = 10) {
  ind1 <- which(weights < resolution)
  tind <- which(weights >= resolution)
  ind2 <- rep(tind, round(weights[tind]/10))
  ind <- c(ind1, ind2)
  
  bagpl <- compute.bagplot(x[ind], y[ind])
  bagpl$hull.center = NULL
  bagpl
}

compareBagPlots <- function(bp.selected, bp.reference, col.selected="#55555555", col.reference="#ffffff00", ...) {
  par(lty=2, lwd=2)
  plot(bp.reference, add=FALSE,
       show.outlier = FALSE, show.whiskers = FALSE, show.looppoints = FALSE, 
       show.loophull = FALSE, show.bagpoints = FALSE, col.baghull=col.reference, las=1, ...)
  par(lty=0, lwd=1)
  plot(bp.selected, add=TRUE,
       show.outlier = FALSE, show.whiskers = FALSE, show.looppoints = FALSE, 
       show.loophull = FALSE, show.bagpoints = FALSE, col.baghull=col.selected, las=1, ...)
  abline(h=0, col="grey", lty=2)
  abline(v=0, col="grey", lty=2)
}
