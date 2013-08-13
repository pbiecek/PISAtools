plotHTree <- function(level1, level2,  labels, sizes=rep(1, length(level2)), mar=c(2,5,20,2), plotit=rep(TRUE, length(level2))) {
    par(mar=mar)
    plot(seq_along(level2), level2, pch=19, cex=sizes, las=1, xaxt="n", xlab="", ylab="", main="", type="n", yaxt="n")
    axis(2, las=1, seq(round(min(level2, na.rm=TRUE), -1), max(level2, na.rm=TRUE), 10))
    axis(3, seq_along(labels), labels, las=2)

    for (i in seq_along(level1)) {
      tx <- grep(names(level2), pattern=paste("^", names(level1)[i], sep=""))
      lines(range(tx), level1[i]*c(1,1))
      for (txx in tx) {
        if (plotit[txx]) {
          lines(txx*c(1,1), c(level2[txx], level1[i]))
          points(txx, level2[txx], pch=19, cex=sizes[txx])
        }
      }
    }
 
}

