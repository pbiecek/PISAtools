minMaxPlot <- function(factor1, factor2, variable, FUN = mean, ..., normalizeCnts = "row", vnames=c("lower", "higher", "average")) {
  tabs <- unclass(by(variable, list(factor1, factor2), FUN, ...))
  cnts <- unclass(by(variable, list(factor1, factor2), function(x) if (is.null(dim(x))) length(x) else dim(x)[1] ))
  cnts <- switch(normalizeCnts,
              row = t(apply(cnts, 1, function(x) x/max(x, na.rm=TRUE))),
                 col = apply(cnts, 2, function(x) x/max(x, na.rm=TRUE)),
                 all = cnts/max(cnts, na.rm=TRUE),
                 cnts)
  
  long <- na.omit(cbind(as.data.frame(as.table(tabs)), cnt=cut(as.data.frame(as.table(cnts))[,3], 10^(0:7))))
  long$Var1 <- factor(long$Var1, levels = rev(levels(long$Var1)))
  
  require(ggplot2)
  ggplot(data = na.omit(long),
         aes(x = Var2, y = Freq, shape=cnt, colour = Var1, group=Var1)) +
    stat_summary(fun.y=mean, geom="point", size=5) +
    stat_summary(fun.y=mean, geom="line", size=2) + 
    xlab(vnames[1]) + ylab(vnames[3]) + scale_color_brewer(palette="Spectral", name=vnames[2]) + theme_bw()
}
