plotFlatHtree <- function(flatHtree, x, y, size, label, color, range) {
  bp <- ggplot(aes_string(x = x, y = y, size = size, label=label, color = color), data=flatHtree)
  bp + geom_point() + theme_bw() +  scale_color_brewer(palette="RdYlBu") +
    geom_text(size=4, hjust=0, vjust=0.5, x=flatHtree[,x] + 0.1) + 
    scale_x_continuous(limits = c(0, 3.2)) + 
    scale_y_continuous("", limits = c(range[1], range[2])) + 
    theme(plot.title = element_text(face="bold", size=14), 
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(face="bold", size=12, angle=90),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          legend.position = "none", 
          legend.title = element_blank(), 
          legend.text = element_text(size=12),
          panel.border = element_rect(linetype = "dotted", colour = "white"),
          legend.key = element_blank() 
    )
}

plotSlopeHtree <- function(val1, val2, lab1, lab2, col1, col2, rang=range(c(val1, val2), na.rm=TRUE)) {
  flatHtree <- rbind(
              data.frame(cnt = 0, avg = val1, lab= lab1, color=col1),
              data.frame(cnt = 1, avg = val2, lab= lab2, color=col2))
  
  ggplot(data = flatHtree, aes(x = cnt, y = avg, group=lab, color=color)) + 
    geom_line() + 
    geom_text(aes(label = lab, x=cnt*1.4 - 0.2 , hjust = 1-cnt, size=0.5)) +
    theme_bw()+
    scale_color_brewer(palette = "RdYlBu") + 
    scale_x_continuous("", limits = c(-3,4)) + 
    scale_y_continuous("", limits = rang) + 
    theme(plot.title = element_text(face = "bold", size = 14), 
          axis.title.x = element_blank(), axis.ticks.x = element_blank(), 
          axis.text.x = element_blank(), axis.title.y = element_text(face = "bold", size = 12, angle = 90), panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), legend.position = "none", 
          legend.title = element_blank(), legend.text = element_text(size = 12), 
          panel.border = element_rect(linetype = "dotted", colour = "white"), legend.key = element_blank())
  
}

getHFlatAverages <- function(Htree, vname, cname, level=0) {
  inds <- which(sapply(Htree, class) == "Hgroup")
  pre <- NULL
  if (length(inds) > 0) {
    pre <- do.call(rbind, 
                   lapply(inds, function(x) getHFlatAverages(Htree[[x]], vname, cname, level+1))
    )
  }
  if (!any(is.na(c(Htree[[vname]]["average"], Htree[cname])))) {
    pre <- rbind(pre,
                 as.data.frame(c(level=level, Htree[[vname]]["average"], Htree[cname])))
  }
  pre
}

