
itemGroupPerformance <- function(itemPerformance, itemClassification, allItemsName = " Average Improvement Across All Items") {
    nitems <- tapply(itemPerformance[,1], itemClassification, function(x) length(na.omit(x)))
    gItemNames <- paste0(names(nitems), " (", nitems, ")")
    
    gAverages <- apply(itemPerformance, 2, function(x)
    tapply(x, itemClassification, mean, na.rm=TRUE)
    )
    averages <- apply(itemPerformance, 2, mean, na.rm=TRUE)
    aAverages <- rbind(averages, gAverages)
    rownames(aAverages) <- c(allItemsName ,gItemNames)
    
    aAverages
}

meltedItemGroupPerformance <- function(igPerformance) {
    require(reshape2)
    res4 <- na.omit(melt(igPerformance))
    res4$mean <- round(100*
    sapply(1:nrow(res4), function(i) 
         sum(res4[i,"value"] > res4[as.character(res4$Var1) == as.character(res4$Var1)[i] ,"value"], na.rm=TRUE) /
             (sum(!is.na(res4[as.character(res4$Var1) == as.character(res4$Var1)[i] ,"value"])) - 1)
           ))
    res4$text <- paste0(res4$mean, "%")
    res4$kol <-  sapply(1:nrow(res4), function(i) {
        maxi <- max(res4[ res4[,"Var2"] == res4[i,"Var2"] ,"mean"], na.rm=TRUE)
        mini <- min(res4[ res4[,"Var2"] == res4[i,"Var2"] ,"mean"], na.rm=TRUE)
        if (res4[i,"mean"]  == maxi) return("green")
        if (res4[i,"mean"]  == mini) return("red")
        "grey"
    })
    colnames(res4) <- c("ItemGgroup", "CNT", "Value", "Centile", "CentileText", "CentileColor")
    res4
}

plotMeltedItemGroupPerformance <- function(migPerformance, selectedCnt = "F011", addText = FALSE, addZero = TRUE) {
  require(ggplot2)
  if (!"tpos" %in% colnames(migPerformance)) migPerformance$tpos <- max(migPerformance$Value)
  migPerformance$sItemGgroup <- as.numeric(factor(migPerformance$ItemGgroup)) + 0.25
  
  p <- ggplot(aes(x=ItemGgroup, y=Value, fill="grey"), data=migPerformance) +
    geom_boxplot(colour=I("white"), outlier.size=0, width=0.5) +
    geom_point(size=I(4), colour=I("grey"), shape=18) +
    theme_bw() + coord_flip() + xlab("") + ylab("") +
    theme( legend.position = "none",
           text=element_text(size=15),
           panel.border = element_blank()) +
    scale_color_manual(values=c("green3", "grey3", "red"))
  
  if (addZero)
    p <- p +
      stat_abline(intercept=0, slope=0, col="black", size=0.5, linetype="dotted") 
    
  if (!is.na(selectedCnt)) 
    p <- p +
      geom_point(colour="red", size=9, data=migPerformance[migPerformance$CNT == selectedCnt, ], shape=18) +
      geom_text(aes(y=tpos, label=CentileText, colour=CentileColor), data=migPerformance[migPerformance$CNT == selectedCnt, ])

  if (addText) 
    p <- p + 
      geom_text(aes(y=Value, x=sItemGgroup, label=CNT), data = migPerformance[migPerformance$Centile > 99,], angle=0) +
      geom_text(aes(y=Value, x=sItemGgroup, label=CNT), data = migPerformance[migPerformance$Centile < 1 + min(migPerformance$Centile),], angle=0)
  
  p
}

plotMeltedItemGroupInAreasPerformance <- function(migPerformance, selectedCnt = "F011", addText = FALSE, addZero = TRUE, addTextCutoff=1, boxwidth=0.75) {
  require(ggplot2)
  if (!"tpos" %in% colnames(migPerformance)) migPerformance$tpos <- max(migPerformance$Value)
  migPerformance$sGgroup <- as.numeric(factor(migPerformance$Group)) + 0.25
  if (addText) {
    migPerformance$CentileRankTop <- sapply(1:nrow(migPerformance), function(i) {
      sum(migPerformance$Group == migPerformance$Group[i] & migPerformance$Centile >= migPerformance$Centile[i])
    })
    migPerformance$CentileRankBottom <- sapply(1:nrow(migPerformance), function(i) {
      sum(migPerformance$Group == migPerformance$Group[i] & migPerformance$Centile <= migPerformance$Centile[i])
    })
  }
  p <- ggplot(aes(x=factor(Group), y=Value, fill=Area), data=migPerformance) + 
    geom_boxplot(colour=I("white"), outlier.size=0, width=boxwidth) + 
    geom_point(size=I(4), colour=I("grey"), shape=18) + 
    theme_bw() + coord_flip() + xlab("") + ylab("") + 
    theme( legend.position = "none",
           text=element_text(size=15),
           panel.border = element_blank()) +
    scale_color_manual(values=c("green3", "grey3", "red"))
  
  if (addZero)
    p <- p +
      stat_abline(intercept=0, slope=0, col="black", size=0.5, linetype="dotted") 
  
  if (!is.na(selectedCnt)) 
    p <- p +
      geom_point(colour="red", size=9, data=migPerformance[migPerformance$Country == selectedCnt, ], shape=18) + 
      geom_text(aes(y=tpos, label=CentileText, colour=CentileColor), data=migPerformance[migPerformance$Country == selectedCnt, ])  
    
  if (addText) 
    p <- p + 
      geom_text(aes(y=Value, x=sGgroup, label=Country), angle=90, data = migPerformance[migPerformance$CentileRankBottom <= addTextCutoff,]) +
      geom_text(aes(y=Value, x=sGgroup, label=Country), angle=90, data = migPerformance[migPerformance$CentileRankTop <= addTextCutoff,])
  
  p
  
  }





plotDifferentGroupPerformance <- function(migPerformance, selectedCnt = "F011") {
  require(ggplot2)
  ggplot(aes(x=Group2, y=Value, fill="grey"), data=migPerformance) +
    geom_boxplot(colour=I("white"), fill=I("green4"), outlier.size=0, width=0.5) +
    geom_point(size=I(4), colour=I("grey"), shape=18) +
    theme_bw() + coord_flip() + xlab("") + ylab("") +
    theme( legend.position = "none",
           text=element_text(size=15),
           panel.border = element_blank()) +
    geom_point(colour="red", size=9, data=migPerformance[migPerformance$CNT == selectedCnt, ], shape=18) +
    geom_text(aes(y=950, label=CentileText), data=migPerformance[migPerformance$CNT == selectedCnt, ]) + 
    ylim(c(min(migPerformance$Value),1000))
  
}
