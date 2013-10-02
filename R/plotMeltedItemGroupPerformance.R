
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
    sapply(1:nrow(res4), function(i) mean(res4[i,"value"] >= res4[as.character(res4$Var1) == as.character(res4$Var1)[i] ,"value"], na.rm=TRUE)))
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

plotMeltedItemGroupPerformance <- function(migPerformance, selectedCnt = "F011") {
    require(ggplot2)
    if (!"tpos" %in% colnames(migPerformance)) migPerformance$tpos <- max(migPerformance$Value)
    
    ggplot(aes(x=ItemGgroup, y=Value, fill="grey"), data=migPerformance) +
    geom_boxplot(colour=I("white"), outlier.size=0, width=0.5) +
    stat_abline(intercept=0, slope=0, col="black", size=0.5, linetype="dotted") +
    geom_point(size=I(4), colour=I("grey"), shape=18) +
    geom_point(colour="red", size=9, data=migPerformance[migPerformance$CNT == selectedCnt, ], shape=18) +
    geom_text(aes(y=tpos, label=CentileText, colour=CentileColor), data=migPerformance[migPerformance$CNT == selectedCnt, ]) +
    theme_bw() + coord_flip() + xlab("") + ylab("") +
    theme( legend.position = "none",
    text=element_text(size=15),
    panel.border = element_blank()) +
    scale_color_manual(values=c("green3", "grey3", "red"))
}

