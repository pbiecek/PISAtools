calculateItemsPerformace <- function(itemNames, dataset, naLevels = c("7", "8"), cntVal = "CNT", weightVal = "W_FSTUWT") {
    res <- matrix(NA, length(itemNames), nlevels(dataset[,cntVal]))
    rownames(res) <- itemNames
    colnames(res) <- levels(dataset[,cntVal])
    for (i in seq_along(itemNames)) {
        item <- dataset[,grep(colnames(dataset), pattern=substr(itemNames[i],1,7))[1]]
        
        for (j in seq_along(naLevels)) {
            item[which(item == naLevels[j])] <- NA
        }
        
        maxAns  <- item == as.character(max(as.numeric(as.character(item)), na.rm=TRUE))
        performance <- prop.table(unclass(by(dataset[,weightVal], list(dataset[,cntVal],  maxAns), base::sum, na.rm=TRUE)), 1)
        
        res[i,] <-  round(performance[,2] * 100,1)
    }
    res
}

