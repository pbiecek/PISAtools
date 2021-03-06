calculateItemsPerformace <- function(itemNames, dataset, naLevels = c("7", "8"), cntVal = "CNT", weightVal = "W_FSTUWT", exactNameMatch = FALSE) {
    res <- matrix(NA, length(itemNames), nlevels(dataset[,cntVal]))
    rownames(res) <- itemNames
    colnames(res) <- levels(dataset[,cntVal])
    for (i in seq_along(itemNames)) {
      if (exactNameMatch) {
        item <- dataset[,itemNames[i]]
      } else {
        item <- dataset[,grep(colnames(dataset), pattern=substr(itemNames[i],1,7))[1]]
      }
      
      if (length(naLevels) > 0)       
          item[which(item %in% naLevels)] <- NA
      
#      for (j in seq_along(naLevels)) {
#            item[which(item == naLevels[j])] <- NA
#        }
        
        mitem <- gsub(as.character(item),pattern="Score ", replacement="")
        maxAns  <- mitem == as.character(max(as.numeric(mitem), na.rm=TRUE))
        performance <- prop.table(unclass(by(dataset[,weightVal], list(dataset[,cntVal],  maxAns), base::sum, na.rm=TRUE)), 1)
        
        res[i,] <-  round(performance[,2] * 100,1)
    }
    res
}

