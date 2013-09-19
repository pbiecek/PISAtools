calculateItemsPerformaceInGroups <- function(itemNames, dataset, grVal, cntVal = "CNT", weightVal = "W_FSTUWT", naLevels = c("7", "8")) {
  res <- array(NA, c(length(itemNames), nlevels(dataset[,cntVal]), nlevels(factor(dataset[,grVal]))))
  dimnames(res) <- list(itemNames, levels(dataset[,cntVal]), levels(factor(dataset[,grVal])))
  
  for (i in seq_along(itemNames)) {
    item <- dataset[,grep(colnames(dataset), pattern=substr(itemNames[i],1,7))[1]]
    
    for (j in seq_along(naLevels)) {
      item[which(item == naLevels[j])] <- NA
    }
    
    maxAns  <- item == as.character(max(as.numeric(as.character(item)), na.rm=TRUE))
    performance <- prop.table(unclass(by(dataset[,weightVal], list(dataset[,cntVal], dataset[,grVal], maxAns), base::sum, na.rm=TRUE)), 1:2)
    
    res[i,,] <-  round(performance[,,2] * 100,1)
  }
  res
}
