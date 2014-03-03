calculateCoeffs.glm <- function(model, datas, weigths = NULL, fweigth) {
  mres <- glm(model, datas, weights=datas[,fweigth]*2/max(datas[,fweigth]), family="binomial")
  starters <- mres$coefficients
  
  if (is.null(weigths)) {
    mresAll <- NULL
    se <- NULL
  } else {
    mresAll <- sapply(seq_along(weigths), function (i) {
      datas$wei <- datas[,weigths[i]]*2/max(datas[,weigths[i]])
      mresW <- glm(model, datas, weights=wei, family="binomial")
      mresW$coefficients
    })
    se <- sqrt(rowMeans((mresAll - starters)^2))/sqrt(length(weigths))
  }
  
  list(coeffs = starters, se = se, allCoefs = mresAll)
}
