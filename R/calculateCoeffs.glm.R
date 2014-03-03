calculateCoeffs.glm <- function(model, datas, weigths = NULL, fweigth, starts = NULL) {
  if (is.null(starts)) {
    mres <- glm(model, datas, weights=datas[,fweigth], family="binomial", method=method)
  } else {
    mres <- glm(model, datas, weights=datas[,fweigth], family="binomial", start=starts)
  }
  starters <- mres$coefficients
  
  if (is.null(weigths)) {
    mresAll <- NULL
    se <- NULL
  } else {
    mresAll <- sapply(seq_along(weigths), function (i) {
      datas$wei <- datas[,weigths[i]]
      mresW <- glm(model, datas, weights=wei, family="binomial")
      smresW$coefficients
    })
    se <- sqrt(rowMeans((mresAll - starters)^2))/sqrt(length(weigths))
  }
  
  list(coeffs = starters, se = se, allCoefs = mresAll)
}
