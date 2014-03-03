calculateCoeffs.glm <- function(model, datas, method="probit", weigths = NULL, fweigth, starts = NULL) {
  if (is.null(starts)) {
    mres <- glm(model, datas, weights=datas[,fweigth], method=method)
  } else {
    mres <- glm(model, datas, weights=datas[,fweigth], method=method, start=starts)
  }
  starters <- mres$coefficients
  
  if (is.null(weigths)) {
    mresAll <- NULL
    se <- NULL
  } else {
    mresAll <- sapply(seq_along(weigths), function (i) {
      datas$wei <- datas[,weigths[i]]
      mresW <- polr(model, datas, weights=wei, method=method)
      smresW$coefficients
    })
    se <- sqrt(rowMeans((mresAll - starters)^2))/sqrt(length(weigths))
  }
  
  list(coeffs = starters, se = se, allCoefs = mresAll)
}
