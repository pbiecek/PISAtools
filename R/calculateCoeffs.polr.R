calculateCoeffs.polr <- function(model, datas, method="probit", weigths, fweigth) {
  mres <- polr(model, datas, weights=datas[,fweigth], method=method)
  starters <- c(mres$coefficients, mres$zeta)
  
  mresAll <- sapply(seq_along(weigths), function (i) {
    datas$wei <- datas[,weigths[i]]
    mresW <- polr(model, datas, weights=wei, method=method, start=starters)
    c(mresW$coefficients, mresW$zeta)
  })
  
  se <- sqrt(rowMeans((mresAll - starters)^2))/sqrt(length(weigths))
  
  list(coeffs = starters, se = se, allCoefs = mresAll)
}
