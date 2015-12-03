#' CI Boot mortality
#'
#' CI for Bootstraped Siler mortality
#' @param CI confidence intervala (Default=90)
#' @param bootM Bootstraped object
#' @param ag vector with age ranges
#' @keywords CI bootstrap
#' @export
#' @examples
#' CI.boot()

CI.boot <- function(boot, CI=90, ag, fun="median"){
  ag <- ag
  CI <- CI
  B <- t(boot)
  loCI <- ((100 - CI)/2)/100
  hiCI <- 1 - (((100 - CI)/2)/100)
  B.me <- rep(NA, length(ag))
    for (i in 1:length(B.me)) {
      if(fun == "median") {B.me[i] <- median(B[, i])}
      if(fun == "mean") {B.me[i] <- mean(B[, i])}
    }
  B.me <- data.frame(age=ag, val=B.me, boot="1")
  B.lo <- rep(NA, length(ag))
    for (i in 1:length(B.lo)) {
      B.lo[i] <- quantile(B[, i], probs = loCI)
    }
  B.lo <- data.frame(age=ag, val=B.lo, boot="1")
  B.hi <- rep(NA, length(ag))
    for (i in 1:length(B.hi)) {
      B.hi[i] <- quantile(B[, i], probs = hiCI)
    }
  B.hi <- data.frame(age=ag, val=B.hi, boot="1")
  
  # Saving median and confidence limits
  MeLoHi <- data.frame(Me=B.me$val, Lo=B.lo$val, Hi=B.hi$val)
  return(MeLoHi)
}
