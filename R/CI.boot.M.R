#' CI Boot mortality
#'
#' CI for Bootstraped Siler mortality
#' @param CI confidence intervala (Default=90)
#' @param bootM Bootstraped object
#' @param ag vector with age ranges
#' @keywords CI bootstrap
#' @export
#' @examples
#' CI.boot.M()

CI.boot.M <- function(bootM, CI=90, ag){
  ag <- ag
  CI <- CI
  M <- t(bootM)
  loCI <- ((100 - CI)/2)/100
  hiCI <- 1 - (((100 - CI)/2)/100)
  M.med <- rep(NA, length(ag))
    for (i in 1:length(M.med)) {
      M.med[i] <- median(M[, i])
    }
  M.med <- data.frame(age=ag, val=M.med, boot="1")
  M5 <- rep(NA, length(ag))
    for (i in 1:length(M5)) {
      M5[i] <- quantile(M[, i], probs = loCI)
    }
  M5 <- data.frame(age=ag, val=M5, boot="1")
  M95 <- rep(NA, length(ag))
    for (i in 1:length(M95)) {
      M95[i] <- quantile(M[, i], probs = hiCI)
    }
  M95 <- data.frame(age=ag, val=M95, boot="1")
  
  # Saving median and confidence limits
  MedLoHiN <- data.frame(Med=M.med$val, Mlo=M5$val, Mhi=M95$val)
  return(MedLoHiN)
}
