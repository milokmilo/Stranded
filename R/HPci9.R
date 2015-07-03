#' Helligman Pollard confidence intervals with 9 parameters
#'
#' Helligman Pollard confidence intervals with 9 parameters. Modified from HPci9 of HPbayes_0.1
#' @param data
#' @param age
#' @param CI Confidence intervals. Default = 95
#' @keywords Heligman Pollard confidence intervals
#' @export
#' @examples
#' HPci9()

HPci9 <- function(data, age=age, CI=95){
  loCI <- ((100 - CI)/2)/100
  hiCI <- 1 - (((100 - CI)/2)/100)
  hpq <- hp.nqx9(H.out = data, age = age)
  hpq.med <- rep(NA, length(age))
  for (i in 1:length(hpq.med)) {
    hpq.med[i] <- median(hpq[, i])
  }
  hpq5 <- rep(NA, length(age))
  for (i in 1:length(hpq5)) {
    hpq5[i] <- quantile(hpq[, i], probs = loCI)
  }
  hpq95 <- rep(NA, length(age))
  for (i in 1:length(hpq95)) {
    hpq95[i] <- quantile(hpq[, i], probs = hiCI)
  }
  MedLoHi <- data.frame(age=age, Med=hpq.med, Mlo=hpq5, Mhi=hpq95)  
  return(MedLoHi)
}
