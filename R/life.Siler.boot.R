#'  Helligman Pollard plot
#'
#' Helligman Pollard plot. Modified from hpbayes.plot.R of HPbayes_0.1
#' @param nrisk. Default = NULL
#' @param ndeath. Default = NULL
#' @param age
#' @param hpp
#' @keywords Heligman Pollard plot
#' @export
#' @examples
#' hpbayes.plot9()
# save all bootstraped Siler life tables

life.Siler.boot <- function(bootM){
  ## bootM = expanded data frame created with boot.M (boot.M object)
  ## nb = number of boots
  ls <- list()
  for (i in 1:nb) {
    qx <- bootM[bootM$boot==i,"val"]
    lifeS <- life.Siler(qx)
    ls[[i]] <- lifeS
  }
  return(ls)
}
