#' Bootstrap of Siler life table
#'
#' Bootstrap of Siler life table
#' @param booM
#' @keywords Heligman Pollard plot
#' @export
#' @examples
#' life.Siler.boot()

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
