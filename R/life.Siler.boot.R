#' Bootstrap of Siler life table
#'
#' Bootstrap of Siler life table
#' @param booM
#' @param nb number of booststraps (Default = 100)
#' @keywords lifetable Siler bootstrap
#' @export
#' @examples
#' life.Siler.boot()

life.Siler.boot <- function(bootM, nb=100, rm=0){
  ## bootM = expanded data frame created with boot.M (boot.M object)
  ## nb = number of boots
  bootM <- t(bootM)
  bootM <- melt(bootM)
  names(bootM) <- c("boot","age","val")
  bootM$boot <- as.factor(bootM$boot)
  ls <- list()
  for (i in 1:nb) {
    qx <- bootM[bootM$boot==i,"val"]
    lifeS <- life.Siler(qx, rm=rm)
    ls[[i]] <- lifeS
  }
  return(ls)
}
