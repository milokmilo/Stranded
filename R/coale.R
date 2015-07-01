#' coale
#'
#' Gives a particular aggregation for life.table(). Original function in demoR_0.4.2 
#' @param b1
#' @param b4
#' @param nMx
#' @keywords coale life table
#' @export
#' @examples
#' coale()

coale <- function(b1,b4,nMx){
   if(nMx[1]>0.107){
     b1 <- c(0.350,0)
     b4 <- c(1.361,0)
   }
   nax12 <- c(0,0)
   nax12[1] <- b1[1] + b1[2] *nMx[1]
   nax12[2] <- b4[1] + b4[2]* nMx[1]
   return(nax12)
 }

