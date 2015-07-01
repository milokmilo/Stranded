#' Bootstrap Siler survivorship
#'
#' Bootstrap Siler optimized survivorships
#' @param par parameters
#' @param boot.n number of simulations
#' @param rm2 remove first two ages. Default = FALSE
#' @keywords bootstrap object survivorship
#' @export
#' @examples
#' boot.Surv()
#'

boot.Surv <- function(par, boot.n, rm2=FALSE){ 
  ## par = parameters created with boot.Par
  ## boot.n = number of boot interactions
  ## rm2 = TRUE if the first two ages were removed
  Sur <- numeric()
  for (i in 1:boot.n){
    S <- exp((-par$a1[i]/par$b1[i])*(1-exp(-par$b1[i]*ag))) *
      exp((-par$a2[i]*ag)) * 
      exp((par$a3[i]/par$b3[i])*(1-exp(par$b3[i]*ag)))
    if(rm2==TRUE){S <- S/S[1]}
    Sur <- cbind(Sur,S)
  }
  plotSur <- as.data.frame(Sur)
  plotSur <- cbind(0:(nrow(plotSur)-1), plotSur)
  names(plotSur) <- c("age", 1:(length(names(plotSur))-1))
  plotSur <- melt(plotSur, id="age")
  names(plotSur) <- c("age","boot","val")
  return(plotSur)
}  
