#' Bootstrap Siler survivorship
#'
#' Bootstrap Siler optimized survivorships
#' @param par parameters
#' @param boot.n number of simulations
#' @param rm remove first two ages. Default = FALSE
#' @keywords bootstrap object survivorship
#' @export
#' @examples
#' boot.Surv()
#'

boot.Surv <- function(par, boot.n, rm=FALSE, ag){ 
  ## par = parameters created with boot.Par
  ## boot.n = number of boot interactions
  ## rm2 = TRUE if the first two ages were removed
  Sur <- numeric()
  for (i in 1:boot.n){
    S <- exp((-par$a1[i]/par$b1[i])*(1-exp(-par$b1[i]*ag))) *
      exp((-par$a2[i]*ag)) * 
      exp((par$a3[i]/par$b3[i])*(1-exp(par$b3[i]*ag)))
    if(rm==TRUE){S <- S/S[1]}
    Sur <- cbind(Sur,S)
  }
  Sur <- as.data.frame(Sur)
  Sur <- cbind(0:(nrow(Sur)-1), Sur)
  names(Sur) <- c("age", 1:(length(names(Sur))-1))
  return(Sur)
}  
