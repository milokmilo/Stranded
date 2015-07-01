#' Boot mortality
#'
#' Bootstrap Siler mortality
#' @param data your data 
#' @param boot.n number of simulations
#' @keywords bootstrap
#' @export
#' @examples
#' boot.M()
#' 

boot.M <- function(data, boot.n){
  boots <- boot.n
  M <- numeric()
  nr <- nrow(data)
  n <- round(runif(boots, 2, nr))  
  for(i in 1:boots){
    Bo <- data[-n[i],]
    par <- optim(c(a1,b1,a2,a3,b3),fit.Siler2, data=Bo,
                 control=list(fnscale=-1,maxit=100))
    M <- rbind(M, par$par[1]*exp(-par$par[2]*ag)+par$par[3]+par$par[4]*exp(par$par[5]*ag))
  }
  Mplot <- t(as.data.frame(M))
  rownames(Mplot) <- (1:nrow(Mplot))-1
  Mplot <- t(Mplot)
  return(Mplot)
}
