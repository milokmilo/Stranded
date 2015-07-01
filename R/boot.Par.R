#' Bootstraped object
#'
#' Create an object with bootstrapping parameters
#' @param data your data 
#' @param boot.n number of simulations
#' @keywords bootstrap object
#' @export
#' @examples
#' boot.Par()
#'

boot.Par <- function(data, boot.n){
  boots <- boot.n
  parSil <- numeric()
  nr <- nrow(data)
  n <- round(runif(boots, 2, nr))  
  for(i in 1:boots){
    Bo <- data[-n[i],]
    par <- optim(c(a1,b1,a2,a3,b3),fit.Siler2, data=Bo,
                 control=list(fnscale=-1,maxit=100))
    parSil <- rbind(parSil, par$par)
  }
  parSil <- as.data.frame(parSil)
  names(parSil) <- c("a1","b1","a2","a3","b3")
  return(parSil)
}