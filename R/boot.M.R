#' Boot mortality
#'
#' Bootstrap Siler mortality
#' @param data your data 
#' @param boot.n number of simulations (Default = 100)
#' @keywords bootstrap
#' @export
#' @examples
#' boot.M()
#' 

boot.M <- function(data, SilerPar, boot.n=100, lifeTab){
  a1 <- SilerPar[1]
  b1 <- SilerPar[2]
  a2 <- SilerPar[3]
  a3 <- SilerPar[4]
  b3 <- SilerPar[5]
  boots <- boot.n
  M <- numeric()
  nr <- nrow(data)
  n <- round(runif(boots, 2, nr))  
  ag <- data$age
  for(i in 1:boots){
    Bo <- data[-n[i],]
    par <- SilerMod(c(a1,b1,a2,a3,b3),data=Bo)
    M <- rbind(M, par$par[1]*exp(-par$par[2]*ag) +
                 par$par[3] + par$par[4]*exp(par$par[5]*ag))
  }
  bootM <- as.data.frame(t(M))
  rownames(bootM) <- (1:nrow(bootM))-1
  names(bootM) <- 1:boot.n
  bootM[bootM<0] <- lifeTab[1,"qx"]
  return(bootM)
}
