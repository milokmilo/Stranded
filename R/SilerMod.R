#' Fit a Siler model 
#' 
#' Fit a Siler model
#' @param x
#' @param data your data
#' @keywords Siler
#' @export
#' @examples
#' SilerMod() 


SilerMod <- function(par = c(-0.153,1.113,0.131,0.005,0.154),
                     data = data, method = "Nelder-Mead", rm=0,
                     control = list(fnscale = -1, maxit = 10000)){
  optim(par, 
        function(par) {
          a1 <- par[1]
          b1 <- par[2]
          a2 <- par[3]
          a3 <- par[4]
          b3 <- par[5]
          if(rm != 0){
            data <- data[-c(1,rm),]
            data$age <- data$age - rm
            data$age1 <- data$age1 - rm
          }
          n <- NROW(data)
          S.t <- function(t) {
            return(exp(-a1/b1*(1-exp(-b1*t)))*
                     exp(-a2*t)*exp(a3/b3*(1-exp(b3*t))))
          }
          dif <- S.t(data[1:n,1])-S.t(data[1:n,2])
          obs <- data[,3]
          lnlk <- as.numeric(crossprod(obs,log(dif)))
          return(lnlk)
        }, 
        method = method, 
        control = control)
}

