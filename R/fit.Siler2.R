#' Fit a Siler model
#'
#' Fit a Siler model
#' @param x
#' @param data your data
#' @keywords Siler
#' @export
#' @examples
#' fit.Siler2() 

fit.Siler2 <- function(x, data) {
  a1<-x[1]
  b1<-x[2]
  a2<-x[3]
  a3<-x[4]
  b3<-x[5]
  nrow<-NROW(data)
  S.t<- function(t) {
    return(exp(-a1/b1*(1-exp(-b1*t)))*exp(-a2*t)*exp(a3/b3*(1-exp(b3*t))))
  }
  dif<-S.t(data[1:nrow,1])-S.t(data[1:nrow,2])
  obs<-data[,3]
  lnlk<-as.numeric(crossprod(obs,log(dif)))
  return(lnlk)
}
