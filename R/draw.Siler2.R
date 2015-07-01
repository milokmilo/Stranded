#' Siler survivorship plot
#'
#' Draw a survivorship curve
#' @param x
#' @param deaths. Default = Bot
#' @keywords Siler survivorship plot
#' @export
#' @examples
#' draw.Siler2() 


draw.Siler2 <- function(x,deaths=Bot) {
  nrow<-NROW(deaths)     
  radix=sum(deaths[,3])
  lx<-1
  for(i in 2:nrow){lx[i]<-lx[i-1]-deaths[i-1,3]/radix}     
  
  a1<-x[1]
  b1<-x[2]
  a2<-x[3]
  a3<-x[4]
  b3<-x[5]
  
  S.t<- function(t) {return(exp(-a1/b1*(1-exp(-b1*t)))*exp(-a2*t)*exp(a3/b3*(1-exp(b3*t))))}
  
  plot(deaths[,1],lx,type='l',lwd=2,col='red',xlab='Age',ylab='Survivorship')
  t<-seq(0,50,.1)
  lines(t,S.t(t),lwd=2,col='blue')
}
