#' Fit a Siler model 
#' 
#' Fit a Siler model
#' @param x
#' @param data your data
#' @keywords Siler
#' @export
#' @examples
#' fit.Siler2() 


SilerAtAge <- function(SilerPar, data, lifeTab){
  a1 <- SilerPar[1]
  b1 <- SilerPar[2]
  a2 <- SilerPar[3]
  a3 <- SilerPar[4]
  b3 <- SilerPar[5]
  ggLife <- lifeTab[,c("age", "qx")]
  names(ggLife) <- c("age", "value")
  age <- data$age
  t <- data[,c(1,3)]
  MSiler <- a1*exp(-b1*age) + a2 + a3*exp(b3*age)
  MSiler[MSiler<0] <- ggLife[1,"value"]
  Madult <- rep(a2,length(age))
  Madult[Madult<0] <- ggLife[1,"value"]
  Myoung <- (a1 * exp(-b1 * age)) + a2
  Myoung[Myoung<0] <- ggLife[1,"value"]
  Msenesc <- (a3 * exp(b3 * age)) + a2
  Msenesc[Msenesc<0] <- ggLife[1,"value"]
  lifeSiler <-  cbind(t,Myoung,Madult,Msenesc,MSiler)
  return(lifeSiler)
}

