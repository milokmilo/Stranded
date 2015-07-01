#' Bootstrapped Siler mortality object
#'
#' Create an object with bootstraped parameters removing two ages
#' @param data your data
#' @param d column of the data
#' @param rm2 remove first two ages. Default = FALSE
#' @keywords bootstrap object mortality
#' @export
#' @examples
#' boot2.Par()
#'

boot2.Par <- function(data, d, rm2=FALSE){
  parSil <- numeric()
  DDE <- table(data[d])
  DDE <-data.frame(age = as.numeric(rownames(DDE)), M = DDE[])
  ages <- data.frame(age=min(DDE$age):max(DDE$age)) # Creates a dataframe with all the ages
  DDE <- merge(ages, DDE, by="age", all.x=TRUE) # Merges dataframes (including missing ages)
  DDE[is.na(DDE)] <- 0 # Replaces NA with zeros
  DDE <- data.frame(age=DDE$age, age1=DDE$age+1, M=DDE$M)
  if (rm2==TRUE) {
    DDE <- DDE[-c(1,2),]
    DDE$age <- DDE$age -2
    DDE$age1 <- DDE$age1 -2
  }
  par <- optim(c(a1,b1,a2,a3,b3),fit.Siler2, data=DDE,
               control=list(fnscale=-1,maxit=100))
  parSil <- rbind(parSil, t(par$par))
  return(parSil)
}
