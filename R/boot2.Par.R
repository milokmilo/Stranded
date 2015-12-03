#' Bootstrapped Siler mortality object
#'
#' Create an object with bootstraped parameters removing two ages
#' @param data your data
#' @param d column of the data
#' @param rm number of ages for removing (Default = 0)
#' @param boot.n number of simulations (Default = 100)
#' @keywords bootstrap object mortality
#' @export
#' @examples
#' boot2.Par()


boot2.Par <- function(data, R=100, SilerPar=Spar$par, rm=0){
  parboot <- boot(data=data,
    function(data, d, SP=SilerPar, rm=0){
      parSil <- numeric()
      dat <- table(data[d])
      dat <-data.frame(age = as.numeric(rownames(dat)), M = dat[])
      ages <- data.frame(age=min(dat$age):max(dat$age)) # Creates a dataframe with all the ages
      dat <- merge(ages, dat, by="age", all.x=TRUE) # Merges dataframes (including missing ages)
      dat[is.na(dat)] <- 0 # Replaces NA with zeros
      dat <- data.frame(age=dat$age, age1=dat$age+1, M=dat$M)
      if (rm != 0) {
        dat <- dat[-c(1,2),]
        dat$age <- dat$age - rm
        dat$age1 <- dat$age1 - rm
      }
      par <- SilerMod(par=SP, data=dat)
      parSil <- rbind(parSil, t(par$par))
      return(parSil)
    }, 
    R=R)
  parboot <- parboot$t
  parboot <- as.data.frame(parboot)
  names(parboot) <- c("a1","b1","a2","a3","b3")
  return(parboot)
}


