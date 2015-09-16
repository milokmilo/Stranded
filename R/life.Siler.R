#' Siler life table
#'
#' Siler life table
#' @param Sqx Default = DDESiler
#' @keywords Siler life table
#' @export
#' @examples
#' life.Siler()

life.Siler <- function (Sqx=DDESiler){ 
  ## Sqx = Siler mortality vector
  lifeSiler <- data.frame(age=0:(length(Sqx)-1), qx=Sqx)
  n <- 1000
  ## nx and dx - survivors and deaths at age
  for (j in 1:nrow(lifeSiler)){
    if (j == 1) {dx <- lifeSiler$qx[1]*n; nx <- n
    } else {
      nx <- c(nx, nx[j-1]- dx[j-1])
      dx <- c(dx, lifeSiler$qx[j]*nx[j])
    }
  }
  lifeSiler <- data.frame(lifeSiler, nx=nx, dx=dx)
  # lx - Survivorship-at-age percent 
  lifeSiler$lx <- lifeSiler$nx/n
  # ex - Life expentancy at age ex = Sumlx/lx
  ex <- vector("numeric")
  for (j in 1:nrow(lifeSiler)) {
    e <- sum(lifeSiler$lx[j:nrow(lifeSiler)])/lifeSiler$lx[j]
    ex <- c(ex,e) }
  lifeSiler$ex <- ex
  # Z - Total mortality-at-age -L(nt/no)/t
  Z <- c(NA)
  for (j in 1:nrow(lifeSiler)){
    if (j == 1) {Z <- vector("numeric")}
    z <- -log(lifeSiler$nx[j+1]/lifeSiler$nx[j])/1
    # Correction for the last mortality
    if (j == nrow(lifeSiler)) {z <- 1}
    Z <- c(Z,z)
  }
  lifeSiler$Z <- Z
  
  # Format for printing
  
  lifeSiler$qx <- round(life$qx,3)
  lifeSiler$nx <- round(life$nx,3) 
  lifeSiler$dx <- round(life$dx,3) 
  lifeSiler$lx <- round(life$lx,3) 
  lifeSiler$ex <- round(life$ex,3) 
  lifeSiler$Z <- round(life$Z,3)
  
  return(lifeSiler)
}

