#' Life table for Leslie matrix
#'
#' Life table for Leslie matrix. Modified from life.table.R of demoR_0.4.2
#' @param x 
#' @param n amount of individual in the theoretical population (Default n = 1000)
#' @keywords life table Leslie matrix
#' @export
#' @examples
#' life.tab()

life.tab <- function(x, n = 1000){
  ## x = data frame with ages and number of deaths
  life <- x
  names(life) <- c("age","M")
  n <- n
  ## S - Creating survivorship column
  d <- sum(life$M) # sum of deaths
  for (j in 1:nrow(life)){
    life$S[[j]] <- d-sum(life$M[1:j-1])} 
  ## nx - Standardizing survivors [(N/SumM) * 1000]
  life$nx <- (life$S/d)*n 
  ## dx - Dolphins death-at-age [nx - n(x+1)]
  for (j in 1:nrow(life)) {
    if (j == 1) {dx <- vector("numeric")}
    d <- life$nx[j]-life$nx[j+1]
    if (j == nrow(life)) {d <- life$nx[j]-0} 
    dx <- c(dx,d)}
  life$dx <- dx
  ## qx - Death-at-age probability [dx / nx]
  for (j in 1:nrow(life)) {
    if (j == 1) {qx <- vector("numeric")}
    q <- life$dx[j]/life$nx[j]
    qx <- c(qx,q) }
  life$qx <- qx 
  ## lx - Survivorship-at-age percent [nx / n]
  life$lx <- life$nx/n 
  ## ex - Life expentancy at age ex = Sumly/lx
  ex <- vector("numeric")
  for (j in 1:nrow(life)) {
    e <- sum(life$lx[j:nrow(life)])/life$lx[j]
    ex <- c(ex,e) }
  life$ex <- ex
  ## Z - Total mortality-at-age -L(nt/no)/t
  Z <- c(NA)
  for (j in 1:nrow(life)){
    if (j == 1) {Z <- vector("numeric")}
    z <- -log(life$nx[j+1]/life$nx[j])/1
    if (j == nrow(life)) {z <- 1.00}
    Z <- c(Z,z)
  }
  life$Z <- Z
  
  # Format for printing
  
  life$qx <- round(life$qx,3)
  life$nx <- round(life$nx,3) 
  life$dx <- round(life$dx,3) 
  life$lx <- round(life$lx,3) 
  life$ex <- round(life$ex,3) 
  life$Z <- round(life$Z,3)
       
  return(life)
}
