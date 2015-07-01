#'  Helligman Pollard plot
#'
#' Helligman Pollard plot. Modified from hpbayes.plot.R of HPbayes_0.1
#' @param nrisk. Default = NULL
#' @param ndeath. Default = NULL
#' @param age
#' @param hpp
#' @keywords Heligman Pollard plot
#' @export
#' @examples
#' hpbayes.plot9()
# life table

life.tab <- function(x){
  ## x = data frame with ages and number of deaths
  life <- x
  names(life) <- c("age","M")
  n <- 1000
  ## S - Creating survivorship column
  d <- sum(life$M) # sum of deaths
  for (j in 1:nrow(life)){
    life$S[[j]] <- d-sum(life$M[1:j-1])} 
  ## nx - Standardizing survivors [(N/SumM) * 1000]
  life$nx <- round((life$S/d)*n,1) 
  ## dx - Dolphins death-at-age [nx - n(x+1)]
  for (j in 1:nrow(life)) {
    if (j == 1) {dx <- vector("numeric")}
    d <- round(life$nx[j]-life$nx[j+1],3)
    if (j == nrow(life)) {d <- round(life$nx[j]-0,3)} 
    dx <- c(dx,d)}
  life$dx <- dx
  ## qx - Death-at-age probability [dx / nx]
  for (j in 1:nrow(life)) {
    if (j == 1) {qx <- vector("numeric")}
    q <- round(life$dx[j]/life$nx[j],3)
    qx <- c(qx,q) }
  life$qx <- qx 
  ## lx - Survivorship-at-age percent [nx / n]
  life$lx <- life$nx/n 
  ## ex - Life expentancy at age ex = Sumly/lx
  ex <- vector("numeric")
  for (j in 1:nrow(life)) {
    e <- round(sum(life$lx[j:nrow(life)])/life$lx[j],3)
    ex <- c(ex,e) }
  life$ex <- ex
  ## Z - Total mortality-at-age -L(nt/no)/t
  Z <- c(NA)
  for (j in 1:nrow(life)){
    if (j == 1) {Z <- vector("numeric")}
    z <- round(-log(life$nx[j+1]/life$nx[j])/1,2)
    if (j == nrow(life)) {z <- 1.00}
    Z <- c(Z,z)
  }
  life$Z <- Z
  return(life)
}
