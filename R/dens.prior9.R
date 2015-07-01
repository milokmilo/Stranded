#' Density priors
#'
#' Modified from dens.prior.R of HPbayes_0.1 package
#' @param x
#' @param pri.lo minimum limits of priors. Default = 0, 0, 0, 0, 0, 0, 15, 0, 0
#' @param pri.hi maximum limits of priors Default = 0.15, 1, 1, 0.5, 0.25, 15, 55, 0.1, 1.25
#' @keywords priors Heligman Pollard
#' @export
#' @examples
#' dens.prior9()

dens.prior9 <- function (x, pri.lo = c(0, 0, 0, 0, 0, 0, 15, 0, 0),  
                         pri.hi = c(0.15, 1, 1, 0.5, 0.25, 15, 55, 0.1, 1.25)) 
{
  y <- (dunif(x[, 1], pri.lo[1], pri.hi[1]) * 
          dunif(x[, 2], pri.lo[2], pri.hi[2]) * 
          dunif(x[, 3], pri.lo[3], pri.hi[3]) * 
          dunif(x[, 4], pri.lo[4], pri.hi[4]) * 
          dunif(x[, 5], pri.lo[5], pri.hi[5]) * 
          dunif(x[, 6], pri.lo[6], pri.hi[6]) * 
          dunif(x[, 7], pri.lo[7], pri.hi[7]) * 
          dunif(x[, 8], pri.lo[8], pri.hi[8]) * 
          dunif(x[, 9], pri.lo[9], pri.hi[9]))
  return(y)
}
