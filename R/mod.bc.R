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
# modified from HPbayes_0.1 package


mod.bc <- function (theta, x) 
{
  D2 <- theta[1]
  D <- theta[2]
  E <- theta[3]
  F <- theta[4]
  f.x <- D2 + D * exp(-E * (log(x) - log(F))^2)
  f.x2 <- f.x
  f.x2[1e-06 > f.x] <- 1e-06
  f.x2[0.999999 < f.x] <- 0.999999
  return(f.x2)
}