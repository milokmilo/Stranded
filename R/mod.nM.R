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


mod.nM <- function (theta, x) 
{
  A <- theta[1]
  B <- theta[2]
  C <- theta[3]
  G <- theta[4]
  H <- theta[5]
  f.x <- A^((x + B)^C) +
    (G * (H^x))/(1 + G * (H^x))
  f.x2 <- f.x
  f.x2[1e-06 > f.x] <- 1e-06
  f.x2[0.999999 < f.x] <- 0.999999
  return(f.x2)
}