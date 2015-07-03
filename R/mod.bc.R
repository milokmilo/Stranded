#' Helligman Pollard model function
#'
#' Helligman Pollard model function. Modified from HPbayes_0.1 package
#' @param theta
#' @param x
#' @keywords Heligman Pollard 
#' @export
#' @examples
#' mod.bc()

mod.bc <- function (theta, x) {
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