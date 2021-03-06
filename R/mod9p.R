#' Helligman Pollard 9 poarameters model function
#'
#' Helligman Pollard 9 poarameters model function. Modified from mod8p.R of HPbayes_0.1 package
#' @param theta
#' @param x
#' @keywords Heligman Pollard
#' @export
#' @examples
#' mod9p()

mod9p <- function (theta, x) {
  A <- theta[1]
  B <- theta[2]
  C <- theta[3]
  D2 <- theta[4]
  D <- theta[5]
  E <- theta[6]
  F <- theta[7]
  G <- theta[8]
  H <- theta[9]
  f.x <- A^((x + B)^C) + (D2 + D * exp(-E * (log(x) - log(F))^2)) + 
    (G * (H^x))/(1 + G * (H^x))
  f.x2 <- f.x
  f.x2[1e-06 > f.x] <- 1e-06
  f.x2[0.999999 < f.x] <- 0.999999
  return(f.x2)
}
