#' Priors for Helligman Pollard
#'
#' Calculate priors for Heligman Pollard model. Modified from prior.form.R of HPbayes_0.1 package
#' @param pri.lo Default = c(0, 0, 0, 0.001, 0, 0, 15, 0, 0)
#' @param pri.hi Default = c(0.15, 1, 1, 0.5, 0.25, 15, 55, 0.1, 1.25)
#' @param theta.dim Default = 9
#' @keywords Heligman Pollard priors
#' @export
#' @examples
#' prior.form9()

prior.form9 <- function (pri.lo = c(0, 0, 0, 0.001, 0, 0, 15, 0, 0), 
                         pri.hi = c(0.15, 1, 1, 0.5, 0.25, 15, 55, 0.1, 1.25), 
                         theta.dim = 9) {
  B0 <- 1000 * theta.dim
  q0 <- cbind(runif(B0, pri.lo[1], pri.hi[1]), 
              runif(B0, pri.lo[2], pri.hi[2]), 
              runif(B0, pri.lo[3], pri.hi[3]), 
              runif(B0, pri.lo[4], pri.hi[4]), 
              runif(B0, pri.lo[5], pri.hi[5]), 
              runif(B0, pri.lo[6], pri.hi[6]), 
              runif(B0, pri.lo[7], pri.hi[7]),
              runif(B0, pri.lo[8], pri.hi[8]),
              runif(B0, pri.lo[9], pri.hi[9]))
  H.k <- q0
  return(H.k)
}