#' Helligman Pollard 9 parameters priors
#'
#' Helligman Pollard 9 parameters priors. Modified from prior.likewts.R of HPbayes_0.1 package
#' @param prior
#' @param nrisk
#' @param ndeath 
#' @param theta.dim Default = 9
#' @param age Default = c(1e-05, 1, seq(5, 100, 5)) 
#' @keywords Heligman Pollard priors
#' @export
#' @examples
#' prior.likewts9()

prior.likewts9 <- function (prior, nrisk, ndeath, 
                            theta.dim = 9, age = c(1e-05, 1, seq(5, 100, 5))) {
  lx <- nrisk
  dx <- ndeath
  mp8.mle <- function(theta, x.fit = age) {
    p.hat <- mod9p(theta = theta, x = x.fit)
    ll <- ll.binom(x = ndeath, n = nrisk, p = p.hat)
    return(ll)
  }
  B0 <- theta.dim * 1000
  log.like.0 <- rep(NA, B0)
  for (i in 1:B0) {
    log.like.0[i] <- mp8.mle(prior[i, ])
  }
  a0 <- -max(log.like.0, na.rm = TRUE)
  like.0 <- exp(log.like.0 + a0 + 700)
  wts.0 <- like.0/sum(like.0, na.rm = TRUE)
  wts.0[is.na(wts.0)] <- 0
  return(list(wts.0 = wts.0, log.like.0 = log.like.0))
}

