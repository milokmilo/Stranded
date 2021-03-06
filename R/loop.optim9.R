#' Loop optimization
#'
#' Loop optimization. modified from loop.optim.R of HPbayes_0.1 package
#' @param prior 
#' @param nrisk
#' @param ndeath
#' @param d Default = 10
#' @param theta.dim Default = 9
#' @param age Default = c(1e-05, 1, seq(5, 100, 5))
#' @keywords Heligman Pollard plot
#' @export
#' @examples
#' loop.optim9()

loop.optim9 <- function (prior, nrisk, ndeath, d = 10, 
                         theta.dim = 9, age = c(1e-05, 1, seq(5, 100, 5))){
  lx <- nrisk
  dx <- ndeath
  H.k <- prior
  pllwts <- prior.likewts9(prior = prior, nrisk = lx, ndeath = dx)
  log.like.0 <- pllwts$log.like.0
  wts.0 <- pllwts$wts.0
  B0 <- 1000 * theta.dim
  q0 <- H.k
  d.keep <- 0
  theta.new <- H.k[wts.0 == max(wts.0), ]
  keep <- H.k
  ll.keep <- log.like.0
  opt.mu.d <- matrix(NA, nrow = d, ncol = theta.dim)
  opt.cov.d <- array(NA, dim = c(theta.dim, theta.dim, d))
  prior.cov <- cov(q0)
  opt.low <- apply(q0, 2, min)
  opt.hi <- apply(q0, 2, max)
  imp.keep <- theta.dim * 100
  max.log.like.0 <- max(log.like.0)
  mp8.mle <- function(theta, x.fit = age) {
    p.hat <- mod9p(theta = theta, x = x.fit)
    ll <- ll.binom(x = dx, n = lx, p = p.hat)
    return(ll)
  }
  for (i in 1:d) {
    out <- optim(par = theta.new, fn = mp8.mle, method = "L-BFGS-B", 
                 lower = opt.low, upper = opt.hi, control = list(fnscale = -1, 
                                                                 maxit = 1e+05))
    out.mu <- out$par
    if (out$value > max.log.like.0) {
      d.keep <- d.keep + 1
      opt.mu.d[i, ] <- out.mu
      out.hess <- hessian(func = mp8.mle, x = out$par)
      if (is.positive.definite(-out.hess)) {
        out.cov <- try(solve(-out.hess))
        opt.cov.d[, , i] <- out.cov
      }
      if (!is.positive.definite(-out.hess)) {
        out.grad <- grad(func = mp8.mle, x = out.mu)
        A <- out.grad %*% t(out.grad)
        out.prec <- try(solve(prior.cov)) + A
        if (!is.positive.definite(out.prec)) {
          out.prec <- solve(prior.cov)
        }
        out.cov <- try(solve(out.prec))
        opt.cov.d[, , i] <- out.cov
      }
    }
    if (i == 1 & out$value <= max.log.like.0) {
      out.hess <- hessian(func = mp8.mle, x = out$par)
      if (is.positive.definite(-out.hess)) {
        out.cov <- solve(-out.hess)
      }
      if (!is.positive.definite(-out.hess)) {
        out.grad <- grad(func = mp8.mle, x = out.mu)
        A <- out.grad %*% t(out.grad)
        out.prec <- solve(prior.cov) + A
        if (!is.positive.definite(out.prec)) {
          out.prec <- solve(prior.cov)
        }
        out.cov <- solve(out.prec)
      }
      warning("likelihood of first local maximum does not exceed maximum \t\t\tlikelihood from the prior")
    }
    if (i < d) {
      keep <- keep[ll.keep != max(ll.keep), ]
      ll.keep <- ll.keep[ll.keep != max(ll.keep)]
      dist.to.mu <- mahalanobis(x = keep, center = out.mu, 
                                cov = out.cov)
      keep <- keep[rank(1/dist.to.mu) <= (d - i) * B0/d, 
                   ]
      ll.keep <- ll.keep[rank(1/dist.to.mu) <= (d - i) * 
                           B0/d]
      theta.new <- keep[ll.keep == max(ll.keep), ]
    }
  }
  return(list(opt.mu.d = opt.mu.d, opt.cov.d = opt.cov.d, theta.new = theta.new, 
              d.keep = d.keep, log.like.0 = log.like.0, wts.0 = wts.0))
}
