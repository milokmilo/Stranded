#' Life table resampling
#'
#' Life table resampling. Modified from like.resamp.R of HPbayes_0.1 package
#' @param K
#' @param log.like.0
#' @param opt.cov.d
#' @param opt.mu.d
#' @param d.keep
#' @param d Default = 10
#' @param thete.dim Default = 9
#' @keywords life table resampling
#' @export
#' @examples
#' like.resamp9()

like.resamp9 <- function (K, log.like.0, opt.cov.d, opt.mu.d, d.keep, d = 10, 
                          theta.dim = 9) 
{
  K <- K
  log.like <- NULL
  log.like.k <- log.like.0
  h.mu <- NULL
  for (i in 1:d) {
    if (!is.na(opt.cov.d[1, 1, i])) {
      h.mu <- rbind(h.mu, opt.mu.d[i, ])
    }
  }
  h.sig <- array(NA, dim = c(theta.dim, theta.dim, (K + d.keep)))
  m <- 0
  for (i in 1:d) {
    if (!is.na(opt.cov.d[1, 1, i])) {
      m <- m + 1
      h.sig[, , m] <- opt.cov.d[, , i]
    }
  }
  log.like <- log.like.k
  return(list(h.mu = h.mu, h.sig = h.sig, log.like = log.like, 
              K = K))
}
