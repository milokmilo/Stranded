#' Leslie matrix
#'
#' Leslie matrix. Modified from leslie.matrix.R of demoR_0.4.2
#' @param lx
#' @param mx
#' @param L Default = TRUE
#' @param peryear Default = 5
#' @param one.sex Default = TRUE
#' @param SRB Default = 1.05
#' @param infant.class Default = TRUE
#' @keywords Leslie matrix
#' @export
#' @examples
#' leslie()

leslie <- function (lx, mx, L = TRUE, peryear = 5, one.sex = TRUE, SRB = 1.05, 
          infant.class = TRUE) 
{
  len1 <- length(lx)
  len2 <- length(mx)
  if (len1 > len2) {
    warning("length of lx greater than the length of mx,\n lx truncated to length of mx")
    lx <- lx[1:len2]
  }
  if (len2 > len1) {
    mx <- mx[1:len1]
  }
  if (infant.class){ 
    mx <- mx[-2]
  }
  fages <- which(mx > 0)
  k <- max(fages)
  mx <- mx[1:k]
  
  if (L) {
    L1 <- lx[1] + lx[2]
    s <- L1
    lx <- c(L1, lx[-c(1, 2)])
  }
                                    s <- lx[1]
#  else {
#    lx <- lx[-2]
#    s <- sqrt(lx[2]) * peryear
#  }
  px <- exp(diff(log(lx)))
  px <- px[1:(k - 1)]
  Fx <- NULL
  for (i in 1:k - 1) {
    Fx[i] <- s * (mx[i] + px[i] * mx[i + 1])/2
  }
  Fx <- c(Fx, mx[k])
  if (one.sex) 
    Fx <- Fx/(1 + SRB)
  A <- matrix(0, nrow = k, ncol = k)
  A[row(A) == col(A) + 1] <- px
  A[1, ] <- Fx
  class(A) <- "leslie.matrix"
  A
}
