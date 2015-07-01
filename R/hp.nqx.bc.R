#' Heligman Pollard mortality rate
#'
#' Heligman Pollard mortality rate. Modified function from hp.nqx.bc.R of HPbayes_0.1
#' @param H.out
#' @param age. Default = seq(0, 85, 1)
#' @keywords Heligman Pollard mortality
#' @export
#' @examples
#' hp.nqx.bc()

hp.nqx.bc <- function (H.out, age = seq(0, 85, 1)) 
{
  H.new.hat <- matrix(NA, nrow = nrow(H.out), ncol = length(age))
  for (i in 1:nrow(H.out)) {
    H.new.hat[i, ] <- mod.bc(theta = H.out[i, ], x = age)
  }
  ans <- H.new.hat
  return(ans)
}