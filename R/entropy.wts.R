#' Entropy
#'
#' Entropy weights. Original script from HPbayes_0.1
#' @param w
#' @keywords entropy
#' @export
#' @examples
#' entropy.wts() 

entropy.wts <- function(w) {
      n <- length(w)
      num <- w * log(w)
      den <- log(n)
      ans <- -sum(num/den, na.rm = TRUE)
      return(ans)
}

