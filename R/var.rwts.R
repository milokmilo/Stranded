#' Helligman Pollard variance weights
#'
#' Variances weights of Helligman Pollard model. Original script from HPbayes_0.1
#' @param w
#' @keywords Heligman Pollard weights
#' @export
#' @examples
#' var.rwts()

var.rwts <- function (w) {
    n <- length(w)
    ans <- mean((n * w - 1)^2)
    return(ans)
}

