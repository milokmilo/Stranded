#' Density binomial
#'
#' Density binomial. Original script from HPbayes_0.1
#' @param nrisk. Default = NULL
#' @param ndeath. Default = NULL
#' @param age
#' @param hpp
#' @keywords Heligman Pollard plot
#' @export
#' @examples
#' ll.binom()

ll.binom <- function(x, n, p) {
    sum(dbinom(x = x, size = n, prob = p, log = TRUE))
}

