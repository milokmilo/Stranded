#' Exponential updates
#'
#' Exponential updates. Original script from HPbayes_0.1
#' @param w
#' @param m
#' @keywords entropy
#' @export
#' @examples
#' expt.upts() 

expt.upts <- function(w, m) {
        sum(1 - (1 - w)^m)
}

