#'  Helligman Pollard plot
#'
#' Helligman Pollard plot. Modified from hpbayes.plot.R of HPbayes_0.1
#' @param nrisk. Default = NULL
#' @param ndeath. Default = NULL
#' @param age
#' @param hpp
#' @keywords Heligman Pollard plot
#' @export
#' @examples
#' hpbayes.plot9()
# original script from HPbayes_0.1


ll.binom <-
function(x, n, p) 
{
    sum(dbinom(x = x, size = n, prob = p, log = TRUE))
}

