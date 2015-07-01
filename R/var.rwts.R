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


var.rwts <-
function (w) 
{
    n <- length(w)
    ans <- mean((n * w - 1)^2)
    return(ans)
}

