#' Generation time
#'
#' Generation time. Original function in demoR_0.4.2
#' @param x
#' @param A
#' @param peryear. Default = 5
#' @keywords leslie matrix generation
#' @export
#' @examples
#' gen.time() 

gen.time <- function(A,peryear=5){
  ro <- calc.ro(A)
  ea <- eigen.analysis(A)
  T <- peryear*log(ro)/log(ea$lambda)

  T
}

