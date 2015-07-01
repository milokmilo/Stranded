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

project.leslie <- function(A,no,tmax,pop.sum=FALSE){
  if(length(no) != dim(A)[1])
    stop("Projection matrix and population vector have different number of states!")
  N <- matrix(0, nr=length(no),nc=tmax+1)
  N[,1] <- no
  pop <- no
  for(t in 1:tmax){
    pop <- A%*%pop
    N[,t+1] <- pop
  }
  if(pop.sum){
    N <- apply(N,2,sum)
  }
  return(N)
}

