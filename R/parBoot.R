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
# calculate parameters of bootstraped Siler mortality

parBoot <- function(boot.out, conf = 0.95){
  matrixPar <- cbind(seq(1:ncol(boot.out$t)), 
                     sapply(seq(1:ncol(boot.out$t)), function(index){ 
                       bootci <- boot.ci(boot.out, type = "perc", index = index, conf = conf)
                       bootci$perc[1, 4]
                     }), 
                     sapply(seq(1:ncol(boot.out$t)), function(index){ 
                       bootci <- boot.ci(boot.out, type = "perc", index = index, conf = conf)
                       bootci$perc[1, 5]
                     }))
  colnames(matrixPar) <- c("index", "LL", "UL")
  matrixPar
}
