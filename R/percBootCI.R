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
# calculate percentage of bootstraped Siler mortality

percBootCI <- function(boot.out, conf = 0.95){
  matrixcis <- cbind(seq(1:ncol(boot.out$t)), 
                     sapply(seq(1:ncol(boot.out$t)), function(index){ 
                       bootci <- boot.ci(boot.out, type = "perc", index = index, conf = conf)
                       bootci$perc[1, 4]
                     }), 
                     sapply(seq(1:ncol(boot.out$t)), function(index){ 
                       bootci <- boot.ci(boot.out, type = "perc", index = index, conf = conf)
                       bootci$perc[1, 5]
                     }))
  colnames(matrixcis) <- c("index", "LL", "UL")
  matrixcis
}

