#' Percentages of bootstraped Siler mortality
#'
#' Calculate percentages from bootstraped Siler mortality
#' @param boot.out
#' @param conf Default = 0.95
#' @keywords Siler mortality percentage bootstrap
#' @export
#' @examples
#' percBootCI()

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

