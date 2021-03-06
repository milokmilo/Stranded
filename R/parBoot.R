#' Parameter from bootstraped Siler mortality
#'
#' Calculate parameters of bootstraped Siler mortality
#' @param boot.out
#' @param conf Default = 0.95
#' @keywords Siler mortality bootstrap parameters
#' @export
#' @examples
#' parBoot()

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
