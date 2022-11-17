#' @name scale01
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title scale01.
#'
#' @export
#'

scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
