#' @name LabelSignif
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title LabelSignif.
#'
#' @export
#'
LabelSignif <- function(x, ns = "NS") {
  return(
    ifelse(
      x < 0.001,
      "***",
      ifelse(
        x > 0.001 & x < 0.01,
        "**",
        ifelse(
          x > 0.05, ns, "*"
        )
      )
    )
  )
}
