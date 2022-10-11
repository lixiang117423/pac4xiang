#' @name findoutliner
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Findoutliner.
#'
#' @export
#'
# find outliner
findoutliner <- function(x) {
  return(
    ifelse(
      x < quantile(x, .25) - 1.5 * IQR(x) | x > quantile(x, .75) + 1.5 * IQR(x),
      "yes",
      "no"
    )
  )
}
