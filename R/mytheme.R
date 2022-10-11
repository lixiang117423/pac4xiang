#' @name mytheme
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title My ggplot theme.
#'
#' @export
#'
# load my functions
# ggplot2 theme
mytheme <- function(...) {
  theme <- ggthemes::theme_pander() +
    theme(
      panel.border = element_rect(
        color = "black",
        size = 0.5,
        linetype = "solid"
      ),
      axis.text = element_text(size =11, color = "black"),
      axis.title = element_text(size = 12, color = "black"),
      plot.title = element_text(size = 12, color = "black"),
      ...
    )
  return(theme)
}
