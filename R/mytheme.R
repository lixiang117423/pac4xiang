#' @name mytheme
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title My ggplot theme.
#'
#' @export
#'
# load my functions
# ggplot2 theme

mytheme <- function(base_size = 14,
                     base_family = "helvetica",
                     ...) {
  theme <- theme_foundation(
    base_size = base_size,
    base_family = base_family
  ) +
    ggthemes::theme_pander() +
    theme(
      plot.title = element_text(
        face = "bold",
        size = rel(1.2),
        hjust = 0.5
      ),
      axis.title.y = element_text(face = "bold", size = rel(1.2), angle = 90, vjust = 2),
      axis.title.x = element_text(face = "bold", size = rel(1.2), vjust = -0.2),
      axis.text = element_text(size = rel(1), color = "black"),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(color = "black", size = 1),
      axis.ticks.length = unit(.20, "cm"),
      panel.border = element_rect(
        color = "black",
        size = 1,
        linetype = "solid"
      ),
      strip.background = element_rect(
        colour = "black",
        fill = "#f0f0f0",
        size = 1
      ),
      strip.text = element_text(face = "bold"),
      strip.text.y = element_text(
        color = "black",
        face = "bold",
        size = rel(1.2),
        angle = 0
      ),
      strip.text.x = element_text(
        color = "black",
        face = "bold",
        size = rel(1.2),
        angle = 0
      ),
      # strip.background = element_rect(
      #   color = "#002FA7",
      #   fill = "#002FA7",
      #   size = 1.5,
      #   linetype = "solid"
      # ),
      plot.margin = unit(c(5, 5, 5, 5), "mm"),
      ...
    )
  return(theme)
}
