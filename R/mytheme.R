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
      # plot title
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 28, ,
        color = "#222222"
      ),
      plot.subtitle = ggplot2::element_text(
        size = 22,
        margin = ggplot2::margin(9, 0, 9, 0)
      ),
      legend.background = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        size = 18,
        color = "#222222"
      ),
      axis.title.y = element_text(size = rel(1.2), angle = 90, vjust = 2),
      axis.title.x = element_text(size = rel(1.2), vjust = -0.2),
      axis.text = ggplot2::element_text(
        size = 18,
        color = "#222222"
      ),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
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
        size = rel(1.2),
        angle = 0
      ),
      plot.margin = unit(c(5, 5, 5, 5), "mm"),
      ...
    )
  return(theme)
}
