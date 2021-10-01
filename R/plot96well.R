#' @name plot96well
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Show 96-well Value of RT-qPCR.
#' @description
#' \code{plot96well} Show 96-well Value of RT-qPCR.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table fread
#' @importFrom stringr str_sub str_split
#' @importFrom dplyr select filter mutate group_by ungroup
#' @importFrom ggplot2 ggplot aes geom_line geom_tile geom_vline geom_hline geom_text
#' @importFrom ggplot2 geom_segment annotate scale_x_continuous theme element_blank element_text
#' @importFrom ggsci scale_fill_igv
#'
#' @examples
#' filepath <- system.file("examples", "20210929lx_1.txt", package = "pac4xiang")
#' show96res <- plot96well(data = filepath)
#' @export
#'
#' @return Return a plot
#'
utils::globalVariables(c("Position", "Cq", "N", "P","fill","xend"))
plot96well <- function(data) {
  df <- data.table::fread(data, header = TRUE) %>%
    dplyr::select(Position, Cq) %>%
    dplyr::mutate(
      P = stringr::str_sub(Position, 1, 1),
      N = as.numeric(stringr::str_sub(Position, 2, nchar(Position)))
    ) %>%
    dplyr::select(N, P, Cq) %>%
    dplyr::mutate(P = factor(P, levels = LETTERS[8:1]),
                  fill = as.character(N))

  p <- ggplot(df, aes(x = N, y = P, fill = fill)) +
    geom_tile() +
    geom_text(aes(label = Cq), color = "black", size = 5) +
    geom_vline(xintercept = c(4.5, 8.5), color = "black", size = 1) +
    geom_hline(yintercept = c(2.5, 4.5, 6.5), color = "black", size = 1) +
    geom_segment(
      aes(
        x = 0.5,
        y = 9.5,
        xend = 4.5,
        yend = 9.5
      )
    ) +
    geom_segment(
      aes(
        x = 4.5,
        y = 9.5,
        xend = 8.5,
        yend = 9.5
      )
    ) +
    geom_segment(
      aes(
        x = 8.5,
        y = 9.5,
        xend = 12.5,
        yend = 9.5
      )
    ) +
    annotate("text", x = 1:12, y = 10, label = "") +
    # annotate("text", x= 1:12, y= 11,label = 1:12, size = 6, color = 'white') +
    annotate("text", x = 1:12, y = 9.2, label = 1:12, size = 6) +
    annotate("text",
      x = 1:12, y = 8.8,
      label = rep(c("Gene1", "Gene2", "Gene3", "Gene4"), 3),
      size = 6
    ) +
    annotate("text",
      x = c(2.5, 6.5, 10.5), y = 9.8,
      label = c("Treatment1", "Treatment2", "Treatment3"),
      size = 6
    ) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(1, 12, 1)) +
    scale_fill_igv() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(
        size = 15,
        color = "black"
      ),
      axis.text.x = element_blank()
    )
  p

  return(p)
}
