#' @name plotCisElements
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Plot Cis-element Location on Sequences.
#' @description
#' \code{plotCisElements} Plot Cis-element Location on Sequences.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table fread
#' @importFrom stringr str_sub str_split
#' @importFrom dplyr select filter mutate group_by ungroup
#' @importFrom ggplot2 ggplot geom_segment aes geom_rect
#' @importFrom ggplot2 scale_x_continuous labs theme_classic theme element_blank
#'
#' @examples
#' filepath <- system.file("examples", "plantCARE_output_PlantCARE_11359.tab", package = "pac4xiang")
#' plot.res <- plotCisElements(data = filepath,length = 1500)
#' @export
#'
#' @return Return a plot
#'
utils::globalVariables(c("V1","V2","V4","V5","temp","y","ymin","ymax"))

plotCisElements <- function(data, length, Cis) {
  cis <- data.table::fread(data,
    header = FALSE
  ) %>%
    dplyr::mutate(xend = V4 + V5) %>%
    dplyr::filter(V2 != '') %>%
    dplyr::mutate(temp = stringr::str_sub(V2, 1, 7)) %>%
    dplyr::filter(temp != 'Unnamed')

  df.num <- data.frame(V1 = unique(cis$V1))
  df.num$y <- 1:nrow(df.num)

  cis <- merge(cis, df.num, by = "V1") %>%
    dplyr::arrange(y) %>%
    dplyr::mutate(
      V1 = factor(V1, levels = unique(V1)),
      ymin = y - 0.4, ymax = y + 0.4
    )

  if (Cis == "") {
    cis <- cis
  } else {
    cis <- cis %>%
      dplyr::filter(V2 %in% Cis)
  }

  cis %>%
    ggplot() +
    geom_segment(aes(
      x = 0,
      xend = length,
      y = V1,
      yend = V1
    ),
    size = 0.8
    ) +
    geom_rect(aes(
      xmin = V4, xmax = xend,
      ymin = ymin, ymax = ymax,
      fill = V2
    )) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, length, 100)) +
    labs(x = "", y = "Gene") +
    theme_classic() +
    theme(legend.title = element_blank()) -> p

  return(p)
}











