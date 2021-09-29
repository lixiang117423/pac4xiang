# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

plot96well <- function(data) {
  df <- fread(data, header = TRUE) %>%
    dplyr::select(Position, Cq) %>%
    dplyr::mutate(
      P = stringr::str_sub(Position, 1, 1),
      N = as.numeric(stringr::str_sub(Position, 2, nchar(Position)))
    ) %>%
    dplyr::select(N, P, Cq) %>%
    # dplyr::mutate(Cq = ifelse(Cq == '-',0, Cq)) %>%
    dplyr::mutate(P = factor(P, levels = LETTERS[8:1]))

  p <- ggplot(df, aes(x = N, y = P, fill = Cq)) +
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
