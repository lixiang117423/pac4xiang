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

# 合并clustalw的结果
getAlignResults <- function(aln) {

  df <- data.table::fread(aln, fill = TRUE) %>%
    dplyr::select(1:2)

  colnames(df)[2] <- "seq"

  res <- NULL

  for (i in unique(df$CLUSTAL)) {
    df.sub <- df %>%
      dplyr::filter(CLUSTAL == i)

    df.res.temp <- data.frame(temp = paste0(">", i))

    res <- rbind(res, df.res.temp)

    seq <- ""

    for (j in df.sub$seq) {
      seq <- paste0(seq, j)
    }

    seq <- data.frame(temp = seq)

    res <- rbind(res, seq)
  }

  return(res)

}
