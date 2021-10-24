#' @name df2fasta
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Convert Data Frame Type File to Fasta.
#' @description
#' \code{df2fasta} Convert Data Frame Type File to Fasta.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @return Return a datafram
utils::globalVariables(c("id","seq","temp"))
df2fasta <- function(df) {
  colnames(df) <- c("id", "seq")

  df.fasta <- NULL

  for (i in 1:nrow(df)) {
    df.temp <- data.frame(temp = paste0(">", df$id[i]))
    df.temp.2 <- data.frame(temp = df$seq[i])

    df.fasta <- rbind(df.fasta, df.temp) %>%
      rbind(df.temp.2)
  }
  return(df.fasta)
}
