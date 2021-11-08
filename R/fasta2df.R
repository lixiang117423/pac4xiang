#' @name fasta2df
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Convert Fasta Type File to Data Frame.
#' @description
#' \code{fasta2df} Convert Fasta Type File to Data Frame.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table fread
#' @importFrom stringr str_sub str_split
#' @importFrom dplyr select filter mutate group_by ungroup
#'
#' @export
#'
#' @return Return a datafram
utils::globalVariables(c("df.seq","seq.num","df.temp","start","end","seq.info","seq.temp"))

fasta2df <- function(fasta) {

  df.seq <- fread(fasta, header = FALSE, sep = "\n") %>% na.omit()

  seq.num <- c()

  for (i in 1:nrow(df.seq)) {
    if (str_sub(df.seq$V1[i], 1, 1) == ">") {
      seq.num <- c(seq.num, i)
    }
  }

  df.temp <- seq.num %>% as.data.frame()

  colnames(df.temp)[1] <- "v1"

  df.temp <- df.temp %>%
    dplyr::mutate(
      start = v1 + 1,
      end = v1 - 1
    )

  seq.info <- df.seq[df.temp$v1, ] %>% dplyr::mutate(seq = "")

  for (i in 1:nrow(df.temp)) {
    start <- df.temp$start[i]

    if (i == nrow(df.temp)) {
      end <- nrow(df.seq)
    } else {
      end <- df.temp$end[i + 1]
    }

    seq.temp <- ""

    for (j in start:end) {
      seq.temp <- paste0(seq.temp, df.seq$V1[j])
    }
    seq.info$seq[i] <- seq.temp
  }

  colnames(seq.info)[1] <- "id"

  return(seq.info)
}
