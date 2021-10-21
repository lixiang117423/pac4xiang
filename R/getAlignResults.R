#' @name getAlignResults
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Get Alignment Result from Clustalw.
#' @description
#' \code{getAlignResults} Get Alignment Result from Clustalw.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table fread
#'
#' @examples
#' filepath <- system.file("examples", "pep.confirmed.aln", package = "pac4xiang")
#' aln.res <- calStandCurve (aln = filepath)
#' @export
#'
#' @return Return a datafram
# 合并clustalw的结果
utils::globalVariables(c("CLUSTAL"))
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

  res <- res[3:nrow(res),] %>% as.data.frame()

  fwrite(res, file = 'YourAlignResults.fasta', row.names = FALSE, col.names = FALSE, quote = FALSE)

  return(res)

}
