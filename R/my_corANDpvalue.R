#' @name my_corANDpvalue
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title group_df2list.
#'
#' @export
#'
my_corANDpvalue <- function(data, method = "pearson", cor = 0.6, pvalue = 0.05) {
  data %>%
    WGCNA::corAndPvalue(method = method) -> cor.p

  cor.p[["p"]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "from") %>%
    tidyr::pivot_longer(
      cols = 2:ncol(.),
      names_to = "to",
      values_to = "p"
    ) %>%
    dplyr::mutate(temp = paste0(from, to)) %>%
    dplyr::select(temp, p) -> my.p

  cor.p[["cor"]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "from") %>%
    tidyr::pivot_longer(
      cols = 2:ncol(.),
      names_to = "to",
      values_to = "cor"
    ) %>%
    dplyr::mutate(temp = paste0(from, to)) %>%
    dplyr::left_join(my.p, by = "temp") %>%
    dplyr::select(-temp) %>%
    dplyr::filter(
      abs(cor) >= cor,
      p < pvalue
    ) -> res
  return(res)
}
