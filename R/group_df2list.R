#' @name group_df2list
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title group_df2list.
#'
#' @export
#'
group_df2list <- function(data, group, value) {
  data %>%
    dplyr::select({{ group }}, {{ value }}) %>%
    dplyr::mutate(id = rownames(.)) %>%
    dplyr::select(3, 1, 2) %>%
    dplyr::rename(value = {{ value }}) %>%
    tidyr::pivot_wider(id_cols = 1, names_from = {{ group }}) %>%
    dplyr::select(-id) %>%
    as.list() %>%
    lapply(function(x) x[!is.na(x)]) -> res.list

  return(res.list)
}
