tidy2tree <- function(df, row, col, value, ann.row, ann.col) {
  df %>%
    dplyr::select({{ row }}, {{ col }}, {{ value }}) %>%
    tidyr::pivot_wider(
      id_cols = 1,
      names_from = {{ col }},
      values_from = {{ value }}
    ) %>%
    tibble::column_to_rownames(var = {{ row }}) -> df.res

  df.res %>%
    dist(method = "euclidean") %>%
    hclust(method = "complete") %>%
    ggtree::ggtree(branch.length = "none") -> tree.row

  df.res %>%
    t() %>%
    as.data.frame() %>%
    dist(method = "euclidean") %>%
    hclust(method = "complete") %>%
    ggtree::ggtree(branch.length = "none") -> tree.col

  return(list(tree.row = tree.row, tree.col = tree.col))
}
