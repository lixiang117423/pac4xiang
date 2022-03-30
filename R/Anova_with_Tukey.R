#' @name Anova_with_Tukey
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Anova with Tukey.
#' @description
#' \code{Anova_with_Tukey} Anova with Tukey.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @return Return a datafram
utils::globalVariables(c("data", "value", "group", "fit", "res"))
Anova_with_Tukey <- function(data, value, group, level = 0.95) {
  data <- data %>%
    dplyr::rename(v = all_of(value), g = all_of(group)) %>%
    dplyr::mutate(g = as.factor(g))
  fit <- aov(v ~ g, data)
  res <- glht(fit, linfct = mcp(g = "Tukey")) %>%
    cld(level = level, decreasing = TRUE)
  res <- res[["mcletters"]][["Letters"]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column()
  colnames(res) <- c("group", "Tukey.signif")

  res <- res %>%
    dplyr::mutate(anova.pvalue = broom::tidy(fit)$p.value[1]) %>%
    dplyr::select(1, 3, 2)
  return(res)
}
