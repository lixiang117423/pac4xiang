#' @name Anova_with_Posttest
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Anova with Post Test.
#' @description
#' \code{Anova_with_Posttest} Anova with Post Test.
#'
#' @importFrom magrittr %>%
#'
#' @param
#' \code{method} Tukey or Duncan
#'
#' @export
#'
#' @return Return a datafram
utils::globalVariables(c("data", "value", "group", "method", "fit", "res"))
Anova_with_Posttest <- function(data, value, group, method, level = 0.95) {
  data <- data %>%
    dplyr::rename(v = all_of(value), g = all_of(group)) %>%
    dplyr::mutate(g = as.factor(g))
  fit <- aov(v ~ g, data)
  if (method == "Tukey") {
    res <- glht(fit, linfct = mcp(g = "Tukey")) %>%
      cld(level = level, decreasing = TRUE)
    res <- res[["mcletters"]][["Letters"]] %>%
      as.data.frame() %>%
      tibble::rownames_to_column()
    colnames(res) <- c("group", "Tukey.signif")

    res <- res %>%
      dplyr::mutate(anova.pvalue = broom::tidy(fit)$p.value[1]) %>%
      dplyr::select(1, 3, 2)
  } else if (method == "Duncan") {
    res <- duncan.test(fit, "g", alpha = 1 - level, console = TRUE)[["groups"]] %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      dplyr::mutate(anova.pvalue = broom::tidy(fit)$p.value[1]) %>%
      dplyr::select(1, 4, 3)
    colnames(res) <- c("group", "anova.pvalue", "Duncan.signif")
  }

  res <- res %>%
    dplyr::mutate(anova.signif = ifelse(anova.pvalue < 0.05 & anova.pvalue > 0.01, "*",
      ifelse(anova.pvalue < 0.01 & anova.pvalue > 0.001, "**",
        ifelse(anova.pvalue < 0.001, "***", "NS")
      )
    )) %>%
    dplyr::select(1, 2, 4, 3)

  return(res)
}
