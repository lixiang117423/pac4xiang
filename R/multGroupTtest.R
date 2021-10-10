#' @name multGroupTtest
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Multiple Group Student's-test.
#' @description
#' \code{multGroupTtest} Multiple Group Student's-test.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table fread
#' @importFrom stringr str_sub str_split
#' @importFrom dplyr select filter mutate group_by ungroup all_of
#' @importFrom stats t.test
#' @examples
#' filepath <- system.file("examples", "df.1.txt", package = "pac4xiang")
#' t.res <- multGroupTtest(
#'   data = filepath,
#'   group1 = "group1", group2 = "group2",
#'   CK = "a", value = "value", level = 0.95
#' )
#' @export
#'
#' @return Return a datafram
#'
utils::globalVariables(c("first.group", "second.group", "pvalue", "signif", "temp"))
multGroupTtest <- function(data, group1, group2, CK, value, level) {
  if (is.data.frame(data) == TRUE) {
    data  %>%
      dplyr::rename(
        first.group = all_of(group1),
        second.group = all_of(group2),
        value = all_of(value)
      ) %>%
      dplyr::mutate(second.group = factor(second.group, levels = c(unique(second.group)))) %>%
      dplyr::mutate(group.temp = paste0(first.group, second.group)) -> df
  } else {
    data.table::fread(data = data) %>%
      dplyr::rename(
        first.group = all_of(group1),
        second.group = all_of(group2),
        value = all_of(value)
      ) %>%
      dplyr::mutate(second.group = factor(second.group, levels = c(unique(second.group)))) %>%
      dplyr::mutate(group.temp = paste0(first.group, second.group)) -> df
  }


  ttest.results <- NULL

  for (i in unique(df$first.group)) {
    df.sub <- df %>% dplyr::filter(first.group == i)

    df.sub.ck <- df.sub %>% dplyr::filter(second.group == CK)
    df.sub.2 <- df.sub %>% dplyr::filter(second.group != CK)

    df.sub.ck$pvalue.ttest <- ''
    df.sub.ck$signif.ttest <- ''

    ttest.results <- rbind(ttest.results, df.sub.ck)

    for (j in unique(df.sub.2$second.group)) {
      df.sub.3 <- df.sub.2 %>% dplyr::filter(second.group == j)

      df.sub.ck.2 = df.sub.ck %>% dplyr::select(1:(ncol(df.sub.ck)-2))

      df.sub.4 <- rbind(df.sub.ck.2, df.sub.3)

      if (dim(df.sub.4)[1] == 0) {
        next
      } else if (length(unique(df.sub.4$second.group)) == 1) {
        next
      } else {
        df.sub.4$second.group <- factor(df.sub.4$second.group, levels = unique(df.sub.4$second.group))

        fit <- t.test(value ~ second.group, data = df.sub.4, conf.level = level)

        pvalue.ttest <- fit$p.value

        signif.ttest <- ifelse(pvalue.ttest < 0.001, "***",
                         ifelse(pvalue.ttest < 0.01 & pvalue.ttest > 0.001, "**",
                                ifelse(pvalue.ttest > 0.05, "NS", "*")
                         )
        )

        df.sub.3$pvalue.ttest <- as.character(pvalue.ttest)
        df.sub.3$signif.ttest <- signif.ttest

        ttest.results <- rbind(ttest.results, df.sub.3)
      }
    }
  }

  ttest.results <- ttest.results %>%
    dplyr::select(first.group, second.group, pvalue.ttest, signif.ttest) %>%
    dplyr::mutate(temp = paste0(first.group, second.group)) %>%
    ungroup()

  ttest.results <- ttest.results[!duplicated(ttest.results$temp), ] %>%
    dplyr::select(-temp) %>%
    dplyr::select(first.group, second.group, pvalue.ttest, signif.ttest) %>%
    dplyr::mutate(second.group = as.character(second.group))

  colnames(ttest.results)[1:2] <- c(group1, group2)

  ttest.results <- ttest.results %>%
    ungroup()

  return(ttest.results)
}
