#' @name mult.aov
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Multiple Group Anova.
#' @description
#' \code{mult.aov} Multiple Group Anova.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table fread
#' @importFrom stringr str_sub str_split
#' @importFrom dplyr select filter mutate group_by ungroup all_of
#' @importFrom stats aov
#' @importFrom multcomp mcp cld glht
#'
#'
#' @examples
#' filepath <- system.file("examples", "df.1.txt", package = "pac4xiang")
#' aov.res <- mult.aov(data = filepath,
#'                     group1 = "group1",group2 = "group2",
#'                     value = "value", level = 0.95)
#' @export
#'
#' @return Return a datafram
#'
utils::globalVariables(c("first.group", "second.group", 
                         "anova.p.value", "signif",
                         "group.temp","."))
# 批量Anova
mult.aov <- function(data, group1, group2, value, level) {
  data.table::fread(data = data) %>%
    dplyr::rename(
      first.group = all_of(group1),
      second.group = all_of(group2),
      value = all_of(value)
    ) %>%
    dplyr::mutate(second.group = factor(second.group, levels = c(unique(second.group)))) %>%
    dplyr::mutate(group.temp = paste0(first.group, second.group)) -> df


  aov.results <- NULL

  for (i in unique(df$first.group)) {
    df %>%
      dplyr::filter(first.group == i) -> df.temp

    fit <- aov(value ~ second.group, data = df.temp)


    tuk <- glht(fit, linfct = mcp(second.group = "Tukey"))
    sig <- cld(tuk,
               level = ifelse(level != 0.95, level, 0.95),
               decreasing = TRUE
    )[["mcletters"]][["Letters"]] %>%
      as.data.frame()

    colnames(sig) <- "signif"
    sig %>%
      dplyr::mutate(
        first.group = i,
        second.group = rownames(.),
        anova.p.value = summary(fit)[[1]][["Pr(>F)"]][1]
      ) %>%
      dplyr::select(first.group, second.group, anova.p.value, signif) -> sig.temp

    aov.results <- rbind(aov.results, sig.temp)
  }

  aov.results %>%
    dplyr::mutate(group.temp = paste0(first.group, second.group)) %>%
    dplyr::select(group.temp, anova.p.value, signif) -> aov.results

  df <- merge(df, aov.results, by = "group.temp", all.x = TRUE) %>%
    dplyr::select(first.group, second.group, value, anova.p.value, signif)

  colnames(df)[1:3] <- c(group1, group2, value)

  return(df)
}
