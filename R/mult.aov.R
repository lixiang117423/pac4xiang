# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# 批量Anova
mult.aov <- function(data, group1, group2, value, level) {
  data %>%
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
