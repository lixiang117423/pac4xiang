#' @name calStandCurve
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Calculate Standard Curve for RT-qPCR.
#' @description
#' \code{calStandCurve} Calculate Standard Curve for RT-qPCR.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table fread
#' @importFrom stringr str_sub str_split
#' @importFrom dplyr select filter mutate group_by ungroup
#' @importFrom ggplot2 ggplot aes geom_smooth geom_point facet_wrap labs
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous theme_bw theme ggsave
#' @importFrom ggpmisc stat_poly_eq
#' @importFrom stats lm
#'
#' @examples
#' filepath <- system.file("examples", "20210928lx_1.txt", package = "pac4xiang")
#' stand.curve <- calStandCurve (data = filepath, genes = c("A","B","C","D"),
#'                               dilution = 4,start = 2, end = 7,rep = 4,
#'                               drop.NA = FALSE, fill.NA = "mean",
#'                               save.fig = TRUE, fig.type = 'pdf')
#' @export
#'
#' @return Return a datafram
utils::globalVariables(c("Position","Cq","N","P","genes","rep","dilution",
                         "group","mean","sd","max","min","Relative.Conc",
                         "relative.conc","temp.conc","gene","group.2","loc",
                         "relativa.conc","..eq.label..","..rr.label..",
                         "..p.value.label.."))
calStandCurve <- function(data, genes, dilution = 4, start = 2, end = 7,
                          rep = 4, drop.NA = FALSE, fill.NA = "mean",
                          save.fig = TRUE, fig.type = 'pdf') {
  df <- data.table::fread(data, header = TRUE) %>%
    dplyr::select(Position, Cq) %>%
    dplyr::mutate(
      P = stringr::str_sub(Position, 1, 1),
      N = stringr::str_sub(Position, 2, nchar(Position))
    ) %>%
    dplyr::select(N, P, Cq)

  df.gene <- data.frame(
    N = as.character(1:(length(genes) * rep)),
    gene = rep(genes, each = rep)
  )

  df <- merge(df, df.gene, by = "N", all.x = TRUE)

  df.loc <- data.frame(
    P = LETTERS[1:8],
    loc = 1:8
  ) %>%
    dplyr::mutate(relativa.conc = c(
      1 * (1 / dilution)^0,
      1 * (1 / dilution)^1,
      1 * (1 / dilution)^2,
      1 * (1 / dilution)^3,
      1 * (1 / dilution)^4,
      1 * (1 / dilution)^5,
      1 * (1 / dilution)^6,
      1 * (1 / dilution)^7
    )) %>%
    dplyr::mutate(temp.conc = log(relativa.conc, base = 2)) %>%
    dplyr::mutate(Relative.Conc = temp.conc + max(abs(temp.conc)))

  df <- merge(df, df.loc, by = "P", all.x = TRUE) %>%
    dplyr::mutate(group = paste0(gene, P))


  df.1 <- df %>% dplyr::filter(Cq == "-")

  df.2 <- df %>%
    dplyr::filter(Cq != "-") %>%
    dplyr::mutate(Cq = as.numeric(Cq)) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(
      mean = mean(Cq), sd = sd(Cq),
      max = max(Cq), min = min(Cq)
    )

  df.3 <- df %>% dplyr::filter(Cq != "-")

  if (drop.NA == "FALSE" | drop.NA == "false" | drop.NA == "F" | drop.NA == "f") {
    if (fill.NA == "mean") {
      df.temp <- merge(df.1, df.2[, c("group", "mean")], by = "group") %>%
        dplyr::mutate(group.2 = paste0(gene, N, P))
      df.temp$Cq <- df.temp$mean
      df.temp <- df.temp %>%
        dplyr::select(c(colnames(df), "group.2"))

      df.temp <- df.temp[!duplicated(df.temp$group.2), ] %>%
        dplyr::select(-group.2)

      df.final <- rbind(df.3, df.temp)
    } else if (fill.NA == "max") {
      df.temp <- merge(df.1, df.2[, c("group", "max")], by = "group") %>%
        dplyr::mutate(group.2 = paste0(gene, N, P))
      df.temp$Cq <- df.temp$max
      df.temp <- df.temp %>%
        dplyr::select(c(colnames(df), "group.2"))

      df.temp <- df.temp[!duplicated(df.temp$group.2), ] %>%
        dplyr::select(-group.2)

      df.final <- rbind(df.3, df.temp)
    } else {
      df.temp <- merge(df.1, df.2[, c("group", "min")], by = "group") %>%
        dplyr::mutate(group.2 = paste0(gene, N, P))
      df.temp$Cq <- df.temp$min
      df.temp <- df.temp %>%
        dplyr::select(c(colnames(df), "group.2"))

      df.temp <- df.temp[!duplicated(df.temp$group.2), ] %>%
        dplyr::select(-group.2)

      df.final <- rbind(df.3, df.temp)
    }

    df.final <- df.final %>%
      dplyr::mutate(Cq = as.numeric(Cq)) %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(
        mean = mean(Cq), sd = sd(Cq),
        max = max(Cq), min = min(Cq)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(loc >= start & loc <= end)
  } else {
    df.final <- df %>%
      dplyr::filter(Cq != "-") %>%
      dplyr::mutate(Cq = as.numeric(Cq)) %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(
        mean = mean(Cq), sd = sd(Cq),
        max = max(Cq), min = min(Cq)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(loc >= start & loc <= end)
  }

  # 构建模型
  fit.res <- NULL

  for (i in unique(df.final$gene)) {

    df.sub <- df.final %>%
      dplyr::filter(gene == i)

    fit <- lm(Relative.Conc ~ mean, data = df.sub)
    intercept <- fit[["coefficients"]][["(Intercept)"]] %>%
      round(2)
    slope <- fit[["coefficients"]][["mean"]] %>%
      round(2)

    formula <- paste0("RC = ",slope, "xCq"," + ",  intercept)

    r.2 <- broom::glance(fit)[1, 1] %>%
      round(4) %>%
      as.numeric()

    p.value <- broom::glance(fit)[1, 5] %>%
      round(5) %>%
      as.numeric()

    df.temp <- data.frame(
      Gene = i,
      Formula = formula,
      Slope = slope,
      Intercept = intercept,
      R2 = r.2,
      P.value = p.value,
      max = max(df.sub$max),
      min = min(df.sub$min),
      Date = as.character(Sys.Date())
    )

    fit.res <- rbind(fit.res, df.temp)
  }

  fit.res <- fit.res[!duplicated(fit.res$Gene), ]

  # 绘图
  p <- ggplot(df.final, aes(mean, Relative.Conc, fill = gene)) +
    geom_smooth(
      formula = y ~ x,
      method = "lm",
      se = TRUE, colour = "black", span = 0.8
    ) +
    geom_point() +
    facet_wrap(.~ gene, ncol = 2) +
    stat_poly_eq(aes(label = paste(..eq.label..,
                                   ..rr.label..,
                                   ..p.value.label..,
                                   sep = "~~~~"
    )),
    formula = y ~ x,
    parse = T,
    rr.digits = 5,
    coef.digits = 3,
    label.x = c(0.05),
    label.y = c(0.03)
    ) +
    labs(y = "Relative.Conc (log2)", x = "Cq") +
    scale_y_continuous(breaks = round(seq(
      min(df.final$Relative.Conc),
      max(df.final$Relative.Conc), 1
    ), 2)) +
    scale_x_continuous(breaks = round(seq(
      min(df.final$mean),
      max(df.final$mean) + 1, 1
    ), 1)) +
    theme_bw() +
    theme(legend.position = 'none')

  filename = paste0('StandardCurve','-', as.character(Sys.Date()))

  for (i in 1:length(genes)) {
    filename = paste0(filename, '+', genes[i])
  }

  filename = stringr::str_replace(paste0(filename,'.', fig.type),'\\+','-')

  if (save.fig == TRUE) {
    ggsave(p, filename = filename, width = 12, height = 8)
  }

  return(fit.res)
}
