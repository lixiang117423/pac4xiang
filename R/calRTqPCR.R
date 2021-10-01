#' @name calRTqPCR
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Calculate Relative Expression value for RT-qPCR.
#' @description
#' \code{calStandCurve} Calculate Standard Curve for RT-qPCR.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table fread
#' @importFrom stringr str_sub str_split
#' @importFrom dplyr select filter mutate group_by ungroup
#'
#' @examples
#' filepath.1 <- system.file("examples", "20210928lx_1.txt", package = "pac4xiang")
#' filepath.2 <- system.file("examples", "20210929lx_1.txt", package = "pac4xiang")
#' stand.curve <- calStandCurve (data = filepath.1, genes = c("A","B","C","D"),
#'                               dilution = 4,start = 2, end = 7,rep = 4,
#'                               drop.NA = FALSE, fill.NA = "mean",
#'                               save.fig = TRUE, fig.type = 'pdf')
#' exp <- calRTqPCR(data = filepath.2,
#'                 StandCurve = stand.curve,
#'                 genes = c("A","B","C","D"),
#'                 treatment = c("T1", "T2", "T3"),
#'                 drop.NA = FALSE,
#'                 fill.NA = "mean")
#' @export
#'
#' @return Return a datafram
#'
utils::globalVariables(c("Treatment", "Gene", "Cq", "N", "Mean", "SD", "SE",
                         "Formula", "R2", "P.value", "Expression", "Date",
                         "out","temp","n","Slope","Intercept"))

calRTqPCR <- function(data,
                      StandCurve,
                      genes,
                      treatment = c("T1", "T2", "T3"),
                      drop.NA = FALSE,
                      fill.NA = "mean") {
  df <- data.table::fread(data, header = TRUE) %>%
    dplyr::select(Position, Cq) %>%
    dplyr::mutate(
      P = stringr::str_sub(Position, 1, 1),
      N = as.numeric(stringr::str_sub(Position, 2, nchar(Position)))
    ) %>%
    dplyr::select(N, P, Cq)


  df.1 <- data.frame(
    N = c(1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12),
    Gene = rep(genes, each = 3)
  )
  df.2 <- data.frame(
    N = 1:12,
    Treatment = rep(treatment, each = 4)
  )

  df <- merge(df, df.1, by = "N") %>%
    merge(df.2, by = "N") %>%
    dplyr::mutate(group = paste0(Gene, "-", Treatment))

  df.3 <- df %>%
    dplyr::filter(Cq == "-") %>%
    dplyr::mutate(temp = paste0(Gene, N, P))

  df.4 <- df %>%
    dplyr::filter(Cq != "-") %>%
    merge(StandCurve, by = "Gene") %>%
    dplyr::mutate(out = "")

  for (i in 1:nrow(df.4)) {
    if (df.4$Cq[i] > df.4$max[i] | df.4$Cq[i] < df.4$min[i]) {
      df.4$out[i] <- 1
    } else {
      df.4$out[i] <- 0
    }
  }

  df.5 <- df.4 %>%
    dplyr::filter(out == 0) %>%
    dplyr::mutate(Cq = as.numeric(Cq)) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(mean = mean(Cq)) %>%
    dplyr::ungroup()

  df.6 <- df.4 %>%
    dplyr::filter(out == 1) %>%
    dplyr::mutate(temp = paste0(Gene, N, P))

  # 填充缺失值
  df.5.1 <- df.5 %>% dplyr::filter(group %in% unique(df.3$group))
  df.5.1 <- df.5.1[!duplicated(df.5.1$group), ]

  df.3 <- merge(df.3[,c('group','temp')], df.5.1, by = "group")
  df.3 <- df.3[!duplicated(df.3$temp), ] %>%
    dplyr::mutate(Cq = mean) %>%
    dplyr::select(-temp) %>%
    dplyr::select(colnames(df.5))


  df.5.1 <- df.5 %>% dplyr::filter(group %in% unique(df.6$group))
  df.5.1 <- df.5.1[!duplicated(df.5.1$group), ]

  df.6 <- merge(df.6[,c('group','temp')], df.5.1, by = "group")

  df.6 <- df.6[!duplicated(df.6$temp), ] %>%
    dplyr::mutate(Cq = mean) %>%
    dplyr::select(-temp) %>%
    dplyr::select(colnames(df.5))

  df.final <- rbind(df.3, df.5, df.6) %>%
    dplyr::mutate(group = paste0(Treatment, Gene)) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(
      N = n(),
      Mean = mean(Cq),
      SD = sd(Cq),
      SE = SD / sqrt(N),
      Expression = Cq * Slope + Intercept
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(Treatment, Gene, Cq, N, Mean, SD, SE, Formula, R2, P.value, Expression, Date)

  return(df.final)
}
