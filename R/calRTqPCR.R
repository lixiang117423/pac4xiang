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
#   Test Package:              'Ctrl + Shift + T'#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate group_by ungroup
calRTqPCR = function(data,
                     StandCurve,
                     genes,
                     treatment = c('T1','T2','T3'),
                     drop.NA = FALSE,
                     fill.NA = 'mean') {
  df <- fread(data, header = TRUE) %>%
    dplyr::select(Position, Cq) %>%
    dplyr::mutate(
      P = stringr::str_sub(Position, 1, 1),
      N = as.numeric(stringr::str_sub(Position, 2, nchar(Position)))
    ) %>%
    dplyr::select(N, P, Cq)


  df.1 = data.frame(N = c(1,5,9,2,6,10,3,7,11,4,8,12),
                    Gene = rep(genes, each = 3))
  df.2 = data.frame(N = 1:12,
                    Treatment = rep(treatment, each = 4))

  df = merge(df, df.1, by = 'N') %>% merge(df.2, by = 'N') %>%
    dplyr::mutate(group = paste0(Gene,'-',Treatment))

  df.3 = df %>% dplyr::filter(Cq == '-') %>%
    dplyr::mutate(temp = paste0(Gene,N,P))

  df.4 = df %>% dplyr::filter(Cq != '-') %>%
    merge(StandCurve, by = 'Gene') %>%
    dplyr::mutate(out = '')

  for (i in 1:nrow(df.4)) {
    if (df.4$Cq[i] > df.4$max[i] | df.4$Cq[i] < df.4$min[i]) {
      df.4$out[i] = 1
    }else{
      df.4$out[i] = 0
    }
  }

  df.5 = df.4 %>% dplyr::filter(out == 0) %>%
    dplyr::mutate(Cq = as.numeric(Cq)) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(mean = mean(Cq)) %>%
    dplyr::ungroup()

  df.6 = df.4 %>% dplyr::filter(out == 1) %>%
    dplyr::mutate(temp = paste0(Gene,N,P))

  # 填充缺失值
  df.4 = merge(df.3, df.5[,c('group','mean')], by = 'group')
  df.4 = df.4[!duplicated(df.4$temp),] %>%
    dplyr::mutate(Cq = mean) %>%
    dplyr::select(Treatment, Gene,P, N, Cq)

  df.6 = merge(df.6, df.5[,c('group','mean')], by = 'group')
  df.6 = df.6[!duplicated(df.6$temp),] %>%
    dplyr::mutate(Cq = mean) %>%
    dplyr::select(Treatment, Gene,P, N, Cq)

  df.5 = df.5 %>% dplyr::select(Treatment, Gene,P, N, Cq)

  df.final = rbind(df.4,df.5,df.6) %>%
    dplyr::mutate(group = paste0(Treatment, Gene)) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(N = n(),
                  Mean = mean(Cq),
                  SD = sd(Cq),
                  SE = SD/sqrt(N)) %>%
    merge(StandCurve, by = 'Gene') %>%
    dplyr::mutate(Expression = Cq * Slope + Intercept) %>%
    dplyr::select(Treatment,Gene,Cq,N,Mean,SD,SE,Formula,R2,P.value,Expression,Date)

  return(df.final)

}















