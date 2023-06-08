getCoreMicrobiome <- function(otu, meta, sample.name, group = "group", top = 500) {
  # 核心微生物鉴定方法
  library(tidyverse)
  library(reshape2)
  library(vegan)
  library(ggsci)
  library(ggthemes)

  ##############################################################################
  ############################## 定义关键函数 ##################################
  ##############################################################################

  # Fits the neutral model from Sloan et al. 2006 to an OTU table and returns several fitting statistics. Alternatively, will return predicted occurrence frequencies for each OTU based on their abundance in the metacommunity when stats=FALSE.
  # spp: 丰度表，行是样本，列是微生物信息，如OTU或者是ASV，而且必须是抽平以后的数据。
  # pool: A community table for defining source community (optional; Default=NULL).
  # taxon: OTU或者是ASV的信息，行是OTU或者ASV，列是分类信息。
  # If stats=TRUE the function will return fitting statistics.
  # If stats=FALSE the function will return a table of observed and predicted values for each otu.

  sncm.fit <- function(spp, pool = NULL, stats = TRUE, taxon = NULL) {
    require(minpack.lm)
    require(Hmisc)
    require(stats4)

    options(warn = -1)

    # Calculate the number of individuals per community
    N <- mean(apply(spp, 1, sum))

    # Calculate the average relative abundance of each taxa across communities
    if (is.null(pool)) {
      p.m <- apply(spp, 2, mean)
      p.m <- p.m[p.m != 0]
      p <- p.m / N
    } else {
      p.m <- apply(pool, 2, mean)
      p.m <- p.m[p.m != 0]
      p <- p.m / N
    }

    # Calculate the occurrence frequency of each taxa across communities
    spp.bi <- 1 * (spp > 0)
    freq <- apply(spp.bi, 2, mean)
    freq <- freq[freq != 0]

    # Combine
    C <- merge(p, freq, by = 0)
    C <- C[order(C[, 2]), ]
    C <- as.data.frame(C)
    C.0 <- C[!(apply(C, 1, function(y) any(y == 0))), ] # Removes rows with any zero (absent in either source pool or local communities)
    p <- C.0[, 2]
    freq <- C.0[, 3]
    names(p) <- C.0[, 1]
    names(freq) <- C.0[, 1]

    # Calculate the limit of detection
    d <- 1 / N

    ## Fit model parameter m (or Nm) using Non-linear least squares (NLS)
    m.fit <- nlsLM(freq ~ pbeta(d, N * m * p, N * m * (1 - p), lower.tail = FALSE), start = list(m = 0.1))
    m.ci <- confint(m.fit, "m", level = 0.95)

    ## Fit neutral model parameter m (or Nm) using Maximum likelihood estimation (MLE)
    sncm.LL <- function(m, sigma) {
      R <- freq - pbeta(d, N * m * p, N * m * (1 - p), lower.tail = FALSE)
      R <- dnorm(R, 0, sigma)
      -sum(log(R))
    }
    m.mle <- mle(sncm.LL, start = list(m = 0.1, sigma = 0.1), nobs = length(p))

    ## Calculate Akaike's Information Criterion (AIC)
    aic.fit <- AIC(m.mle, k = 2)
    bic.fit <- BIC(m.mle)

    ## Calculate goodness-of-fit (R-squared and Root Mean Squared Error)
    freq.pred <- pbeta(d, N * coef(m.fit) * p, N * coef(m.fit) * (1 - p), lower.tail = FALSE)
    Rsqr <- 1 - (sum((freq - freq.pred)^2)) / (sum((freq - mean(freq))^2))
    RMSE <- sqrt(sum((freq - freq.pred)^2) / (length(freq) - 1))

    pred.ci <- binconf(freq.pred * nrow(spp), nrow(spp), alpha = 0.05, method = "wilson", return.df = TRUE)

    ## Calculate AIC for binomial model
    bino.LL <- function(mu, sigma) {
      R <- freq - pbinom(d, N, p, lower.tail = FALSE)
      R <- dnorm(R, mu, sigma)
      -sum(log(R))
    }
    bino.mle <- mle(bino.LL, start = list(mu = 0, sigma = 0.1), nobs = length(p))

    aic.bino <- AIC(bino.mle, k = 2)
    bic.bino <- BIC(bino.mle)

    ## Goodness of fit for binomial model
    bino.pred <- pbinom(d, N, p, lower.tail = FALSE)
    Rsqr.bino <- 1 - (sum((freq - bino.pred)^2)) / (sum((freq - mean(freq))^2))
    RMSE.bino <- sqrt(sum((freq - bino.pred)^2) / (length(freq) - 1))

    bino.pred.ci <- binconf(bino.pred * nrow(spp), nrow(spp), alpha = 0.05, method = "wilson", return.df = TRUE)

    ## Calculate AIC for Poisson model
    pois.LL <- function(mu, sigma) {
      R <- freq - ppois(d, N * p, lower.tail = FALSE)
      R <- dnorm(R, mu, sigma)
      -sum(log(R))
    }
    pois.mle <- mle(pois.LL, start = list(mu = 0, sigma = 0.1), nobs = length(p))

    aic.pois <- AIC(pois.mle, k = 2)
    bic.pois <- BIC(pois.mle)

    ## Goodness of fit for Poisson model
    pois.pred <- ppois(d, N * p, lower.tail = FALSE)
    Rsqr.pois <- 1 - (sum((freq - pois.pred)^2)) / (sum((freq - mean(freq))^2))
    RMSE.pois <- sqrt(sum((freq - pois.pred)^2) / (length(freq) - 1))

    pois.pred.ci <- binconf(pois.pred * nrow(spp), nrow(spp), alpha = 0.05, method = "wilson", return.df = TRUE)

    ## Results
    if (stats == TRUE) {
      fitstats <- data.frame(m = numeric(), m.ci = numeric(), m.mle = numeric(), maxLL = numeric(), binoLL = numeric(), poisLL = numeric(), Rsqr = numeric(), Rsqr.bino = numeric(), Rsqr.pois = numeric(), RMSE = numeric(), RMSE.bino = numeric(), RMSE.pois = numeric(), AIC = numeric(), BIC = numeric(), AIC.bino = numeric(), BIC.bino = numeric(), AIC.pois = numeric(), BIC.pois = numeric(), N = numeric(), Samples = numeric(), Richness = numeric(), Detect = numeric())
      fitstats[1, ] <- c(coef(m.fit), coef(m.fit) - m.ci[1], m.mle@coef["m"], m.mle@details$value, bino.mle@details$value, pois.mle@details$value, Rsqr, Rsqr.bino, Rsqr.pois, RMSE, RMSE.bino, RMSE.pois, aic.fit, bic.fit, aic.bino, bic.bino, aic.pois, bic.pois, N, nrow(spp), length(p), d)
      return(fitstats)
    } else {
      A <- cbind(p, freq, freq.pred, pred.ci[, 2:3], bino.pred, bino.pred.ci[, 2:3])
      A <- as.data.frame(A)
      colnames(A) <- c("p", "freq", "freq.pred", "pred.lwr", "pred.upr", "bino.pred", "bino.lwr", "bino.upr")
      if (is.null(taxon)) {
        B <- A[order(A[, 1]), ]
      } else {
        B <- merge(A, taxon, by = 0, all = TRUE)
        row.names(B) <- B[, 1]
        B <- B[, -1]
        B <- B[order(B[, 1]), ]
      }
      return(B)
    }
  }

  ##############################################################################
  ########################### 时间序列核心微生物 ###############################
  ##############################################################################

  {{ otu }} %>%
    as.data.frame() -> df.otu

  {{ meta }} %>%
    as.data.frame() -> df.meta

  # 使用tidyverse重写原来的代码
  df.otu %>%
    tibble::rownames_to_column(var = "otu") %>%
    tidyr::pivot_longer(cols = 2:ncol(.)) %>%
    # 存在-不存在鉴定，将丰度大于0的标记为1，丰度为0的标记为0
    dplyr::mutate(
      otu.pa = case_when(
        value > 0 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(otu) %>%
    dplyr::mutate(otu.sum = sum(otu.pa)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(name) %>%
    # 计算OTU存在的频率，也就是在多少样本里面出现过
    dplyr::mutate(
      n.sample = ncol(df.otu),
      sum.sample = sum(value)
    ) %>%
    dplyr::mutate(
      otu.occ = otu.sum / n.sample,
      decostand = value / sum.sample
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(otu) %>%
    # 计算每个OTU的平均相对丰度
    dplyr::mutate(otu.rel = mean(decostand)) %>%
    dplyr::ungroup() -> df.otu.new

  # 基于出现频率对OTU进行排序
  df.otu %>%
    tibble::rownames_to_column(var = "otu") %>%
    dplyr::mutate(otu = factor(otu)) %>%
    tidyr::pivot_longer(
      cols = 2:ncol(.),
      names_to = {{ sample.name }}, values_to = "abun"
    ) %>%
    dplyr::left_join(df.meta, by = {{ sample.name }}) %>%
    dplyr::group_by(otu, {{ group }}) %>%
    summarise(
      freq = sum(abun > 0) / length(abun), # frequency of detection between time points
      core = ifelse(freq == 1, 1, 0)
    ) %>%
    group_by(otu) %>%
    summarise(
      sumF = sum(freq),
      sumG = sum(core),
      nS = length({{ group }}),
      Index = (sumF + sumG) / nS
    ) -> PresenceSum

  df.otu.new %>%
    dplyr::select(otu, otu.rel, otu.occ) %>%
    dplyr::distinct_all() %>%
    dplyr::left_join(PresenceSum, by = "otu") %>%
    dplyr::transmute(
      otu = otu,
      rank = Index
    ) %>%
    dplyr::arrange(desc(rank)) -> otu.ranked


  # 计算OTU的贡献度
  BCaddition <- NULL

  otu.start <- otu.ranked$otu[1]
  start.matrix <- df.otu[otu.start, ] %>% as.matrix()
  x <- apply(combn(ncol(start.matrix), 2), 2, function(x) sum(abs(start.matrix[, x[1]] - start.matrix[, x[2]])) / (2 * colSums(df.otu)[1]))
  x.names <- apply(combn(ncol(start.matrix), 2), 2, function(x) paste(colnames(start.matrix)[x], collapse = " - "))

  data.frame(x.names, x) -> df.s
  names(df.s)[2] <- 1
  BCaddition <- rbind(BCaddition, df.s)

  # length(otu.ranked$otu)
  for (i in 2:{{ top }}) {
    otu.add <- otu.ranked$otu[i]
    add.matrix <- df.otu[otu.add, ] %>% as.matrix()
    start.matrix <- rbind(start.matrix, add.matrix)
    x <- apply(combn(ncol(start.matrix), 2), 2, function(x) sum(abs(start.matrix[, x[1]] - start.matrix[, x[2]])) / (2 * colSums(df.otu)[1]))
    x.names <- apply(combn(ncol(start.matrix), 2), 2, function(x) paste(colnames(start.matrix)[x], collapse = " - "))
    df.a <- data.frame(x.names, x)
    names(df.a)[2] <- i
    BCaddition <- dplyr::left_join(BCaddition, df.a, by = c("x.names"))
    sprintf("%s/%s is done!==========", i, {{ top }}) %>% print()
  }

  # 计算整个群落的贡献度
  x <- apply(combn(ncol(df.otu), 2), 2, function(x) sum(abs(df.otu[, x[1]] - df.otu[, x[2]])) / (2 * colSums(df.otu)[1]))
  x.names <- apply(combn(ncol(df.otu), 2), 2, function(x) paste(colnames(df.otu)[x], collapse = " - "))
  df.full <- data.frame(x.names, x)
  names(df.full)[2] <- length(rownames(df.otu))
  BCfull <- dplyr::left_join(BCaddition, df.full, by = "x.names") %>%
    tibble::column_to_rownames(var = "x.names")

  temp_BC <- BCfull
  temp_BC$x_names <- NULL
  temp_BC_matrix <- as.matrix(temp_BC)

  BC_ranked <- data.frame(rank = as.factor(row.names(t(temp_BC_matrix))), t(temp_BC_matrix)) %>%
    gather(comparison, BC, -rank) %>%
    group_by(rank) %>%
    summarise(MeanBC = mean(BC)) %>% # mean Bray-Curtis dissimilarity
    arrange(desc(-MeanBC)) %>%
    mutate(proportionBC = MeanBC / max(MeanBC)) # proportion of the dissimilarity explained by the n number of ranked OTUs
  Increase <- BC_ranked$MeanBC[-1] / BC_ranked$MeanBC[-length(BC_ranked$MeanBC)]
  increaseDF <- data.frame(IncreaseBC = c(0, (Increase)), rank = factor(c(1:(length(Increase) + 1))))
  BC_ranked <- dplyr::left_join(BC_ranked, increaseDF)
  BC_ranked <- BC_ranked[-nrow(BC_ranked), ]

  # 设置阈值进行筛选

  # 方法一：Elbow method (first order difference)

  fo_difference <- function(pos) {
    left <- (BC_ranked[pos, 2] - BC_ranked[1, 2]) / pos
    right <- (BC_ranked[nrow(BC_ranked), 2] - BC_ranked[pos, 2]) / (nrow(BC_ranked) - pos)
    return(left - right)
  }

  BC_ranked$fo_diffs <- sapply(1:nrow(BC_ranked), fo_difference)

  elbow <- which.max(BC_ranked$fo_diffs)

  # 方法二：Final increase in BC similarity of equal or greater then 2%
  lastCall <- last(as.numeric(as.character(BC_ranked$rank[(BC_ranked$IncreaseBC >= 1.02)])))

  # 绘图
  thread <- BC_ranked[BC_ranked$rank == lastCall, ]$proportionBC

  BC_ranked %>%
    dplyr::slice(1:100) %>%
    dplyr::mutate(rank = as.numeric(rank)) %>%
    dplyr::mutate(col = case_when(
      proportionBC < thread ~ "Core",
      TRUE ~ "Non-core"
    )) %>%
    dplyr::mutate(rank = factor(rank, levels = rank)) -> df.rankedOTUs

  # 排序图
  df.rankedOTUs %>%
    ggplot(aes(rank, proportionBC, color = col)) +
    geom_point(size = 2) +
    geom_vline(xintercept = elbow, lty = 3, col = "red", cex = .5, linewidth = 1) +
    geom_vline(xintercept = lastCall, lty = 3, col = "blue", cex = .5, linewidth = 1) +
    annotate(
      geom = "text", x = elbow + 14, y = .1,
      label = paste("Elbow method", " (", elbow, ")", sep = ""), color = "red"
    ) +
    annotate(
      geom = "text", x = lastCall + 15, y = .5,
      label = paste("Last 2% increase (", lastCall, ")", sep = ""), color = "blue"
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.1)
    ) +
    labs(x = "ranked OTUs", y = "Bray-Curtis similarity") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = c(0.8, 0.2),
      legend.title = element_blank(),
      legend.text = element_text(size = 20)
    ) -> plot.rankedOTUs

  # 另外一种展示方式
  # 基于中性模型
  # Use Sloan neutral model to prioritize OTUs
  # Fitting neutral model (Burns et al., 2016 (ISME J) - functions are in the sncm.fit.R)
  spp <- t(df.otu)
  taxon <- as.vector(rownames(df.otu))

  # Models for the whole community
  obs.np <- sncm.fit(spp, taxon, stats = FALSE, pool = NULL)
  sta.np <- sncm.fit(spp, taxon, stats = TRUE, pool = NULL)

  above.pred <- sum(obs.np$freq > (obs.np$pred.upr), na.rm = TRUE) / sta.np$Richness # fraction of OTUs above prediction
  below.pred <- sum(obs.np$freq < (obs.np$pred.lwr), na.rm = TRUE) / sta.np$Richness # fraction of OTUs below prediction

  # Create a column defining "core" OTUs
  df.otu.new %>%
    dplyr::mutate(fill = case_when(
      otu %in% otu.ranked$otu[1:lastCall] ~ "Core",
      TRUE ~ "Non-core"
    )) %>%
    dplyr::select(otu.rel, otu.occ, fill) %>%
    dplyr::distinct_all() -> df.fit.results

  df.fit.results %>%
    ggplot() +
    geom_point(aes(log10(otu.rel), otu.occ, color = fill), size = 4, alpha = 0.8) +
    geom_line(
      color = "black", data = obs.np, size = 1,
      aes(y = obs.np$freq.pred, x = log10(obs.np$p)), alpha = .5
    ) +
    geom_line(
      color = "black", lty = "twodash", size = 1, data = obs.np,
      aes(y = obs.np$pred.upr, x = log10(obs.np$p)), alpha = .5
    ) +
    geom_line(
      color = "black", lty = "twodash", size = 1, data = obs.np,
      aes(y = obs.np$pred.lwr, x = log10(obs.np$p)), alpha = .5
    ) +
    labs(x = "log10(mean relative abundance)", y = "Occupancy") +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.1)
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      legend.position = c(0.2, 0.8),
      legend.title = element_blank(),
      legend.text = element_text(size = 20)
    ) -> plot.fit.results

  return.list <- list(
    elbow = elbow,
    lastCall = lastCall,
    obs.np = obs.np,
    rankedOTUs = df.rankedOTUs,
    fitting.results = df.fit.results,
    plot.rankedOTUs = plot.rankedOTUs,
    plot.fit.results = plot.fit.results
  )
  return(return.list)
}
