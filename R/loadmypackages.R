#' @name loadmypackages
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Load my packages.
#'
#' @export
#'
loadmypackages = function(...){
  # load packages
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(Biobase))
  suppressPackageStartupMessages(library(knitr))
  suppressPackageStartupMessages(library(magrittr))
  suppressPackageStartupMessages(library(multcomp))
  suppressPackageStartupMessages(library(emmeans))
  suppressPackageStartupMessages(library(cowplot))
  suppressPackageStartupMessages(library(patchwork))
  suppressPackageStartupMessages(library(writexl))
  suppressPackageStartupMessages(library(ggsci))
  suppressPackageStartupMessages(library(ggpmisc))
  suppressPackageStartupMessages(library(kableExtra))
  suppressPackageStartupMessages(library(plotly))
  suppressPackageStartupMessages(library(gganimate))
  suppressPackageStartupMessages(library(ggthemes))
  suppressPackageStartupMessages(library(agricolae))
  suppressPackageStartupMessages(library(ggtext))
  suppressPackageStartupMessages(library(DESeq2))
  suppressPackageStartupMessages(library(FactoMineR))
  suppressPackageStartupMessages(library(factoextra))
  suppressPackageStartupMessages(library(ggh4x))
  suppressPackageStartupMessages(library(ggheatmap))
  suppressPackageStartupMessages(library(clusterProfiler))
  suppressPackageStartupMessages(library(enrichplot))
  suppressPackageStartupMessages(library(qPCRtools))
  suppressPackageStartupMessages(library(janitor))
  suppressPackageStartupMessages(library(viridis))
}
