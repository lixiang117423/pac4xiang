#' @name well96to384
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title 96 well to 384.
#'
#' @export
#'
well96to384 <- function(file.input, return = "all") {
  df <- file.input %>%
    tidyr::pivot_longer(cols = 2:13) %>%
    magrittr::set_names(c("tag", "num", "sample")) %>%
    dplyr::mutate(temp = paste0(tag, num))

  new.tab <- data.frame(temp = 1:16)

  for (i in seq(1, 12, 2)) {
    df %>%
      dplyr::filter(num %in% c(i, i + 1)) %>%
      dplyr::arrange(tag) %>%
      dplyr::select(temp) %>%
      cbind(new.tab) -> new.tab

    if (i == 11) {
      new.tab %>%
        magrittr::set_names(c(1:7)) %>%
        dplyr::select(6:1) -> new.tab.model
    }
  }

  new.tab <- data.frame(temp = 1:16)

  for (i in seq(1, 12, 2)) {
    df %>%
      dplyr::filter(num %in% c(i, i + 1)) %>%
      dplyr::arrange(tag) %>%
      dplyr::select(sample) %>%
      cbind(new.tab) -> new.tab

    if (i == 11) {
      new.tab %>%
        magrittr::set_names(c(1:7)) %>%
        dplyr::select(6:1) -> new.tab.sample
    }
  }

  if (return == "all") {
    return.list <- list(
      model = new.tab.model,
      sample = new.tab.sample
    )
  } else if (return == "sample") {
    return(new.tab.sample)
  } else {
    return(new.tab.model)
  }
}
