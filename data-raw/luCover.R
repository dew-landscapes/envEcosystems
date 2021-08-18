
  library(magrittr)

  # Cover
  lucover <- rio::import("data-raw/luCover.csv") %>%
    tibble::as_tibble() %>%
    stats::setNames(tolower(names(.)))
