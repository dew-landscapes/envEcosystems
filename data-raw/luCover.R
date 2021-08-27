
  library(magrittr)

  # Cover
  lucover <- rio::import("data-raw/lucover.csv") %>%
    tibble::as_tibble()
