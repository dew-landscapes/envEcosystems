
  library(magrittr)
  library(dplyr)
  library(purrr)
  library(fs)

  codes <- list.files(path = "data-raw",pattern = "\\.R$", full.names = TRUE) %>%
    grep("makeData",.,value = TRUE, invert = TRUE)

  lapply(codes,source)

  datas <- ls(pattern = "cut|lu|Spp") %>%
    tibble::enframe(name = NULL, value = "name") %>%
    dplyr::mutate(obj = map(name,get)
                  , savefile = path("data",paste0(name,".rData"))
                  )

  data <- datas %>%
    dplyr::pull(obj) %>%
    stats::setNames(datas$name)

  walk2(data,datas$savefile,~save(.x,file = .y))
