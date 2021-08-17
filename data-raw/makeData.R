
  codes <- list.files(path = "data-raw",pattern = "\\.R$", full.names = TRUE)

  lapply(codes,source)

  datas <- ls(pattern = "cut|lu|Spp")

  do.call(save, c(lapply(datas,as.name), file = "R/sysdata.rda"))
