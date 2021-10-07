
  library(magrittr)

  temp <- tibble::tribble(
    ~data_name, ~link,
    "WorldClim", "https://www.worldclim.org/",
    "DEW", "https://www.environment.sa.gov.au/topics/Science/mapland/spatial-gis-data",
    "Landsat", "https://landsat.gsfc.nasa.gov/",
    "Sentinel", "https://www.sentinel-hub.com/",
    "SRTM", "https://www2.jpl.nasa.gov/srtm/",
    "ASRIS", "https://www.asris.csiro.au/",
    "KIDTM1m", "https://www.airborneresearch.org.au/ki-fires-2020-downloads"
    ) %>%
    dplyr::mutate(link_md = paste0("[",data_name,"]"
                                   , "(",link,")"
                                   )
                  )

  luenv <- rio::import("data-raw/luenv.xlsx") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(layer = gsub("\\..*$","",file)
                  , env_id = gsub("[[:punct:]]","",layer)
                  ) %>%
    dplyr::left_join(temp) %>%
    dplyr::select(data_name,file,layer,desc,group,indicator,everything())

  rm(temp)


