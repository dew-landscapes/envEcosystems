
  library(magrittr)

  temp <- tibble::tribble(
    ~data_name, ~link,
    "WorldClim", "https://www.worldclim.org/",
    "DEW", "https://www.environment.sa.gov.au/topics/Science/mapland/spatial-gis-data",
    "Landsat", "https://landsat.gsfc.nasa.gov/",
    "Sentinel", "https://www.sentinel-hub.com/",
    "SRTM", "https://www2.jpl.nasa.gov/srtm/",
    "ASRIS", "https://www.asris.csiro.au/"
    ) %>%
    dplyr::mutate(link_md = paste0("[",data_name,"]"
                                   , "(",link,")"
                                   )
                  )

  luenv <- rio::import("data-raw/luenv.xlsx") %>%
    tibble::as_tibble() %>%
    dplyr::left_join(temp)

  rm(temp)


