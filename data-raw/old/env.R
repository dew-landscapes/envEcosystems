
  library(magrittr)

  temp <- tibble::tribble(
    ~data_name, ~link,
    "WorldClim", "https://www.worldclim.org/",
    "DEW", "https://www.environment.sa.gov.au/topics/Science/mapland/spatial-gis-data",
    "Hobbs", "https://data.environment.sa.gov.au/Content/Publications/DEW-TR-2018-04.pdf",
    "Sentinel", "https://www.sentinel-hub.com/",
    "SRTM", "https://www2.jpl.nasa.gov/srtm/",
    "ASRIS", "https://www.asris.csiro.au/",
    "KIDTM1m", "https://www.airborneresearch.org.au/ki-fires-2020-downloads",
    "persistent green", "https://geonetwork.tern.org.au/geonetwork/srv/eng/catalog.search#/metadata/e60f5125-ed2f-47cb-99a7-c9a201e44d2f",
    "green cover", "https://geonetwork.tern.org.au/geonetwork/srv/eng/catalog.search#/metadata/f0c32576-9ad7-4c9c-9aa9-22787867e28b",
    "bare ground", "https://geonetwork.tern.org.au/geonetwork/srv/eng/catalog.search#/metadata/f0c32576-9ad7-4c9c-9aa9-22787867e28b",
    "non-green cover", "https://geonetwork.tern.org.au/geonetwork/srv/eng/catalog.search#/metadata/f0c32576-9ad7-4c9c-9aa9-22787867e28b",
    "surface reflectance", "https://geonetwork.tern.org.au/geonetwork/srv/eng/catalog.search#/metadata/5a31eed4-e43a-404d-b534-3f820305ed61",
    "water from space", "https://www.ga.gov.au/scientific-topics/community-safety/flood/wofs"
    ) %>%
    dplyr::mutate(link_md = paste0("[",data_name,"]"
                                   , "(",link,")"
                                   )
                  )

  env <- rio::import("data-raw/env.xlsx") %>%
    dplyr::mutate(layer = as.character(layer)) %>%
    tibble::as_tibble() %>%
    dplyr::left_join(temp)

  rm(temp)
