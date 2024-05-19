
  library(magrittr)

  temp <- tibble::tribble(
    ~data_name, ~link, ~DOI, ~ref_md,
    "WorldClim", "https://www.worldclim.org/", "10.1002/joc.1276", "[@RN4478]",
    "DEW", "https://www.environment.sa.gov.au/topics/Science/mapland/spatial-gis-data", NA, NA,
    "Hobbs", "https://data.environment.sa.gov.au/Content/Publications/DEW-TR-2018-04.pdf", NA, "[@RN4479]",
    "Sentinel", "https://www.sentinel-hub.com/", "10.5066/F76W992G", "knitcitations::citep('10.5066/f76w992g')",
    "SRTM", "https://www2.jpl.nasa.gov/srtm/", "10.5066/F7PR7TFT", "knitcitations::citep('10.5066/f7pr7tft')",
    "ASRIS", "https://www.asris.csiro.au/", "10.1071/SR02033", "knitcitations::citep('10.1071/SR02033')",
    "KIDTM1m", "https://www.airborneresearch.org.au/ki-fires-2020-downloads", NA, NA,
    "Persistent Green", "https://geonetwork.tern.org.au/geonetwork/srv/eng/catalog.search#/metadata/e60f5125-ed2f-47cb-99a7-c9a201e44d2f", NA, NA
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


