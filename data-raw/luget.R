
luget <- fs::dir_info("data-raw"
                    , regexp = "\\.ods"
                    ) |>
  dplyr::select(path) |>
  dplyr::mutate(sheet = purrr::map(path, readODS::ods_sheets)) |>
  tidyr::unnest(cols = c(sheet)) |>
  dplyr::filter(grepl("Profile", sheet)) |>
  dplyr::mutate(data = purrr::map2(path, sheet, readODS::read_ods)) |>
  tidyr::unnest(cols = c(data)) |>
  janitor::remove_constant() |>
  dplyr::mutate(realm = gsub("\\d.*", "", code)
                , biome = purrr::map2_int(code, realm
                                          , \(x, y) as.numeric(gsub(paste0(y, "|\\..*"), "", x))
                                          )
                , name = purrr::map2_chr(code, name
                                         , \(x, y) gsub(paste0(x, " "), "", y)
                                         )
                , efg = as.integer(gsub(".*\\.", "", code))
                , efg_code = paste0(realm, biome, ".", stringr::str_pad(efg, width = 2, pad = "0"))
                , efg_name = paste0(efg_code, " ", name)
                ) %>%
  purrr::set_names(gsub("ecosystem |ecological ", "", names(.))) |>
  dplyr::select(realm, biome, tidyselect::matches("efg"), tidyselect::everything())

