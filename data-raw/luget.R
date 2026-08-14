
library(magrittr)

get <- fs::dir_info("data-raw"
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
                , biome = purrr::map_chr(code
                                          , \(x) gsub("\\..*", "", x)
                                          )
                , name = purrr::map2_chr(code, name
                                         , \(x, y) gsub(paste0(x, " "), "", y)
                                         )
                , efg = as.integer(gsub(".*\\.", "", code))
                , efg_code = paste0(biome, ".", stringr::str_pad(efg, width = 2, pad = "0"))
                , efg_name = paste0(efg_code, " ", name)
                ) %>%
  purrr::set_names(gsub("ecosystem |ecological ", "", names(.))) |>
  dplyr::select(realm, biome, tidyselect::matches("efg_"), tidyselect::everything())

add_get <- tibble::tribble(
  ~realm, ~biome, ~efg_code, ~efg_name, ~code, ~name, ~properties, ~drivers, ~distribution, ~contributors, ~update,
  "T", "T5", "T5.06", "T5.06 Semi-arid woodlands", "T5.6", "Semi-arid woodlands",
  # properties
  "These ecosystems include a sparse to open layer of low trees commonly 3-10 m tall, with an open cover of shrubs and highly variable and ephemeral ground vegetation of grasses and forbs. They differ from T4.4 Subhumid temperate woodlands in having sparser vegetation with lower stature, lower and more interannually variable productivity, and greater representation of xeric traits and opportunist life cycles in both plants and animals. Trees and long-lived shrubs are evergreen stress-tolerators with slow growth rates, low or intermittent fecundity, deep and laterally extensive root systems, and nanophyll-microphyll leaves with low SLA, sunken diurnally regulated stomates, thick cuticles, hairs, heat-reflective coloration and vertical orientation. They structure habitats of vertebrates and invertebrates dependent on tree canopies, woody debris and leaf litter. Many grasses, forbs and some shrubs have short-lived standing plant phases and long-lived soil seed banks cued by substantial rainfall events and, in some cases, seasonal dormancy to avoid desiccating summers. Grasses may include species with either C3 or C4 photosynthetic pathways, but C4 is typically dominant. Most mammals are nocturnal, sheltering below ground or under shade trees during hot days. They include fossorial species that contribute to soil turnover and nutrient cycling, species with physiological and behavioural water conservation traits, as well as small mammals whose populations fluctuate with interannual rainfall variation. Granivory is conspicuous among birds and small mammals, while herbivores with medium body sizes may influence vegetation structure and composition, particularly recruitment of long-lived woody plants. Some semi-arid woodlands are fire-prone and include species with fire-cued life histories.",
  # drivers
  "These open ecosystems are transitional on a water balance gradient between deserts and grassy ecosystems. They experience a substantial, interannually variable water deficit, peaking with evapotranspiration in hot summers. Mean annual rainfall is 200–500 mm, weakly seasonal. Temperatures may fall below zero in winter. Soils may be fine-textured or sandy, red or pale, with moderate to low fertility and reliably moist only at depth. While some ecosystems within this group are not fire-prone, others experience canopy fires at multi-decadal intervals, driven largely by ephemeral fuels after wet periods.",
  # distribution
  "Semi-arid regions of southern Australia, South America, western and eastern North America, the Mediterranean region, and Eurasia.",
  # contributors
  NA_character_,
  # update
  NA_character_
  )

luget <- dplyr::bind_rows(get, add_get) |>
  dplyr::arrange(efg_code)

