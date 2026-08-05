
  library(magrittr)

  cut_cov <- tibble::tribble(
    ~cov_thresh, ~pfc
    , 200, "Extra dense"
    , 70, "Dense"
    , 30, "Mid-dense"
    , 10, "Sparse"
    , 5, "Very sparse"
    , 0, "Very very sparse"
    )

  cut_ht <- tibble::tribble(
    ~ht_thresh
    , 200
    , 30
    , 10
    , 5
    , 3
    , 2
    , 1
    , 0.5
    , 0
  )

  sa_vsf <- tibble::tribble(
    ~cov_thresh, ~ht_thresh, ~str, ~storey, ~sa_vsf
    , 100, 200, "Trees", "over", "Tall closed forest"
    , 70, 200, "Trees", "over", "Tall open forest"
    , 30, 200, "Trees", "over", "Tall woodland"
    , 10, 200, "Trees", "over", "Tall open woodland"
    , 5, 200, "Trees", "over", "Tall very open woodland"

    , 100, 30, "Trees", "over", "Closed forest"
    , 70, 30, "Trees", "over", "Open forest"
    , 30, 30, "Trees", "over", "Woodland"
    , 10, 30, "Trees", "over", "Open woodland"
    , 5, 30, "Trees", "over", "Very open woodland"

    , 100, 10, "Trees", "over", "Low closed forest"
    , 70, 10, "Trees", "over", "Low open forest"
    , 30, 10, "Trees", "over", "Low woodland"
    , 10, 10, "Trees", "over", "Low open woodland"
    , 5, 10, "Trees", "over", "Low very open woodland"

    , 100, 5, "Trees", "over", "Very low closed forest"
    , 70, 5, "Trees", "over", "Very low open forest"
    , 30, 5, "Trees", "over", "Very low woodland"
    , 10, 5, "Trees", "over", "Very low open woodland"
    , 5, 5, "Trees", "over", "Very low very open woodland"

    , 100, 3, "Trees", "over", "Very low closed forest"
    , 70, 3, "Trees", "over", "Very low open forest"
    , 30, 3, "Trees", "over", "Very low woodland"
    , 10, 3, "Trees", "over", "Very low open woodland"
    , 5, 3, "Trees", "over", "Very low very open woodland"

    , 100, 2, "Trees", "over", "Very low closed forest"
    , 70, 2, "Trees", "over", "Very low open forest"
    , 30, 2, "Trees", "over", "Very low woodland"
    , 10, 2, "Trees", "over", "Very low open woodland"
    , 5, 2, "Trees", "over", "Very low very open woodland"

    , 100, 30, "Mallees", "over", "Very tall closed mallee"
    , 70, 30, "Mallees", "over", "Very tall mallee"
    , 30, 30, "Mallees", "over", "Very tall open mallee"
    , 10, 30, "Mallees", "over", "Very tall very open mallee"
    , 5, 30, "Mallees", "over", "Very tall very very open mallee"

    , 100, 10, "Mallees", "over", "Tall closed mallee"
    , 70, 10, "Mallees", "over", "Tall mallee"
    , 30, 10, "Mallees", "over", "Tall open mallee"
    , 10, 10, "Mallees", "over", "Tall very open mallee"
    , 5, 10, "Mallees", "over", "Tall very very open mallee"

    , 100, 5, "Mallees", "over", "Closed mallee"
    , 70, 5, "Mallees", "over", "Mallee"
    , 30, 5, "Mallees", "over", "Open mallee"
    , 5, 5, "Mallees", "over", "Very open mallee"
    , 10, 5, "Mallees", "over", "Very open mallee"

    , 100, 3, "Mallees", "over", "Closed low mallee"
    , 70, 3, "Mallees", "over", "Low mallee"
    , 30, 3, "Mallees", "over", "Open low mallee"
    , 10, 3, "Mallees", "over", "Very open low mallee"
    , 5, 3, "Mallees", "over", "Very very open low mallee"

    , 100, 2, "Mallees", "over", "Closed low mallee"
    , 70, 2, "Mallees", "over", "Low mallee"
    , 30, 2, "Mallees", "over", "Open low mallee"
    , 10, 2, "Mallees", "over", "Very open low mallee"
    , 5, 2, "Mallees", "over", "Very very open low mallee"

    , 100, 1, "Mallees", "over", "Closed low mallee"
    , 70, 1, "Mallees", "over", "Low mallee"
    , 30, 1, "Mallees", "over", "Open low mallee"
    , 10, 1, "Mallees", "over", "Very open low mallee"
    , 5, 1, "Mallees", "over", "Very very open low mallee"

    , 100, 30, "Shrubs", "over", "Very very tall closed shrubland"
    , 70, 30, "Shrubs", "over", "Very very tall shrubland"
    , 30, 30, "Shrubs", "over", "Very very tall open shrubland"
    , 10, 30, "Shrubs", "over", "Very very tall very open shrubland"
    , 5, 30, "Shrubs", "over", "Very very tall very very open shrubland"

    , 100, 10, "Shrubs", "over", "Very tall closed shrubland"
    , 70, 10, "Shrubs", "over", "Very tall shrubland"
    , 30, 10, "Shrubs", "over", "Very tall open shrubland"
    , 10, 10, "Shrubs", "over", "Very tall very open shrubland"
    , 5, 10, "Shrubs", "over", "Very tall very very open shrubland"

    , 100, 5, "Shrubs", "over", "Tall closed shrubland"
    , 70, 5, "Shrubs", "over", "Tall shrubland"
    , 30, 5, "Shrubs", "over", "Tall open shrubland"
    , 10, 5, "Shrubs", "over", "Tall very open shrubland"
    , 5, 5, "Shrubs", "over", "Tall very very open shrubland"

    , 100, 3, "Shrubs", "mid", "Tall closed shrubland"
    , 70, 3, "Shrubs", "mid", "Tall shrubland"
    , 30, 3, "Shrubs", "mid", "Tall open shrubland"
    , 10, 3, "Shrubs", "mid", "Tall very open shrubland"
    , 5, 3, "Shrubs", "mid", "Tall very very open shrubland"

    , 100, 2, "Shrubs", "mid", "Closed shrubland"
    , 70, 2, "Shrubs", "mid", "Shrubland"
    , 30, 2, "Shrubs", "mid", "Open shrubland"
    , 10, 2, "Shrubs", "mid", "Very open shrubland"
    , 5, 2, "Shrubs", "mid", "Very very open shrubland"

    , 100, 1, "Shrubs", "mid", "Low closed shrubland"
    , 70, 1, "Shrubs", "mid", "Low shrubland"
    , 30, 1, "Shrubs", "mid", "Low open shrubland"
    , 10, 1, "Shrubs", "mid", "Low very open shrubland"
    , 5, 1, "Shrubs", "mid", "Low very very open shrubland"

    , 100, 0.5, "Shrubs", "ground", "Low closed shrubland"
    , 70, 0.5, "Shrubs", "ground", "Low shrubland"
    , 30, 0.5, "Shrubs", "ground", "Low open shrubland"
    , 10, 0.5, "Shrubs", "ground", "Low very open shrubland"
    , 5, 0.5, "Shrubs", "ground", "Low very very open shrubland"

    , 100, 1, "Mat plants", "mid", "Tall closed mat plants"
    , 70, 1, "Mat plants", "mid", "Tall mat plants"
    , 30, 1, "Mat plants", "mid", "Tall open mat plants"
    , 10, 1, "Mat plants", "mid", "Tall very open mat plants"
    , 5, 1, "Mat plants", "mid", "Tall very very open mat plants"

    , 100, 1, "Hummock grasses", "hummock", "Tall closed hummock grassland"
    , 70, 1, "Hummock grasses", "hummock", "Tall hummock grassland"
    , 30, 1, "Hummock grasses", "hummock", "Tall open hummock grassland"
    , 10, 1, "Hummock grasses", "hummock", "Tall very open hummock grassland"
    , 5, 1, "Hummock grasses", "hummock", "Tall very very open hummock grassland"

    , 100, 1, "Sedges", "mid", "Tall closed sedgeland"
    , 70, 1, "Sedges", "mid", "Tall sedgeland"
    , 30, 1, "Sedges", "mid", "Tall open sedgeland"
    , 10, 1, "Sedges", "mid", "Tall very open sedgeland"
    , 5, 1, "Sedges", "mid", "Tall very very open sedgeland"

    , 100, 1, "Sedges", "mid", "Tall closed sedgeland"
    , 70, 1, "Sedges", "mid", "Tall sedgeland"
    , 30, 1, "Sedges", "mid", "Tall open sedgeland"
    , 10, 1, "Sedges", "mid", "Tall very open sedgeland"
    , 5, 1, "Sedges", "mid", "Tall very very open sedgeland"

    , 100, 1, "Forbs", "mid", "Tall closed grassland"
    , 70, 1, "Forbs", "mid", "Tall grassland"
    , 30, 1, "Forbs", "mid", "Tall open grassland"
    , 10, 1, "Forbs", "mid", "Tall very open grassland"
    , 5, 1, "Forbs", "mid", "Tall very very open grassland"

    , 100, 1, "Ferns", "fern", "Tall closed fernland"
    , 70, 1, "Ferns", "fern", "Tall fernland"
    , 30, 1, "Ferns", "fern", "Tall open fernland"
    , 10, 1, "Ferns", "fern", "Tall very open fernland"
    , 5, 1, "Ferns", "fern", "Tall very very open fernland"

    , 100, 1, "Mistletoe", "mistletoe", "Closed mistletoe"
    , 70, 1, "Mistletoe", "mistletoe", "Mistletoe"
    , 30, 1, "Mistletoe", "mistletoe", "Open mistletoe"
    , 10, 1, "Mistletoe", "mistletoe", "Very open mistletoe"
    , 5, 1, "Mistletoe", "mistletoe", "Very very open mistletoe"

    , 100, 0.5, "Mat plants", "mat", "Closed matplants"
    , 70, 0.5, "Mat plants", "mat", "Matplants"
    , 30, 0.5, "Mat plants", "mat", "Open matplants"
    , 10, 0.5, "Mat plants", "mat", "Very open matplants"
    , 5, 0.5, "Mat plants", "mat", "Very very open matplants"

    , 100, 0.5, "Hummock grasses", "hummock", "Closed hummock grassland"
    , 70, 0.5, "Hummock grasses", "hummock", "Hummock grassland"
    , 30, 0.5, "Hummock grasses", "hummock", "Open hummock grassland"
    , 10, 0.5, "Hummock grasses", "hummock", "Very open hummock grassland"
    , 5, 0.5, "Hummock grasses", "hummock", "Very very open hummock grassland"

    , 100, 2, "Grasses", "ground", "Closed tall grassland"
    , 70, 2, "Grasses", "ground", "Tall grassland"
    , 30, 2, "Grasses", "ground", "Open tall grassland"
    , 10, 2, "Grasses", "ground", "Very open tall grassland"
    , 5, 2, "Grasses", "ground", "Very very open tall grassland"

    , 100, 1, "Grasses", "ground", "Closed tall grassland"
    , 70, 1, "Grasses", "ground", "Tall grassland"
    , 30, 1, "Grasses", "ground", "Open tall grassland"
    , 10, 1, "Grasses", "ground", "Very open tall grassland"
    , 5, 1, "Grasses", "ground", "Very very open tall grassland"

    , 100, 0.5, "Grasses", "ground", "Closed grassland"
    , 70, 0.5, "Grasses", "ground", "Grassland"
    , 30, 0.5, "Grasses", "ground", "Open grassland"
    , 10, 0.5, "Grasses", "ground", "Very open grassland"
    , 5, 0.5, "Grasses", "ground", "Very very open grassland"

    , 100, 0.5, "Sedges", "ground", "Closed sedgeland"
    , 70, 0.5, "Sedges", "ground", "Sedgeland"
    , 30, 0.5, "Sedges", "ground", "Open sedgeland"
    , 10, 0.5, "Sedges", "ground", "Very open sedgeland"
    , 5, 0.5, "Sedges", "ground", "Very very open sedgeland"

    , 100, 0.5, "Forbs", "ground", "Closed grassland"
    , 70, 0.5, "Forbs", "ground", "Grassland"
    , 30, 0.5, "Forbs", "ground", "Open grassland"
    , 10, 0.5, "Forbs", "ground", "Very open grassland"
    , 5, 0.5, "Forbs", "ground", "Very very open grassland"

    , 100, 0.5, "Ferns", "fern", "Closed fernland"
    , 70, 0.5, "Ferns", "fern", "Fernland"
    , 30, 0.5, "Ferns", "fern", "Open fernland"
    , 10, 0.5, "Ferns", "fern", "Very open fernland"
    , 5, 0.5, "Ferns", "fern", "Very very open fernland"

    , 100, 0.5, "Mistletoe", "mistletoe", "Closed mistletoe"
    , 70, 0.5, "Mistletoe", "mistletoe", "Mistletoe"
    , 30, 0.5, "Mistletoe", "mistletoe", "Open mistletoe"
    , 10, 0.5, "Mistletoe", "mistletoe", "Very open mistletoe"
    , 5, 0.5, "Mistletoe", "mistletoe", "Very very open mistletoe"
    ) |>
    dplyr::mutate(cov_thresh = dplyr::if_else(cov_thresh == 100, 200, cov_thresh)
                  , cov_class = cut(cov_thresh
                                    , breaks = c(unique(cov_thresh), 0)
                                    )
                  , ht_class = cut(ht_thresh
                                  , breaks = c(unique(ht_thresh),0)
                                  )
                  , sa_sf = gsub(".*\\s", "", tolower(sa_vsf))
                  ) |>
    dplyr::select(str, cov_class, ht_class, sa_vsf, sa_sf)

  sa_sf <-tibble::tribble(
    ~sf, ~colour, ~order
    , "forest", "forestgreen", 5
    , "grassland", "yellow", 1
    , "sedgeland", "yellow", 1.5
    , "mallee", "orange3", 3
    , "shrubland", "darkorchid4", 2
    , "woodland", "darkolivegreen3", 4
    , "wetland", "mediumseagreen", 7
    , "samphire", "black", 8
    , "fernland", "darkgreen", 6
    , "matplants", "yellow", 0.5
    ) |>
    dplyr::mutate(sf = forcats::fct_reorder(sf,order)) |>
    dplyr::arrange(sf)

  taxa_samphire <- c("Salicornia"
                     , "Tecticornia"
                     )

  taxa_wetland <- c("Phragmites"
                  , "Typha"
                  , "Juncus"
                  , "Schoenoplectus"
                  , "Eragrostis australasica" # Specht Veg of SA
                  , "Cressa australis" # Specht Veg of SA
                  , "Eragrostis dielsii" # Specht Veg of SA
                  , "Atriplex spongiosa" # Specht Veg of SA
                  , "Marsilea" # Specht Veg of SA
                  , "Duma" # Specht Veg of SA
                  , "Chenopodium nitrariaceum" # Specht Veg of SA
                  , "Chenopodium auricomum" # Specht Veg of SA
                  , "Cyperus laevigatus"
                  , "Cyperus gymnocaulos"
                  , "Sporobolus virginicus"
                  , "Sporobolus mitchellii"
                  , "Sphaeromorphaea littoralis"
                  , "Acacia stenophylla"
                  )

