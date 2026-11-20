
  library(magrittr)
  library(tibble)


  artificialSurface <- rgb(218, 92, 105, maxColorValue = 255)
  cultTerrVegWoodyOpen <- rgb(205, 181, 75, maxColorValue = 255)
  cultTerrVegWoodyClosed <- rgb(197, 168, 51, maxColorValue = 255)
  water <- rgb(77, 159, 220, maxColorValue = 255)
  cultTerrVegHerbOpen <- rgb(242, 240, 27 ,maxColorValue = 255)
  cultTerrVegHerbClosed <- rgb(228, 224, 52, maxColorValue = 255)
  naturalBareGround <- rgb(230, 230, 230, maxColorValue = 255)
  benthicVegSeagrass <- rgb(56, 174, 99, maxColorValue = 255)
  benthicReefRocky <- rgb(139, 90, 43, maxColorValue = 255)
  marine <- rgb(77, 159, 220, maxColorValue = 255)

# Landcover colours
  lulandcover <- tribble(
    ~ecotype, ~definition, ~veg, ~lc_col, ~colour
    , "built", "to add", FALSE, artificialSurface, artificialSurface
    , "bare", "to add", TRUE, naturalBareGround, naturalBareGround
    , "dryland_ag", "to add", FALSE, cultTerrVegHerbClosed, cultTerrVegHerbClosed
    , "plantation", "to add", FALSE, cultTerrVegWoodyClosed, cultTerrVegWoodyClosed
    , "irrigated_ag", "to add", FALSE, "green2", "gray50"
    , "mangrove", "to add", TRUE, "green4", "green4"
    , "marine", "to add", TRUE, marine, marine
    , "rock", "to add", FALSE, "black", "black"
    , "saltlake", "to add", TRUE, "thistle1", "thistle1"
    , "sand", "to add", TRUE, "beige", "beige"
    , "water", "to add", TRUE, water, water
    ) %>%
    dplyr::arrange(ecotype) %>%
    dplyr::mutate(ecotype = factor(ecotype)
                  , desc = ecotype
                  )
