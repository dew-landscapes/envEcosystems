
  library(magrittr)
  library(tibble)


  artificialSurface <- rgb(218,92,105,maxColorValue = 255)
  cultTerrVegWoodyOpen <- rgb(205,181,75,maxColorValue = 255)
  cultTerrVegWoodyClosed <- rgb(197,168,51,maxColorValue = 255)
  water <- rgb(77,159,220,maxColorValue = 255)
  cultTerrVegHerbOpen <- rgb(242,240,127,maxColorValue = 255)
  cultTerrVegHerbClosed <- rgb(228,224,52,maxColorValue = 255)

# Landcover colours
  lulandcover <- tribble(
    ~use_class, ~definition, ~veg, ~lc_col, ~colour
    , "built", "to add", FALSE, artificialSurface, artificialSurface
    , "cropping_cereals", "to add", FALSE, cultTerrVegHerbClosed, cultTerrVegHerbClosed
    , "cropping_oilseeds", "to add", FALSE, cultTerrVegHerbClosed, cultTerrVegHerbClosed
    , "cropping_pulses", "to add", FALSE, cultTerrVegHerbClosed, cultTerrVegHerbClosed
    , "hardwood", "to add", FALSE, cultTerrVegWoodyOpen, cultTerrVegWoodyOpen
    , "irrigated_citrus", "to add", FALSE, "green2", "gray50"
    , "irrigated_crop_pasture", "to add", FALSE, "green2", "gray50"
    , "irrigated_grapes", "to add", FALSE, "green2", "gray40"
    , "irrigated_tree_crops", "to add", FALSE, "green2", "gray50"
    , "mangrove", "to add", TRUE, "green4", "green4"
    , "outcrop", "to add", FALSE, "black", "black"
    , "pasture_annual", "to add", FALSE, cultTerrVegHerbOpen, cultTerrVegHerbOpen
    , "pasture_grass", "to add", FALSE, cultTerrVegHerbOpen, cultTerrVegHerbOpen
    , "pasture_legumes", "to add", FALSE, cultTerrVegHerbOpen, cultTerrVegHerbOpen
    , "pasture_mixed", "to add", FALSE, cultTerrVegHerbOpen, cultTerrVegHerbOpen
    , "saltlake", "to add", TRUE, "thistle1", "thistle1"
    , "sand", "to add", TRUE, "beige", "beige"
    , "willow", "to add", FALSE, "green2", "gray80"
    , "softwood", "to add", FALSE, cultTerrVegWoodyClosed, cultTerrVegWoodyClosed
    , "water", "to add", TRUE, water, water
    , "water_coastal", "to add", TRUE, water, water
    ) %>%
    dplyr::arrange(use_class)
