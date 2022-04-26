
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
    ~ecotype, ~use_class, ~definition, ~veg, ~lc_col, ~colour
    , "built", "built", "to add", FALSE, artificialSurface, artificialSurface
    , "cropping", "cropping_cereals", "to add", FALSE, cultTerrVegHerbClosed, cultTerrVegHerbClosed
    , "cropping", "cropping_oilseeds", "to add", FALSE, cultTerrVegHerbClosed, cultTerrVegHerbClosed
    , "cropping", "cropping_pulses", "to add", FALSE, cultTerrVegHerbClosed, cultTerrVegHerbClosed
    , "plantation", "hardwood", "to add", FALSE, cultTerrVegWoodyOpen, cultTerrVegWoodyOpen
    , "irrigated", "irrigated_citrus", "to add", FALSE, "green2", "gray50"
    , "irrigated", "irrigated_crop_pasture", "to add", FALSE, "green2", "gray50"
    , "irrigated", "irrigated_grapes", "to add", FALSE, "green2", "gray40"
    , "irrigated", "irrigated_tree_crops", "to add", FALSE, "green2", "gray50"
    , "mangrove", "mangrove", "to add", TRUE, "green4", "green4"
    , "outcrop", "outcrop", "to add", FALSE, "black", "black"
    , "pasture", "pasture_annual", "to add", FALSE, cultTerrVegHerbOpen, cultTerrVegHerbOpen
    , "pasture", "pasture_grass", "to add", FALSE, cultTerrVegHerbOpen, cultTerrVegHerbOpen
    , "pasture", "pasture_legumes", "to add", FALSE, cultTerrVegHerbOpen, cultTerrVegHerbOpen
    , "pasture", "pasture_mixed", "to add", FALSE, cultTerrVegHerbOpen, cultTerrVegHerbOpen
    , "saltlake", "saltlake", "to add", TRUE, "thistle1", "thistle1"
    , "sand", "sand", "to add", TRUE, "beige", "beige"
    , "willow", "willow", "to add", FALSE, "green2", "gray80"
    , "plantation", "softwood", "to add", FALSE, cultTerrVegWoodyClosed, cultTerrVegWoodyClosed
    , "water", "water", "to add", TRUE, water, water
    , "water", "water_coastal", "to add", TRUE, water, water
    ) %>%
    dplyr::arrange(ecotype, use_class) %>%
    dplyr::mutate(ecotype = factor(ecotype)
                  , desc = purrr::map2_chr(ecotype
                                  , use_class
                                  , ~if(.x == .y) .y else paste0(.x
                                                                 , ": "
                                                                 , gsub(paste0(.x
                                                                               , "_"
                                                                               )
                                                                        , ""
                                                                        , .y
                                                                        )
                                                                 )
                                  )
                  )
