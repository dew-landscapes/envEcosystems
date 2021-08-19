
  library(magrittr)

  # Muir codes
  lulifeform <- rio::import("data-raw/luLifeform.csv") %>%
    dplyr::mutate(storey = dplyr::if_else(grepl("Trees|Mallee",lifeform_class)
                                     ,"over"
                                     ,dplyr::if_else(grepl("Shrubs",lifeform_class)
                                              , "mid"
                                              , "ground"
                                              )
                                     )
                  , str = gsub("^\\d_|Low ","",lifeform_class)
                  , str = dplyr::if_else(lifeform == "H","Hummock grasses",str)
                  , str = dplyr::if_else(lifeform == "P","Mat plants",str)
                  , str = dplyr::if_else(lifeform == "X","Ferns",str)
                  , str = dplyr::if_else(lifeform == "MI","Mistletoe",str)
                  , str = forcats::fct_reorder(str,sort)
                  # , storey = replace(storey,grepl("Hummock",LF_Description),"hummock")
                  # , storey = replace(storey,grepl("Mistletoe",LF_Description),"mistletoe")
                  # , storey = replace(storey,grepl("Fern",LF_Description),"fern")
                  # , storey = replace(storey,grepl("Mat",LF_Description),"mat")
                  , storey = replace(storey,grepl("VT|GT",lifeform),"mid")
                  , storey = replace(storey,grepl("SD",lifeform),"ground")
                  , storey = forcats::fct_reorder(storey,ht,.fun=mean)
                  , storey = factor(storey, ordered = TRUE)
                  ) %>%
    dplyr::select(sort
                  , lifeform
                  , storey
                  , ht
                  , str
                  , description
                  ) %>%
    tibble::as_tibble()

