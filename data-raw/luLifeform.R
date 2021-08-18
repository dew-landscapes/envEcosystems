
  library(magrittr)

  # Muir codes
  lulifeform <- rio::import("data-raw/luLifeform.csv") %>%
    dplyr::mutate(lifeform = LifeForm_Code
                  , storey = dplyr::if_else(grepl("Trees|Mallee",LF_Class)
                                     ,"over"
                                     ,dplyr::if_else(grepl("Shrubs",LF_Class)
                                              , "mid"
                                              , "ground"
                                              )
                                     )
                  , ht = Height
                  , LF_Class_Num = readr::parse_number(LF_Class)
                  , str = gsub("^\\d_|Low ","",LF_Class)
                  , str = dplyr::if_else(lifeform == "H","Hummock grasses",str)
                  , str = dplyr::if_else(lifeform == "P","Mat plants",str)
                  , str = dplyr::if_else(lifeform == "X","Ferns",str)
                  , str = dplyr::if_else(lifeform == "MI","Mistletoe",str)
                  , str = forcats::fct_reorder(str,LF_Class_Num)
                  # , storey = replace(storey,grepl("Hummock",LF_Description),"hummock")
                  # , storey = replace(storey,grepl("Mistletoe",LF_Description),"mistletoe")
                  # , storey = replace(storey,grepl("Fern",LF_Description),"fern")
                  # , storey = replace(storey,grepl("Mat",LF_Description),"mat")
                  , storey = replace(storey,grepl("VT|GT",lifeform),"mid")
                  , storey = replace(storey,grepl("SD",lifeform),"ground")
                  , storey = forcats::fct_reorder(storey,Height,.fun=mean)
                  , storey = factor(storey, ordered = TRUE)
                  ) %>%
    dplyr::select(sort = SortID
                  , lifeform
                  , storey
                  , ht
                  , str
                  , description = LF_Description
                  ) %>%
    tibble::as_tibble()

