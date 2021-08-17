


#' Generate a structural description from cluster_spp_per and the clustering
#'
#' @param strPer Dataframe of structural information for each ecosystem. Ouput
#' from eco_str_per().
#' @param sppPer Dataframe of taxa information for each ecosystem. Output from
#' eco_spp_per().
#'
#' @return Dataframe of full ecosystem floristic * structural descriptions.
#' Indcludes text suitable for display in .md
#' @export
#'
#' @examples
eco_desc <- function(strPer,sppPer) {

  source("R/SAVegetationStructuralFormations.R")

  apply_savsf <- function(df,coverCol = "sumCov", heightCol = "wtHt", strCol = "storey") {

    savsfCols <- if(strCol == "storey") c("storey","htClass","covClass") else c("str","savsf","htClass","covClass")

    df %>%
      dplyr::group_by(!!ensym(strCol)) %>%
      dplyr::mutate(covClass = cut(!!ensym(coverCol)
                                   , breaks = c(cutCov$covThresh)
                                   )
                    , htClass = cut(!!ensym(heightCol)
                                    , breaks = c(cutHt$htThresh)
                                    )
                    ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(savsf %>%
                         dplyr::select(all_of(savsfCols)) %>%
                         unique() %>%
                         dplyr::arrange(desc(htClass),desc(covClass))
                       )

  }

  freq_class <- function(perPres) {

    if_else(perPres == 100
            , "always"
            , if_else(perPres > 75
                      , "often"
                      , if_else(perPres > 50
                                , "frequent"
                                , if_else(perPres > 5
                                          , "occasional"
                                          , "infrequent"
                                )
                      )
            )
    ) %>%
      factor(levels = c("always","often","frequent","occasional","infrequent"))

  }

  make_storey_text <- function(df,keepStorey = TRUE) {

    groups <- if("pres" %in% names(df)) "pres"
    groups <- if(keepStorey) c("cluster","AD","overstorey",groups) else c("cluster","AD",groups)

    df %>%
      dplyr::left_join(taxaTaxonomy) %>%
      dplyr::arrange(cluster,perPres) %>%
      dplyr::mutate(useTaxa = if_else(ind == "N",paste0("&ast;_",Taxa,"_"),paste0("_",Taxa,"_"))) %>%
      dplyr::group_by(!!!syms(groups)) %>%
      dplyr::summarise(perCov = sum(perCov)
                       , text = paste0(useTaxa
                                       , collapse=", "
                       )
      ) %>%
      dplyr::summarise(perCov = sum(perCov)
                       , text = vec_to_sentence(paste0(if("pres" %in% groups) paste0(pres," ")
                                                       , text
                       )
                       )
      ) %>%
      dplyr::ungroup()

  }

  allStorey <- strPer %>%
    dplyr::group_by(cluster,storey) %>%
    dplyr::summarise(sumCov = sum(perCov)
                     , wtHt = weighted.mean(ht,perCov)
                     ) %>%
    dplyr::ungroup()

  storey <- allStorey %>%
    dplyr::group_by(cluster) %>%
    dplyr::filter(sumCov > 5) %>%
    #dplyr::filter(sumCov == max(sumCov)) %>%
    dplyr::filter(wtHt == max(wtHt)) %>%
    dplyr::filter(sumCov == max(sumCov)) %>%
    dplyr::ungroup() %>%
    apply_savsf()

  storeyBackup <- allStorey %>%
    dplyr::anti_join(storey %>% dplyr::select(cluster)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::filter(sumCov == max(sumCov)) %>%
    dplyr::filter(wtHt == max(wtHt)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(oldCov = sumCov
                  , sumCov = 5.1
                  ) %>%
    apply_savsf() %>%
    dplyr::mutate(sumCov = oldCov) %>%
    dplyr::select(-oldCov)

  storey <- storey %>%
    dplyr::bind_rows(storeyBackup)

  allStr <- strPer %>%
    dplyr::group_by(cluster,lifeform,str) %>%
    dplyr::summarise(sumCov = sum(perCov)
                     , wtHt = weighted.mean(ht,perCov)
                     ) %>%
    dplyr::ungroup()

  str <- allStr %>%
    dplyr::group_by(cluster) %>%
    dplyr::filter(sumCov > 5) %>%
    dplyr::filter(wtHt == max(wtHt)) %>%
    dplyr::filter(sumCov == max(sumCov)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(wtHt = if_else(str != "Mallees"
                                 , wtHt
                                 , if_else(wtHt > 5
                                           , 4.9999
                                           , wtHt
                                           )
                                 )
                  ) %>%
    apply_savsf(strCol = "lifeform") %>%
    dplyr::mutate(savsf = if_else(str == "Mallees" & wtHt == 4.9999
                                  , paste0("Tall ",tolower(savsf))
                                  , savsf
                                  )
                  )

  strBackup <- allStr %>%
    dplyr::anti_join(str %>% dplyr::select(cluster)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::filter(sumCov == max(sumCov)) %>%
    dplyr::filter(wtHt == max(wtHt)) %>%
    dplyr::slice(1) %>% # hack to ensure only one str per cluster
    dplyr::ungroup() %>%
    dplyr::mutate(oldCov = sumCov
                  , sumCov = 5.1
                  ) %>%
    apply_savsf(strCol = "lifeform") %>%
    dplyr::mutate(savsf = gsub("open","very open",savsf)) %>%
    dplyr::mutate(sumCov = oldCov) %>%
    dplyr::select(-oldCov)

  str <- str %>%
    dplyr::bind_rows(strBackup)

  over <- sppPer %>%
    dplyr::inner_join(str %>% dplyr::select(cluster,str)) %>%
    dplyr::group_by(cluster,str) %>%
    dplyr::slice_max(order_by = perCov, n = 3) %>%
    dplyr::slice_max(order_by = perPres,n = 3) %>%
    dplyr::mutate(AD = "O"
                  , overstorey = names(which.max(table(storey)))
                  , pres = freq_class(perPres)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(pres != "infrequent") %>%
    make_storey_text(keepStorey = TRUE) %>%
    dplyr::mutate(over = gsub("_","",text)) %>%
    dplyr::rename(overText = text)

  emer <- sppPer %>%
    dplyr::left_join(over %>% dplyr::select(-perCov)) %>%
    dplyr::filter(storey > overstorey) %>%
    dplyr::group_by(cluster) %>%
    dplyr::slice_max(order_by = perCov, n = 3) %>%
    dplyr::slice_max(order_by = perPres,n = 3) %>%
    dplyr::mutate(AD = "E"
                  , overstorey = names(which.max(table(storey)))
                  , pres = freq_class(perPres)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(pres != "infrequent") %>%
    make_storey_text(keepStorey = FALSE) %>%
    dplyr::rename(emerText = text)

  under <- sppPer %>%
    dplyr::left_join(over %>% dplyr::select(-perCov)) %>%
    dplyr::filter(storey < overstorey) %>%
    dplyr::group_by(cluster,storey) %>%
    dplyr::slice_max(order_by = perCov, n = 5) %>%
    dplyr::slice_max(order_by = perPres,n = 3) %>%
    dplyr::mutate(AD = "U"
                  , overstorey = names(which.max(table(storey)))
                  , pres = freq_class(perPres)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(pres != "infrequent") %>%
    make_storey_text(keepStorey = FALSE) %>%
    dplyr::rename(underText = text)


  sfWetland <- sppPer %>%
    dplyr::filter(grepl(paste0(wetlandSpp,collapse="|"),Taxa)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(wetCov = sum(perCov)) %>%
    dplyr::filter(wetCov > 500)

  sfSamphire <- sppPer %>%
    dplyr::filter(grepl(paste0(samphireSpp,collapse="|"),Taxa)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(samCov = sum(perCov)) %>%
    dplyr::filter(samCov > 500)

  str <- str %>%
    dplyr::left_join(sfWetland) %>%
    dplyr::left_join(sfSamphire) %>%
    dplyr::mutate(savsf = if_else(wetCov > sumCov & !is.na(wetCov)
                                  ,"Wetland"
                                  , if_else(samCov > sumCov & !is.na(samCov)
                                            , "Samphire"
                                            , savsf
                                            )
                                  )
                  )

  saveg <- sppPer %>%
    dplyr::count(cluster) %>%
    dplyr::select(-n) %>%
    dplyr::left_join(over %>% dplyr::select(cluster,over,overText)) %>%
    dplyr::left_join(emer %>% dplyr::select(cluster,emerText)) %>%
    dplyr::left_join(under %>% dplyr::select(cluster,underText)) %>%
    dplyr::left_join(str %>% dplyr::select(cluster,sumCov,savsf)) %>%
    dplyr::mutate(savsf = if_else(grepl(paste0(paste0("always ",samphireSpp,collapse="|")
                                               ,"|"
                                               , paste0("frequent",samphireSpp,collapse="|")
                                               )
                                        ,paste0(overText," ",underText)
                                        )
                                  ,"Samphire"
                                  ,savsf
                                  )
                  , savsf = if_else(grepl(paste0(paste0("always ",wetlandSpp,collapse="|")
                                                 ,"|"
                                                 , paste0("frequent ",wetlandSpp,collapse="|")
                                                 )
                                          ,paste0(overText," ",underText)
                                          )
                                    ,"Wetland"
                                    ,savsf
                                    )
                  #, overText = if_else(is.na(overText) & grepl("very very",tolower(savsf)),emerText,overText)
                  #, emerText = ifelse(overText == emerText,NA,emerText)
                  , sf = tolower(stringr::word(savsf,-1))
                  , sf = factor(sf, levels = levels(sfCol$sf))
                  ) %>%
    dplyr::group_by(cluster,savsf,sf,sumCov,over,overText) %>%
    dplyr::summarise(saveg = pmap(list(overText
                                       , emerText
                                       , underText
                                       , savsf
                                       )
                                  , function(a,b,c,d) paste0(if(!is.na(d)) paste0(d, ": ")
                                                             , if(!is.na(a)) paste0(a)
                                                             , if(!is.na(b)) paste0("; with emergent "
                                                                                    ,b
                                                                                    )
                                                             , if(!is.na(c)) paste0("; over ",c)
                                                             )
                                  )
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(over = gsub(";.*|_","",saveg)
                  , over = gsub("&ast;_","*",saveg)
                  ) %>%
    tidyr::unnest(cols = c(saveg))

}
