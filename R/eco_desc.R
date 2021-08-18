


#' Generate a structural description from cluster_spp_per and the clustering
#'
#' @param strper Dataframe of structural information for each ecosystem. Ouput
#' from eco_str_per().
#' @param sppper Dataframe of taxa information for each ecosystem. Output from
#' eco_spp_per().
#' @param taxonomy Dataframe with ind column indicating indigenous status.
#'
#' @return Dataframe of full ecosystem floristic * structural descriptions.
#' Indcludes text suitable for display in .md
#' @export
#'
#' @examples
eco_desc <- function(strper,sppper,taxonomy) {

  apply_savsf <- function(df,covercol = "sumcov", heightcol = "wtht", strcol = "storey") {

    savsfcols <- if(strcol == "storey") c("storey","htclass","covclass") else c("str","savsf","htclass","covclass")

    df %>%
      dplyr::group_by(!!ensym(strcol)) %>%
      dplyr::mutate(covclass = cut(!!ensym(covercol)
                                   , breaks = c(cutcov$covthresh)
                                   )
                    , htclass = cut(!!ensym(heightcol)
                                    , breaks = c(cutht$htthresh)
                                    )
                    ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(savsf %>%
                         dplyr::select(all_of(savsfcols)) %>%
                         unique() %>%
                         dplyr::arrange(desc(htclass),desc(covclass))
                       )

  }

  freq_class <- function(perpres) {

    if_else(perpres == 100
            , "always"
            , if_else(perpres > 75
                      , "often"
                      , if_else(perpres > 50
                                , "frequent"
                                , if_else(perpres > 5
                                          , "occasional"
                                          , "infrequent"
                                )
                      )
            )
    ) %>%
      factor(levels = c("always","often","frequent","occasional","infrequent"))

  }

  make_storey_text <- function(df,keepstorey = TRUE) {

    groups <- if("pres" %in% names(df)) "pres"
    groups <- if(keepstorey) c("cluster","AD","overstorey",groups) else c("cluster","AD",groups)

    df %>%
      dplyr::left_join(taxonomy) %>%
      dplyr::arrange(cluster,perpres) %>%
      dplyr::mutate(usetaxa = if_else(ind == "N",paste0("&ast;_",Taxa,"_"),paste0("_",Taxa,"_"))) %>%
      dplyr::group_by(!!!syms(groups)) %>%
      dplyr::summarise(percov = sum(percov)
                       , text = paste0(usetaxa
                                       , collapse=", "
                                       )
                       ) %>%
      dplyr::summarise(percov = sum(percov)
                       , text = vec_to_sentence(paste0(if("pres" %in% groups) paste0(pres," ")
                                                       , text
                                                       )
                                                )
                       ) %>%
      dplyr::ungroup()

  }

  allstorey <- strper %>%
    dplyr::group_by(cluster,storey) %>%
    dplyr::summarise(sumcov = sum(percov)
                     , wtht = weighted.mean(ht,percov)
                     ) %>%
    dplyr::ungroup()

  storey <- allstorey %>%
    dplyr::group_by(cluster) %>%
    dplyr::filter(sumcov > 5) %>%
    #dplyr::filter(sumcov == max(sumcov)) %>%
    dplyr::filter(wtht == max(wtht)) %>%
    dplyr::filter(sumcov == max(sumcov)) %>%
    dplyr::ungroup() %>%
    apply_savsf()

  storeybackup <- allstorey %>%
    dplyr::anti_join(storey %>% dplyr::select(cluster)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::filter(sumcov == max(sumcov)) %>%
    dplyr::filter(wtht == max(wtht)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(oldcov = sumcov
                  , sumcov = 5.1
                  ) %>%
    apply_savsf() %>%
    dplyr::mutate(sumcov = oldcov) %>%
    dplyr::select(-oldcov)

  storey <- storey %>%
    dplyr::bind_rows(storeybackup)

  allstr <- strper %>%
    dplyr::group_by(cluster,lifeform,str) %>%
    dplyr::summarise(sumcov = sum(percov)
                     , wtht = weighted.mean(ht,percov)
                     ) %>%
    dplyr::ungroup()

  str <- allstr %>%
    dplyr::group_by(cluster) %>%
    dplyr::filter(sumcov > 5) %>%
    dplyr::filter(wtht == max(wtht)) %>%
    dplyr::filter(sumcov == max(sumcov)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(wtht = if_else(str != "Mallees"
                                 , wtht
                                 , if_else(wtht > 5
                                           , 4.9999
                                           , wtht
                                           )
                                 )
                  ) %>%
    apply_savsf(strcol = "lifeform") %>%
    dplyr::mutate(savsf = if_else(str == "Mallees" & wtht == 4.9999
                                  , paste0("Tall ",tolower(savsf))
                                  , savsf
                                  )
                  )

  strbackup <- allstr %>%
    dplyr::anti_join(str %>% dplyr::select(cluster)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::filter(sumcov == max(sumcov)) %>%
    dplyr::filter(wtht == max(wtht)) %>%
    dplyr::slice(1) %>% # hack to ensure only one str per cluster
    dplyr::ungroup() %>%
    dplyr::mutate(oldcov = sumcov
                  , sumcov = 5.1
                  ) %>%
    apply_savsf(strcol = "lifeform") %>%
    dplyr::mutate(savsf = gsub("open","very open",savsf)) %>%
    dplyr::mutate(sumcov = oldcov) %>%
    dplyr::select(-oldcov)

  str <- str %>%
    dplyr::bind_rows(strbackup)

  over <- sppper %>%
    dplyr::inner_join(str %>% dplyr::select(cluster,str)) %>%
    dplyr::group_by(cluster,str) %>%
    dplyr::slice_max(order_by = percov, n = 3) %>%
    dplyr::slice_max(order_by = perpres,n = 3) %>%
    dplyr::mutate(AD = "O"
                  , overstorey = names(which.max(table(storey)))
                  , pres = freq_class(perpres)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(pres != "infrequent") %>%
    make_storey_text(keepstorey = TRUE) %>%
    dplyr::mutate(over = gsub("_","",text)) %>%
    dplyr::rename(overtext = text)

  emer <- sppper %>%
    dplyr::left_join(over %>% dplyr::select(-percov)) %>%
    dplyr::filter(storey > overstorey) %>%
    dplyr::group_by(cluster) %>%
    dplyr::slice_max(order_by = percov, n = 3) %>%
    dplyr::slice_max(order_by = perpres,n = 3) %>%
    dplyr::mutate(AD = "E"
                  , overstorey = names(which.max(table(storey)))
                  , pres = freq_class(perpres)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(pres != "infrequent") %>%
    make_storey_text(keepstorey = FALSE) %>%
    dplyr::rename(emertext = text)

  under <- sppper %>%
    dplyr::left_join(over %>% dplyr::select(-percov)) %>%
    dplyr::filter(storey < overstorey) %>%
    dplyr::group_by(cluster,storey) %>%
    dplyr::slice_max(order_by = percov, n = 5) %>%
    dplyr::slice_max(order_by = perpres,n = 3) %>%
    dplyr::mutate(AD = "U"
                  , overstorey = names(which.max(table(storey)))
                  , pres = freq_class(perpres)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(pres != "infrequent") %>%
    make_storey_text(keepstorey = FALSE) %>%
    dplyr::rename(undertext = text)


  sfWetland <- sppper %>%
    dplyr::filter(grepl(paste0(wetlandspp,collapse="|"),Taxa)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(wetcov = sum(percov)) %>%
    dplyr::filter(wetcov > 500)

  sfSamphire <- sppper %>%
    dplyr::filter(grepl(paste0(samphirespp,collapse="|"),Taxa)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(samcov = sum(percov)) %>%
    dplyr::filter(samcov > 500)

  str <- str %>%
    dplyr::left_join(sfwtland) %>%
    dplyr::left_join(sfsamphire) %>%
    dplyr::mutate(savsf = if_else(wetcov > sumcov & !is.na(wetcov)
                                  ,"Wetland"
                                  , if_else(samcov > sumcov & !is.na(samcov)
                                            , "Samphire"
                                            , savsf
                                            )
                                  )
                  )

  saveg <- sppper %>%
    dplyr::count(cluster) %>%
    dplyr::select(-n) %>%
    dplyr::left_join(over %>% dplyr::select(cluster,over,overtext)) %>%
    dplyr::left_join(emer %>% dplyr::select(cluster,emertext)) %>%
    dplyr::left_join(under %>% dplyr::select(cluster,undertext)) %>%
    dplyr::left_join(str %>% dplyr::select(cluster,sumcov,savsf)) %>%
    dplyr::mutate(savsf = if_else(grepl(paste0(paste0("always ",samphirespp,collapse="|")
                                               ,"|"
                                               , paste0("frequent",samphirespp,collapse="|")
                                               )
                                        ,paste0(overtext," ",undertext)
                                        )
                                  ,"Samphire"
                                  ,savsf
                                  )
                  , savsf = if_else(grepl(paste0(paste0("always ",wetlandspp,collapse="|")
                                                 ,"|"
                                                 , paste0("frequent ",wetlandspp,collapse="|")
                                                 )
                                          ,paste0(overtext," ",undertext)
                                          )
                                    ,"Wetland"
                                    ,savsf
                                    )
                  #, overtext = if_else(is.na(overtext) & grepl("very very",tolower(savsf)),emertext,overtext)
                  #, emertext = ifelse(overtext == emertext,NA,emertext)
                  , sf = tolower(stringr::word(savsf,-1))
                  , sf = factor(sf, levels = levels(sfcol$sf))
                  ) %>%
    dplyr::group_by(cluster,savsf,sf,sumcov,over,overtext) %>%
    dplyr::summarise(saveg = pmap(list(overtext
                                       , emertext
                                       , undertext
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
