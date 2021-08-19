


#' Generate a structural description from cluster_spp_per and the clustering
#'
#' @param str_per Dataframe of structural information for each ecosystem. Ouput
#' from eco_str_per().
#' @param spp_per Dataframe of taxa information for each ecosystem. Output from
#' eco_spp_per().
#' @param taxonomy Dataframe with ind column indicating indigenous status.
#'
#' @return Dataframe of full ecosystem floristic * structural descriptions.
#' Indcludes text suitable for display in .md
#' @export
#'
#' @examples
eco_desc <- function(str_per,spp_per,taxonomy) {

  apply_sa_vsf <- function(df,cover_col = "sumcov", ht_col = "wtht", str_col = "storey") {

    sa_vsf_cols <- if(str_col == "storey") c("storey","htclass","covclass") else c("str","sa_vsf","htclass","covclass")

    df %>%
      dplyr::group_by(!!ensym(str_col)) %>%
      dplyr::mutate(cov_class = cut(!!ensym(cover_col)
                                   , breaks = c(cut_cov$cov_thresh)
                                   )
                    , ht_class = cut(!!ensym(ht_col)
                                    , breaks = c(cut_ht$ht_thresh)
                                    )
                    ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(sa_vsf %>%
                         dplyr::select(all_of(sa_vsf_cols)) %>%
                         unique() %>%
                         dplyr::arrange(desc(ht_class),desc(cov_class))
                       )

  }

  freq_class <- function(per_pres) {

    if_else(per_pres == 100
            , "always"
            , if_else(per_pres > 75
                      , "often"
                      , if_else(per_pres > 50
                                , "frequent"
                                , if_else(per_pres > 5
                                          , "occasional"
                                          , "infrequent"
                                )
                      )
            )
    ) %>%
      factor(levels = c("always","often","frequent","occasional","infrequent"))

  }

  make_storey_text <- function(df,keep_storey = TRUE) {

    groups <- if("pres" %in% names(df)) "pres"
    groups <- if(keep_storey) c("cluster","AD","overstorey",groups) else c("cluster","AD",groups)

    df %>%
      dplyr::left_join(taxonomy) %>%
      dplyr::arrange(cluster,per_pres) %>%
      dplyr::mutate(use_taxa = if_else(ind == "N",paste0("&ast;_",taxa,"_"),paste0("_",taxa,"_"))) %>%
      dplyr::group_by(!!!syms(groups)) %>%
      dplyr::summarise(per_cov = sum(per_cov)
                       , text = paste0(use_taxa
                                       , collapse=", "
                                       )
                       ) %>%
      dplyr::summarise(per_cov = sum(per_cov)
                       , text = envFunc::vec_to_sentence(paste0(if("pres" %in% groups) paste0(pres," ")
                                                       , text
                                                       )
                                                )
                       ) %>%
      dplyr::ungroup()

  }

  all_storey <- str_per %>%
    dplyr::group_by(cluster,storey) %>%
    dplyr::summarise(sum_cov = sum(per_cov)
                     , wt_ht = stats::weighted.mean(ht,per_cov)
                     ) %>%
    dplyr::ungroup()

  storey <- all_storey %>%
    dplyr::group_by(cluster) %>%
    dplyr::filter(sum_cov > 5) %>%
    #dplyr::filter(sumcov == max(sumcov)) %>%
    dplyr::filter(wt_ht == max(wt_ht)) %>%
    dplyr::filter(sum_cov == max(sum_cov)) %>%
    dplyr::ungroup() %>%
    apply_sa_vsf()

  storey_backup <- all_storey %>%
    dplyr::anti_join(storey %>% dplyr::select(cluster)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::filter(sum_cov == max(sum_cov)) %>%
    dplyr::filter(wt_ht == max(wt_ht)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(old_cov = sum_cov
                  , sum_cov = 5.1
                  ) %>%
    apply_sa_vsf() %>%
    dplyr::mutate(sum_cov = old_cov) %>%
    dplyr::select(-old_cov)

  storey <- storey %>%
    dplyr::bind_rows(storey_backup)

  allstr <- str_per %>%
    dplyr::group_by(cluster,lifeform,str) %>%
    dplyr::summarise(sum_cov = sum(per_cov)
                     , wt_ht = stats::weighted.mean(ht,per_cov)
                     ) %>%
    dplyr::ungroup()

  str <- all_str %>%
    dplyr::group_by(cluster) %>%
    dplyr::filter(sum_cov > 5) %>%
    dplyr::filter(wt_ht == max(wt_ht)) %>%
    dplyr::filter(sum_cov == max(sum_cov)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(wt_ht = if_else(str != "Mallees"
                                 , wt_ht
                                 , if_else(wt_ht > 5
                                           , 4.9999
                                           , wt_ht
                                           )
                                 )
                  ) %>%
    apply_sa_vsf(str_col = "lifeform") %>%
    dplyr::mutate(sa_vsf = if_else(str == "Mallees" & wt_ht == 4.9999
                                  , paste0("Tall ",tolower(sa_vsf))
                                  , sa_vsf
                                  )
                  )

  str_backup <- all_str %>%
    dplyr::anti_join(str %>% dplyr::select(cluster)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::filter(sum_cov == max(sum_cov)) %>%
    dplyr::filter(wt_ht == max(wt_ht)) %>%
    dplyr::slice(1) %>% # hack to ensure only one str per cluster
    dplyr::ungroup() %>%
    dplyr::mutate(old_cov = sum_cov
                  , sum_cov = 5.1
                  ) %>%
    apply_sa_vsf(str_col = "lifeform") %>%
    dplyr::mutate(sa_vsf = gsub("open","very open",sa_vsf)) %>%
    dplyr::mutate(sum_cov = old_cov) %>%
    dplyr::select(-old_cov)

  str <- str %>%
    dplyr::bind_rows(str_backup)

  over <- spp_per %>%
    dplyr::inner_join(str %>% dplyr::select(cluster,str)) %>%
    dplyr::group_by(cluster,str) %>%
    dplyr::slice_max(order_by = per_cov, n = 3) %>%
    dplyr::slice_max(order_by = per_pres,n = 3) %>%
    dplyr::mutate(AD = "O"
                  , overstorey = names(which.max(table(storey)))
                  , pres = freq_class(per_pres)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(pres != "infrequent") %>%
    make_storey_text(keep_storey = TRUE) %>%
    dplyr::mutate(over = gsub("_","",text)) %>%
    dplyr::rename(over_text = text)

  emer <- spp_per %>%
    dplyr::left_join(over %>% dplyr::select(-percov)) %>%
    dplyr::filter(storey > overstorey) %>%
    dplyr::group_by(cluster) %>%
    dplyr::slice_max(order_by = per_cov, n = 3) %>%
    dplyr::slice_max(order_by = per_pres,n = 3) %>%
    dplyr::mutate(AD = "E"
                  , overstorey = names(which.max(table(storey)))
                  , pres = freq_class(per_pres)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(pres != "infrequent") %>%
    make_storey_text(keep_storey = FALSE) %>%
    dplyr::rename(emer_text = text)

  under <- spp_per %>%
    dplyr::left_join(over %>% dplyr::select(-per_cov)) %>%
    dplyr::filter(storey < overstorey) %>%
    dplyr::group_by(cluster,storey) %>%
    dplyr::slice_max(order_by = per_cov, n = 5) %>%
    dplyr::slice_max(order_by = per_pres,n = 3) %>%
    dplyr::mutate(AD = "U"
                  , overstorey = names(which.max(table(storey)))
                  , pres = freq_class(per_pres)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(pres != "infrequent") %>%
    make_storey_text(keep_storey = FALSE) %>%
    dplyr::rename(under_text = text)


  sfWetland <- spp_per %>%
    dplyr::filter(grepl(paste0(wetland_spp,collapse="|"),taxa)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(wet_cov = sum(per_cov)) %>%
    dplyr::filter(wet_cov > 50)

  sfSamphire <- spp_per %>%
    dplyr::filter(grepl(paste0(samphire_spp,collapse="|"),taxa)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(sam_cov = sum(per_cov)) %>%
    dplyr::filter(sam_cov > 50)

  str <- str %>%
    dplyr::left_join(sf_wtland) %>%
    dplyr::left_join(sf_samphire) %>%
    dplyr::mutate(sa_vsf = if_else(wet_cov > sum_cov & !is.na(wet_cov)
                                  ,"Wetland"
                                  , if_else(sam_cov > sum_cov & !is.na(sam_cov)
                                            , "Samphire"
                                            , sa_vsf
                                            )
                                  )
                  )

  saveg <- spp_per %>%
    dplyr::count(cluster) %>%
    dplyr::select(-n) %>%
    dplyr::left_join(over %>% dplyr::select(cluster,over,over_text)) %>%
    dplyr::left_join(emer %>% dplyr::select(cluster,emer_text)) %>%
    dplyr::left_join(under %>% dplyr::select(cluster,under_text)) %>%
    dplyr::left_join(str %>% dplyr::select(cluster,sum_cov,sa_vsf)) %>%
    dplyr::mutate(sa_vsf = if_else(grepl(paste0(paste0("always ",samphire_spp,collapse="|")
                                               ,"|"
                                               , paste0("frequent",samphire_spp,collapse="|")
                                               )
                                        ,paste0(over_text," ",under_text)
                                        )
                                  ,"Samphire"
                                  ,sa_vsf
                                  )
                  , sa_vsf = if_else(grepl(paste0(paste0("always ",wetland_spp,collapse="|")
                                                 ,"|"
                                                 , paste0("frequent ",wetland_spp,collapse="|")
                                                 )
                                          ,paste0(over_text," ",under_text)
                                          )
                                    ,"Wetland"
                                    ,sa_vsf
                                    )
                  #, overtext = if_else(is.na(overtext) & grepl("very very",tolower(sa_vsf)),emertext,overtext)
                  #, emertext = ifelse(overtext == emertext,NA,emertext)
                  , sf = tolower(stringr::word(sa_vsf,-1))
                  , sf = factor(sf, levels = levels(sf_col$sf))
                  ) %>%
    dplyr::group_by(cluster,sa_vsf,sf,sum_cov,over,over_text) %>%
    dplyr::summarise(saveg = purrr::pmap(list(over_text
                                       , emer_text
                                       , under_text
                                       , sa_vsf
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
