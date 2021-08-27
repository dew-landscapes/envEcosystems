


#' Generate an ecosystem description
#'
#' @param taxa_per Dataframe of taxa information for each ecosystem. Output from
#' eco_taxa_per().
#' @param str_per Dataframe of structural information for each ecosystem. Output
#' from eco_str_per().
#' @param taxonomy Dataframe with ind column indicating indigenous status.
#' @param type_colours Dataframe with `type` (usually `sf`) and `colours` to apply.
#' @param add_eco Dataframe with any additional clusters to add (e.g. landcover
#' clusters).
#' @param add_colours Dataframe with
#' @param add_col_cluser Character. Name of column in `add_eco` that contains
#' the cluster/groups/ecosystems.
#'
#' @return Dataframe of full ecosystem floristic * structural descriptions.
#' Indcludes text suitable for display in .md
#' @export
#'
#' @examples
make_eco_desc <- function(bio_df
                          , clust_df
                          , context
                          , cov_col = "use_cover"
                          , lustr
                          , taxonomy
                          , use_prop_thresh
                          , type_colours = NULL
                          , add_eco = NULL
                          , add_colours = NULL
                          , add_col_cluster = "use_class"
                          ) {

  #------str-------

  lifeforms_all <- bio_df %>%
    dplyr::left_join(lustr) %>%
    dplyr::left_join(clust_df) %>%
    dplyr::group_by(!!ensym(clust_col), across(all_of(context)), across(all_of(names(lustr)))) %>%
    dplyr::summarise(cov = sum(!!ensym(cov_col))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(cluster,across(all_of(context)),desc(sort))

  context_vsf_all <- lifeforms_all %>%
    dplyr::group_by(!!ensym(clust_col)
                    , across(all_of(context))
                    , str
                    #, storey
                    ) %>%
    dplyr::summarise(sum_cov = sum(cov)
                     , wt_ht = weighted.mean(ht,cov)
                     , sort = min(sort)
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cov_class = cut(sum_cov*100
                                  , breaks = c(cut_cov$cov_thresh,0)
                                  )
                  , ht_class = cut(wt_ht
                                   , breaks = c(cut_ht$ht_thresh)
                                   )
                  ) %>%
    dplyr::left_join(sa_vsf)

  context_vsf <- context_vsf_all %>%
    dplyr::group_by(!!ensym(clust_col)
                    , across(all_of(context))
                    ) %>%
    dplyr::filter(sum_cov > 0.05) %>%
    dplyr::filter(sort == min(sort, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(sa_vsf)) %>%
    dplyr::mutate(sf = tolower(gsub(".* ","",sa_vsf)))

  eco_sf <- context_vsf %>%
    dplyr::group_by(!!ensym(clust_col)) %>%
    dplyr::summarise(sites = n()
                     , range_sf = paste0(vec_to_sentence(names(table(sf)[table(sf) > quantile(table(sf),probs = 0.5)])))
                     , sf = names(which.max(table(sf)))
                     , range_sf = if_else(range_sf == "", sf, range_sf)
                     ) %>%
    dplyr::ungroup()

  eco_vsf <- context_vsf %>%
    dplyr::inner_join(eco_sf) %>%
    dplyr::group_by(!!ensym(clust_col)) %>%
    dplyr::summarise(range_vsf = paste0(vec_to_sentence(names(table(sa_vsf)[table(sa_vsf) > quantile(table(sa_vsf),probs = 0.5)])))
                     , vsf = names(which.max(table(sa_vsf)[.data$sf == sf]))
                     , range_vsf = if_else(range_vsf == "", vsf, range_vsf)
                     ) %>%
    dplyr::ungroup()


  #------taxa-------

  eco_taxa <- flor_tidy %>%
    dplyr::left_join(clust_df) %>%
    dplyr::group_by(!!ensym(clust_col)) %>%
    dplyr::mutate(cluster_sites = n_distinct(cell)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(taxonomy) %>%
    dplyr::count(cluster, cluster_sites, taxa, ind, name = "taxa_sites") %>%
    dplyr::mutate(prop = taxa_sites/cluster_sites) %>%
    dplyr::group_by(!!ensym(clust_col)) %>%
    dplyr::filter(prop > use_prop_thresh) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(freq = add_freq_class(prop*100)
                  , use_taxa = if_else(ind == "N",paste0("&ast;_",taxa,"_"),paste0("_",taxa,"_"))
                  ) %>%
    dplyr::group_by(!!ensym(clust_col),freq) %>%
    dplyr::summarise(text = envFunc::vec_to_sentence(use_taxa)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(text = paste0(freq, " ", text)) %>%
    dplyr::group_by(!!ensym(clust_col)) %>%
    dplyr::summarise(range_taxa = envFunc::vec_to_sentence(text)) %>%
    dplyr::ungroup()

  eco_ind <- models_final$ind_val[[1]] %>%
    dplyr::group_by(!!ensym(clust_col)) %>%
    dplyr::filter(ind_val > quantile(ind_val, probs = 0.95)) %>%
    dplyr::select(cluster,everything()) %>%
    dplyr::arrange(cluster) %>%
    dplyr::left_join(taxa_taxonomy) %>%
    dplyr::mutate(use_taxa = if_else(ind == "N",paste0("&ast;_",taxa,"_"),paste0("_",taxa,"_"))) %>%
    dplyr::group_by(!!ensym(clust_col)) %>%
    dplyr::summarise(range_ind = envFunc::vec_to_sentence(use_taxa)) %>%
    dplyr::ungroup()

  if(isTRUE(!is.null(add_eco))) {

    eco_add <- add_eco %>%
      dplyr::count(!!ensym(clust_col)
                   , name = "sites"
                   ) %>%
      dplyr::mutate(loose_md = paste0("Landcover: ", cluster)
                    , loose = loose_md
                    , desc = loose_md
                    )

  }

  eco_desc <- eco_sf %>%
    dplyr::left_join(eco_vsf) %>%
    dplyr::left_join(eco_taxa) %>%
    dplyr::left_join(eco_ind) %>%
    dplyr::mutate(range_taxa = if_else(is.na(range_taxa)
                                       , " and with no widespread taxa"
                                       , paste0(" and with "
                                                , range_taxa
                                                )
                                       )
                  , range_ind = if_else(is.na(range_ind)
                                        , " with no good indicators"
                                        , paste0(" best indicated by "
                                                 , range_ind
                                                 )
                                        )
                  , loose_md = paste0(range_sf
                                 , range_ind
                                 , range_taxa
                                 )
                  , loose = gsub("_","",loose_md)
                  , desc_md = paste0(vsf
                                  , range_ind
                                  , range_taxa
                                  )
                  , desc = gsub("_","",desc_md)
                  ) %>%
    {if(isTRUE(!is.null(add_eco))) (.) %>% dplyr::bind_rows(eco_add) else (.)}

}

make_eco_name <- function(blahdyblahblah) {

  make_storey_text <- function(df, keep_storey = TRUE) {

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

  over <- taxa_per %>%
    dplyr::inner_join(stories) %>%
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


  emer <- taxa_per %>%
    dplyr::left_join(over %>% dplyr::select(cluster, overstorey)) %>%
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


  under <- taxa_per %>%
    dplyr::left_join(over %>% dplyr::select(-per_cov)) %>%
    dplyr::filter(storey < overstorey) %>%
    dplyr::group_by(cluster,storey) %>%
    dplyr::slice_max(order_by = per_cov, n = 3) %>%
    dplyr::slice_max(order_by = per_pres,n = 3) %>%
    dplyr::mutate(AD = "U"
                  , overstorey = names(which.max(table(storey)))
                  , pres = freq_class(per_pres)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(pres != "infrequent") %>%
    make_storey_text(keep_storey = FALSE) %>%
    dplyr::rename(under_text = text)


  sf_wetland <- taxa_per %>%
    dplyr::filter(grepl(paste0(taxa_wetland,collapse="|"),taxa)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(wet_cov = sum(per_cov)) %>%
    dplyr::filter(wet_cov > 50)


  sf_samphire <- taxa_per %>%
    dplyr::filter(grepl(paste0(taxa_samphire,collapse="|"),taxa)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(sam_cov = sum(per_cov)) %>%
    dplyr::filter(sam_cov > 50)


  vsf <- vsf %>%
    dplyr::left_join(sf_wetland) %>%
    dplyr::left_join(sf_samphire) %>%
    dplyr::mutate(sa_vsf = if_else(wet_cov > sum_cov & !is.na(wet_cov)
                                  ,"Wetland"
                                  , if_else(sam_cov > sum_cov & !is.na(sam_cov)
                                            , "Samphire"
                                            , sa_vsf
                                            )
                                  )
                  )


  definition <- over %>%
    dplyr::select(cluster,over,over_text) %>%
    dplyr::left_join(emer %>% dplyr::select(cluster,emer_text)) %>%
    dplyr::left_join(under %>% dplyr::select(cluster,under_text)) %>%
    dplyr::left_join(vsf %>% dplyr::select(cluster,sum_cov,sa_vsf)) %>%
    dplyr::mutate(sa_vsf = if_else(grepl(paste0(paste0("always ",taxa_samphire,collapse="|")
                                               ,"|"
                                               , paste0("frequent",taxa_samphire,collapse="|")
                                               )
                                        ,paste0(over_text," ",under_text)
                                        )
                                  ,"Samphire"
                                  ,sa_vsf
                                  )
                  , sa_vsf = if_else(grepl(paste0(paste0("always ",taxa_wetland,collapse="|")
                                                 ,"|"
                                                 , paste0("frequent ",taxa_wetland,collapse="|")
                                                 )
                                          ,paste0(over_text," ",under_text)
                                          )
                                    ,"Wetland"
                                    ,sa_vsf
                                    )
                  #, overtext = if_else(is.na(overtext) & grepl("very very",tolower(sa_vsf)),emertext,overtext)
                  #, emertext = ifelse(overtext == emertext,NA,emertext)
                  , sf = tolower(stringr::word(sa_vsf,-1))
                  , sf = factor(sf, levels = levels(sa_sf$sf))
                  ) %>%
    dplyr::group_by(cluster,sa_vsf,sf,sum_cov,over,over_text) %>%
    dplyr::summarise(definition = purrr::pmap(list(over_text
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
    dplyr::mutate(over = gsub(";.*|_","",definition)
                  , over = gsub("&ast;_","*",definition)
                  ) %>%
    tidyr::unnest(cols = c(definition)) %>%
    dplyr::mutate(veg = TRUE)

  if(isTRUE(!is.null(type_colours))) {

    definition <- definition %>%
      dplyr::left_join(type_colours)

  } else {

    n_colours <- length(unique(definition$cluster))

    use_colours <- colourvalues::color_palettes(n_colours)

    definition <- definition %>%
      dplyr::group_by(cluster) %>%
      dplyr::mutate(colour = viridis::viridis(n_colours)[cur_group_id()]) %>%
      dplyr::ungroup()

  }

  if(isTRUE(!is.null(add_eco))) {

    add <- add_eco %>%
      dplyr::distinct(cluster) %>%
      dplyr::mutate(sa_vsf = paste0("Landcover: ",cluster)
                    , sf = sa_vsf
                    ) %>%
      dplyr::left_join(lulandcover, by = c("cluster" = add_col_cluster)) %>%
      dplyr::mutate(colour = lc_col)


    definition <- definition %>%
      dplyr::bind_rows(add)

  }

  if(isTRUE(!is.null(type_colours))) {

    definition <- definition %>%
      dplyr::group_by(colour) %>%
      dplyr::mutate(ecotype_n = n()
                    , ecosystem_n = row_number()
                    ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(veg_col = pmap_chr(list(colour,ecosystem_n,ecotype_n)
                                       , set_eco_col
                                       )
                    , lc_col = map_chr(colour,set_eco_col,1,1)
                    , cluster = factor(cluster, levels(spp$cluster))
                    , pred_num = as.numeric(cluster)
                    )

  }

  return(definition)

}
