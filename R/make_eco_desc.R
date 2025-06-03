


#' Add 'extra' ecosystem descriptions
#'
#' These are usually 'landcover' descriptions.
#'
#' @param eco_desc Dataframe of existing (floristic) ecosystem descriptions.
#' @param add_eco Dataframe of ecosystems to add.
#' @param clust_col Character name of column with cluster membership in
#' `eco_desc`.
#' @param add_clust_col Character name of column with cluster membership in
#' `add_eco`.
#' @param add_name Character name of any prefix to use in descriptions for extra
#' ecosystems. e.g. "Landcover" to make, say, "cropping" into "Landcover:
#' cropping".
#' @param colour_map Dataframe mapping `add_clust_col` to colour values to use
#' in mapping.
#' @param add_colour_col Character name of column in `colour_map` that has the
#' colour values.
#' @param use_p_val Numeric 0 to 1. The p-value to use to accept a taxa as an
#' indicator for an ecosystem.
#'
#' @return Dataframe with the same columns as `eco_desc` but with added rows for
#' each row of `add_eco`
#' @export
#'
#' @examples
add_landcover_desc <- function(eco_desc
                               , add_eco
                               , clust_col = "cluster"
                               , add_clust_col = "cluster"
                               , add_name = "Landcover"
                               , colour_map = NULL
                               , add_colour_col = "lc_col"
                               , use_p_val = 0.05
                               ) {

  eco_add <- add_eco %>%
    dplyr::count(!!rlang::ensym(add_clust_col)
                 , name = "sites"
                 ) %>%
    dplyr::mutate(ecotype = factor(add_name)
                  , ecotype_id = gsub(" |[[:punct:]]","",ecotype)
                  , desc = paste0(add_name,": ", gsub("_"," ",!!rlang::ensym(add_clust_col)))
                  ) %>%
    dplyr::rename(!!rlang::ensym(clust_col) := !!rlang::ensym(add_clust_col))

  missing_names <- setdiff(names(eco_desc)[sapply(eco_desc
                                                  , function(x) is.character(x)|is.factor(x))
                                           ]
                           , names(eco_add)
                           )

  eco_add <- eco_add %>%
    dplyr::bind_cols(purrr::map(missing_names[!grepl("desc|md",missing_names)]
                                , function(x) eco_add$x = as.character(eco_add[clust_col][[1]])
                                ) %>%
                       stats::setNames(missing_names[!grepl("desc|md",missing_names)]) %>%
                       as_tibble()
                     ) %>%
    dplyr::bind_cols(purrr::map(missing_names[grepl("desc|md",missing_names)]
                                , function(x) eco_add$x = eco_add$desc
                                ) %>%
                       stats::setNames(missing_names[grepl("desc|md",missing_names)]) %>%
                       as_tibble()
                     ) %>%
    dplyr::select(-colour)

  if(isTRUE(is.null(colour_map))) {

    colour_map <- tibble(!!rlang::ensym(clust_col) := unique(eco_add[clust_col][[1]])) %>%
      dplyr::mutate(colour = paste0("grey",ceiling(100/(2*row_number()))))

  } else {

    colour_map <- colour_map %>%
      dplyr::mutate(!!rlang::ensym(clust_col) := !!rlang::ensym(add_clust_col)) %>%
      dplyr::mutate(!!rlang::ensym(clust_col) := forcats::fct_inorder(!!rlang::ensym(clust_col))
                    , colour = !!rlang::ensym(add_colour_col)
                    )

  }

  res <- eco_desc %>%
    dplyr::mutate(cluster = forcats::fct_expand(cluster, levels(eco_add[clust_col][[1]]))) %>%
    dplyr::bind_rows(eco_add %>%
                       dplyr::left_join(colour_map)
                     ) %>%
    dplyr::select(names(eco_desc)) %>%
    dplyr::mutate(dplyr::across(contains("_id"), ~gsub(" |[[:punct:]]","",.x)))

}



#' Make a description for an ecosystem
#'
#' @param bio_df Dataframe with biological information.
#' @param clust_df Dataframe with cluster membership and join columns to `bio_df`.
#' @param bio_wide Dataframe of taxa by sites.
#' @param bio_ind Dataframe of indicator taxa per class. Output from
#' `envCluster::make_ind_val_df`. Will be created if not supplied.
#' @param clust_col Character name of column in `clust_df` that defines clusters.
#' @param context Character name of columns in `bio_df` that define the context.
#' @param cov_col Character name of column in `bio_df` that contain numeric cover
#' values.
#' @param taxa_col Character name of column with taxa.
#' @param lustr Dataframe containing structural information.
#' @param taxonomy Dataframe containing indigenous status of taxa in `bio_df`
#' @param use_prop_thresh Numeric. Threshold (proportion) for taxa to include in
#' description. Taxa that occur in more than `use_prop_thresh` proportion of
#' sites in the cluster will be included in the description.
#'
#' @return
#' @export
#'
#' @examples
make_eco_desc <- function(bio_df
                          , clust_df
                          , bio_wide = NULL
                          , bio_ind = NULL
                          , context
                          , clust_col = "cluster"
                          , cov_col = "use_cover"
                          , taxa_col = "taxa"
                          , str_col = "lifeform"
                          , ind_col = "ind"
                          , lustr
                          , taxonomy
                          , use_prop_thresh
                          ) {

  taxas <- unique(taxonomy$taxonomy[taxa_col][[1]])

  keep_cols <- c(clust_col, cov_col, taxa_col, str_col, ind_col)

  # unique ------
  bio_df <- dplyr::distinct(bio_df
                            , dplyr::across(tidyselect::any_of(c(context
                                                                 , keep_cols
                                                                 )
                                                               )
                                            )
                            )

  clust_df <- dplyr::distinct(clust_df
                              , dplyr::across(tidyselect::any_of(c(context
                                                                   , clust_col
                                                                   )
                                                                 )
                                              )
                              )

  #------str-------

  lifeforms_all <- bio_df %>%
    dplyr::left_join(lustr) %>%
    dplyr::inner_join(clust_df) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)
                    , dplyr::across(tidyselect::all_of(context))
                    , dplyr::across(tidyselect::all_of(names(lustr)))
                    ) %>%
    dplyr::summarise(sum_cov = sum(!!rlang::ensym(cov_col)
                                   , na.rm = TRUE
                                   )
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(!!rlang::ensym(clust_col)
                   , dplyr::across(tidyselect::all_of(context))
                   , desc(sort)
                   )

  context_vsf_all <- lifeforms_all %>%
    dplyr::group_by(!!rlang::ensym(clust_col)
                    , dplyr::across(tidyselect::all_of(context))
                    , str
                    , sum_cov
                    #, storey
                    ) %>%
    dplyr::summarise(wt_ht = weighted.mean(ht, sum_cov)
                     , sort = min(sort)
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cov_class = cut(sum_cov * 100
                                  , breaks = c(cut_cov$cov_thresh, 0)
                                  )
                  , ht_class = cut(wt_ht
                                   , breaks = c(cut_ht$ht_thresh)
                                   )
                  ) %>%
    dplyr::left_join(sa_vsf)

  context_vsf <- context_vsf_all %>%
    dplyr::group_by(!!rlang::ensym(clust_col)
                    , dplyr::across(tidyselect::all_of(context))
                    ) %>%
    dplyr::mutate(tot_cov = sum(sum_cov)) %>%
    dplyr::filter(sum_cov > 0.05) %>%
    dplyr::filter(sort == min(sort, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(sa_vsf)) %>%
    dplyr::mutate(sf = tolower(gsub(".* ", "", sa_vsf)))

  context_vsf_backup <- context_vsf_all %>%
    dplyr::anti_join(context_vsf %>%
                       dplyr::distinct(!!rlang::ensym(clust_col))
                     ) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)
                    , dplyr::across(tidyselect::all_of(context))
                    ) %>%
    dplyr::mutate(tot_cov = sum(sum_cov)) %>%
    dplyr::filter(sum_cov == max(sum_cov)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sf = "open vegetation"
                  , sa_vsf = "Open vegetation"
                  )

  id_col <- paste0(clust_col, "_id")

  eco_sf <- context_vsf %>%
    dplyr::bind_rows(context_vsf_backup) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    dplyr::summarise(cov = median(tot_cov)
                     , range_sf = paste0(vec_to_sentence(names(table(sf)[table(sf) > quantile(table(sf),probs = 2/3)])))
                     , sf = names(which.max(table(sf)))
                     , range_sf = dplyr::if_else(range_sf == "", sf, range_sf)
                     ) %>%
    dplyr::mutate(!!rlang::ensym(id_col) := gsub(" |[[:punct:]]","",!!rlang::ensym(clust_col))) %>%
    dplyr::select(!!rlang::ensym(clust_col), !!rlang::ensym(id_col), everything()) %>%
    dplyr::ungroup()

  eco_vsf <- context_vsf %>%
    dplyr::bind_rows(context_vsf_backup) %>%
    dplyr::inner_join(eco_sf) %>%
    dplyr::group_by(!!rlang::ensym(clust_col),cov) %>%
    dplyr::summarise(range_vsf = paste0(vec_to_sentence(names(table(sa_vsf)[table(sa_vsf) > quantile(table(sa_vsf),probs = 2/3)])))
                     , vsf = names(which.max(table(sa_vsf)[.data$sf == sf]))
                     , range_vsf = dplyr::if_else(range_vsf == "", vsf, range_vsf)
                     ) %>%
    dplyr::ungroup()


  #------taxa-------

  if(isTRUE(is.null(bio_ind))) {

    eco_ind_val_df <- make_ind_val_df(clust_df = clust_df
                                      , bio_wide = bio_wide
                                      , clust_col = clust_col
                                      , taxas = taxas
                                      , context = context
                                      )

  } else eco_ind_val_df <- bio_ind

  eco_ind <- eco_ind_val_df %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    dplyr::filter(p_val <= use_p_val) %>%
    dplyr::mutate(best = p_val == min(p_val)) |>
    dplyr::ungroup() |>
    dplyr::arrange(!!rlang::ensym(clust_col)) %>%
    dplyr::left_join(dplyr::distinct(taxonomy$ind)) %>%
    dplyr::mutate(use_taxa = dplyr::if_else(ind == "N"
                                            , paste0("&ast;_", taxa, "_")
                                            , paste0("_", taxa, "_")
                                            )
                  ) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    dplyr::summarise(range_ind = envFunc::vec_to_sentence(use_taxa)
                     , best_ind = envFunc::vec_to_sentence(ifelse(best, use_taxa, NA))
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(best_ind_nomd = gsub("_", "", best_ind))

  eco_taxa <- bio_df %>%
    dplyr::inner_join(clust_df) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    dplyr::mutate(cluster_sites = dplyr::n_distinct(cell_lat, cell_long)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(taxonomy$ind) %>%
    dplyr::count(!!rlang::ensym(clust_col), cluster_sites, taxa, ind, name = "taxa_sites") %>%
    dplyr::mutate(prop = taxa_sites / cluster_sites) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    dplyr::anti_join(eco_ind %>%
                       dplyr::select(!!rlang::ensym(clust_col)
                                     , !!rlang::ensym(taxa_col) := best_ind_nomd
                                     ) |>
                       dplyr::mutate(!!rlang::ensym(taxa_col) := gsub("&ast;", "", !!rlang::ensym(taxa_col)))
                     ) %>%
    dplyr::mutate(best = prop == max(prop, na.rm = TRUE)) %>%
    dplyr::filter(prop > use_prop_thresh | best) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(freq = add_freq_class(prop*100)
                  , use_taxa = dplyr::if_else(ind == "N",paste0("&ast;_",taxa,"_"),paste0("_",taxa,"_"))
                  ) %>%
    dplyr::group_by(!!rlang::ensym(clust_col),freq) %>%
    dplyr::summarise(text = envFunc::vec_to_sentence(use_taxa)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(text = paste0(freq, " ", text)) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    dplyr::summarise(range_taxa = envFunc::vec_to_sentence(text)) %>%
    dplyr::ungroup()

  #--------desc ---------

  desc_res <- clust_df %>%
    dplyr::count(!!rlang::ensym(clust_col), name = "sites") %>%
    dplyr::left_join(eco_sf) %>%
    dplyr::left_join(eco_vsf) %>%
    dplyr::left_join(eco_taxa) %>%
    dplyr::left_join(eco_ind) %>%
    dplyr::mutate(desc_md = paste0(!!rlang::ensym(clust_col)
                                   , ": "
                                   , range_sf
                                   , dplyr::if_else(is.na(range_ind)
                                                    , ""
                                                    , paste0(" indicated by "
                                                             , best_ind
                                                             )
                                                    )
                                   , dplyr::if_else(is.na(range_taxa)
                                                    , ""
                                                    , paste0(" with "
                                                             , range_taxa
                                                             )
                                                    )
                                   )
                  , desc = gsub("_","",desc_md)
                  , desc = gsub("&ast;","*",desc)
                  )

}

make_eco_name <- function(blahdyblahblah) {

  make_storey_text <- function(df, keep_storey = TRUE) {

    groups <- if("pres" %in% names(df)) "pres"
    groups <- if(keep_storey) c("cluster","AD","overstorey",groups) else c("cluster","AD",groups)

    df %>%
      dplyr::left_join(taxonomy$ind) %>%
      dplyr::arrange(cluster,per_pres) %>%
      dplyr::mutate(use_taxa = dplyr::if_else(ind == "N",paste0("&ast;_",taxa,"_"),paste0("_",taxa,"_"))) %>%
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
    dplyr::mutate(sa_vsf = dplyr::if_else(wet_cov > sum_cov & !is.na(wet_cov)
                                  ,"Wetland"
                                  , dplyr::if_else(sam_cov > sum_cov & !is.na(sam_cov)
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
    dplyr::mutate(sa_vsf = dplyr::if_else(grepl(paste0(paste0("always ",taxa_samphire,collapse="|")
                                               ,"|"
                                               , paste0("frequent",taxa_samphire,collapse="|")
                                               )
                                        ,paste0(over_text," ",under_text)
                                        )
                                  ,"Samphire"
                                  ,sa_vsf
                                  )
                  , sa_vsf = dplyr::if_else(grepl(paste0(paste0("always ",taxa_wetland,collapse="|")
                                                 ,"|"
                                                 , paste0("frequent ",taxa_wetland,collapse="|")
                                                 )
                                          ,paste0(over_text," ",under_text)
                                          )
                                    ,"Wetland"
                                    ,sa_vsf
                                    )
                  #, overtext = dplyr::if_else(is.na(overtext) & grepl("very very",tolower(sa_vsf)),emertext,overtext)
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
      dplyr::mutate(ecotype_n = dplyr::n()
                    , ecosystem_n = dplyr::row_number()
                    ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(veg_col = purrr::pmap_chr(list(colour,ecosystem_n,ecotype_n)
                                              , set_eco_col
                                              )
                    , lc_col = purrr::map_chr(colour,set_eco_col,1,1)
                    , cluster = factor(cluster, levels(spp$cluster))
                    , pred_num = as.numeric(cluster)
                    )

  }

  return(definition)

}
