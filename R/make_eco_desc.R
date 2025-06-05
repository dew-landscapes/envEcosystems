
#' Make a description for an ecosystem
#'
#' @param clust_df Dataframe with `context` column(s) and a column with cluster
#' membership for that context. Optional if `clust_col` appears in bio_df.
#' @param bio_df Dataframe containing the site and taxa data in long format.
#' @param context Character. Name(s) of column(s) that define the context.
#' @param clust_col Character. Name of column containing cluster membership.
#' @param taxa_col Character. Name of column containing the taxa names.
#' @param cov_col Character name of column in `bio_df` that contain numeric cover
#' values.
#' @param str_col Character name of column in `bio_df` containing lifeform (or
#' structural) information.
#' @param lustr Dataframe containing lifeform (structural) information.
#' @param taxonomy Dataframe containing indigenous status of taxa in `bio_df`
#' @param use_prop_thresh Numeric. Threshold (proportion) for taxa to include in
#' description. Taxa that occur in more than `use_prop_thresh` proportion of
#' sites in the cluster will be included in the description.
#' @param use_p_val Numeric 0 to 1. The p-value to use to accept a taxa as an
#' indicator for an ecosystem.
#' @param n_ind_max Maximum number of taxa to list as indicators.
#' @param ind_val_iter Passed to the `...` argument of
#' `envCluster::make_ind_val_df()`, and then into the `numitr` argument of
#' `labdsv::indval()`.
#'
#' @return
#' @export
#'
#' @examples
make_eco_desc <- function(bio_df
                          , clust_df
                          , context
                          , clust_col = "cluster"
                          , taxa_col = "taxa"
                          , cov_col = "use_cover"
                          , str_col = "lifeform"
                          , lustr
                          , taxonomy
                          , use_prop_thresh
                          , use_p_val = 0.05
                          , n_ind_max = 3
                          , ind_val_iter = 3000
                          ) {

  taxas <- unique(taxonomy$taxonomy[taxa_col][[1]])

  keep_cols <- c(clust_col, cov_col, taxa_col, str_col, "ind")

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

  ## ind -------
  eco_ind_val_df <- make_ind_val_df(clust_df = clust_df
                                    , bio_df = bio_df
                                    , context = context
                                    , clust_col = clust_col
                                    , numitr = ind_val_iter
                                    )

  eco_ind_prep <- eco_ind_val_df %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    dplyr::filter(p_val <= use_p_val) %>%
    dplyr::mutate(best = p_val == min(p_val)) |>
    dplyr::slice_max(ind_val
                     , n = n_ind_max
                     ) |>
    dplyr::ungroup()

  eco_ind <- eco_ind_prep |>
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
    dplyr::mutate(cluster_sites = dplyr::n_distinct(dplyr::across(tidyselect::any_of(context)))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(taxonomy$ind) %>%
    dplyr::count(!!rlang::ensym(clust_col), cluster_sites, taxa, ind, name = "taxa_sites") %>%
    dplyr::mutate(prop = taxa_sites / cluster_sites) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    dplyr::anti_join(eco_ind_prep %>%
                       dplyr::select(!!rlang::ensym(clust_col)
                                     , !!rlang::ensym(taxa_col)
                                     )
                     ) %>%
    dplyr::mutate(best = prop == max(prop, na.rm = TRUE)) %>%
    dplyr::filter(prop > use_prop_thresh | best) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(freq = add_freq_class(prop * 100)
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
