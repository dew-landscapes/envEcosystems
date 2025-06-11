
#' Make a description for an ecosystem
#'
#' @param clust_df Dataframe with `context` column(s) and a column with cluster
#' membership for that context. Optional if `clust_col` appears in bio_df.
#' @param bio_df Dataframe containing the site and taxa data in long format.
#' @param bio_wide Wide version of `bio_df`.
#' @param ind_val_df Result from `envCluster::make_ind_val_df()`. If not
#' provided it will be made from the other provided dfs (which is slow).
#' @param context Character. Name(s) of column(s) that define the context.
#' @param clust_col Character. Name of column containing cluster membership.
#' @param taxa_col Character. Name of column containing the taxa names.
#' @param cov_col Character. Name of column containing numeric abundance data (
#' usually 'cover' for plants).
#' @param ind_abu_col Character. Name of column containing numeric abundance
#' data for use in indicator analysis (`labdsv::indval()`). This can be the same
#' as `cov_col` but at times a different measure of abundance is helpful here.
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
                          , bio_wide
                          , clust_df
                          , ind_val_df = NULL
                          , context
                          , clust_col = "cluster"
                          , taxa_col = "taxa"
                          , cov_col = "cover_adj"
                          , ind_abu_col = "use_cover"
                          , str_col = "lifeform"
                          , lustr
                          , taxonomy
                          , use_prop_thresh
                          , use_p_val = 0.05
                          , n_ind_max = 3
                          , ind_val_iter = 3000
                          ) {

  taxas <- unique(taxonomy$taxonomy[taxa_col][[1]])

  keep_cols <- c(clust_col, cov_col, taxa_col, str_col, ind_abu_col, "ind")

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

  lifeforms_all <- bio_df |>
    dplyr::left_join(lustr) |>
    dplyr::inner_join(clust_df) |>
    dplyr::group_by(!!rlang::ensym(clust_col)
                    , dplyr::across(tidyselect::all_of(context))
                    , str
                    ) |>
    dplyr::mutate(str_cov = sum(!!rlang::ensym(cov_col)
                                   , na.rm = TRUE
                                   )
                  , wt_ht = weighted.mean(ht, cover_adj)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!rlang::ensym(clust_col)
                    , dplyr::across(tidyselect::all_of(context))
                    ) %>%
    dplyr::mutate(tot_cov = sum(str_cov)) |>
    dplyr::ungroup() |>
    dplyr::arrange(!!rlang::ensym(clust_col)
                   , dplyr::across(tidyselect::all_of(context))
                   , desc(sort)
                   ) |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(c(taxa_col, clust_col, context)))
                    , sort
                    , str
                    , str_cov
                    , tot_cov
                    , wt_ht
                    ) |>
    dplyr::mutate(cov_class = cut(str_cov * 100
                                  , breaks = c(cut_cov$cov_thresh, 0)
                                  )
                  , ht_class = cut(wt_ht
                                   , breaks = c(cut_ht$ht_thresh)
                                   )
                  ) |>
    dplyr::left_join(sa_vsf) |>
    dplyr::mutate(sf = tolower(gsub(".* ", "", sa_vsf)))

  context_vsf <- lifeforms_all |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(c(clust_col, context)))
                    , str_cov, wt_ht, sf
                    ) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(clust_col, context)))) |>
    dplyr::mutate(tot_cov = sum(str_cov, na.rm = TRUE)) |>
    dplyr::filter(str_cov > 0.05) |>
    dplyr::filter(wt_ht == max(wt_ht, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(sf)) |>
    dplyr::distinct()

  context_vsf_backup <- lifeforms_all |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(c(clust_col, context)))
                    , str_cov, wt_ht, sf
                    ) |>
    dplyr::anti_join(context_vsf |>
                       dplyr::distinct(!!rlang::ensym(clust_col))
                     ) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(clust_col, context)))) |>
    dplyr::mutate(tot_cov = sum(str_cov)) |>
    dplyr::filter(str_cov == max(tot_cov)) |>
    dplyr::ungroup() |>
    dplyr::mutate(sf = "open vegetation")

  eco_sf <- context_vsf |>
    dplyr::bind_rows(context_vsf_backup) |>
    dplyr::group_by(!!rlang::ensym(clust_col), sf) |>
    dplyr::mutate(ht = median(wt_ht, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::mutate(cov = median(tot_cov, na.rm = TRUE)) |>
    dplyr::count(!!rlang::ensym(clust_col), sf, cov, ht) |>
    dplyr::filter(n == max(n)) |>
    dplyr::filter(ht == max(ht)) |>
    dplyr::ungroup()


  # str taxa ---------

  eco_sf_taxa_prep <- eco_sf |>
    dplyr::inner_join(lifeforms_all) |>
    dplyr::count(dplyr::across(tidyselect::any_of(c(clust_col, taxa_col))), sf) |>
    dplyr::group_by(!!rlang::ensym(clust_col), sf) |>
    dplyr::filter(n == max(n)) |>
    dplyr::ungroup()

  eco_sf_taxa <- eco_sf_taxa_prep |>
    dplyr::left_join(dplyr::distinct(taxonomy$ind)) |>
    dplyr::mutate(str_taxa = dplyr::if_else(ind == "N"
                                            , paste0("&ast;_", taxa, "_"), paste0("_", taxa, "_")
                                            )
                  ) |>
    dplyr::group_by(!!rlang::ensym(clust_col), sf) |>
    dplyr::summarise(str_taxa = envFunc::vec_to_sentence(str_taxa, end = "and/or")) |>
    dplyr::ungroup() |>
    dplyr::mutate(str_taxa = paste0(sf, " (e.g. ", str_taxa, ")")) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::summarise(str_taxa = envFunc::vec_to_sentence(str_taxa, sep = "/", end = "/")) |>
    dplyr::ungroup()

  #------taxa-------

  ## ind -------
  if(is.null(ind_val_df)) {

    ind_val_df <- make_ind_val_df(clust_df = clust_df
                                  , bio_wide = bio_wide
                                  , cov_col = ind_abu_col
                                  , context = context
                                  , clust_col = clust_col
                                  , numitr = ind_val_iter
                                  )

  }

  eco_ind_prep <- ind_val_df %>%
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
    dplyr::summarise(range_ind = envFunc::vec_to_sentence(use_taxa, end = "and/or")
                     , best_ind = envFunc::vec_to_sentence(ifelse(best, use_taxa, NA), end = "and/or")
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(best_ind_nomd = gsub("_", "", best_ind))


  ## prop -----
  eco_taxa <- bio_df %>%
    dplyr::inner_join(clust_df) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    dplyr::mutate(cluster_sites = dplyr::n_distinct(dplyr::across(tidyselect::any_of(context)))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dplyr::distinct(taxonomy$ind)) %>%
    dplyr::count(!!rlang::ensym(clust_col), cluster_sites, taxa, ind, name = "taxa_sites") %>%
    dplyr::mutate(prop = taxa_sites / cluster_sites) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    dplyr::anti_join(eco_ind_prep |>
                       dplyr::distinct(dplyr::across(tidyselect::any_of(c(clust_col, taxa_col))))
                     ) %>%
    dplyr::anti_join(eco_sf_taxa_prep |>
                       dplyr::distinct(dplyr::across(tidyselect::any_of(c(clust_col, taxa_col))))
                     ) |>
    dplyr::mutate(best = prop == max(prop, na.rm = TRUE)) %>%
    dplyr::filter(prop > use_prop_thresh | best) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(freq = envFunc::add_freq_class(prop * 100)
                  , use_taxa = dplyr::if_else(ind == "N",paste0("&ast;_",taxa,"_"),paste0("_",taxa,"_"))
                  ) %>%
    dplyr::group_by(!!rlang::ensym(clust_col),freq) %>%
    dplyr::summarise(text = envFunc::vec_to_sentence(use_taxa, end = "and/or")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(text = paste0(freq, " ", text)) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    dplyr::summarise(range_taxa = envFunc::vec_to_sentence(text, end = "and/or")) %>%
    dplyr::ungroup()


  #--------desc ---------

  id_col <- paste0(clust_col, "_id")

  desc_res <- clust_df %>%
    dplyr::count(!!rlang::ensym(clust_col), name = "sites") |>
    dplyr::left_join(eco_taxa) |>
    dplyr::left_join(eco_ind) |>
    dplyr::left_join(eco_sf) |>
    dplyr::left_join(eco_sf_taxa) |>
    dplyr::mutate(desc_md = paste0(!!rlang::ensym(clust_col)
                                   , ": "
                                   , str_taxa
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
                  , desc_html = gsub("&ast;", "*", desc_md)
                  , desc = gsub("_", "", desc_html)
                  ) |>
    dplyr::mutate(!!rlang::ensym(id_col) := gsub(" |[[:punct:]]","",!!rlang::ensym(clust_col)))


}
