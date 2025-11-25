
#' Make a (biotic) description for an ecosystem
#'
#' The description includes: key structural formation; taxa associated with
#' that structural formation; indicator taxa; and taxa present at a 'high'
#' proportion of sites.
#'
#' The structural component is determined by:
#'
#' * finding the highest 'storey' per site with greater than 5% cover
#' * within that storey, across all sites in a cluster, finding the highest,
#' most frequent structural formation(s). See `ht_sf_quant` and `sites_sf_prop`
#' * finding taxa that represent that structural formation across sites (`sites_sf_taxa_quant`)
#'
#' Indicator taxa are determined using `labdsv::inval()`. See `use_p_val`,
#' `n_ind_max` and `inf_val_iter`.
#'
#' Common taxa are determined as any taxa occurring at more than
#' `use_prop_thresh` sites within a cluster.
#'
#' Indicator taxa and structural taxa are prevented from also being common taxa.
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
#' @param clust_keep_cols Character. Name of any columns in `clust_df` that
#' should be passed through to the output. These should not lead to any further
#' combinations (rows) than `clust_col` alone does.
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
#' @param sites_sf_prop Numeric. Threshold (proportion) of sites above which to
#' choose structural features for the description.
#' @param ht_sf_quant Numeric. Threshold (quantile) of heights above which to
#' choose structural features for the description.
#' @param sites_sf_taxa_quant Numeric. Threshold (quantile) of counts above
#' which a taxa will be used as an example of a structural feature.
#' @param colour_map Dataframe mapping any column in result to colour values
#' (in a column called `colour`).
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
                          , clust_keep_cols = c("landcover", "veg")
                          , lustr
                          , taxonomy
                          , use_prop_thresh = 0.8
                          , use_p_val = 0.05
                          , n_ind_max = 3
                          , ind_val_iter = 3000
                          , sites_sf_prop = 1 / 3
                          , ht_sf_quant = 0.5
                          , sites_sf_taxa_quant = 0.95
                          , colour_map = NULL
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
                                                                   , clust_keep_cols
                                                                   )
                                                                 )
                                              )
                              )

  sites_col <- paste0(clust_col, "_sites")

  # Add storey and structure -------
  lifeforms_all <- bio_df |>
    dplyr::left_join(lustr) |>
    dplyr::inner_join(clust_df |>
                        dplyr::add_count(dplyr::across(tidyselect::any_of(c(clust_col)))
                                         , name = sites_col
                                         )
                      ) |>
    dplyr::group_by(!!rlang::ensym(clust_col)
                    , dplyr::across(tidyselect::all_of(context))
                    , storey
                    ) |>
    dplyr::mutate(storey_cov = sum(!!rlang::ensym(cov_col)
                                   , na.rm = TRUE
                                   )
                  , storey_cov = dplyr::if_else(storey_cov > 1, 1, storey_cov)
                  , wt_ht = weighted.mean(ht, cover_adj, na.rm = TRUE)
                  ) %>%
    dplyr::ungroup() %>%
    # total cover
    dplyr::group_by(!!rlang::ensym(clust_col)
                    , dplyr::across(tidyselect::all_of(context))
                    ) %>%
    dplyr::mutate(tot_cov = sum(!!rlang::ensym(cov_col)
                                , na.rm = TRUE
                                )
                  ) |>
    dplyr::ungroup() |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(c(taxa_col, clust_col, context)))
                    , sort
                    , str
                    , storey
                    , storey_cov
                    , tot_cov
                    , wt_ht
                    , !!rlang::ensym(sites_col)
                    ) |>
    dplyr::arrange(!!rlang::ensym(clust_col)
                   , dplyr::across(tidyselect::all_of(context))
                   , desc(sort)
                   ) |>
    dplyr::mutate(cov_class = cut(storey_cov * 100
                                  , breaks = c(envEcosystems::cut_cov$cov_thresh)
                                  )
                  , ht_class = cut(wt_ht
                                   , breaks = c(envEcosystems::cut_ht$ht_thresh)
                                   )
                  ) |>
    dplyr::left_join(sa_vsf |>
                       dplyr::mutate(sf = sa_sf) |>
                       dplyr::select(! matches("sa_"))
                     )


  # Storey -------
  # Filter to the tallest storey PER SITE (with > 5% cover)
  suppressWarnings(
    context_storey <- lifeforms_all |>
      dplyr::distinct(dplyr::across(tidyselect::all_of(c(clust_col, context)))
                      , storey_cov, wt_ht, storey, sf, tot_cov, !!rlang::ensym(sites_col)
                      ) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(clust_col, context))), !!rlang::ensym(sites_col)) |>
      # find any storey with more than 5% cover per context
      dplyr::filter(storey_cov > 0.05) |>
      # find highest storey per context
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(clust_col, context))), !!rlang::ensym(sites_col)) |>
      dplyr::filter(wt_ht == max(wt_ht, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(sf)) |>
      dplyr::distinct()
  )

  suppressWarnings(
    # backup if context has no storey that reaches 5% cover
    context_storey_backup <- lifeforms_all |>
      dplyr::distinct(dplyr::across(tidyselect::all_of(c(clust_col, context)))
                      , storey_cov, wt_ht, sf, tot_cov, !!rlang::ensym(sites_col)
                      ) |>
      dplyr::anti_join(context_storey |>
                         dplyr::distinct(!!rlang::ensym(clust_col))
                       ) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(clust_col, context))), !!rlang::ensym(sites_col)) |>
      dplyr::filter(storey_cov == max(storey_cov, na.rm = TRUE)) |>
      dplyr::ungroup()
  )

  # Structure ---------
  # using only the highest storey from last section
  # Find the most frequent, tallest, structure PER CLUSTER
  eco_sf <- context_storey |>
    dplyr::bind_rows(context_storey_backup) |>
    # median height for each sf and cluster
    dplyr::group_by(!!rlang::ensym(clust_col), sf) |>
    dplyr::mutate(med_ht = median(wt_ht, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    # median of tot_cov each cluster
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::mutate(med_cov = median(tot_cov, na.rm = TRUE)) |>
    # find the most frequent, highest sf
    dplyr::distinct(dplyr::across(tidyselect::any_of(context))
                    , !!rlang::ensym(clust_col), sf, med_cov, med_ht
                    , !!rlang::ensym(sites_col)
                    ) |>
    dplyr::count(!!rlang::ensym(clust_col), sf, med_cov, med_ht
                 , !!rlang::ensym(sites_col)
                 , name = "sites_sf"
                 ) |>
    dplyr::mutate(sites_prop = sites_sf / !!rlang::ensym(sites_col)) |>

    dplyr::filter(med_ht > quantile(med_ht, probs = ht_sf_quant, na.rm = TRUE) | med_ht == max(med_ht, na.rm = TRUE)) |>
    dplyr::filter(sites_prop > sites_sf_prop | sites_prop == max(sites_prop)) |>

    dplyr::mutate(sf_most = sf[which.max(sites_sf)]) |>
    # find a value for 'ht' across all clust_col * sf (still need cov below)
    dplyr::group_by(!!rlang::ensym(clust_col), sf_most, sf, med_cov) |>
    dplyr::mutate(med_ht = max(med_ht, na.rm = TRUE)) |>
    dplyr::ungroup()


  # Structure taxa ---------
  # Find the taxa that represent the previously determined structure per cluster
  eco_sf_taxa_prep <- eco_sf |>
    dplyr::inner_join(lifeforms_all) |>
    dplyr::count(dplyr::across(tidyselect::any_of(c(clust_col, taxa_col))), sf_most, sf, med_cov, med_ht, !!rlang::ensym(sites_col)
                 , name = "sites_sf_taxa"
                 ) |>
    dplyr::group_by(!!rlang::ensym(clust_col), sf, med_cov, med_ht, !!rlang::ensym(sites_col)) |>
    dplyr::filter(sites_sf_taxa > quantile(sites_sf_taxa, probs = sites_sf_taxa_quant) | sites_sf_taxa == max(sites_sf_taxa)) |>
    dplyr::ungroup()

  eco_sf_taxa <- eco_sf_taxa_prep |>
    dplyr::left_join(dplyr::distinct(taxonomy$ind)) |>
    dplyr::mutate(prop = sites_sf_taxa / !!rlang::ensym(sites_col)
                  , freq = envFunc::add_freq_class(prop * 100)
                  , str_taxa = dplyr::if_else(ind == "N"
                                              , paste0("&ast;_", taxa, "_")
                                              , paste0("_", taxa, "_")
                                              )
                  ) |>
    # build str taxa per frequency
    dplyr::group_by(!!rlang::ensym(clust_col), freq, sf_most, sf, med_cov, med_ht) |>
    dplyr::summarise(str_taxa = stringr::str_flatten_comma(str_taxa)) |>
    dplyr::arrange(!!rlang::ensym(clust_col), freq) |>
    # build desc per sf * clust_col
    dplyr::group_by(!!rlang::ensym(clust_col), sf_most, sf, med_cov, med_ht) |>
    dplyr::mutate(str_taxa = paste0(freq, " ", str_taxa)) |>
    dplyr::summarise(str_taxa = stringr::str_flatten(str_taxa, collapse = "; ")) |>
    # add structural component to str_taxa
    dplyr::mutate(str_taxa = paste0(sf, " ", str_taxa)) |>
    # summarise str_taxa
    dplyr::group_by(!!rlang::ensym(clust_col), med_cov, sf_most) |>
    dplyr::summarise(med_ht = median(med_ht, na.rm = TRUE)
                     , sf_range = envFunc::vec_to_sentence(sf, end = "or")
                     , sf_taxa_range = envFunc::vec_to_sentence(str_taxa, end = "or")
                     ) |>
    dplyr::ungroup()

  # Indicator taxa ---------
  if(is.null(ind_val_df)) {

    ind_val_df <- make_ind_val_df(clust_df = clust_df
                                  , bio_wide = bio_wide
                                  , cov_col = ind_abu_col
                                  , context = context
                                  , clust_col = clust_col
                                  , numitr = ind_val_iter
                                  )|>
      tidyr::unnest(cols = c(inds)) |>
      tidyr::unnest(cols = c(inds))

  }

  eco_ind_prep <- ind_val_df |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::slice_min(p_val
                     , n = n_ind_max
                     ) |>
    dplyr::slice_max(frq
                     , n = n_ind_max
                     ) |>
    dplyr::filter(p_val <= use_p_val) |>
    dplyr::arrange(!!rlang::ensym(clust_col)
                   , p_val
                   , desc(ind_val)
                   ) |>
    dplyr::mutate(ind_num = dplyr::row_number()
                  , best = ind_num == min(ind_num)
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
    dplyr::summarise(range_ind = stringr::str_flatten_comma(taxa)
                     , best_ind = ifelse(best, taxa, NA) |> na.omit(object = _)
                     , range_ind_md = envFunc::vec_to_sentence(use_taxa, end = "and/or")
                     , best_ind_md = ifelse(best, use_taxa, NA) |> na.omit(object = _)
                     ) %>%
    dplyr::ungroup()


  # Common taxa  -----
  eco_taxa <- bio_df %>%
    dplyr::inner_join(clust_df) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    dplyr::mutate(!!rlang::ensym(sites_col) := dplyr::n_distinct(dplyr::across(tidyselect::any_of(context)))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dplyr::distinct(taxonomy$ind)) %>%
    dplyr::count(!!rlang::ensym(clust_col), !!rlang::ensym(sites_col), taxa, ind, name = "taxa_sites") %>%
    dplyr::mutate(prop = taxa_sites / !!rlang::ensym(sites_col)) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    # prevent indicators turning up again in common taxa
    dplyr::anti_join(eco_ind_prep |>
                       dplyr::distinct(dplyr::across(tidyselect::any_of(c(clust_col, taxa_col))))
                     ) %>%
    # prevent structural taxa turning up again in common taxa
    dplyr::anti_join(eco_sf_taxa_prep |>
                       dplyr::distinct(dplyr::across(tidyselect::any_of(c(clust_col, taxa_col))))
                     ) |>
    dplyr::filter(prop > use_prop_thresh) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(freq = envFunc::add_freq_class(prop * 100)
                  , use_taxa = dplyr::if_else(ind == "N",paste0("&ast;_",taxa,"_"),paste0("_",taxa,"_"))
                  ) %>%
    dplyr::group_by(!!rlang::ensym(clust_col), freq) %>%
    dplyr::summarise(text = envFunc::vec_to_sentence(use_taxa, end = "and/or")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(text = paste0(freq, " ", text)) %>%
    dplyr::group_by(!!rlang::ensym(clust_col)) %>%
    dplyr::summarise(range_taxa = envFunc::vec_to_sentence(text, end = "and/or")) %>%
    dplyr::ungroup()

  # Cluster colour --------
  if(isTRUE(is.null(colour_map))) {

    colour_map <- tibble::tibble(!!rlang::ensym(clust_col) := unique(clust_df[clust_col][[1]])) %>%
      dplyr::mutate(colour = viridis::viridis(n = nrow(.)))

  }

  # Final description ---------

  id_col <- paste0(clust_col, "_id")

  desc_res <- clust_df %>%
    dplyr::count(dplyr::across(tidyselect::any_of(c(clust_col, clust_keep_cols)))
                 , name = sites_col
                 ) |>
    dplyr::left_join(eco_taxa) |>
    dplyr::left_join(eco_ind) |>
    dplyr::left_join(eco_sf_taxa) |>
    dplyr::mutate(desc_md = paste0(!!rlang::ensym(clust_col)
                                   , ": "
                                   , sf_taxa_range
                                   , dplyr::if_else(is.na(range_ind_md)
                                                    , ""
                                                    , paste0(". indicated by "
                                                             , range_ind_md
                                                             )
                                                    )
                                   , dplyr::if_else(is.na(range_taxa)
                                                    , ""
                                                    , paste0(". with "
                                                             , range_taxa
                                                             )
                                                    )
                                   )
                  , desc_html = gsub("&ast;", "*", desc_md)
                  , desc = gsub("_", "", desc_html)
                  ) |>
    dplyr::mutate(!!rlang::ensym(id_col) := gsub("\\s|[[:punct:]]","",!!rlang::ensym(clust_col))) |>
    dplyr::rename(sf = sf_most) |>
    dplyr::left_join(colour_map)

  return(desc_res)

}
