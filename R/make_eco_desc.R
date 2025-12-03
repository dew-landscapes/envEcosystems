
#' Make a (biotic) description for an ecosystem
#'
#' @description
#' The description includes: 'important' taxa and their structural form(s);
#' indicator taxa; and other taxa present at a 'high' proportion of sites.
#'
#' @details
#' 'Important' taxa are determined by thresholding:
#'
#' * quantile median cover value for each taxa; and
#' * absolute median cover value for each taxa
#'
#' Indicator taxa requires a result from `envCluster::make_ind_val_df()` (a
#' wrapper around `labdsv::inval()`). Choosing indicators uses  `indicator_p_val` and
#' `indicator_max_n`.
#'
#' Common taxa are determined as any taxa occurring at more than
#' `common_taxa_prop_thresh` sites within a cluster.
#'
#' Important taxa and indicator taxa are prevented from also being common taxa.
#'
#' @param bio_df Dataframe containing all `context`, all the `_col`s and taxa
#' data in long format.
#' @param ind_val_df Result from `envCluster::make_ind_val_df()`.
#' @param context Character. Name(s) of column(s) in `bio_df` that define bins.
#' @param clust_col Character. Name of column in `bio_df` containing cluster
#' membership.
#' @param taxa_col Character. Name of column in `bio_df` and/or `indigenous_df`
#' containing the taxa names.
#' @param cov_col Character. Name of column in `bio_df` with numeric
#' abundance data (usually 'cover' for plants).
#' @param ht_col Character. Name of column in `bio_df` with numeric height data.
#' @param ind_abu_col Character. Name of column in `bio_df` with numeric
#' abundance data for use in indicator analysis (`labdsv::indval()`).
#' This can be the same as `cov_col` but at times a different measure of
#' abundance is helpful here.
#' @param str_col Character name of column in `bio_df` and `lustr`containing
#' lifeform (or structural) information.
#' @param lustr Dataframe containing lifeform (structural) information.
#' @param indigenous_df dataframe containing indigenous status for each unique
#' `taxa_col` in `bio_df`
#' @param imp_taxa_quantile_thresh Numeric. Quantile theshold of median cover
#' across all sites in a cluster for choosing `important` taxa. Combined with
#' `imp_taxa_quantile_thresh`. If no taxa meet this threshold the taxa with the
#' maximum of median cover across all sites is kept.
#' @param imp_taxa_prop_thresh Numeric. Proportion theshold of median cover
#' across all sites in a cluster for choosing `important` taxa. Combined with
#' `imp_taxa_prop_thresh`. If no taxa meet this threshold the taxa with the
#' maximum of median cover across all sites is kept.
#' @param indicator_p_val Numeric 0 to 1. The p-value to use to accept a taxa as an
#' indicator for an ecosystem.
#' @param indicator_max_n Maximum number of taxa to list as indicators.
#' @param common_taxa_prop_thresh Numeric. Threshold (proportion) for taxa to include in
#' description. Taxa that occur in more than `common_taxa_prop_thresh` proportion of
#' sites in the cluster will be included in the description.

#' @param colour_map Dataframe mapping any column in result to colour values
#' (in a column called `colour`).
#'
#' @return Tibble of ecosystems descriptions per unique value of `clust_col` in
#' `bio_df`
#' @export
#'
#' @examples
make_eco_desc <- function(bio_df
                          , ind_val_df = NULL
                          , context
                          , clust_col = "cluster"
                          , taxa_col = "taxa"
                          , cov_col = "cover_adj"
                          , ind_abu_col = "use_cover"
                          , ht_col = "ht"
                          , lustr
                          , str_col = "lifeform"
                          , indigenous_df = NULL
                          , indicator_p_val = 0.05
                          , indicator_max_n = 3
                          , imp_taxa_quantile_thresh = 0.9
                          , imp_taxa_prop_thresh = 0.5
                          , common_taxa_prop_thresh = 0.8
                          , colour_map = NULL
                          ) {

  # unique ------
  bio_df <- bio_df |>
    tibble::as_tibble() |>
    dplyr::distinct(dplyr::across(tidyselect::any_of(c(context
                                                       , clust_col, cov_col
                                                       , taxa_col, str_col
                                                       , ht_col, ind_abu_col
                                                       , "ind"
                                                       )
                                                     )
                                  )
                    )

  sites_col <- paste0(clust_col, "_sites")

  # sites col --------
  clust_col_sites <- bio_df |>
    dplyr::count(dplyr::across(tidyselect::any_of(c(clust_col, context)))) |>
    dplyr::count(dplyr::across(tidyselect::any_of(clust_col))
                 , name = sites_col
                 )

  # Add importance, storey and structure -------
  lifeforms_all <- bio_df |>
    tibble::as_tibble() |>
    dplyr::left_join(lustr) |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(c(taxa_col, clust_col, context, cov_col, ht_col)))
                    , sort
                    , str
                    , storey
                    )

  get_mode <- function(x) {

    result <- if(length(x) > 5) {

      tbl <- table(x)
      mode_val <- names(which.max(tbl))
      if(is.null(mode_val)) mode_val <- "" else mode_val

    } else NA_character_

    return(result)

  }

  # ht and cov -----
  eco_ht_cov <- lifeforms_all |>
    dplyr::group_by(dplyr::across(tidyselect::any_of(c(clust_col, context)))) |>
    dplyr::summarise(cov = sum(cover_adj, na.rm = TRUE)
                     , ht = max(ht, na.rm = TRUE)
                     ) |>
    dplyr::ungroup() |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::summarise(med_cov = median(cov)
                     , med_ht = median(ht)
                     )

  # Important ---------
  # Find the most important taxa PER CLUSTER
  eco_imp_prep <- lifeforms_all |>
    dplyr::filter(!is.na(cover_adj), !is.na(ht)) |>
    dplyr::group_by(dplyr::across(tidyselect::any_of(c(clust_col, taxa_col)))) |>
    dplyr::summarise(med_cov = median(cover_adj)
                     , med_ht = weighted.mean(ht, cover_adj, na.rm = TRUE)
                     , freq = dplyr::n()
                     , str = get_mode(str)
                     ) |>
    dplyr::ungroup() |>
    dplyr::left_join(clust_col_sites) |>
    dplyr::filter(!is.na(str)) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::mutate(prop = freq / !!rlang::ensym(sites_col)
                  , freq = envFunc::add_freq_class(prop * 100)
                  ) |>
    dplyr::filter(med_cov > quantile(med_cov, probs = imp_taxa_quantile_thresh) | med_cov == max(med_cov)) |>
    dplyr::filter(prop > imp_taxa_prop_thresh | prop == max(prop)) |>
    dplyr::ungroup() |>
    dplyr::mutate(cov_class = cut(med_cov * 100
                                  , breaks = c(envEcosystems::cut_cov$cov_thresh)
                                  )
                  , ht_class = cut(med_ht
                                   , breaks = c(envEcosystems::cut_ht$ht_thresh)
                                   )
                  ) |>
    dplyr::left_join(sa_vsf |>
                       dplyr::mutate(sf = sa_sf) |>
                       dplyr::select(! matches("sa_"))
                     )

  eco_imp <- eco_imp_prep |>
    dplyr::left_join(indigenous_df |>
                       dplyr::distinct()
                     ) |>
    dplyr::mutate(imp_taxa = dplyr::if_else(ind == "N"
                                              , paste0("&ast;_", taxa, "_")
                                              , paste0("_", taxa, "_")
                                              )
                  , imp_taxa = paste0(imp_taxa, " ", tolower(str))
                  ) |>
    # find sf most
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::mutate(sf = sf[med_ht == max(med_ht, na.rm = TRUE)][[1]]) |>
    dplyr::arrange(!!rlang::ensym(clust_col), desc(med_ht)) |>
    # build imp taxa per freq
    dplyr::group_by(!!rlang::ensym(clust_col), freq, sf) |>
    dplyr::summarise(imp_taxa = stringr::str_flatten_comma(imp_taxa)) |>
    # build imp taxa
    dplyr::group_by(!!rlang::ensym(clust_col), sf) |>
    dplyr::mutate(imp_taxa = paste0(freq, " ", imp_taxa)) |>
    # summarise imp_taxa
    dplyr::group_by(!!rlang::ensym(clust_col), sf) |>
    dplyr::summarise(imp_taxa = envFunc::vec_to_sentence(imp_taxa, end = "and/or")) |>
    dplyr::ungroup()

  # indicator -----
  eco_ind_prep <- ind_val_df |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::slice_min(p_val
                     , n = indicator_max_n
                     ) |>
    dplyr::slice_max(frq
                     , n = indicator_max_n
                     ) |>
    dplyr::filter(p_val <= indicator_p_val) |>
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
    dplyr::left_join(dplyr::distinct(indigenous_df)) %>%
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


  # Common  -----
  eco_common <- bio_df |>
    dplyr::left_join(dplyr::distinct(indigenous_df)) |>
    dplyr::count(!!rlang::ensym(clust_col), taxa, ind, name = "taxa_sites") |>
    dplyr::left_join(clust_col_sites) |>
    dplyr::mutate(prop = taxa_sites / !!rlang::ensym(sites_col)) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    # prevent indicators turning up again in common taxa
    dplyr::anti_join(eco_ind_prep |>
                       dplyr::distinct(dplyr::across(tidyselect::any_of(c(clust_col, taxa_col))))
                     ) |>
    # prevent important taxa turning up again in common taxa
    dplyr::anti_join(eco_imp_prep |>
                       dplyr::distinct(dplyr::across(tidyselect::any_of(c(clust_col, taxa_col))))
                     ) |>
    dplyr::ungroup() |>
    dplyr::filter(prop > common_taxa_prop_thresh) |>
    dplyr::mutate(freq = envFunc::add_freq_class(prop * 100)
                  , use_taxa = dplyr::if_else(ind == "N",paste0("&ast;_",taxa,"_"),paste0("_",taxa,"_"))
                  ) |>
    dplyr::group_by(!!rlang::ensym(clust_col), freq) |>
    dplyr::summarise(text = envFunc::vec_to_sentence(use_taxa, end = "and/or")) |>
    dplyr::ungroup() |>
    dplyr::mutate(text = paste0(freq, " ", text)) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::summarise(common_taxa = envFunc::vec_to_sentence(text, end = "and/or")) |>
    dplyr::ungroup()

  # Cluster colour --------
  if(isTRUE(is.null(colour_map))) {

    colour_map <- tibble::tibble(!!rlang::ensym(clust_col) := unique(bio_df[clust_col][[1]])) %>%
      dplyr::mutate(colour = viridis::viridis(n = nrow(.)))

  }

  # Final description ---------

  id_col <- paste0(clust_col, "_id")

  desc_res <- clust_col_sites |>
    dplyr::left_join(eco_ht_cov) |>
    dplyr::left_join(eco_common) |>
    dplyr::left_join(eco_ind) |>
    dplyr::left_join(eco_imp) |>
    dplyr::mutate(desc_md = paste0(!!rlang::ensym(clust_col)
                                   , ": "
                                   , imp_taxa
                                   , dplyr::if_else(is.na(range_ind_md)
                                                    , ""
                                                    , paste0(". indicated by "
                                                             , range_ind_md
                                                             )
                                                    )
                                   , dplyr::if_else(is.na(common_taxa)
                                                    , ""
                                                    , paste0(". with "
                                                             , common_taxa
                                                             )
                                                    )
                                   )
                  , desc_html = gsub("&ast;", "*", desc_md)
                  , desc = gsub("_", "", desc_html)
                  ) |>
    dplyr::mutate(!!rlang::ensym(id_col) := gsub("\\s|[[:punct:]]","",!!rlang::ensym(clust_col))) |>
    dplyr::left_join(colour_map)

  return(desc_res)

}
