
#' Make a (biotic) description for an ecosystem
#'
#' @description
#' The description includes: 'obvious' taxa; indicator taxa; taxa present
#' at a 'high' proportion of bins; and structural layers.
#'
#' @details
#'
#' Obvious taxa have large values for `per_pres` by `per_cov_pres` by `ht`.
#'
#' Indicator taxa requires a result from `envCluster::make_ind_val_df()` (a
#' wrapper around `labdsv::inval()`). Choosing indicators uses
#' `indicator_p_val` and `indicator_max_n`.
#'
#' Common taxa are determined as any taxa occurring at more than
#' `common_taxa_prop_thresh` bins within a cluster.
#'
#' Obvious and indicator taxa are prevented from also being common taxa.
#'
#' The structural layer chosen to represent structure for the cluster has the
#' highest value of `per_pres` by `per_cov_pres` by `ht`.
#'
#' @param bio_clust_df Dataframe containing all `context`, all the `_col`s and taxa
#' data in long format.
#' @param ind_val_df Result from `envCluster::make_ind_val_df()`.
#' @param context Character. Name(s) of column(s) in `bio_clust_df` that define bins.
#' @param clust_col Character. Name of column in `bio_clust_df` containing cluster
#' membership.
#' @param taxa_col Character. Name of column in `bio_clust_df` and/or `indigenous_df`
#' containing the taxa names.
#' @param cov_col Character. Name of column in `bio_clust_df` with numeric
#' abundance data (usually 'cover' for plants).
#' @param ht_col Character. Name of column in `bio_clust_df` with numeric height data.
#' @param str_col Character name of column in `bio_clust_df` and `lustr`containing
#' lifeform (or structural) information.
#' @param lustr Dataframe containing lifeform (structural) information.
#' @param indigenous_df dataframe containing indigenous status for each unique
#' `taxa_col` in `bio_clust_df`
#' @param indicator_p_val Numeric 0 to 1. The p-value to use to accept a taxa as an
#' indicator for an ecosystem.
#' @param indicator_max_n Maximum number of taxa to list as indicators.
#' @param common_taxa_prop_thresh Numeric. Threshold (proportion) for taxa to include in
#' description. Taxa that occur in more than `common_taxa_prop_thresh` proportion of
#' bins in the cluster will be included in the description.
#' @param colour_map Dataframe mapping any column in result to colour values
#' (in a column called `colour`).
#'
#' @return Tibble of ecosystems descriptions per unique value of `clust_col` in
#' `bio_clust_df`
#' @export
#'
#' @examples
make_eco_desc <- function(bio_clust_df
                          , ind_val_df = NULL
                          , context
                          , clust_col = "cluster"
                          , taxa_col = "taxa"
                          , cov_col = "cover_adj"
                          , ht_col = "ht"
                          , lustr
                          , str_col = "lifeform"
                          , indigenous_df = NULL
                          , obvious_max_n = 3
                          , indicator_p_val = 0.05
                          , indicator_max_n = 3
                          , common_taxa_prop_thresh = 0.8
                          , colour_map = NULL
                          ) {

  sa_vsf <- envEcosystems::sa_vsf
  sa_sf <- envEcosystems::sa_sf
  cut_cov <- envEcosystems::cut_cov
  cut_ht <- envEcosystems::cut_ht

  bins_col <- paste0(clust_col, "_bins")

  # Taxa ------
  ## eco taxa per --------

  eco_taxa_per <- envEcosystems::make_eco_taxa_per(bio_clust_df = bio_clust_df
                                                   , context = context
                                                   , clust_col = clust_col
                                                   , taxa_col = taxa_col
                                                   , cov_col = cov_col
                                                   , ht_col = ht_col
                                                   , str_col = str_col
                                                   , lustr = lustr
                                                   ) |>
    dplyr::left_join(indigenous_df |>
                       dplyr::distinct()
                     )

  ## obvious --------
  eco_obv_prep <- eco_taxa_per |>
    dplyr::mutate(ht_scale = scales::rescale(ht)
                  , val = (per_pres / 100) * (per_cov / 100) * ht_scale
                  ) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::slice_max(order_by = val, n = obvious_max_n) |>
    dplyr::ungroup()

  eco_obv <- eco_obv_prep |>
    dplyr::mutate(use_taxa = dplyr::if_else(ind == "N",paste0("&ast;_",taxa,"_"),paste0("_",taxa,"_"))
                  , use_taxa = paste0(use_taxa, " ", tolower(str)
                                      , " ("
                                      , round(per_pres, 0)
                                      , "% bins; "
                                      , signif(per_cov_pres, 1)
                                      , "% cover)"
                                      )
                  ) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::arrange(str) |>
    dplyr::summarise(text = envFunc::vec_to_sentence(use_taxa, end = "and/or")) |>
    dplyr::ungroup() |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::summarise(obv_taxa = envFunc::vec_to_sentence(text, end = "and/or")) |>
    dplyr::ungroup()

  ## indicator -----
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
    dplyr::arrange(!!rlang::ensym(clust_col)) |>
    dplyr::left_join(eco_taxa_per) |>
    dplyr::mutate(use_taxa = dplyr::if_else(ind == "N"
                                            , paste0("&ast;_", taxa, "_")
                                            , paste0("_", taxa, "_")
                                            )
                  , use_taxa = paste0(use_taxa, " ", tolower(str)
                                      , " ("
                                      , round(frq * 100, 0)
                                      , "% bins; "
                                      , signif(per_cov_pres, 1)
                                      , "% cover; p = "
                                      , signif(p_val, 1)
                                      , ")"
                                      )
                  ) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::arrange(str) |>
    dplyr::summarise(range_ind = stringr::str_flatten_comma(taxa)
                     , best_ind = ifelse(best, taxa, NA) |> na.omit(object = _)
                     , range_ind_md = envFunc::vec_to_sentence(use_taxa, end = "and/or")
                     , best_ind_md = ifelse(best, use_taxa, NA) |> na.omit(object = _)
                     ) |>
    dplyr::ungroup()


  ## common  -----
  eco_common <- eco_taxa_per |>
    # prevent obvious taxa turning up again in common taxa
    dplyr::anti_join(eco_obv_prep |>
                       dplyr::distinct(dplyr::across(tidyselect::any_of(c(clust_col, taxa_col))))
                     ) |>
    # prevent indicator taxa turning up again in common taxa
    dplyr::anti_join(eco_ind_prep |>
                       dplyr::distinct(dplyr::across(tidyselect::any_of(c(clust_col, taxa_col))))
                     ) |>
    dplyr::filter(per_pres > common_taxa_prop_thresh * 100) |>
    dplyr::mutate(use_taxa = dplyr::if_else(ind == "N",paste0("&ast;_",taxa,"_"),paste0("_",taxa,"_"))
                  , use_taxa = paste0(use_taxa, " ", tolower(str)
                                       , " ("
                                       , round(per_pres, 0)
                                       , "% bins; "
                                      , signif(per_cov_pres, 1)
                                      , "% cover)"
                                      )
                  ) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::arrange(str) |>
    dplyr::summarise(text = envFunc::vec_to_sentence(use_taxa, end = "and/or")) |>
    dplyr::ungroup() |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::summarise(common_taxa = envFunc::vec_to_sentence(text, end = "and/or")) |>
    dplyr::ungroup()

  # Structure ----------
  ## eco str per --------

  eco_sf <- bio_clust_df |>
    tibble::as_tibble() |>
    dplyr::left_join(clust_col_bins) |>
    dplyr::left_join(lustr) |>
    dplyr::select(tidyselect::all_of(c(context, clust_col, bins_col, cov_col, ht_col, str_col))) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(context, clust_col, bins_col, str_col)))) |>
    dplyr::summarise(cov = mean(cover_adj, na.rm = TRUE)
                     , ht = mean(ht, na.rm = TRUE)
                     ) |>
    dplyr::left_join(lustr) |>
    dplyr::ungroup() |>
    dplyr::mutate(cov_class = cut(cov
                                  , breaks = c(cut_cov$cov_thresh)
                                  )
                  , ht_class = cut(ht
                                   , breaks = c(cut_ht$ht_thresh)
                                   )
                  ) |>
    dplyr::left_join(sa_vsf |>
                       dplyr::distinct()
                     ) |>
    dplyr::filter(!is.na(sa_sf)) |>
    dplyr::group_by(sa_sf, dplyr::across(tidyselect::all_of(c(context, clust_col, bins_col)))) |>
    dplyr::summarise(cov = sum(cov, na.rm = TRUE)
                     , ht = mean(ht, na.rm = TRUE)
                     ) |>
    dplyr::ungroup() |>
    dplyr::mutate(val = cov * ht) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(context, clust_col, bins_col)))) |>
    dplyr::filter(val == max(val, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::count(!!rlang::ensym(clust_col), !!rlang::ensym(bins_col), sa_sf) |>
    dplyr::mutate(prop_pres = n / !!rlang::ensym(bins_col)) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::filter(prop_pres > 0.3 | prop_pres == max(prop_pres)) |>
    dplyr::mutate(sa_sf_text = paste0(sa_sf, " (", signif(100 * prop_pres, 1), "% bins)")) |>
    dplyr::summarise(sf_text = envFunc::vec_to_sentence(sa_sf_text, end = "or")
                     , sf = sa_sf[n == max(n)]
                     ) |>
    dplyr::ungroup()

  # eco_str_per <- envEcosystems::make_eco_str_per(bio_clust_df = bio_clust_df
  #                                                , context = context
  #                                                , clust_col = clust_col
  #                                                , cov_col = cov_col
  #                                                , ht_col = ht_col
  #                                                , str_col = str_col
  #                                                , lustr = lustr
  #                                                ) |>
  #   dplyr::mutate(cov_class = cut(per_cov_pres
  #                                 , breaks = c(cut_cov$cov_thresh)
  #                                 )
  #                 , ht_class = cut(ht
  #                                  , breaks = c(cut_ht$ht_thresh)
  #                                  )
  #                 ) |>
  #   dplyr::left_join(sa_vsf |>
  #                      dplyr::rename(sf = sa_sf) |>
  #                      dplyr::mutate(sf = factor(sf, levels = levels(sa_sf$sf)))
  #                    ) |>
  #   dplyr::filter(!is.na(str))
  #
  # ## eco str prep --------
  #
  # eco_str_prep <- eco_str_per |>
  #   dplyr::group_by(!!rlang::ensym(clust_col), sf) |>
  #   dplyr::summarise(ht = weighted.mean(ht, presences, na.rm = TRUE)
  #                    , per_pres = weighted.mean(per_pres, presences, na.rm = TRUE)
  #                    , per_cov = weighted.mean(per_cov, presences, na.rm = TRUE)
  #                    , per_cov_pres = weighted.mean(per_cov_pres, presences, na.rm = TRUE)
  #                    ) |>
  #   dplyr::mutate(ht_scale = scales::rescale(ht)
  #                 , val = ht_scale * (per_pres / 100) * (per_cov / 100)
  #                 ) |>
  #   dplyr::group_by(!!rlang::ensym(clust_col)) |>
  #   dplyr::filter(val == max(val, na.rm = TRUE)) |>
  #   dplyr::ungroup()
  #
  # ## eco str --------
  # eco_str <- eco_str_prep |>
  #   dplyr::arrange(desc(sf)) |>
  #   dplyr::mutate(sf_text = paste0(sf
  #                                  , " ("
  #                                  , round(per_pres, 0)
  #                                  , "% bins)"
  #                                  )
  #                 ) |>
  #   dplyr::group_by(!!rlang::ensym(clust_col)) |>
  #   dplyr::summarise(sf_text = envFunc::first_up(envFunc::vec_to_sentence(sf_text, end = "and/or"))
  #                    , sf = sf[ht == max(ht)]
  #                    , ht = ht[ht == max(ht)]
  #                    , per_cov = per_cov[ht == max(ht)]
  #                    ) |>
  #   dplyr::ungroup()


  # sa vsf ----------

  eco_savsf <- bio_clust_df |>
    tibble::as_tibble() |>
    dplyr::left_join(clust_col_bins) |>
    dplyr::left_join(lustr) |>
    dplyr::select(tidyselect::all_of(c(context, clust_col, bins_col, cov_col, ht_col, str_col))) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(context, clust_col, bins_col, str_col)))) |>
    dplyr::summarise(cov = mean(cover_adj, na.rm = TRUE)
                     , ht = mean(ht, na.rm = TRUE)
                     ) |>
    dplyr::left_join(lustr) |>
    dplyr::ungroup() |>
    dplyr::mutate(cov_class = cut(cov
                                  , breaks = c(cut_cov$cov_thresh)
                                  )
                  , ht_class = cut(ht
                                   , breaks = c(cut_ht$ht_thresh)
                                   )
                  ) |>
    dplyr::left_join(sa_vsf |>
                       dplyr::distinct()
                     ) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(context, clust_col)))) |>
    dplyr::mutate(val = cov * ht) |>
    dplyr::filter(val == max(val, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::count(!!rlang::ensym(clust_col), !!rlang::ensym(bins_col), sa_vsf) |>
    dplyr::mutate(prop_pres = n / !!rlang::ensym(bins_col)) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::filter(prop_pres > 0.2 | prop_pres == max(prop_pres)) |>
    dplyr::mutate(sa_vsf_text = paste0(sa_vsf, " (", signif(100 * prop_pres, 1), "% bins)")) |>
    dplyr::summarise(sa_vsf_text = envFunc::vec_to_sentence(sa_vsf_text, end = "or")
                     , sa_vsf = sa_vsf[n == max(n)]
                     ) |>
    dplyr::ungroup()

  # cover --------
  eco_cov <- bio_clust_df |>
    dplyr::group_by(dplyr::across(tidyselect::any_of(c(clust_col, context)))) |>
    dplyr::summarise(cov = sum(!!rlang::ensym(cov_col), na.rm = TRUE)) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::summarise(tot_cov = median(cov, na.rm = TRUE)) |>
    dplyr::ungroup()

  # Cluster colour --------
  if(isTRUE(is.null(colour_map))) {

    colour_map <- tibble::tibble(!!rlang::ensym(clust_col) := unique(bio_clust_df[clust_col][[1]])) %>%
      dplyr::mutate(colour = viridis::viridis(n = nrow(.)))

  }

  # Final description ---------

  id_col <- paste0(clust_col, "_id")

  replace_text_after_first <- function(string, text) {

    string <- string |>
      stringr::str_replace(text, "__blah__")

    string <- string |>
      stringr::str_replace_all(text, "")

    string <- string |>
      stringr::str_replace("__blah__", text)

  }

  desc_res <- eco_taxa_per |>
    dplyr::distinct(dplyr::across(tidyselect::any_of(c(clust_col, bins_col)))) |>
    dplyr::left_join(eco_obv) |>
    dplyr::left_join(eco_common) |>
    dplyr::left_join(eco_ind) |>
    dplyr::left_join(eco_sf) |>
    dplyr::left_join(eco_savsf) |>
    dplyr::left_join(eco_cov) |>
    dplyr::mutate(desc_md = paste0(envFunc::first_up(as.character(!!rlang::ensym(clust_col)))
                                   , dplyr::if_else(is.na(obv_taxa)
                                                    , ""
                                                    , paste0(". "
                                                             , obv_taxa
                                                             )
                                                    )
                                   , dplyr::if_else(is.na(range_ind_md)
                                                    , ""
                                                    , paste0(". Indicated by "
                                                             , range_ind_md
                                                             )
                                                    )
                                   , dplyr::if_else(is.na(common_taxa)
                                                    , ""
                                                    , paste0(". With "
                                                             , common_taxa
                                                             )
                                                    )
                                   )
                  , desc_md = gsub("NA|\\s\\.\\s", "", desc_md)
                  , desc_md = replace_text_after_first(desc_md, " cover")
                  , desc_md = replace_text_after_first(desc_md, " bins")
                  , desc_md = replace_text_after_first(desc_md, " p =")
                  , desc_md = paste0(desc_md
                                     , ". "
                                     , envFunc::first_up(sf_text)
                                     , ": "
                                     , sa_vsf_text
                                     )
                  , desc_md = stringr::str_squish(desc_md)
                  , desc_html = gsub("&ast;", "*", desc_md)
                  , desc = gsub("_", "", desc_html)
                  ) |>
    dplyr::mutate(!!rlang::ensym(id_col) := gsub("\\s|[[:punct:]]","",!!rlang::ensym(clust_col))) |>
    dplyr::left_join(colour_map)

  return(desc_res)

}
