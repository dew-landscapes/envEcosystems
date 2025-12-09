
#' Make a (biotic) description for an ecosystem
#'
#' @description
#' The description includes: indicator taxa; other taxa present at a 'high'
#' proportion of bins; and structural layers.
#'
#' @details
#'
#' Indicator taxa requires a result from `envCluster::make_ind_val_df()` (a
#' wrapper around `labdsv::inval()`). Choosing indicators uses  `indicator_p_val` and
#' `indicator_max_n`.
#'
#' Common taxa are determined as any taxa occurring at more than
#' `common_taxa_prop_thresh` bins within a cluster.
#'
#' Indicator taxa are prevented from also being common taxa.
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
                          , indicator_p_val = 0.05
                          , indicator_max_n = 3
                          , common_taxa_prop_thresh = 0.8
                          , colour_map = NULL
                          ) {

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
                                      , "% cover)"
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
    # prevent indicators turning up again in common taxa
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

  # Cluster colour --------
  if(isTRUE(is.null(colour_map))) {

    colour_map <- tibble::tibble(!!rlang::ensym(clust_col) := unique(bio_clust_df[clust_col][[1]])) %>%
      dplyr::mutate(colour = viridis::viridis(n = nrow(.)))

  }

  # Structure ----------
  ## eco str per --------

  eco_str_per <- make_eco_str_per(bio_clust_df = bio_clust_df
                                  , context = context
                                  , clust_col = clust_col
                                  , cov_col = cov_col
                                  , ht_col = ht_col
                                  , str_col = str_col
                                  , lustr = lustr
                                  ) |>
    dplyr::mutate(cov_class = cut(per_cov_pres
                                  , breaks = c(envEcosystems::cut_cov$cov_thresh)
                                  )
                  , ht_class = cut(ht
                                   , breaks = c(envEcosystems::cut_ht$ht_thresh)
                                   )
                  ) |>
    dplyr::left_join(envEcosystems::sa_vsf |>
                       dplyr::mutate(sf = sa_sf
                                     , sf = factor(sf, levels = levels(envEcosystems::sa_sf$sf))
                                     )
                     ) |>
    dplyr::filter(!is.na(sf))

  ## eco str prep --------

  eco_str_prep <- eco_str_per |>
    dplyr::group_by(!!rlang::ensym(clust_col), sf) |>
    dplyr::summarise(ht = weighted.mean(ht, presences, na.rm = TRUE)
                     , per_pres = weighted.mean(per_pres, presences, na.rm = TRUE)
                     , per_cov = weighted.mean(per_cov, presences, na.rm = TRUE)
                     , per_cov_pres = weighted.mean(per_cov_pres, presences, na.rm = TRUE)
                     ) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::filter(per_pres > imp_taxa_prop_thresh * 100 | per_pres == max(per_pres, na.rm = TRUE)) |>
    dplyr::filter(ht == max(ht)) |>
    dplyr::ungroup()

  ## eco str --------
  eco_str <- eco_str_prep |>
    dplyr::arrange(desc(sf)) |>
    dplyr::mutate(sf_text = paste0(sf
                                   , " ("
                                   , round(per_pres, 0)
                                   , "% bins)"
                                   )
                  ) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::summarise(sf_text = envFunc::first_up(envFunc::vec_to_sentence(sf_text, end = "and/or"))
                     , sf = sf[ht == max(ht)]
                     ) |>
    dplyr::ungroup()

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
    dplyr::left_join(eco_common) |>
    dplyr::left_join(eco_ind) |>
    dplyr::left_join(eco_str) |>
    dplyr::mutate(desc_md = paste0(envFunc::first_up(as.character(!!rlang::ensym(clust_col)))
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
                  , desc_md = paste0(desc_md
                                     , ". "
                                     , envFunc::first_up(sf_text)
                                     )
                  , desc_md = stringr::str_squish(desc_md)
                  , desc_html = gsub("&ast;", "*", desc_md)
                  , desc = gsub("_", "", desc_html)
                  ) |>
    dplyr::mutate(!!rlang::ensym(id_col) := gsub("\\s|[[:punct:]]","",!!rlang::ensym(clust_col))) |>
    dplyr::left_join(colour_map)

  return(desc_res)

}
