
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
#' Common taxa are determined as any taxa occurring at more than
#' `common_prop_thresh` bins within a cluster.
#'
#' Obvious and indicator taxa are prevented from also being common taxa.
#'
#' The structural layer chosen to represent structure for the cluster has the
#' highest value of `per_pres` by `per_cov_pres` by `ht`.
#'
#' @param bio_clust_df Dataframe containing all `context`, all the `_col`s and taxa
#' data in long format.
#' @param assoc_df Dataframe. Usually a result from
#' `envCluster::make_assoc_df()` (a wrapper around `indicspecies::strassoc()`).
#' @param context Character. Name(s) of column(s) in `bio_clust_df` that define
#' bins.
#' @param clust_col Character. Name of column in `bio_clust_df` containing
#' cluster membership.
#' @param taxa_col Character. Name of column in `bio_clust_df` and/or
#' `indigenous_df` containing the taxa names.
#' @param cov_col Character. Name of column in `bio_clust_df` with numeric
#' abundance data (usually 'cover' for plants).
#' @param ht_col Character. Name of column in `bio_clust_df` with numeric height
#' data.
#' @param str_col Character name of column in `bio_clust_df` containing lifeform
#' (or structural) information.
#' @param indigenous_df dataframe containing indigenous status for each unique
#' `taxa_col` in `bio_clust_df`
#' @param obvious_max_n,rg_max_n,common_max_n How many taxa to include in
#' descriptions for obvious, rg and common taxa.
#' @param rg_thresh Numeric 0 to 1. The value to use to accept a taxa as
#' associated with an ecosystem (from `func = "r.g"` in
#' `indicspecies::strassoc()`)
#' @param common_prop_thresh Numeric. Threshold (proportion) for taxa to include in
#' description. Taxa that occur in more than `common_prop_thresh` proportion of
#' bins in the cluster will be included in the description.
#' @param colour_map Dataframe mapping any column in result to colour values
#' (in a column called `colour`).
#' @param add_structure Logical. Add structural information (such as 'woodland'
#' or 'shrubland' etc)
#'
#' @return Tibble of ecosystems descriptions per unique value of `clust_col` in
#' `bio_clust_df`
#' @export
#'
#' @examples
make_eco_desc <- function(bio_clust_df
                          , assoc_df = NULL
                          , context
                          , clust_col = "cluster"
                          , taxa_col = "taxa"
                          , cov_col = "cover_adj"
                          , ht_col = "use_height"
                          , str_col = "lifeform"
                          , indigenous_df = NULL
                          , obvious_max_n = 3
                          , rg_thresh = 0.5
                          , rg_max_n = 5
                          , common_prop_thresh = 0.8
                          , common_max_n
                          , colour_map = NULL
                          , add_structure = TRUE
                          ) {

  options(scipen = 999)

  lustr = envEcosystems::lulifeform
  sa_vsf <- envEcosystems::sa_vsf
  sa_sf <- envEcosystems::sa_sf
  cut_cov <- envEcosystems::cut_cov
  cut_ht <- envEcosystems::cut_ht

  # bin col --------
  bins_col <- paste0(clust_col, "_bins")

  clust_col_bins <- bio_clust_df |>
    dplyr::count(dplyr::across(tidyselect::any_of(c(clust_col, context)))) |>
    dplyr::count(dplyr::across(tidyselect::any_of(clust_col))
                 , name = bins_col
                 )

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
                     ) |>
    dplyr::filter(per_cov > 0)  # this line feels like a hack. Not sure why it is still needed.
                                # make_eco_taxa_per already has dplyr::filter(!!rlang::ensym(cov_col) != 0)

  ## obvious --------
  eco_obv_prep <- eco_taxa_per |>
    dplyr::mutate(ht_rescale = scales::rescale(ht, to = c(0, 1))
                  , per_pres_rescale = scales::rescale(per_pres, to = c(0, 1))
                  , per_cov_rescale = scales::rescale(per_cov, to = c(0, 1))
                  , .by = tidyselect::any_of(clust_col)
                  ) |>
    dplyr::mutate(val = ht_rescale * per_pres_rescale * per_cov_rescale) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::slice_max(order_by = val, n = obvious_max_n) |>
    dplyr::ungroup()

  eco_obv <- eco_obv_prep |>
    dplyr::mutate(use_taxa = dplyr::if_else(ind == "N",paste0("&ast;_",taxa,"_"),paste0("_",taxa,"_"))
                  , use_taxa = dplyr::if_else(is.na(str), use_taxa, paste0(use_taxa, " ", tolower(str)))
                  , use_taxa = paste0(use_taxa, " "
                                      , " ("
                                      , round(per_pres, 0)
                                      , "% bins;\u00A0"
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

  ## assoc -----
  eco_assoc_prep <- assoc_df |>
    dplyr::filter(rg > rg_thresh) |>
    dplyr::slice_max(rg
                     , n = rg_max_n
                     , by = tidyselect::any_of(clust_col)
                     ) |>
    dplyr::arrange(!!rlang::ensym(clust_col)
                   , rg
                   , desc(b)
                   ) |>
    dplyr::mutate(ind_num = dplyr::row_number()
                  , best = ind_num == min(ind_num)
                  , .by = tidyselect::any_of(clust_col)
                  )

  eco_assoc <- eco_assoc_prep |>
    dplyr::arrange(!!rlang::ensym(clust_col)) |>
    dplyr::left_join(eco_taxa_per) |>
    dplyr::mutate(use_taxa = dplyr::if_else(ind == "N"
                                            , paste0("&ast;_", taxa, "_")
                                            , paste0("_", taxa, "_")
                                            )
                  , use_taxa = dplyr::if_else(is.na(str), use_taxa, paste0(use_taxa, " ", tolower(str)))
                  , use_taxa = paste0(use_taxa
                                      , " ("
                                      , round(b * 100, 0)
                                      , "% bins;\u00A0"
                                      , signif(per_cov_pres, 1)
                                      , "% cover; rg = "
                                      , signif(rg, 1)
                                      , ")"
                                      )
                  ) |>
    dplyr::arrange(str) |>
    dplyr::summarise(range_assoc = stringr::str_flatten_comma(taxa)
                     , best_assoc = taxa[best]
                     , range_assoc_md = envFunc::vec_to_sentence(use_taxa, end = "and/or")
                     , best_assoc_md = use_taxa[best]
                     , .by = tidyselect::any_of(clust_col)
                     )


  ## common  -----
  eco_common <- eco_taxa_per |>
    # prevent obvious taxa turning up again in common taxa
    dplyr::anti_join(eco_obv_prep |>
                       dplyr::distinct(dplyr::across(tidyselect::any_of(c(clust_col, taxa_col))))
                     ) |>
    # prevent indicator taxa turning up again in common taxa
    dplyr::anti_join(eco_assoc_prep |>
                       dplyr::distinct(dplyr::across(tidyselect::any_of(c(clust_col, taxa_col))))
                     ) |>
    dplyr::filter(per_pres > common_prop_thresh * 100) |>
    dplyr::mutate(use_taxa = dplyr::if_else(ind == "N",paste0("&ast;_",taxa,"_"),paste0("_",taxa,"_"))
                  , use_taxa = dplyr::if_else(is.na(str), use_taxa, paste0(use_taxa, " ", tolower(str)))
                  , use_taxa = paste0(use_taxa
                                       , " ("
                                       , round(per_pres, 0)
                                       , "% bins;\u00A0"
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
  ## sf --------

  if(add_structure) {

    eco_sf <- bio_clust_df |>
      tibble::as_tibble() |>
      dplyr::left_join(clust_col_bins) |>
      dplyr::left_join(lustr) |>
      dplyr::select(tidyselect::all_of(c(context, clust_col, bins_col, cov_col, ht_col, str_col))) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(context, clust_col, bins_col, str_col)))) |>
      dplyr::summarise(cov = mean(cover_adj, na.rm = TRUE)
                       , ht = mean(!!rlang::ensym(ht_col), na.rm = TRUE)
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
                       , sf = sample(sa_sf[n == max(n)], 1)
                       ) |>
      dplyr::ungroup()


    ## sa vsf ----------

    eco_savsf <- bio_clust_df |>
      tibble::as_tibble() |>
      dplyr::left_join(clust_col_bins) |>
      dplyr::left_join(lustr) |>
      dplyr::select(tidyselect::all_of(c(context, clust_col, bins_col, cov_col, ht_col, str_col))) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(context, clust_col, bins_col, str_col)))) |>
      dplyr::summarise(cov = mean(cover_adj, na.rm = TRUE)
                       , ht = mean(!!rlang::ensym(ht_col), na.rm = TRUE)
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
      dplyr::filter(!is.na(sa_vsf)) |>
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
                       , sa_vsf = sample(sa_vsf[n == max(n)], 1)
                       ) |>
      dplyr::ungroup()

  }

  # cover --------
  eco_cov <- bio_clust_df |>
    dplyr::group_by(dplyr::across(tidyselect::any_of(c(clust_col, context)))) |>
    dplyr::summarise(cov = sum(!!rlang::ensym(cov_col), na.rm = TRUE)) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::summarise(tot_cov = median(cov, na.rm = TRUE)) |>
    dplyr::ungroup()

  # ht --------
  eco_ht <- bio_clust_df |>
    dplyr::group_by(dplyr::across(tidyselect::any_of(c(clust_col, context)))) |>
    dplyr::summarise(ht = max(!!rlang::ensym(ht_col), na.rm = TRUE)) |>
    dplyr::group_by(!!rlang::ensym(clust_col)) |>
    dplyr::summarise(ht = median(ht, na.rm = TRUE)) |>
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
    dplyr::left_join(eco_assoc)

  if(add_structure) {

    desc_res <- desc_res |>
      dplyr::left_join(eco_sf) |>
      dplyr::left_join(eco_savsf)

    }

  desc_res <- desc_res |>
    dplyr::left_join(eco_cov) |>
    dplyr::left_join(eco_ht) |>
    dplyr::mutate(desc_md = paste0(envFunc::first_up(as.character(!!rlang::ensym(clust_col)))
                                   , dplyr::if_else(is.na(obv_taxa)
                                                    , ""
                                                    , paste0(". "
                                                             , obv_taxa
                                                             )
                                                    )
                                   , dplyr::if_else(is.na(range_assoc_md)
                                                    , ""
                                                    , paste0(". Indicated by "
                                                             , range_assoc_md
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
                  )

  if(add_structure) {

    desc_res <- desc_res |>
      dplyr::mutate(desc_md = paste0(desc_md
                                     , ". "
                                     , envFunc::first_up(sf_text)
                                     , ": "
                                     , sa_vsf_text
                                     )
                    ) |>
      dplyr::left_join(colour_map)

  }

  desc_res <- desc_res |>
    dplyr::mutate(desc_md = stringr::str_squish(desc_md)
                  , desc_html = gsub("&ast;", "*", desc_md)
                  , desc = gsub("_", "", desc_html)
                  , dplyr::across(dplyr::where(is.character)
                                  , \(x) replace_text_after_first(x, " cover")
                                  )
                  , dplyr::across(dplyr::where(is.character)
                                  , \(x) replace_text_after_first(x, " bins")
                                  )
                  , dplyr::across(dplyr::where(is.character)
                                  , \(x) replace_text_after_first(x, " p =")
                                  )
                  ) |>
    dplyr::mutate(!!rlang::ensym(id_col) := gsub("\\s|[[:punct:]]","",!!rlang::ensym(clust_col)))

  return(desc_res)

}
