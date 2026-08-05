
#' Create a dataframe of structural information for each cluster
#'
#' @param bio_clust_df Long format dataframe with: `context`(s) defining bins;
#' cluster membership in `clust_col`; and taxa information (`cov_col`, `ht_col`,
#' `str_col`).
#' @param context Character. Name of column(s) in `bio_clust_df` defining the
#' context.
#' @param clust_col Character. Name of column in `bio_clust_df` containing
#' cluster membership.
#' @param cov_col Character. Name of column in `bio_clust_df` containing
#' (numeric, 0 to 1) cover values.
#' @param ht_col Character. Name of column in `bio_clust_df` containing height
#' information.
#' @param str_col Character. Name of column in `bio_clust_df` and `lustr`
#' containing lifeform information.
#' @param lustr Dataframe of structural information. Needs column 'str'
#' (structure) plus `str_col`
#'
#' @return Tibble with one row per unique value in `clust_col`
#' @export

# Generate the structural percent cover for each cluster
make_eco_str_per <- function(bio_clust_df
                             , context
                             , clust_col = "cluster"
                             , cov_col = "cover_adj"
                             , ht_col = "use_height"
                             , str_col = "lifeform"
                             , lustr = envEcosystems::lulifeform
                             ) {

  options(scipen = 9999)

  bins_col <- paste0(clust_col, "_bins")

  # bin col --------
  clust_col_bins <- bio_clust_df |>
    dplyr::count(dplyr::across(tidyselect::any_of(c(clust_col, context)))) |>
    dplyr::count(dplyr::across(tidyselect::any_of(clust_col))
                 , name = bins_col
                 )


  bio_clust_df |>
    tibble::as_tibble() |>
    dplyr::filter(!!rlang::ensym(cov_col) != 0) |>
    dplyr::left_join(clust_col_bins) |>
    dplyr::left_join(lustr) |>
    dplyr::select(tidyselect::all_of(c(context, clust_col, bins_col, cov_col, ht_col, str_col)), str) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(context, clust_col, bins_col))), str) |>
    dplyr::summarise(taxa = dplyr::n()
                     , !!rlang::ensym(cov_col) := sum(!!rlang::ensym(cov_col), na.rm = TRUE)
                     , !!rlang::ensym(ht_col) := mean(!!rlang::ensym(ht_col), na.rm = TRUE)
                     , lifeform = envFunc::get_mode(!!rlang::ensym(str_col))
                     ) |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(clust_col, bins_col))), str) |>
    dplyr::summarise(presences = dplyr::n()
                     , sr = mean(taxa)
                     , sum_cover = sum(!!rlang::ensym(cov_col))
                     , ht = mean(!!rlang::ensym(ht_col))
                     , lifeform = envFunc::get_mode(!!rlang::ensym(str_col))
                     ) |>
    dplyr::ungroup() |>
    dplyr::mutate(per_pres = 100 * presences / !!rlang::ensym(bins_col)
                  , per_cov = 100 * sum_cover / !!rlang::ensym(bins_col)
                  , per_cov_pres = 100 * sum_cover / presences
                  , str = factor(str, levels = levels(lustr$str))
                  ) |>
    dplyr::select(-sum_cover)

}
