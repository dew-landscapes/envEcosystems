
#' Create a dataframe of taxa information for each cluster
#'
#' @param bio_clust_df Long format dataframe with: `context`(s) defining bins;
#' cluster membership in `clust_col`; and taxa information (`taxa_col`,
#' `cov_col`, `ht_col`, `str_col`).
#' @param context Character. Name of column(s) in `bio_clust_df`
#' defining the context.
#' @param clust_col Character. Name of column in `bio_clust_df` containing cluster
#' membership.
#' @param taxa_col Character. Name of column in `bio_clust_df` containing taxa
#' names.
#' @param cov_col Character. Name of column in `bio_clust_df` containing
#' (numeric, 0 to 1) cover values.
#' @param ht_col Character. Name of column in `bio_clust_df` containing height
#' information.
#' @param str_col Character. Name of column in `bio_clust_df` and `lustr`
#' containing lifeform information.
#' @param lustr Dataframe of structural information. Needs columns 'str' and
#' 'storey': structure and storey, respectively plus `str_col`
#'
#' @return Tibble with one row per unique value in `clust_col`
#' @export
#'
#' @examples
make_eco_taxa_per <- function(bio_clust_df
                              , context
                              , clust_col = "cluster"
                              , taxa_col = "taxa"
                              , cov_col = "cover_adj"
                              , ht_col = "use_height"
                              , str_col = "lifeform"
                              , lustr = envEcosystems::lulifeform
                              ) {

  bins_col <- paste0(clust_col, "_bins")

  # sites col --------
  clust_col_sites <- bio_clust_df |>
    dplyr::count(dplyr::across(tidyselect::any_of(c(clust_col, context)))) |>
    dplyr::count(dplyr::across(tidyselect::any_of(clust_col))
                 , name = bins_col
                 )

  bio_clust_df |>
    tibble::as_tibble() |>
    dplyr::left_join(lustr) |>
    dplyr::left_join(clust_col_sites) |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(c(context, clust_col, bins_col, taxa_col, cov_col, ht_col, str_col)))
                    , storey
                    , str
                    ) |>
    dplyr::group_by(!!rlang::ensym(clust_col)
                    , !!rlang::ensym(taxa_col)
                    , !!rlang::ensym(bins_col)
                    ) |>
    dplyr::summarise(presences = dplyr::n()
                     , lifeform = envFunc::get_mode(lifeform)
                     , str = envFunc::get_mode(str)
                     , storey = envFunc::get_mode(storey)
                     , sum_cover = sum(!!rlang::ensym(cov_col))
                     , ht = mean(!!rlang::ensym(ht_col))
                     ) |>
    dplyr::ungroup() |>
    dplyr::mutate(per_pres = 100 * presences / !!rlang::ensym(bins_col)
                  , per_cov = 100 * sum_cover / !!rlang::ensym(bins_col)
                  , per_cov_pres = 100 * sum_cover / presences
                  , str = factor(str, levels = levels(lustr$str))
                  , storey = factor(storey, levels = levels(lustr$storey), ordered = TRUE)
                  ) |>
    dplyr::select(-sum_cover)

}
