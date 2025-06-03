

#
#' Create a dataframe of taxa percent cover for each cluster
#'
#' Optionally including structural information associated with each taxa.
#'
#' @param clust_df Dataframe with column indicating cluster membership.
#' @param clust_col Name of column in clustdf with cluster membership.
#' @param bio_df Dataframe with taxa records. Linked to clust_df by context.
#' @param context Name of columns defining the context.
#' @param taxa_col Name of column with taxa.
#' @param cov_col Name of column with (numeric) cover values.
#' @param lustr Dataframe of structural information. Needs columns 'str' and
#' 'storey': structure and storey, respectively.
#'
#' @return Dataframe with columns clustcol, taxacol, str, storey,
#' @export
#'
#' @examples
make_eco_taxa_per <- function(clust_df
                              , clust_col = "cluster"
                              , bio_df
                              , context
                              , taxa_col = "taxa"
                              , cov_col = "cover"
                              , lustr = NULL
                              ) {

  if(isTRUE(is.null(lustr))) {

    lustr <- tibble::tibble(join = 1
                            , str = factor("unknown")
                            , storey = factor("unknown")
                            )

    bio_df <- bio_df %>% dplyr::mutate(join = 1)

  }

  clust_df |>
    dplyr::distinct(dplyr::across(tidyselect::any_of(c(context, clust_col)))) |>
    dplyr::add_count(!!rlang::ensym(clust_col)
                     , name = "cluster_sites"
                     ) |>
    dplyr::inner_join(bio_df |>
                        dplyr::distinct(lifeform
                                        , dplyr::across(tidyselect::any_of(c(context, taxa_col, cov_col)))
                                        )
                      ) |>
    dplyr::inner_join(lustr) |>
    dplyr::filter(!is.na(str)) |>
    dplyr::group_by(!!rlang::ensym(clust_col)
                    , !!rlang::ensym(taxa_col)
                    , cluster_sites
                    ) %>%
    dplyr::summarise(presences = dplyr::n()
                     , str = names(which.max(table(str)))
                     , storey = names(which.max(table(storey)))
                     , sum_cover = sum(!!rlang::ensym(cov_col))
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(per_pres = 100 * presences / cluster_sites
                  , per_cov = 100 * sum_cover / cluster_sites
                  , per_cov_pres = 100 * sum_cover / presences
                  , str = factor(str, levels = levels(lustr$str))
                  , storey = factor(storey, levels = levels(lustr$storey), ordered = TRUE)
                  ) %>%
    dplyr::select(-sum_cover)

}
