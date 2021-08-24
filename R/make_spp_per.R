

#
#' Create a dataframe of taxa percent cover for each cluster
#'
#' Optionally including structural information associated with each taxa.
#'
#' @param clust_df Dataframe with column indicating cluster membership.
#' @param clust_col Name of column in clustdf with cluster membership.
#' @param taxa_df Dataframe with columns clustcol and taxacol.
#' @param site_col Name of column with 'sites'.
#' @param taxa_col Name of column with taxa.
#' @param cov_col Name of column with (numeric) cover values.
#' @param lustr Dataframe of structural information. Needs columns 'str' and
#' 'storey': structure and storey, respectively.
#'
#' @return Dataframe with columns clustcol, taxacol, str, storey,
#' @export
#'
#' @examples
  make_spp_per <- function(clust_df
                      , clust_col = "cluster"
                      , taxa_df
                      , site_col = "cell"
                      , taxa_col = "taxa"
                      , cov_col = "cover"
                      , lustr = NULL
                      ) {

    if(isTRUE(is.null(lustr))) {

      lustr <- tibble::tibble(join = 1
                              , str = factor("unknown")
                              , storey = factor("unknown")
                              )

      taxa_df <- taxa_df %>% dplyr::mutate(join = 1)

    }

    clust_df %>%
      dplyr::add_count(!!ensym(clust_col), name = "cluster_sites") %>%
      dplyr::select(!!ensym(site_col)
                    , !!ensym(clust_col)
                    , cluster_sites
                    ) %>%
      dplyr::inner_join(taxa_df) %>%
      dplyr::inner_join(lustr) %>%
      dplyr::filter(!is.na(str)) %>%
      dplyr::group_by(!!ensym(clust_col),!!ensym(taxa_col),cluster_sites) %>%
      dplyr::summarise(presences = n()
                      , str = names(which.max(table(str)))
                       , storey = names(which.max(table(storey)))
                       , sum_cover = sum(!!ensym(cov_col))
                       ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(per_pres = 100*presences/cluster_sites
                    , per_cov = 100*sum_cover/cluster_sites
                    , per_cov_pres = 100*sum_cover/presences
                    , str = factor(str, levels = levels(lustr$str))
                    , storey = factor(storey, levels = levels(lustr$storey), ordered = TRUE)
                    ) %>%
      dplyr::select(-sum_cover)

}
