

#
#' Create a dataframe of taxa percent cover for each cluster
#'
#' Optionally including structural information associated with each taxa.
#'
#' @param clustdf Dataframe with column indicating cluster membership.
#' @param clustcol Name of column in clustdf with cluster membership.
#' @param taxadf Dataframe with columns clustcol and taxacol.
#' @param sitecol Name of column in taxadf with 'sites'.
#' @param taxacol Name of column in taxadf with taxa.
#' @param lustr Dataframe of structural information. Needs columns 'str' and
#' 'storey': structure and storey, respectively.
#'
#' @return Dataframe with columns clustcol, taxacol, str, storey,
#' @export
#'
#' @examples
  spp_per <- function(clustdf
                            ,clutcol = "cluster"
                            , taxadf
                            , sitecol = "cell"
                            , taxacol = "Taxa"
                            , covcol = "cover"
                            , lustr = NULL
                            ) {

  if(isTRUE(is.null(lustr))) {

    lustr <- tibble::tibble(join = 1
                            , str = factor("unknown")
                            , storey = factor("unknown")
                            )

    taxadf <- taxadf %>% dplyr::mutate(join = 1)

  }

  clustdf %>%
    dplyr::add_count(!!ensym(clutcol), name = "clusterSites") %>%
    dplyr::select(!!ensym(sitecol),!!ensym(clustcol),clusterSites) %>%
    dplyr::inner_join(taxadf) %>%
    dplyr::inner_join(lustr) %>%
    dplyr::filter(!is.na(str)) %>%
    dplyr::group_by(cluster,Taxa,clusterSites) %>%
    dplyr::summarise(Presences = n()
                    , str = names(which.max(table(str)))
                     , storey = names(which.max(table(storey)))
                     , sumCover = sum(!!ensym(covcol))
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perPres = 100*Presences/clusterSites
                  , perCov = 100*sumCover/clusterSites
                  , perCovPres = 100*sumCover/Presences
                  , str = factor(str, levels = levels(lustr$str))
                  , storey = factor(storey, levels = levels(lustr$storey), ordered = TRUE)
                  ) %>%
    dplyr::select(-sumCover)

}
