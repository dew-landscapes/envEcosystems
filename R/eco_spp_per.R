

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
                            , taxacol = "taxa"
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
    dplyr::add_count(!!ensym(clustcol), name = "clustersites") %>%
    dplyr::select(!!ensym(sitecol),!!ensym(clustcol),clustersites) %>%
    dplyr::inner_join(taxadf) %>%
    dplyr::inner_join(lustr) %>%
    dplyr::filter(!is.na(str)) %>%
    dplyr::group_by(cluster,taxa,clustersites) %>%
    dplyr::summarise(presences = n()
                    , str = names(which.max(table(str)))
                     , storey = names(which.max(table(storey)))
                     , sumcover = sum(!!ensym(covcol))
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perpres = 100*presences/clustersites
                  , percov = 100*sumcover/clustersites
                  , percovpres = 100*sumcover/presences
                  , str = factor(str, levels = levels(lustr$str))
                  , storey = factor(storey, levels = levels(lustr$storey), ordered = TRUE)
                  ) %>%
    dplyr::select(-sumcover)

}
