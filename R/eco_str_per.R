#
#' Create a dataframe of taxa percent cover for each cluster
#'
#' Optionally including structural information associated with each taxa.
#'
#' @param clustdf Dataframe with column indicating cluster membership.
#' @param clustcol Name of column in clustdf with cluster membership.
#' @param taxadf Dataframe with columns clustcol and taxacol.
#' @param sitecol Name of column in taxadf with 'sites'.
#' @param lustr Dataframe of structural information. Needs columns 'lifeform',
#' 'str' and 'storey': structure and storey, respectively.
#'
#' @return Dataframe with structural information per cluster
#' @export

# Generate the structural percent cover for each cluster
str_per <- function(clustdf
                    , clustcol = "cluster"
                    , taxadf
                    , sitecol = "cell"
                    , taxacol = "Taxa"
                    , covcol = "cover"
                    , lustr
                    ) {

  clustdf %>%
    dplyr::select(!!ensym(sitecol),!!ensym(clustcol)) %>%
    dplyr::add_count(cluster, name = "clustersites") %>%
    dplyr::inner_join(taxadf %>%
                        dplyr::filter(!is.na(lifeform))
                      ) %>%
    # cover per lifeform*site (otherwise presences is per taxa, not per str)
    dplyr::group_by(!!ensym(clustcol),!!ensym(sitecol),lifeform,clustersites) %>%
    dplyr::summarise(cov = sum(!!ensym(covcol))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(lustr) %>%
    dplyr::group_by(!!ensym(clustcol),lifeform,ht,str,storey,clustersites) %>%
    # cover per lifeform * ecosystem
    dplyr::summarise(presences = n()
                     , str = names(which.max(table(str)))
                     , storey = names(which.max(table(storey)))
                     , sumcover = sum(cov)
                     , ht = mean(ht)
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perpres = 100*presences/clustersites
                  , percov = 100*sumcover/clustersites
                  , percovpres = 100*sumcover/presences
                  , str = factor(str, levels = levels(lustr$str))
                  , storey = factor(storey, levels = levels(lustr$storey), ordered = TRUE)
                  ) %>%
    dplyr::select(-sumcover) %>%
    dplyr::arrange(!!ensym(clustcol),desc(ht)) %>%
    dplyr::mutate(storey = factor(storey,levels = levels(lustr$storey), ordered = TRUE))

}
