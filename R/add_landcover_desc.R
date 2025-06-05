

#' Add 'extra' ecosystem descriptions
#'
#' These are usually 'landcover' descriptions.
#'
#' @param eco_desc Dataframe of existing (floristic) ecosystem descriptions.
#' @param add_eco Dataframe of ecosystems to add.
#' @param clust_col Character name of column with cluster membership in
#' `eco_desc`.
#' @param add_clust_col Character name of column with cluster membership in
#' `add_eco`.
#' @param add_name Character name of any prefix to use in descriptions for extra
#' ecosystems. e.g. "Landcover" to make, say, "cropping" into "Landcover:
#' cropping".
#' @param colour_map Dataframe mapping `add_clust_col` to colour values to use
#' in mapping.
#' @param add_colour_col Character name of column in `colour_map` that has the
#' colour values.
#'
#' @return Dataframe with the same columns as `eco_desc` but with added rows for
#' each row of `add_eco`
#' @export
#'
#' @examples
add_landcover_desc <- function(eco_desc
                               , add_eco
                               , clust_col = "cluster"
                               , add_clust_col = "cluster"
                               , add_name = "Landcover"
                               , colour_map = NULL
                               , add_colour_col = "lc_col"
                               ) {

  eco_add <- add_eco %>%
    dplyr::count(!!rlang::ensym(add_clust_col)
                 , name = "sites"
                 ) %>%
    dplyr::mutate(ecotype = factor(add_name)
                  , ecotype_id = gsub(" |[[:punct:]]","",ecotype)
                  , desc = paste0(add_name,": ", gsub("_"," ",!!rlang::ensym(add_clust_col)))
                  ) %>%
    dplyr::rename(!!rlang::ensym(clust_col) := !!rlang::ensym(add_clust_col))

  missing_names <- setdiff(names(eco_desc)[sapply(eco_desc
                                                  , function(x) is.character(x)|is.factor(x))
                                           ]
                           , names(eco_add)
                           )

  eco_add <- eco_add %>%
    dplyr::bind_cols(purrr::map(missing_names[!grepl("desc|md",missing_names)]
                                , function(x) eco_add$x = as.character(eco_add[clust_col][[1]])
                                ) %>%
                       stats::setNames(missing_names[!grepl("desc|md",missing_names)]) %>%
                       as_tibble()
                     ) %>%
    dplyr::bind_cols(purrr::map(missing_names[grepl("desc|md",missing_names)]
                                , function(x) eco_add$x = eco_add$desc
                                ) %>%
                       stats::setNames(missing_names[grepl("desc|md",missing_names)]) %>%
                       as_tibble()
                     ) %>%
    dplyr::select(-colour)

  if(isTRUE(is.null(colour_map))) {

    colour_map <- tibble(!!rlang::ensym(clust_col) := unique(eco_add[clust_col][[1]])) %>%
      dplyr::mutate(colour = paste0("grey",ceiling(100/(2*row_number()))))

  } else {

    colour_map <- colour_map %>%
      dplyr::mutate(!!rlang::ensym(clust_col) := !!rlang::ensym(add_clust_col)) %>%
      dplyr::mutate(!!rlang::ensym(clust_col) := forcats::fct_inorder(!!rlang::ensym(clust_col))
                    , colour = !!rlang::ensym(add_colour_col)
                    )

  }

  res <- eco_desc %>%
    dplyr::mutate(cluster = forcats::fct_expand(cluster, levels(eco_add[clust_col][[1]]))) %>%
    dplyr::bind_rows(eco_add %>%
                       dplyr::left_join(colour_map)
                     ) %>%
    dplyr::select(names(eco_desc)) %>%
    dplyr::mutate(dplyr::across(contains("_id"), ~gsub(" |[[:punct:]]","",.x)))

}

