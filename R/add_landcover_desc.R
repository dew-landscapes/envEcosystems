

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
#' @param clust_keep_cols Character. Name of any columns in `add_eco` that
#' should be passed through to the output. These should not lead to any further
#' combinations (rows) than `clust_col` alone does.
#' @param add_name Character name of any prefix to use in descriptions for extra
#' ecosystems. e.g. "Landcover" to make, say, "cropping" into "Landcover:
#' cropping".
#' @param colour_map Dataframe mapping any column in `add_eco` to colour values
#' (in a column called `colour`).
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
                               , clust_keep_cols = c("landcover", "veg")
                               , add_name = "Landcover"
                               , colour_map = NULL
                               ) {

  eco_add <- add_eco |>
    dplyr::count(dplyr::across(tidyselect::any_of(c(clust_col, clust_keep_cols)))
                 , name = paste0(clust_col, "_bins")
                 ) |>
    dplyr::mutate(desc = paste0(add_name,": ", gsub("_"," ",!!rlang::ensym(add_clust_col))))

  missing_names <- setdiff(names(eco_desc)[sapply(eco_desc
                                                  , function(x) is.character(x)|is.factor(x))
                                           ]
                           , names(eco_add)
                           ) |>
    grep("colour", x = _, value = TRUE, invert = TRUE)

  eco_add <- eco_add %>%
    dplyr::bind_cols(purrr::map(missing_names[!grepl("desc|md",missing_names)]
                                , function(x) eco_add$x = as.character(eco_add[clust_col][[1]])
                                ) %>%
                       stats::setNames(missing_names[!grepl("desc|md",missing_names)]) %>%
                       tibble::as_tibble()
                     ) %>%
    dplyr::bind_cols(purrr::map(missing_names[grepl("desc|md",missing_names)]
                                , function(x) eco_add$x = eco_add$desc
                                ) %>%
                       stats::setNames(missing_names[grepl("desc|md",missing_names)]) %>%
                       tibble::as_tibble()
                     )

  if(isTRUE(is.null(colour_map))) {

    colour_map <- tibble::tibble(!!rlang::ensym(clust_col) := unique(eco_add[clust_col][[1]])) %>%
      dplyr::mutate(colour = paste0("grey", ceiling(100 / (2 * dplyr::row_number()))))

  }

  res <- eco_desc %>%
    dplyr::bind_rows(eco_add |>
                       dplyr::left_join(colour_map)
                     ) |>
    dplyr::select(names(eco_desc)) %>%
    dplyr::mutate(dplyr::across(contains("_id")
                                , \(x) gsub("\\s|[[:punct:]]", "", x)
                                )
                  ) |>
    dplyr::mutate(!!rlang::ensym(clust_col) := forcats::fct_relevel(!!rlang::ensym(clust_col)
                                                                    , levels(eco_desc[[clust_col]])
                                                                    )
                  )

  return(res)

}

