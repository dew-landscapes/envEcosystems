


#' Make a dataframe summarising environmental variables per cluster.
#'
#' Optionally include the importance object from random forest.
#'
#' @param clust_df Dataframe with cluster membership and environmental data.
#' @param imp_df Importance from random forest.
#' @param clust_col Character. Name of column in `clust_df` with cluster
#' membership.
#' @param env_col Character. Name of column in `imp_df` with
#' @param env_cols Character. Name of columns in `clust_df` with environmental
#' data.
#'
#' @return Dataframe with `clust_col`, `env_col` and summary columns including
#' mean, median, standard deviation and (optionally) importance from random
#' forest.
#' @export
#'
#' @examples
make_eco_env <- function(clust_df
                         , imp_df = NULL
                         , clust_col = "cluster"
                         , env_col = "env"
                         , env_cols
                         ) {

  if(!is.data.frame(imp_df)) imp_df <- as_tibble(imp_df
                                                 , rownames = env_col
                                                 )

  factor_levels <- levels(clust_df[clust_col][[1]])

  if(isTRUE(!is.null(imp_df))) {

    if(!is.data.frame(imp_df)) imp_df <-
        tibble::rownames_to_column(data.frame(imp_df
                                              , check.names = FALSE
                                              )
                                   , var = env_col
                                   )

    imp <- imp_df %>%
      dplyr::select(!!ensym(env_col),all_of(factor_levels)) %>%
      tidyr::pivot_longer(all_of(factor_levels)
                          , names_to = clust_col
                          , values_to = "importance"
                          ) %>%
      dplyr::select(!!ensym(clust_col),!!ensym(env_col),everything()) %>%
      dplyr::mutate(!!ensym(clust_col) := factor(!!ensym(clust_col)
                                                 , levels = factor_levels
                                                 )
                    ) %>%
      dplyr::group_by(!!ensym(clust_col)) %>%
      dplyr::mutate(rank_imp = rank(importance)) %>%
      dplyr::ungroup()

  }

  clust_df %>%
    tidyr::pivot_longer(all_of(env_cols)
                        , names_to = env_col
                        ) %>%
    dplyr::group_by(!!ensym(env_col)) %>%
    dplyr::mutate(mean_env = mean(value)
                  , med_env = median(value)
                  , min_env = min(value)
                  , max_env = max(value)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!ensym(clust_col)
                    , !!ensym(env_col)
                    , across(contains("_env"))
                    ) %>%
    dplyr::summarise(sites = length(unique(cell))
                     , mean_eco = mean(value)
                     , med_eco = median(value)
                     , min_eco = min(value)
                     , max_eco = max(value)
                     , st_eco = sd(value)
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prop_med = (max_env - med_eco)/(max_env - min_env)
                  , prop_mean = (max_env - mean_eco)/(max_env - min_env)
                  ) %>%
    dplyr::group_by(!!ensym(env_col)) %>%
    dplyr::mutate(rank_med = rank(prop_med)) %>%
    dplyr::ungroup() %>%
    {if(isTRUE(!is.null(imp_df))) (.) %>% dplyr::left_join(imp) else (.)}

  }
