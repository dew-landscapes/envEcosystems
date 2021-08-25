

make_eco_env_imp <- function(clust_df
                             , imp_df
                             , clust_col = "cluster"
                             , env_col = "env"
                             , env_cols
                             ) {

  if(!is.data.frame(imp_df)) imp_df <- as_tibble(imp_df
                                                 , rownames = env_col
                                                 )

  factor_levels <- levels(clust_df[clust_col][[1]])

  env_median <- clust_df %>%
    tidyr::pivot_longer(env_cols
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
    dplyr::summarise(sites = n()
                     , mean_eco = mean(value)
                     , med_eco = median(value)
                     , min_eco = min(value)
                     , max_eco = max(value)
                     , st_eco = sd(value)
                     ) %>%
    dplyr::ungroup()

  imp_df %>%
    dplyr::select(!!ensym(env_col),any_of(factor_levels)) %>%
    tidyr::pivot_longer(factor_levels
                        , names_to = clust_col
                        , values_to = "importance"
                        ) %>%
    dplyr::select(!!ensym(clust_col),!!ensym(env_col),everything()) %>%
    dplyr::mutate(!!ensym(clust_col) := factor(!!ensym(clust_col)
                                               , levels = factor_levels
                                               )
                  ) %>%
    dplyr::arrange(!!ensym(clust_col),desc(importance)) %>%
    dplyr::left_join(env_median) %>%
    dplyr::mutate(prop_med = (max_env - med_eco)/(max_env - min_env)
                  , prop_mean = (max_env - mean_eco)/(max_env - min_env)
                  )

  }
