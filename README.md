---
output:
  rmarkdown::github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# `envEcosystems`: an R package of objects and tools to create ecosystem descriptions

<!-- badges: start -->
<!-- badges: end -->

The goal of `envEcosystems` is to provide tools and objects for creating ecosystem descriptions based on outputs from other `env`Packages.

## Installation

`envEcosystems` is not on [CRAN](https://CRAN.R-project.org).

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Acanthiza/envEcosystems")
```

Load `envEcosystems`


```r
library("envEcosystems")
```

## Convert between cover value systems

`lucover` provides a lookup table from modified Braun-Blanquet cover codes to various numeric cover values.


```r

  lucover
#> # A tibble: 7 x 12
#>   cover_code cover_description                  cover_desc   cover_seq cover_1 cover_2 cover_3 cover_4    pa cover_mid cover_max cover_min
#>   <chr>      <chr>                              <chr>            <int>   <int>   <dbl>   <dbl>   <dbl> <int>     <dbl>     <dbl>     <dbl>
#> 1 N          not many, 1-10 individuals         1-10 indivi~        13       1    0.02    0.01    0.01     1     0.01       0.01      0   
#> 2 T          sparsely or very sparsely present~ sparsely pr~        11       2    0.01    0.5     0.1      1     0.015      0.02      0.01
#> 3 1          plentiful but of small cover (les~ <5%                  9       3    0.03    1       1        1     0.025      0.05      0.02
#> 4 2          any number of individuals coverin~ 5-25%                7       4    0.05    2       2        1     0.15       0.25      0.05
#> 5 3          any no. of individuals covering 2~ 25-50%               5       5    0.25    3       3        1     0.375      0.5       0.25
#> 6 4          any no. of individuals covering 5~ 50-75%               3       6    0.5     4       4        1     0.625      0.75      0.5 
#> 7 5          covering more than 75% of the area >75%                 1       7    0.75    5       5        1     0.875      1         0.75
```

## What else is in `envEcosystems`

The following functions and data sets are provided in `envEcosystems`. See https://acanthiza.github.io/envEcosystems/ for more examples.


|object                 |class                      |description                                                            |
|:----------------------|:--------------------------|:----------------------------------------------------------------------|
|[add_landcover_desc()] |function                   |Add 'extra' ecosystem descriptions                                     |
|[cut_cov]              |tbl_df, tbl and data.frame |Cover class threshold values                                           |
|[cut_ht]               |tbl_df, tbl and data.frame |Height class threshold values                                          |
|[env]                  |tbl_df, tbl and data.frame |Lookup for environmental data                                          |
|[lucover]              |tbl_df, tbl and data.frame |Lookup for cover codes                                                 |
|[luenv]                |tbl_df, tbl and data.frame |Lookup for environmental data                                          |
|[lulandcover]          |tbl_df, tbl and data.frame |Lookup for landcover classes and their associated colours              |
|[lulifeform]           |tbl_df, tbl and data.frame |Lookup for lifeform codes                                              |
|[lulifespan]           |tbl_df, tbl and data.frame |Lookup for lifespan classes                                            |
|[make_eco_desc()]      |function                   |Make a description for an ecosystem                                    |
|[make_eco_env()]       |function                   |Make a dataframe summarising environmental variables per cluster.      |
|[make_eco_str_per()]   |function                   |Create a dataframe of taxa percent cover for each cluster              |
|[make_eco_taxa_per()]  |function                   |Create a dataframe of taxa percent cover for each cluster              |
|[sa_sf]                |tbl_df, tbl and data.frame |Lookup for broad structural formations                                 |
|[sa_vsf]               |tbl_df, tbl and data.frame |Lookup for South Australian vegetation structural formations           |
|[set_eco_col()]        |function                   |Set colours for individual ecosystems, using gradations within ecotype |
|[taxa_samphire]        |character                  |Vector of samphire taxa                                                |
|[taxa_wetland]         |character                  |Vector of wetland taxa                                                 |




