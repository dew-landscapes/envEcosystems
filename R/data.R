#' Lookup for environmental data
#'
#'
#' @format A data frame with `r nrow(env)` rows and `r ncol(env)` variables:
#' \describe{
#'   \item{group}{Broad type of environmental layer.}
#'   \item{category}{Another broad grouping of environmental layer.}
#'   \item{provider}{Who made this environmental layer?}
#'   \item{data_name}{The level at which a reference applies.}
#'   \item{product}{Three letter code identifying the product.}
#'   \item{layer}{Index of band/layer in `product`.}
#'   \item{process}{What process was used to summarise `prod`.}
#'   \item{season_type}{One of; year: whole year, each: split by season or NA}
#'   \item{year_min}{Minimum year available for `data_name` and `prod`.}
#'   \item{transform}{Numeric. Divide by `transform` to convert to `units`
#'   from numbers in `layer`.}
#'   \item{units}{Units for number in `layer` / `transform`.}
#'   \item{description}{Description of the data in `prod`.}
#'   \item{detail}{Other relevant details.}
#'   \item{indicates}{Why is the data in `layer` useful in predicting ecosystem
#'   distribution?}
#'   \item{link}{Link to webpage for `data_name`.}
#'   \item{DOI}{Digital object identifier for `data_name`.}
#'   \item{ref_md}{Citation for use in rmarkdown.}
#'   \item{link_md}{`link` formatted for use in rmarkdown.}
#'   ...
#' }
"env"


#' Lookup for environmental data
#'
#'
#' @format A data frame with 65 rows and 9 variables:
#' \describe{
#'   \item{data_name}{Character. Source of `data_name`}
#'   \item{file}{Character. File name for environmental data}
#'   \item{layer}{Name to use for `file`}
#'   \item{desc}{Description of the data in `layer`}
#'   \item{group}{Broad groupings of `layer` types}
#'   \item{indicator}{Why is the data in `layer` useful in predicting ecosystem
#'   distribution?}
#'   \item{transform}{Numeric. Divide by `transform` to convert to `units`
#'   from numbers in `layer`}
#'   \item{units}{Units for number in `layer` / `transform`}
#'   \item{env_id}{Useful in rmarkdown chunk labels (no punctuation version of
#'   `layer`)}
#'   \item{link}{Link to webpage for `data_name`}
#'   \item{link_md}{`link` formatted for use in markdown}
#'   ...
#' }
"luenv"

#' Lookup for lifespan classes
#'
#'
#' @format A data frame with 4 rows and 2 variables:
#' \describe{
#'   \item{lifespan}{Character. Lifespan code}
#'   \item{desc}{Character. Lifespan description}
#'   ...
#' }
"lulifespan"

#' Lookup for landcover classes and their associated colours
#'
#'
#' @format A data frame with 16 rows and 5 variables:
#' \describe{
#'   \item{use_class}{Character. Name of landcover class}
#'   \item{definition}{Character}
#'   \item{veg}{Logical. Does the landcover class represent a native ecosystem?}
#'   \item{lc_col}{Character. What colour to map `use_class` in land cover map?}
#'   \item{colour}{Character. What colour to map `use_class` in vegetation map?}
#'   ...
#' }
"lulandcover"

#' Cover class threshold values
#'
#' A lookup table to convert from numeric percentage cover to cover classes.
#'
#' @format A data frame with 5 rows and 2 variables:
#' \describe{
#'   \item{cov_thresh}{Numeric percentage cover threshold}
#'   \item{pfc}{Text description of projected foliage cover}
#'   ...
#' }
"cut_cov"


#' Height class threshold values
#'
#' A lookup table to convert numeric height to height classes.
#'
#' @format A data frame with 9 rows and 1 variables:
#' \describe{
#'   \item{ht_thresh}{Numeric height (metres) thresholds}
#'   ...
#' }
"cut_ht"


#' Lookup for cover codes
#'
#' A lookup table to convert modified Braun-Blanquet scale
#' \insertRef{RN4265}{envEcosystems} to various numeric scales.
#'
#' @format A data frame with 7 rows and 12 variables:
#' \describe{
#'   \item{cover_code}{Modified Braun-Blanquet scale}
#'   \item{cover_description}{Text description of cover_code}
#'   \item{cover_desc}{Short text description of cover_code}
#'   \item{cover_seq}{}
#'   \item{cover_1}{1 to 7}
#'   \item{cover_2}{0.01 to 0.75}
#'   \item{cover_3}{0.01 to 5. Sparsely present = 0.5}
#'   \item{cover_4}{0.01 to 5. Sparsely present = 0.1}
#'   \item{pa}{presence/absence. All 1}
#'   \item{cover_mid}{Mid point of scale. 0.01 to 0.875}
#'   \item{cover_max}{Maximum end of scale. 0.01 to 1}
#'   \item{cover_min}{Minimum end of scale. 0 to 0.75}
#'   ...
#' }
"lucover"


#' Lookup for lifeform codes
#'
#' A lookup table for various lifeform codes, particularly those used in the
#' [Biological Databases of South Australia](https://www.environment.sa.gov.au/topics/Science/Information_data/Biological_databases_of_South_Australia)
#' and documented in _Guide to a Native Vegetation Survey_
#' \insertRef{RN192}{envEcosystems}.
#'
#' @format A data frame with 23 rows and 6 variables:
#' \describe{
#'   \item{sort}{Sort order for the codes. Essentially from tallest to shortest.}
#'   \item{lifeform}{The code as documented in \insertRef{RN192}{envEcosystems}}
#'   \item{storey}{Over, mid or under storey}
#'   \item{ht}{Height in metres}
#'   \item{str}{Structure as Trees, Mallees, Shrubs, Mat plants, Hummock
#'   grasses, Grasses, Forbs, Sedges, Mistletoe, Ferns or Groundcover}
#'   \item{description}{Text description of lifeform}
#'   ...
#' }
"lulifeform"


#' Lookup for broad structural formations
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{sf}{text structural formation}
#'   \item{colour}{base colour to use in maps/figures}
#'   \item{order}{sort order (roughly lowest to highest)}
#'   ...
#' }
"sa_sf"


#' Lookup for South Australian vegetation structural formations
#'
#' As documented in _Guide to a Native Vegetation Survey_
#' \insertRef{RN192}{envEcosystems}.
#'
#' @format A data frame with 112 rows and 5 variables:
#' \describe{
#'   \item{storey}{Ordered factor: over < mid < ground < hummock < fern < mistletoe < mat}
#'   \item{str}{Structure as Trees, Mallees, Shrubs, Mat plants, Hummock
#'   grasses, Grasses, Forbs, Sedges, Mistletoe, Ferns or Groundcover}
#'   \item{cov_class}{Factor cover class, generated by cut(cov_thresh, breaks =
#'   c(unique(cov_thresh),5)}
#'   \item{ht_class}{Factor height class, generated by cut(ht_thresh, breaks =
#'   c(unique(ht_thresh),0)}
#'   \item{sa_vsf}{South Australian vegetation structural formations, as defined
#'   in _Guide to a Native Vegetation Survey_ \insertRef{RN192}{envEcosystems}}
#'   ...
#' }
"sa_vsf"


#' Vector of samphire taxa
#'
#' Adapted from Specht \insertRef{RN175}{envEcosystems}. Used to determine
#' 'samphire' structural formation when these taxa have a combined cover above
#' a threshold.
#'
#' @format A character vector of samphire taxa
#'   ...
"taxa_samphire"

#' Vector of wetland taxa
#'
#' Adapted from Specht \insertRef{RN175}{envEcosystems}. Used to determine
#' 'wetland' structural formation when these taxa have a combined cover above
#' a threshold.
#'
#' @format A character vector of wetland taxa
#'   ...
"taxa_wetland"
