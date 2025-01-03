#' Example Species Suitability Data
#'
#' A spatial data frame containing the species habitat suitability values by
#' each planning unit. This data set is used for creating an input for the
#' optimTFE algorithm and spatially referencing output solutions.
#'
#' @format A data frame with 693 rows and 38 variables:
#' \describe{
#'   \item{PU_num}{Planning unit/unit_id number}
#'   \item{sp_01 - sp_36}{Habitat suitability value for each species (0-1)}
#'   \item{area_km2}
#'   \item{good_hab_kmsq}
#'   \item{not_good_hab_kmsq}
#'   \item{max_slope}
#'   \item{fenced_kmsq}
#'   \item{not_fenced_kmsq}
#'   \item{reserved_kmsq}
#'   \item{max_FB_richness}
#'   \item{max_accessibility}
#'   \item{geom}{geometry }
#' }
"example_data"

#' Example Targets Data
#'
#' An example targets data frame, specifying the conservation targets for 36
#' species. The data frame also include subregion targets for 2 Bioregions,
#' `region1` and `region2`.
#'
#' @format A data frame with 36 rows and 4 variables:
#' \describe{
#'   \item{species}{Species name / identifier}
#'   \item{total}{the total target value for the species}
#'   \item{region1}{species target value for region1}
#'   \item{region2}{species target value for region2}
#' }
"example_targets"

#' Example Subregions input
#'
#' An example subregions data frame, specifying which of the example planning
#' units comprise `region1` and `region2`.
#'
#' @format A data frame with 693 rows and 3 variables:
#' \describe{
#'   \item{unit_id}{Planning unit name / identifier}
#'   \item{region1}{0/1 indicating if the planning unit is in region1}
#'   \item{region2}{0/1 indicating if the planning unit is in region2}
#' }
"example_subregions"

#' Example Known population input
#'
#' An example know population data frame, specifying the known population
#'
#' @format A data frame with 693 rows and 3 variables:
#' \describe{
#'   \item{unit_id}{Planning unit name / identifier}
#'   \item{sp_01 - sp_36}{Habitat suitability value for each species (0-1)}
#' }
"example_populations"
