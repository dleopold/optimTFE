#' Example Spatial Data
#'
#' A hexagonal grid of planning units.
#'
#' @format A spatial data frame with 795 rows:
#' \describe{
#'   \item{unit_id}{Planning unit name / identifier}
#'   \item{geometry}{hexagonal grid}
#' }
"example_spatial"

#' Example Species Suitability Data
#'
#' A matrix of habitat suitability values for 30 species.
#'
#' @format numeric matrix with 796 rows (units) and 30 columns (species)
"example_suitability"

#' Example Targets Data
#'
#' An example targets matrix, specifying 10 total conservation targets for each of 30
#' species (rows). The matrix include subregion targets for 2 Bioregions,
#' `left` and `right`.
#'
#' @format A matrix with 30 rows and 3 columns:
#' \describe{
#'   \item{total}{the total target value for the species}
#'   \item{left}{species target value for the left region}
#'   \item{right}{species target value for the right region}
#' }
"example_targets"

#' Example Subregions input
#'
#' An example subregions matrix, specifying which of the example planning
#' units comprise the `left` and `right` regions.
#'
"example_subregions"

#' Example Known population input
#'
#' An example know population matrix, with 795 rows (units) and 30 columns (species).
#' Unique values in each species column indicates a known population, which some
#' popultaions spanning more that one unit.
#'
#' @format A matrix with 795 rows and 30 columns
"example_populations"

#' Example Species incompatibility input
#'
#' A n x n binary matrix indicating which species pairs are compatable (0) or incompatibile (1).
#'
#' @format A matrix with 30 rows and 30 columns
"example_incompatibility"
