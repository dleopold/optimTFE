#' A spatial dataset containing the species habitat suitability values by each
#' planning unit. This dataset is used for creating an input for the optimTFE
#' algorithm and spatially referencing output solutions.
#' The columns are as follows:
#'
#' @format A data frame with 693 rows and 38 variables:
#' \describe{
#'   \item{PU_num}{Planning unit/unit_id number}
#'   \item{sp_01 - sp_36}{Habitat suitability value for each species (0-1)}
#'   \item{geom}{geometry }
#' }
#'
#' @source {optimTFE} optimTFE R package.
