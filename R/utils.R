#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%nin%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}


#' Extract specific solutions from solution dataset
#'
#' Retrieves specific solutions from a parquet dataset of optimization solutions.
#'
#' @param solution numbers to return
#' @param dir Directory containing the solution files. Default is current directory.
#' @param run_id The run identifier used to create the subfolder. Default is "optimTFE".
#'
#' @return A data frame containing the requested solutions, or NULL if solutions is empty.
#'
#' @importFrom arrow open_dataset
#' @importFrom dplyr filter collect
#'
#'
#' @family utils
#' @export
extract_solutions <- function(
  solutions = NULL,
  dir = ".",
  run_id = "optimTFE",
  columns = NULL
) {

  res <- arrow::open_dataset(
    file.path(dir, run_id, "solutions"),
    format = "parquet"
  )

  if(!is.null(solutions)){
    res <- res |>
      dplyr::filter(solution %in% solutions)
  }

  if(!is.null(columns)) {
    res <- res |>
      dplyr::select(dplyr::any_of(columns))
  }

  dplyr::collect(res)

}
