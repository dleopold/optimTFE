#' Launch the Criteria Explorer Shiny Application
#'
#' @param spatial Either a file path to spatial data (e.g., GeoPackage, Shapefile) or
#'   an `sf` object containing polygon geometries. The first column should contain
#'   unique unit identifiers that match those in the solutions data.
#' @param data Either a file path to a CSV file containing solution summary data or
#'   a data.frame. Must include a 'units' column with JSON-formatted unit lists.
#'   If NULL, will attempt to load from `file.path(dir, run_id, paste0(run_id, ".summary.csv"))`.
#' @param dir Character string specifying the directory containing optimization results.
#'   Defaults to current working directory (".").
#' @param run_id Character string identifying the optimization run. Used to construct
#'   file paths when `data` is NULL. Defaults to "optimTFE".
#' @param map_tiles Character, name of the leaflet provider tiles. Default is "Esri.
#' WorldTopoMap". Set to NULL to use no background map tiles.
#'
#' @details
#' The application provides:
#' \itemize{
#'   \item Interactive filtering of solutions based on summary statistics
#'   \item Spatial visualization of solution footprints
#'   \item Histogram displays of solution criteria
#'   \item Ranking and comparison tools
#' }
#'
#' @examples
#' \dontrun{
#' # Launch with default settings
#' criteria_explorer(
#'   spatial = "path/to/spatial.gpkg",
#'   run_id = "optimTFE" # optimTFE run id
#' )
#' }
#'
#' @import shiny bslib gargoyle
#' @importFrom dplyr select any_of
#' @importFrom purrr flatten
#' @importFrom jsonlite fromJSON
#' @importFrom sf read_sf st_is
#' @importFrom crayon bold red
#' @importFrom shinyjs useShinyjs
#'
#' @family utils
#' @export
criteria_explorer <- function(
  spatial = NULL,
  data = NULL,
  dir = ".",
  run_id = "optimTFE",
  map_tiles = "Esri.WorldTopoMap"
) {
  # Load summary data ----
  if (is.null(data)) {
    data <- file.path(dir, run_id, paste0(run_id, ".summary.csv"))
  }
  if (is.character(data) && file.exists(data)) {
    data <- tryCatch(
      read.csv(data) |>
        dplyr::select(!dplyr::any_of("passing")),
      error = function(e) NULL
    )
  }
  if (!is.data.frame(data)) {
    stop(crayon::bold(crayon::red(
      "Failed to load summary data."
    )))
  }
  if ("units" %!in% colnames(data)) {
    stop(crayon::bold(crayon::red(
      "Summary data must include a 'units' column."
    )))
  }

  units <- data$units |> lapply(jsonlite::fromJSON)
  unit_ids <- purrr::flatten(units) |> unlist() |> unique()

  # long format scaled selectioncriteria for fast weighted ranking
  criteria <- data |>
    collapse::fselect(-units) |>
    collapse::pivot(
      "solution",
      factor = NULL
    ) |>
    collapse::fgroup_by("variable") |>
    collapse::fmutate(
      value = collapse::fscale(value)
    ) |>
    collapse::fungroup()

  # Load spatial data ---
  if (is.character(spatial) && file.exists(spatial)) {
    spatial <- tryCatch(
      sf::read_sf(spatial),
      error = function(e) NULL
    )
  }
  if (!inherits(spatial, "sf") && !inherits(spatial, "Spatial")) {
    stop(crayon::bold(crayon::red(
      "Invalid spatial data input."
    )))
  }
  if (
    !all(sf::st_is(spatial, "POLYGON") | sf::st_is(spatial, "MULTIPOLYGON"))
  ) {
    stop(crayon::bold(crayon::red(
      "Spatial input must only contain POLYGON geometries."
    )))
  }
  if (!all(unit_ids %in% spatial[[1]])) {
    stop(crayon::bold(crayon::red(
      "Spatial input must include all unit ids from the solutions data."
    )))
  }
  colnames(spatial)[1] <- "unit"

  # Attempt to force to WGS84 for consistency with most provider tiles
  spatial <- tryCatch(
    st_transform(spatial, "EPSG:4326"),
    error = \(e) spatial
  )

  # App UI ----
  ui <- function(request) {
    tagList(
      shinyjs::useShinyjs(),
      bslib::page_sidebar(
        title = "optimTFE Criteria Explorer",
        theme = bslib::bs_theme(
          version = 5,
          bootswatch = "yeti"
        ),
        sidebar = bslib::sidebar(
          ce_sidebar("sidebar"),
          width = 320
        ),
        ce_map_ui("map"),
        ce_histograms_ui("hist")
      )
    )
  }

  # App server ----
  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 2000 * 1024^2)

    # Reactive values shared across modules ----
    rv <- reactiveValues(
      spatial = spatial, # Spatial data
      solutions = data, # Solution summary data
      criteria = criteria, # Scaled criteria data
      selected_stats = NULL, # Currently elected summary stat columns
      observers = NULL, # list of stats that currently have slider inputs
      ranks = NULL, # current ranks of the solutions based on the selected stats
      selected_solution = NULL, # currently selected solution(s) to display
      provider_tiles = map_tiles
    )

    # Module servers ----
    ce_sidebar_server("sidebar", rv)
    ce_map_server("map", rv)
    ce_histograms_server("hist", rv)
  }

  shinyApp(
    ui = ui,
    server = server
  )
}
