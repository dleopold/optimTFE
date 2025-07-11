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
#' @param criteria_include Character vector specifying column names to include as selection
#'   criteria. If NULL (the default), all columns from the input data will be used.
#' @param criteria_presets Named list specifying a set of selection criteria to be preselected
#'   when the application is launched. Must be a named list where the names specific the
#'   criteria to be preselected. Each element should be a list with 2 elements, weight (0>1) and
#'   descending (T/F). For example: `list(accessibility = list(weight = 0.5, descending = T))`
#' @param map_tiles Character, name of the leaflet provider tiles. Default is "Esri.
#' WorldTopoMap". Set to NULL to use no background map tiles.
#' @param auxiliary_layers Named list, paths to additional spatial files to add as overlay layers.
#' @param auxiliary_pallet RColorBrewer palette to use for auxiliary layers (default is "Set2").
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
  criteria_include = NULL,
  criteria_presets = NULL,
  map_tiles = "Esri.WorldTopoMap",
  auxiliary_layers = NULL,
  auxiliary_pallet = "Set2"
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

  if (!is.null(criteria_include)) {
    if (any(criteria_include %nin% colnames(data))) {
      stop(crayon::bold(crayon::red(
        "Column names provided to the criteria parameter do not match the column name in the data."
      )))
    }
    data <- data |>
      dplyr::select(
        units,
        dplyr::any_of(c(criteria_include, "units", "solution"))
      )
  }

  # Parse criteria presets ----
  if (!is.null(criteria_presets)) {
    if (!is.list(criteria_presets)) {
      stop(crayon::bold(crayon::red(
        "Invalid input provided to the criteria_presets parameter."
      )))
    }
    check_criteria <- purrr::imap_lgl(
      criteria_presets,
      ~ {
        if (.y %nin% colnames(data)) {
          return(FALSE)
        }
        if (!all(all(names(.x) %in% c("weight", "descending")))) {
          return(FALSE)
        }
        if (!is.numeric(.x$weight) || .x$weight < 0 || .x$weight > 1) {
          return(FALSE)
        }
        if (!is.logical(.x$descending)) {
          return(FALSE)
        }
        return(TRUE)
      }
    ) |>
      all()
    if (!all(check_criteria)) {
      stop(crayon::bold(crayon::red(
        "Improper input provided to the criteria_presets parameter."
      )))
    }
    criteria_presets <- purrr::imap(
      criteria_presets,
      ~ {
        list(
          val = .x$weight,
          desc = ifelse(.x$descending, -1, 1)
        )
      }
    )
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

  # Auxiliary layers ----
  if (length(auxiliary_layers) > 0) {
    if(length(names(auxiliary_layers)) != length(auxiliary_layers) || any(names(auxiliary_layers) == "")) {
      stop(crayon::bold(crayon::red(
        "Invalid auxiliary layer names."
      )))
    }
    auxiliary_layers <- purrr::imap(
      auxiliary_layers,
      ~ {
        layer <- tryCatch(
          sf::read_sf(.x) |>
            sf::st_transform("EPSG:4326"),
          error = function(e) {
            return(NULL)
          }
        )
        if (is.null(layer)) {
          stop(glue::glue("failed to load auxiliary layer: {.y}"))
          return(NULL)
        }
        return(layer)
      }
    )
  }

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
          ce_sidebar_ui(
            "sidebar",
            footprints = unique(data$solution)
          ),
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
      provider_tiles = map_tiles,
      weights = criteria_presets,
      auxiliary_layers = auxiliary_layers,
      auxiliary_pallet = auxiliary_pallet
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
