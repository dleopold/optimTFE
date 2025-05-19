#' Calculate spatial statistics from solutoin summary file
#'
#' This function computes area, perimeter, and perimeter-to-area ratio for a set of
#' spatial solutions. It processes solution data in parallel for improved performance
#' when dealing with large datasets.
#'
#' @param data A data frame of solutions or a path to a CSV file. Must include a 'units' column
#'   containing JSON strings representing the selected planning units for each solution.
#' @param spatial An sf object or path to spatial data file containing polygons of planning units.
#'   Defaults to the example spatial dataset included with optimTFE.
#' @param cores Number of CPU cores to use for parallel processing. If NULL, uses one less
#'   than the available cores.
#' @param batch_size Number of solutions to process in each batch. If NULL, automatically
#'   calculated based on cores and data size.
#' @param max_batch_size Maximum allowed batch size for processing.
#'
#' @return A data frame with the original solution data plus three new columns:
#'   \item{area}{Total area of the selected planning units}
#'   \item{perimeter}{Total perimeter of the selected planning units, accounting for shared boundaries}
#'   \item{pa_ratio}{Perimeter-to-area ratio, a measure of compactness}
#'
#' @import collapse
#' @importFrom sf st_area st_geometry st_is st_boundary st_cast st_coordinates st_crs
#' @importFrom dplyr pull mutate
#' @importFrom future plan multicore multisession
#' @importFrom furrr future_map furrr_options
#'
#' @family utils
#' @export
spatial_stats <- function(
  data = NULL,
  spatial = NULL,
  cores = NULL,
  batch_size = NULL,
  max_batch_size = 1000
) {
  start_time <- Sys.time()
  cores <- cores %||% (future::availableCores() - 1)

  # Load summary data ----
  if (is.character(data) && file.exists(data)) {
    data <- tryCatch(
      read.csv(data),
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

  ## Parse unit ids ----
  units <- data$units |> lapply(jsonlite::fromJSON)
  unit_ids <- purrr::flatten(units) |> unlist() |> unique()
  unit_idx <- seq_along(unit_ids)
  units_indexed <- units |>
    purrr::map(
      ~ {
        collapse::fmatch(.x, unit_ids, nomatch = 0L)
      }
    )

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
  if (!all(spatial[[1]] %iin% unit_ids)) {
    stop(crayon::bold(crayon::red(
      "Spatial input must include all unit ids from the solutions data."
    )))
  }

  if (!(is_equal_area_sf(spatial))) {
    proceed <- menu(
      title = "The detected CRS of the spatial data could not be confirmed to be an equal area projection. Calculation of perimeter and area may be incorrect. Do you want to proceed anyway?",
      choices = c("Yes", "No")
    )
    if(proceed == 2) {
      return()
    }
  }

  ## Ensure row order of spatial data matches unit_ids
  spatial <- spatial[collapse::fmatch(unit_ids, spatial[[1]], nomatch = 0L), ]
  colnames(spatial)[1] <- "unit_id"
  spatial$unit_idx <- unit_idx
  spatial <- spatial |> collapse::fsubset(unit_id %iin% unit_ids)

  message(crayon::cyan(glue::glue(
    "Summarizing spatial data for {nrow(data)} solutions..."
  )))

  # Calculate area ----
  area <- spatial |>
    fmutate(
      area = as.numeric(sf::st_area(sf::st_geometry(spatial)))
    ) |>
    dplyr::pull("area")
  grp <- rep(seq_along(units), lengths(units))
  area <- collap(
    area[unlist(units_indexed, use.names = FALSE)],
    grp,
    fsum,
    na.rm = TRUE
  )[[2]]

  # Calculate perimeter ----
  if (is.null(batch_size)) {
    batch_size <- min(ceiling(nrow(data) / (4 * cores)), max_batch_size)
  }
  batch_size <- max(20, batch_size)
  btchs <- units_indexed |>
    {
      \(x) split(x, ceiling(seq_along(x) / batch_size))
    }()
  # Decompose polygons into segments
  segment_key <- generate_segment_key(spatial)
  # Set up future backend
  if (future::supportsMulticore()) {
    future_mode <- future::multicore
  } else {
    future_mode <- future::multisession
  }
  future::plan(future_mode, workers = min(cores, length(btchs)))

  p <- progressor(
    along = seq_along(btchs)
  )

  # DEBUG ----
  # list2env(as.list(environment()), envir = .GlobalEnv)
  # return()

  perimeter <- btchs |>
    furrr::future_map(
      ~ {
        p()
        compute_perimeters(
          subsets = .x,
          keyPolygonIds = segment_key$unit_idx,
          lengths = segment_key$length,
          Pmax = Pmax
        )
      },
      .options = furrr::furrr_options(
        seed = NULL,
        globals = list(
          segment_key = segment_key,
          Pmax = max(unit_idx)
        )
      )
    ) |>
    purrr::reduce(c)
  pa_ratio <- perimeter / area

  # Return ----
  data |>
    dplyr::mutate(
      area = area,
      perimeter = perimeter,
      pa_ratio = pa_ratio,
      .before = units
    )
}

#' Generate a segment key for fast perimeter calculation
#'
#' Creates a lookup table of all boundary segments in a spatial dataset,
#' identifying which polygons share each segment. This enables efficient
#' perimeter calculations for arbitrary subsets of polygons.
#'
#' @param spatial An sf object containing geometries for planning units
#'
#' @return A data frame with polygon IDs and segment lengths for each boundary segment
#'
#' @noRd
#'
generate_segment_key <- function(spatial) {
  all_rings <- suppressWarnings(sf::st_cast(
    sf::st_boundary(spatial),
    "LINESTRING"
  ))
  coords <- sf::st_coordinates(all_rings) |>
    tibble::as_tibble()
  coords$unit_idx <- all_rings$unit_idx[coords[['L1']]]

  seg_df <- coords |>
    fgroup_by(unit_idx) |>
    fmutate(xend = dplyr::lead(X), yend = dplyr::lead(Y)) |>
    fungroup() |>
    fsubset(!is.na(xend))

  seg_sfc <- sf::st_sfc(
    purrr::pmap(
      list(seg_df$X, seg_df$Y, seg_df$xend, seg_df$yend),
      ~ sf::st_linestring(matrix(c(..1, ..2, ..3, ..4), ncol = 2, byrow = TRUE))
    ),
    crs = sf::st_crs(spatial)
  )

  segments <- sf::st_as_sf(
    data.frame(
      unit_idx = seg_df$unit_idx,
      geometry = seg_sfc
    )
  )

  coords <- sf::st_coordinates(sf::st_geometry(segments))
  n <- nrow(segments)
  idx1 <- seq(1, by = 2, length.out = n)
  idx2 <- idx1 + 1
  x1 <- coords[idx1, "X"]
  y1 <- coords[idx1, "Y"]
  x2 <- coords[idx2, "X"]
  y2 <- coords[idx2, "Y"]
  cond <- (x1 < x2) | ((x1 == x2) & (y1 <= y2))

  p1 <- paste(x1, y1, sep = ",")
  p2 <- paste(x2, y2, sep = ",")

  keys <- ifelse(
    cond,
    paste(p1, p2, sep = "|"),
    paste(p2, p1, sep = "|")
  )

  segments$key <- keys

  segments_info <- segments |>
    sf::st_set_geometry(NULL) |>
    fgroup_by(key) |>
    fsummarise(
      unit_idx = list(sort(unique(unit_idx)))
    )

  segments |>
    funique(cols = c("key")) |>
    fselect(key, geometry) |>
    join(segments_info, on = "key", how = 'left', verbose = 0) |>
    fmutate(length = as.numeric(sf::st_length(geometry))) |>
    sf::st_drop_geometry() |>
    fselect(unit_idx, length)
}

#' Check if a spatial object has an equal area projection
#'
#' Determines whether a spatial object's CRS is an equal area projection
#' by checking for "Equal_Area" in the WKT definition or by looking for
#' common equal area projection codes in the PROJ4 string.
#'
#' @param x An sf object to check
#'
#' @return Logical value indicating if the object has an equal area projection
#'
#' @noRd
#'
is_equal_area_sf <- function(x) {
  crs <- sf::st_crs(x)
  if (is.na(crs)) return(FALSE)
  # many WKT2 definitions of equal‐area projections include "Equal_Area"
  wkt <- crs$wkt %||% ""
  if (nzchar(wkt) && grepl("Equal_Area", wkt, ignore.case = TRUE)) {
    return(TRUE)
  }
  # fallback: check PROJ4 for known proj names
  p4 <- crs$proj4string %||% ""
  grepl(
    "\\+proj=(aea|laea|cea|eqc|sinu|utm|sterea|tmerc|lcc|merc|omerc)",
    p4,
    ignore.case = TRUE
  )
}
