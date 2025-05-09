#' Generate summary statistics for optimization solutions
#'
#' This function calculates various summary statistics for optimization solutions,
#' including unit counts, suitability metrics, species richness, and spatial
#' metrics (when spatial data is provided).
#'
#' @param data Dataframe containing solution data. If NULL, data will be loaded
#'   from the specified output directory.
#' @param out_dir Character string specifying the output directory path. Default is ".".
#' @param run_id Character string specifying the run identifier. Default is "optimTFE".
#' @param suitability_mx Matrix of suitability values with unit IDs as row names and
#'   species as column names.
#' @param spatial Spatial data frame (sf object) containing geometries for planning units.
#' @param spatial_crs CRS object for spatial data. By default this will be set to the CRS
#'   of the spatial data using `sf::st_crs(spatial)`. However, calculations of spatial metrics
#'   assume an equal area projection and a suitable crs can be provided to project the
#'   existing spatial data. For exameple, `spatial_crs = sf::st_crs("+proj=utm +zone=4")`` will
#'   project the existing spatial data to UTM zone 4 before calculating spatial metrics.
#' @param min_units_plot Character string specifying the output path for the minimum units
#'   plot description (default = `file.path(out_dir, run_id, "n_units.pdf")`).
#' @param summary_out Character string specifying the output path for the summary
#'   data frame (default = `file.path(out_dir, run_id, "summary.csv")`).
#' @param return_df Logical value indicating whether to return the summary data frame.
#' @param progress Logical value indicating whether to show progress bar
#'
#' @return A data frame with summary statistics for each solution, including number
#'   of units, mean/median suitability, species richness metrics, and spatial metrics
#'   (area and perimeter) if spatial data is provided.
#'
#' @import collapse
#'
#' @family main
#' @export
#'
generate_summary <- function(
  data = NULL,
  out_dir = ".",
  run_id = "optimTFE",
  suitability_mx = NULL,
  spatial = NULL,
  spatial_crs = NULL,
  min_units_plot = file.path(out_dir, run_id, "n_units.pdf"),
  summary_out = file.path(out_dir, run_id, "summary.csv"),
  return_df = FALSE,
  progress = TRUE
) {
  # Determine if progress bar should be used
  if (
    isTRUE(progress) &&
      interactive() &&
      !isTRUE(getOption("knitr.in.progress")) &&
      !isTRUE(getOption("rstudio.notebook.executing"))
  ) {
    progressr::handlers(global = TRUE)
  } else if (
    interactive() &&
      !isTRUE(getOption("knitr.in.progress")) &&
      !isTRUE(getOption("rstudio.notebook.executing"))
  ) {
    progressr::handlers(global = FALSE)
  }

  if (is.null(data)) {
    data <- arrow::open_dataset(file.path(out_dir, run_id, "solutions")) |>
      dplyr::collect()
  }

  meta <- tryCatch(
    jsonlite::fromJSON(file.path(
      out_dir,
      run_id,
      paste0(run_id, ".meta")
    )),
    error = function(e) NULL
  )

  if (is.null(suitability_mx) && !is.null(meta)) {
    suitability_mx <- meta$suitability
    colnames(suitability_mx) <- meta$spp_names
    rownames(suitability_mx) <- meta$unit_ids
  }
  if (is.null(suitability_mx)) {
    stop(crayon::bold(crayon::red(
      "Failed to load suitability matrix"
    )))
  }
  unit_ids <- rownames(suitability_mx)

  if (!is.null(spatial)) {
    if (is.character(spatial) && file.exists(spatial)) {
      spatial <- tryCatch(
        sf::read_sf(spatial),
        error = function(e) NULL
      )
    }
    if (!inherits(spatial, "sf") && !methods::is(spatial, "Spatial")) {
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
    if (!identical(sort(spatial[[1]]), sort(unit_ids))) {
      stop(crayon::bold(crayon::red(
        "Spatial input unit_ids do not match suitability matrix."
      )))
    }
    if (
      !is_equal_area_sf(spatial) &&
        is_equal_area_sf(spatial_crs)
    ) {
      spatial <- sf::st_transform(spatial, spatial_crs)
    }
    if (
      !is_equal_area_sf(spatial) &&
        !is_equal_area_sf(spatial_crs)
    ) {
      stop(crayon::bold(crayon::red(
        "Spatial input must be in an equal area projection or a suitable projection must be provided as spatial_crs (eg, a UTM projection)."
      )))
    }
    # Ensure row order of spatial data matches unit_ids
    spatial <- spatial[fmatch(unit_ids, spatial[[1]], nomatch = 0L), ]
    colnames(spatial)[1] <- "unit_id"
    # Filter unused units
    uids <- unique(data$unit_id)
    spatial <- spatial |> fsubset(unit_id %iin% uids)
  }

  # Calculate standard summary metrics
  unit_richness <- rowSums(data[, -1:-4], )
  unit_suitability <-
    rowSums(
      as.matrix(data[, -1:-4]) * suitability_mx[data$unit_id, ],
      na.rm = TRUE
    ) /
    unit_richness

  out <- data |>
    fselect(
      solution,
      passing,
      unit_id
    ) |>
    fmutate(
      suitability = unit_suitability,
      richness = unit_richness
    ) |>
    fgroup_by(solution) |>
    fsummarise(
      n_units = fnobs(solution),
      mean_suitability = fmean(suitability),
      median_suitability = fmedian(suitability),
      mean_richness = fmean(richness),
      max_richness = fmax(richness),
      passing = ffirst(passing),
      units = list(unit_id)
    )

  # Calculate spatial metrics
  if (!is.null(spatial)) {
    # Set up future backend
    if (parallelly::supportsMulticore()) {
      future_mode <- future::multicore
    } else {
      future_mode <- future::multisession
    }
    future::plan(future_mode, workers = future::availableCores() - 1)
    # Calculate area ----
    area <- spatial |>
      fmutate(
        area = as.numeric(sf::st_area(sf::st_geometry(spatial)))
      ) |>
      dplyr::pull("area")
    units_flat <- unlist(out$units, use.names = FALSE)
    grp <- rep(seq_along(out$units), lengths(out$units))
    pos <- fmatch(units_flat, spatial$unit_id, nomatch = 0L)
    out$area <- collap(area[pos], grp, fsum, na.rm = TRUE)[[2]]

    # Calculate perimeter ----
    message(
      crayon::cyan(
        "Calculating spatial summary metrics"
      )
    )
    segment_key <- generate_segment_key(spatial)
    p <- progressor(
      along = seq_along(out$solution)
    )
    out$perimeter <- out$units |>
      furrr::future_map_dbl(
        ~ {
          # p()
          optimTFE:::compute_perimeter(.x, segment_key)
        },
        .options = furrr::furrr_options(
          seed = NULL,
          globals = "segment_key"
        )
      )
    out$pa_ratio <- out$perimeter / out$area
  }

  if (length(min_units_plot) == 1) {
    p <- out |>
      dplyr::arrange(solution) |>
      fmutate(
        min_units = cummin(n_units)
      ) |>
      ggplot2::ggplot(
        ggplot2::aes(x = solution, y = min_units)
      ) +
      ggplot2::geom_line() +
      ggplot2::theme_minimal()
    ggplot2::ggsave(
      filename = min_units_plot,
      plot = p,
      width = 6,
      height = 4
    )
  }

  # Save to csv
  if (length(summary_out) == 1) {
    out |>
      fselect(-units) |>
      arrow::write_csv_arrow(summary_out)
  }

  if (return_df) {
    return(out)
  }
  return(invisible())
}

# Spatial helper functions ----

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

#' Extract all unique boundary segments from a geometry
#'
#' Extracts all boundary segments (linestrings) from a spatial geometry.
#' This is a helper function used in perimeter calculations.
#'
#' @param geom An sf geometry object
#'
#' @return An sfc collection of LINESTRING objects representing the boundaries
#'
#' @noRd
#'
extract_segments <- function(geom, crs) {
  bnd_lines <- sf::st_cast(sf::st_boundary(geom), "LINESTRING")
  do.call(
    c,
    lapply(bnd_lines, function(ls) {
      coords <- sf::st_coordinates(ls)[, 1:2]
      n <- nrow(coords)
      sf::st_sfc(
        lapply(
          seq_len(n - 1),
          \(i) sf::st_linestring(coords[i:(i + 1), ])
        ),
        crs = crs
      )
    })
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
  poly_edges <- do.call(
    rbind,
    lapply(seq_len(nrow(spatial)), function(i) {
      segs <- extract_segments(sf::st_geometry(spatial)[i], sf::st_crs(spatial))
      data.frame(poly_id = spatial$unit_id[i], geometry = segs)
    })
  ) |>
    sf::st_as_sf()

  poly_edges$key <- sapply(poly_edges$geometry, \(seg) {
    pts <- sf::st_coordinates(seg)[, 1:2]
    p_str <- apply(pts, 1, paste, collapse = ",")
    paste(sort(p_str), collapse = "|")
  })

  edge_info <- poly_edges |>
    sf::st_set_geometry(NULL) |>
    fgroup_by(key) |>
    fsummarise(
      polygon_ids = list(sort(unique(poly_id)))
    )

  poly_edges |>
    funique(cols = c("key")) |>
    fselect(key, geometry) |>
    join(edge_info, on = "key", how = 'left', verbose = 0) |>
    fmutate(length = as.numeric(sf::st_length(geometry))) |>
    sf::st_drop_geometry() |>
    fselect(polygon_ids, length)
}

#' Compute perimeter of a subset of polygons
#'
#' Calculates the perimeter of a collection of polygons, counting shared boundaries
#' only once. This provides the true outer perimeter of the combined shape.
#'
#' @param subset Vector of polygon IDs to calculate the perimeter for
#' @param key A segment key data frame created by generate_segment_key()
#'
#' @return Numeric value of the perimeter length in the units of the spatial data
#'
#' @noRd
#'
compute_perimeter <- \(subset, key) {
  cnt <- sapply(key$polygon_ids, \(ids) sum(ids %in% subset))
  sum(key$length[cnt == 1])
}
