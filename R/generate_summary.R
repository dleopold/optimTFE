# #' Generate summary statistics for optimization solutions
# #'
# #' This function calculates various summary statistics for optimization solutions,
# #' including unit counts, suitability metrics, species richness, and spatial
# #' metrics (when spatial data is provided).
# #'
# #' @param data Dataframe containing solution data. If NULL, data will be loaded
# #'   from the specified output directory.
# #' @param out_dir Character string specifying the output directory path. Default is ".".
# #' @param run_id Character string specifying the run identifier. Default is "optimTFE".
# #' @param suitability_mx Matrix of suitability values with unit IDs as row names and
# #'   species as column names.
# #' @param spatial Spatial data frame (sf object) containing geometries for planning units.
# #' @param spatial_crs CRS object for spatial data. By default this will be set to the CRS
# #'   of the spatial data using `sf::st_crs(spatial)`. However, calculations of spatial metrics
# #'   assume an equal area projection and a suitable crs can be provided to project the
# #'   existing spatial data. For exameple, `spatial_crs = sf::st_crs("+proj=utm +zone=4")`` will
# #'   project the existing spatial data to UTM zone 4 before calculating spatial metrics.
# #' @param min_units_plot Character string specifying the output path for the minimum units
# #'   plot description (default = `file.path(out_dir, run_id, "n_units.pdf")`).
# #' @param summary_out Character string specifying the output path for the summary
# #'   data frame (default = `file.path(out_dir, run_id, "summary.csv")`).
# #' @param return_df Logical value indicating whether to return the summary data frame.
# #' @param progress Logical value indicating whether to show progress bar
# #' @param cores Number of cores to use for parallel processing
# #' @param batch_size when parallel processing is used, this parameter can be
# #'   used to process solution in batches for improved efficiency (default =
# #'   NULL). If not provided (NULL), batch size will be calculated automatically.
# #' @param max_batch_size limit batch size for parallel processing (default =
# #'   1000). Smaller values will allow the progress bar to update more
# #'   frequently, but at the cost of more disk writes.
# #'
# #' @return A data frame with summary statistics for each solution, including number
# #'   of units, mean/median suitability, species richness metrics, and spatial metrics
# #'   (area and perimeter) if spatial data is provided.
# #'
# #' @import collapse
# #'
# #' @family main
# #' @export
# #'
# generate_summary <- function(
#   data = NULL,
#   out_dir = ".",
#   run_id = "optimTFE",
#   suitability_mx = NULL,
#   spatial = NULL,
#   spatial_crs = NULL,
#   min_units_plot = file.path(out_dir, run_id, "n_units.pdf"),
#   summary_out = file.path(out_dir, run_id, "summary.csv"),
#   return_df = FALSE,
#   progress = TRUE,
#   cores = NULL,
#   batch_size = NULL,
#   max_batch_size = 1000
# ) {
#   # Progressr setup ---
#   if (
#     isTRUE(progress) &&
#       interactive() &&
#       !isTRUE(getOption("knitr.in.progress")) &&
#       !isTRUE(getOption("rstudio.notebook.executing"))
#   ) {
#     progressr::handlers(global = TRUE)
#   } else if (
#     interactive() &&
#       !isTRUE(getOption("knitr.in.progress")) &&
#       !isTRUE(getOption("rstudio.notebook.executing"))
#   ) {
#     progressr::handlers(global = FALSE)
#   }

#   if (is.null(data)) {
#     data <- arrow::open_dataset(file.path(out_dir, run_id, "solutions")) |>
#       dplyr::collect()
#   }

#   message(crayon::cyan(glue::glue(
#     "Generating summary data for {fndistinct(data$solution)} solutions..."
#   )))
#   start_time <- Sys.time()
#   cores <- cores %||% (future::availableCores() - 1)

#   meta <- tryCatch(
#     jsonlite::fromJSON(file.path(
#       out_dir,
#       run_id,
#       paste0(run_id, ".meta")
#     )),
#     error = function(e) NULL
#   )

#   if (is.null(suitability_mx) && !is.null(meta)) {
#     suitability_mx <- meta$suitability
#     colnames(suitability_mx) <- meta$spp_names
#     rownames(suitability_mx) <- meta$unit_ids
#   }
#   if (is.null(suitability_mx)) {
#     stop(crayon::bold(crayon::red(
#       "Failed to load suitability matrix"
#     )))
#   }
#   unit_ids <- rownames(suitability_mx)
#   unit_idx <- seq_along(unit_ids)

#   if (!is.null(spatial)) {
#     if (is.character(spatial) && file.exists(spatial)) {
#       spatial <- tryCatch(
#         sf::read_sf(spatial),
#         error = function(e) NULL
#       )
#     }
#     if (!inherits(spatial, "sf") && !methods::is(spatial, "Spatial")) {
#       stop(crayon::bold(crayon::red(
#         "Invalid spatial data input."
#       )))
#     }
#     if (
#       !all(sf::st_is(spatial, "POLYGON") | sf::st_is(spatial, "MULTIPOLYGON"))
#     ) {
#       stop(crayon::bold(crayon::red(
#         "Spatial input must only contain POLYGON geometries."
#       )))
#     }
#     if (!identical(sort(spatial[[1]]), sort(unit_ids))) {
#       stop(crayon::bold(crayon::red(
#         "Spatial input unit_ids do not match suitability matrix."
#       )))
#     }
#     if (
#       !is_equal_area_sf(spatial) &&
#         is_equal_area_sf(spatial_crs)
#     ) {
#       spatial <- sf::st_transform(spatial, spatial_crs)
#     }
#     if (
#       !is_equal_area_sf(spatial) &&
#         !is_equal_area_sf(spatial_crs)
#     ) {
#       stop(crayon::bold(crayon::red(
#         "Spatial input must be in an equal area projection or a suitable projection must be provided as spatial_crs (eg, a UTM projection)."
#       )))
#     }
#     # Ensure row order of spatial data matches unit_ids
#     spatial <- spatial[fmatch(unit_ids, spatial[[1]], nomatch = 0L), ]
#     colnames(spatial)[1] <- "unit_id"
#     spatial$unit_idx <- unit_idx
#     # Filter unused units
#     uids <- unique(data$unit_id)
#     spatial <- spatial |> fsubset(unit_id %iin% uids)
#   }

#   # Calculate standard summary metrics
#   unit_richness <- rowSums(data[, -1:-4], )
#   unit_suitability <-
#     rowSums(
#       as.matrix(data[, -1:-4]) * suitability_mx[data$unit_id, ],
#       na.rm = TRUE
#     ) /
#     unit_richness

#   out <- data |>
#     fselect(
#       solution,
#       passing,
#       unit_id
#     ) |>
#     fmutate(
#       suitability = unit_suitability,
#       richness = unit_richness,
#       unit_idx = match(unit_id, unit_ids)
#     ) |>
#     fgroup_by(solution) |>
#     fsummarise(
#       n_units = fnobs(solution),
#       mean_suitability = fmean(suitability),
#       median_suitability = fmedian(suitability),
#       mean_richness = fmean(richness),
#       max_richness = fmax(richness),
#       passing = ffirst(passing),
#       units = list(sort(unit_idx))
#     )
#   rm(data)
#   gc()

#   # Calculate spatial metrics
#   if (!is.null(spatial)) {
#     message(
#       crayon::cyan(
#         "Calculating spatial summary metrics"
#       )
#     )

#     # Calculate area ----
#     area <- spatial |>
#       fmutate(
#         area = as.numeric(sf::st_area(sf::st_geometry(spatial)))
#       ) |>
#       dplyr::pull("area")
#     units_flat <- unlist(out$units, use.names = FALSE)
#     grp <- rep(seq_along(out$units), lengths(out$units))
#     pos <- fmatch(units_flat, spatial$unit_idx, nomatch = 0L)
#     out$area <- collap(area[pos], grp, fsum, na.rm = TRUE)[[2]]

#     # Calculate perimeter ----
#     if (is.null(batch_size)) {
#       batch_size <- min(ceiling(nrow(out) / (2 * cores)), max_batch_size)
#     }
#     btchs <- out$units |>
#       {
#         \(x) split(x, ceiling(seq_along(x) / batch_size))
#       }()
#     # Decompose polygons into segments
#     segment_key <- generate_segment_key(spatial)
#     # Set up future backend
#     if (parallelly::supportsMulticore()) {
#       future_mode <- future::multicore
#     } else {
#       future_mode <- future::multisession
#     }
#     future::plan(future_mode, workers = min(cores, length(btchs)))

#     p <- progressor(
#       along = seq_along(btchs)
#     )

#     # DEBUG ----
#     # list2env(as.list(environment()), envir = .GlobalEnv)
#     # return()

#     perimeter <- btchs |>
#       furrr::future_map(
#         ~ {
#           p()
#           optimTFE:::compute_perimeters(
#             subsets = .x,
#             keyPolygonIds = segment_key$unit_idx,
#             lengths = segment_key$length,
#             Pmax = Pmax
#           )
#         },
#         .options = furrr::furrr_options(
#           seed = NULL,
#           globals = list(
#             segment_key = segment_key,
#             Pmax = max(unit_idx)
#           )
#         )
#       ) |>
#       purrr::reduce(c)
#     out$perimeter <- perimeter
#     out$pa_ratio <- out$perimeter / out$area
#   }

#   if (length(min_units_plot) == 1) {
#     p <- out |>
#       dplyr::arrange(solution) |>
#       fmutate(
#         min_units = cummin(n_units)
#       ) |>
#       ggplot2::ggplot(
#         ggplot2::aes(x = solution, y = min_units)
#       ) +
#       ggplot2::geom_line() +
#       ggplot2::theme_minimal()
#     ggplot2::ggsave(
#       filename = min_units_plot,
#       plot = p,
#       width = 6,
#       height = 4
#     )
#   }

#   # Save to csv
#   if (length(summary_out) == 1) {
#     out |>
#       fselect(-units) |>
#       arrow::write_csv_arrow(summary_out)
#   }

#   elapsed_time <- Sys.time() - start_time
#   elapsed_time <- glue::glue(
#     "{round(elapsed_time, 1)} {attr(elapsed_time, 'units')}"
#   )
#   message(crayon::cyan(glue::glue(
#     "Finished summarizing {fnrow(out)} solutions in {elapsed_time}"
#   )))

#   if (return_df) {
#     return(out)
#   }
#   return(invisible())
# }

# # Spatial helper functions ----
