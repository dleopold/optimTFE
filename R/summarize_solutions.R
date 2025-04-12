summarize_solutions <- function(
    solution_dir = "output2/solutions",
    unit_data = optimTFE::example_data,
    unit_count = TRUE,
    metrics = list(
      area_km2 = "sum(area_km2)",
      accessibility_max_mean = "sum(max_accessibility)"
    )) {
  require(collapse)

  solutions <- arrow::open_dataset(solution_dir) |>
    data.table::as.data.table()

  unit_metrics <- unit_data |>
    sf::st_drop_geometry() |>
    data.table::as.data.table()
  unit_metrics <- read.csv("dev/suitability.csv") |>
    dplyr::select(uniqueID, area_km2, elev_mean) |>
    data.table::as.data.table()
  unit_metrics <- sf::st_read("~/Downloads/input_metrics_EMaui_2025.gpkg") |>
    sf::st_drop_geometry() |>
    dplyr::select(PU_num, area_km2, elev_mean) |>
    data.table::as.data.table()



  # unit_metrics[, .SD, .SDcols = c("unit_id", "area_km2")]
  colnames(unit_metrics)[1] <- "unit_id"
  unit_metrics[, unit_id := as.character(unit_id)]
  unit_metrics[, (intersect(colnames(solutions)[-1], colnames(unit_metrics))) := NULL]

  solutions <- solutions[unit_metrics, on = "unit_id"][!is.na(solution)]

  summary <- solutions |>
    fgroup_by(solution) |>
    fsummarise(
      units = jsonlite::toJSON(unit_id),
      unit_count = fnobs(solution),
      total_area = fsum(area_km2),
      mean_elev = fmean(elev_mean)
    )

  write.csv(summary, file.path(dirname(solution_dir), "solutions_summary2.csv"), row.names = FALSE)
}
