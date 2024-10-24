# solution summary functions

#' Number of unique and identical solutions within the optimTFE algorithm output
#'
#' @param solution_output output from the optimTFE_algorithm, as an object in the
#'  R environment
#'
#' @return Returns a dataframe with the count and list of solutions that are identical;
#' also includes the planning unit (PU) combinations for each unique solution.
#' @export
#'

unique_solution_ct <- function(solution_output) {
  unique_sols <- solution_output |>
  group_by(solution) |>
  summarise(unit_id_combination = paste(sort(unique(unit_id)), collapse = ",")) |>
  group_by(unit_id_combination) |>
  summarise(
    solution = paste(solution, collapse = ", "),
    count = n()
  ) |>
  arrange(desc(count))
total_solutions <- sum(unique_sols$count)
unique_sols_final <- unique_sols |>
  mutate(proportion = count / total_solutions) |>
  relocate(unit_id_combination, .after = last_col())
unique_sols_final <- unique_sols_final |>
  relocate(count, .before = solution)
unique_sols_final <- unique_sols_final |>
  relocate(proportion, .after = count)

# Print the result
  return(unique_sols_final)
}

#' Planning unit count for each solution in the optimTFE algorithm output
#'
#' @param solution_output output from the optimTFE_algorithm, as an object in the
#'  R environment
#'
#' @return Returns a data.frame with two columns; solution (solution number) and
#' PU_ct (number of planning units in the solution)
#' @export

solutions_unit_ct <- function(solution_output) {
  PU_ct_sols <- solution_output |>
    group_by(solution) |>
    summarise(PU_ct = n())
  message(glue::glue("Solution unit count ranges {paste(range(PU_ct_sols$PU_ct), collapse = '-')}"))

  PU_ct_sols |>
    filter(PU_ct == min(PU_ct_sols$PU_ct)) |>
    pull(solution)

  return(PU_ct_sols)
}

#' Plot unit count frequencies across all solutions
#'
#' @param pu_count_by_solution_df dataframe with solution column and unit count
#' per solution column
#'
#' @return Return the data.frame of unit
#' count frequencies
#' @export
#'
all_solutions_freq <- function(pu_count_by_solution_df){
  PU_count_freq <- table(pu_count_by_solution_df$PU_ct) / length(pu_count_by_solution_df$PU_ct)
  PU_count_freq_df <- as.data.frame(PU_count_freq)
  names(PU_count_freq_df) <- c("PU_ct", "Frequency")
  return(PU_count_freq_df)
}

#' Plot frequency distribution of planning unit counts across solutions
#'
#' @param PU_count_freq_df
#'
#' @return Plot the distribution frequency of planning unit counts
#' @export
#'
plot_solutions_freq <- function(PU_count_freq_df){
  # Convert PU_ct to numeric if it's not already
  PU_count_freq_df <- PU_count_freq_df |>
    mutate(PU_ct = as.numeric(as.character(PU_ct)))

  # added max 10 breaks on x-axis
  x_range <- range(PU_count_freq_df$PU_ct, na.rm = TRUE)
  x_breaks <- unique(pretty(x_range, n = 10))

  plot <- ggplot(PU_count_freq_df, aes(x = factor(PU_ct), y = Frequency)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    #    theme_minimal() +
    theme(
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = "Planning unit counts across all solutions",
      x = "Planning unit count",
      y = "Frequency"
    ) +
    scale_x_discrete(breaks = x_breaks, labels = as.character(x_breaks))
  print(plot)
}


#' Map of planning unit inclusion frequency across all solutions
#'
#' @param solution_output solution output object from optimTFE algorithm
#' @param pu_spatial_data spatial data associated with planning units; sf object
#'
#' @return Map: inclusion of planning units across a solution
#' @export
#'
pu_ct_freq_map <- function(solution_output, pu_spatial_data){
  all_PUs_by_solct <- solution_output |>
    filter(solution > 0) |>
    group_by(unit_id) |>
    summarise(count = n())
  # Join solution PU_counts with pu_data to include all PUs considered
  PU_freq <- pu_spatial_data |>
    left_join(all_PUs_by_solct, by = c("PU_num" = "unit_id")) |>
    mutate(count = ifelse(is.na(count), 0, count))

  # Calculate the proportion frequency of occurrence for each unique PU_num
  PU_freq <- PU_freq %>%
    mutate(total_solutions = n_distinct(solution_output$solution),
           freq = count / total_solutions)
  freq_map <- ggplot(data = PU_freq) +
    geom_sf(aes(fill = freq)) +
    scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 1), name = "Frequency") +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    ) +
    labs(title = "Frequency of planning unit inclusion across all solutions") +
    theme(plot.title = element_text(hjust = 0.5))
  print(freq_map)
}

#' Generate suitability value summaries for a solution
#'
#' @param solution_number solution number for which to generate values
#' @param solution_output solution output object from optimTFE algorithm
#' @param meta_filepath solutions.meta file from optimTFE algorithm; saved with
#' the solution output
#'
#' @return Dataframe with suitability values by each input feature including columns:
#' unit_id, solution, select_order, feature suitability columns.
#' @export
#'
solution_suit_values <- function(solution_number, solution_output, meta_filepath) {
  suitability <- jsonlite::fromJSON(meta_filepath)$suitability
  sol_subset <- solution_output |>
    filter(solution_output$solution==solution_number)
  sol_subset <- sol_subset |>
    select(unit_id, solution, select_order) %>%
    left_join(., suitability, by = c("unit_id" = colnames(suitability)[1]))
  cat("Solution", solution_number, "has a mean suitability value of",
      mean(as.matrix(sol_subset[,4:ncol(sol_subset)]), na.rm = T),
      "across all feature inputs.\n")
  return(sol_subset)
}

#' Single solution map
#'
#' @param solution_number solution number to plot
#' @param solution_output output from optimTFE algorithm, as data.frame object
#' @param pu_data sf object with first column the unit_id or planning unit number
#' and second column the geom; no other data.
#'
#' @return Map of the selected solution; a potential conservation footprint
#' @export

single_solution_map <- function(solution_number, solution_output, pu_data){

  sol_to_plot <- subset(solution_output, solution==solution_number)
  pu_data <- pu_data |>
    rename(unit_id = names(pu_data)[1])
  sol_sp_data <-
    left_join(pu_data, sol_to_plot, by = "unit_id")
  sol_sp_data_map <- ggplot() +
    geom_sf(data = sol_sp_data, aes(fill = solution), alpha = 0.8) +

    scale_fill_gradient(low = "royalblue", high = "royalblue", na.value = "gray93") +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    ) +
    labs(title = paste("Solution", solution_number, "conservation footprint")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(sol_sp_data_map)
}

#' Select solution: suitability values for 'picked' features
#'
#' @param solution_number Solution number to calculate suitability value
#' @param solution_output output from optimTFE algorithm, as data.frame object
#' @param meta_filepath solutions.meta file from optimTFE algorithm; saved with
#' the solution output
#'
#' @return Suitability value across selected features for a solution
#' @export
#'
selected_spp_suit <- function(solution_number, solution_output, meta_filepath){
  sol_subset <- subset(solution_output, solution==solution_number) |>
    select(-c(2:3))
  sol_long <- sol_subset |>
    pivot_longer(-all_of(names(sol_subset)[1]), names_to = "Species", values_to = "Suitability")
  sol_long$Suitability[sol_long$Suitability == 0] <- NA
  # double-check just 1s (selected for the unit, and NAs, not selected for but could occur within the unit)
  # unique(sol_long$Suitability)
  # [1] NA  1

  # now make dataframe of species suitability that's long form
  spp_suitability <- jsonlite::fromJSON(meta_filepath)$suitability
  spp_suit_long <- spp_suitability |>
    pivot_longer(-all_of(names(spp_suitability)[1]), names_to = "Species", values_to = "Suitability")

  # make an index to match by column location, not name
  index_solution <- c(1,2)
  index_suitability <- c(1,2)
  selected_suitability <- merge(sol_long, spp_suit_long, by.x = index_solution, by.y = index_suitability, all.x = TRUE)|>
    filter(Suitability.x ==1) |>
    select(-Suitability.x)
  meansuit_sol_by_species <- selected_suitability |>
    group_by(Species) |>
    summarise(mean_suitability = mean(Suitability.y, na.rm = TRUE))
  # get overall mean suitability for entire footprint
  message(glue::glue("Suitability across selected features = {paste(mean(meansuit_sol_by_species$mean_suitability))}"))
  return(meansuit_sol_by_species)
}

#' Title
#'
#' @param data Dataframe with metrics summarized by solution. See 'all_sols_metric'
#' dataframe in vignette for setup.
#' @param metrics Columns in data that contain metrics to calculate
#' @param summary_type "min" or "max" (minimum or maximum value)
#'
#' @return dataframe with minimum or maximum values across all metrics
#' @export

metrics_top_solutions <- function(data, metrics, summary_type = min) {
  # Group by solution and find extreme values for each metric
  summary_values <- data |>
    summarise(across(all_of(metrics), ~summary_type(., na.rm=T)), .by = "solution")

  which_sols <- summary_values |>
    pivot_longer(all_of(metrics)) |>
    slice(
      which(value==summary_type(value, na.rm=T)),
      .by = "name"
    ) |>
    pull(solution)

  summary_values |>
    filter(solution %in% which_sols)
}

#' Title
#'
#' @param solution_number Solution number to identify in the distribution plot
#' @param solution_metric_df Dataframe with metrics summarized across each solution.
#' See example all_sols_metric in vignette for setup
#' @param metric Metric within solution_metric_df to plot. This must be in in text
#' format, e.g. "minimum_area"
#' @param quantile Optional. Quantile to highlight, e.g. top 10% of solutions
#' then quantile = 0.9
#'
#' @return object, then call object to view ggplot
#' @export
#'
plot_solution_metric <- function(solution_number, solution_metric_df, metric, quantile=NA){
  solution_value <- solution_metric_df[[metric]][solution_metric_df$solution==solution_number]
  top_solutions <- quantile(solution_metric_df[[metric]], quantile)
  solution_metric_df$above_quantile <- solution_metric_df[[metric]] > top_solutions
  # Adjust the y axis ticks
  max_count <- ceiling(max(ggplot_build(ggplot(solution_metric_df, aes(x = .data[[metric]])) +
                                          geom_histogram(bins = 33))$data[[1]]$count))

  # Wrap the second label text
  wrapped_label <- paste0("Top ", quantile * 100, "%\n", metric)
  # Plot
  plot <- ggplot(solution_metric_df, aes(x = .data[[metric]], fill = above_quantile)) +
    geom_histogram(color = "black", bins = 33) +
    scale_fill_manual(values = c("gray80", "skyblue"),
                      labels = c("Solutions", wrapped_label),
                      name = "") +
    geom_vline(xintercept = solution_value, color = "red", linetype = "dashed") +
    labs(title = paste0("Distribution of ", metric),
         x = metric,
         y = "Count") +
    scale_y_continuous(breaks = seq(0, max_count, by = 1),
                       labels = function(x) format(x, scientific = FALSE)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill='transparent'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.text = element_text(size = 8),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

  return(plot)
}
