# solution summary functions
#' Planning unit count for each solution in the optimTFE algorithm output
#'
#' @param solution_output output from the optimTFE_algorithm, as an object in the
#'                        R environment
#'
#' @return Returns a data.frame with two columns; solution (solution number) and
#' PU_ct (number of planning units in the solution)
#' @export

solutions_unit_ct <- function(solution_output) {
  PU_ct_sols <- solution_output |>
    group_by(solution) |>
    summarise(PU_ct = n())
  message(glue::glue("Solution units range {paste(range(PU_ct_sols$PU_ct), collapse = '-')}"))

  PU_ct_sols |>
    filter(PU_ct == min(PU_ct_sols$PU_ct)) |>
    pull(solution)

  return(PU_ct_sols)
}

#' Plot unit count frequencies
#'
#' @param pu_count_by_solution_df
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

#' Plot frequency distribution of PU counts across solutions
#'
#' @param PU_count_freq_df
#'
#' @return Plot the distribution frequency of PU counts
#' @export
#'
plot_solutions_freq <- function(PU_count_freq_df){
  plot <- ggplot(PU_count_freq_df, aes(x = PU_ct, y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(panel.background = element_blank()) +
  labs(title = "Distribution of solution planning unit count frequencies",
       x = "Unit count",
       y = "Frequency") +
  theme(plot.title = element_text(hjust=0.5))+
  geom_text(aes(label = Frequency), vjust = -0.5, size = 3)
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
#' @param solution_output solution output object from optimTFE algorithm
#' @param solution_number solution number for which to generate values
#' @param meta_filepath solutions.meta file from optimTFE algorithm; saved with
#' the solution output
#'
#' @return Dataframe with suitability values by each input feature
#' @export
#'
solution_suit_values <- function(solution_output, solution_number, meta_filepath) {
  print(meta_filepath)
  suitability <- jsonlite::fromJSON(meta_filepath)$suitability
  sol_subset <- solution_output |>
    filter(solution_output$solution==solution_number)
  sol_subset <- sol_subset |>
    select(unit_id, solution, select_order) %>%
    left_join(., suitability, by = c("unit_id" = colnames(suitability)[1]))
  print(mean(as.matrix(sol_subset[,4:ncol(sol_subset)]), na.rm = T))
  return(sol_subset)
}
