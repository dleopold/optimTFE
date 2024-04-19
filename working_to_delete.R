library(greedyOpt)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(prioritizr)
library(prioritizrdata)
library(vegan)
library(cluster)
library(magrittr)
library(tidyverse)
library(tibble)
library(topsis)
library(withr)
library(stars)

# create spp suitability from spatial data, make sure PU column is first!
spp_suit <- read_sf("dev/units_spp_suit.gpkg")
spp_suit_df <- spp_suit |>
  as.data.frame() |>
  select(-geom)
#  relocate("pu_id") # must be first column in all datasets in which it occurs
write.csv(spp_suit_df, "./spp_suitability.csv", row.names = FALSE)

## read in files if you want to visually assess; not needed
# spp_suit <- read.csv("spp_suitability.csv")
# # read in targets file
# spp_targets_subregions <- read.csv("dev/targets.csv")

# read in spatial data with PU id's and background files
pu_data <- read_sf("./dev/unit_spp_suit.gpkg")
pu_data <- pu_data |>
  select(-c(2:37))
# plot(pu_data)

# try simple greedy algo, no known pops or occurences yet
# input files must be .csv format; supply filepath
test1 <- greedyOpt::greedyOpt(dir = "C:/Christina_learning_2022/greedyOpt_testing/",
                              spp_targets_fn = "C:/Christina_learning_2022/greedyOpt_testing/testData/targets.csv",
                              spp_suit_fn = "C:/Christina_learning_2022/greedyOpt_testing/testData/spp_suitability.csv",
                              #spp_occ_fn = "C:/Christina_learning_2022/greedyOpt_testing/testData/known_occurrences.csv",
                              rand_tolerance = 5,
                              min_spp_suit_score = 0.1,
                              sub_regions_fn = "C:/Christina_learning_2022/greedyOpt_testing/testData/subregions.csv",
                              n = 500,
                              cores = 6,
                              output_csv = T,
                              return_df = T)

## Get unit_id count for all solutions in the output data.fram, and print range
# read in solution
sol1_result <- read.csv("greedyOpt_testing/output/solutions.csv")

solutions_unit_ct <- function(solution_output) {
  PU_ct_sol1 <- solution_output |>
    group_by(solution) |>
    summarise(PU_ct = n())
  message(glue::glue("Solution units range {paste(range(PU_ct_sol1$PU_ct), collapse = '-')}"))

  PU_ct_sol1 |>
    filter(PU_ct == min(PU_ct_sol1$PU_ct)) |>
    pull(solution)

  return(PU_ct_sol1)
}
get_mins <- solutions_unit_ct(sol1_result)

## make a distribution plot of PU_counts and return the dataframe of unit count frequencies
#' Title
#'
#' @param pu_count_by_solution_df
#'
#' @return
#' @export
#'
#' @examples
all_solutions_footprint_size <- function(pu_count_by_solution_df){
  PU_count_freq <- table(pu_count_by_solution_df$PU_ct) / length(pu_count_by_solution_df$PU_ct)
  PU_count_freq_df <- as.data.frame(PU_count_freq)
  names(PU_count_freq_df) <- c("PU_ct", "Frequency")

  plot <- ggplot(PU_count_freq_df, aes(x = PU_ct, y = Frequency)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme(panel.background = element_blank()) +
    labs(title = "Distribution of solution planning unit count frequencies",
         x = "unit count",
         y = "Frequency") +
    theme(plot.title = element_text(hjust=0.5))+
    geom_text(aes(label = Frequency), vjust = -0.5, size = 3)
  print(plot)
  return(PU_count_freq_df)
}
# run function
PU_ct_freq_plot <- all_solutions_footprint_size(get_mins)

############
# plot and save unit frequency map across all solutions

all_PUs_by_solct <- sol1_result |>
  filter(solution > 0) |>
  group_by(unit_id) |>
  summarise(count = n())
# Join solution PU_counts with pu_data to include all PUs considered
PU_freq <- pu_data |>
  left_join(all_PUs_by_solct, by = c("PU_num" = "unit_id")) |>
  mutate(count = ifelse(is.na(count), 0, count))

# Calculate the proportion frequency of occurrence for each unique PU_num
PU_freq <- PU_freq %>%
  mutate(total_solutions = n_distinct(sol1_result$solution),
         freq = count / total_solutions)
ggplot(data = PU_freq) +
  geom_sf(aes(fill = freq)) +
  scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 1), name = "Frequency") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  labs(title = "Frequency of planning unit inclusion across all solutions") +
  theme(plot.title = element_text(hjust = 0.5))



############################
# pull out a solution with min PU count and plot

sol1 <- left_join(pu_data, sol1_result, by = c("PU_num" = "unit_id")) |>
  filter(!is.na(solution))
# sol1 obs count should match sol1_result obs count

# get solutions with minimum unit_id counts; find from 'solutions_unit_ct' output df
sol141 <- subset(sol1, solution==141)
# plot solution 141
ggplot() +
  geom_sf(data = mn_coastline, color = "blue") +
  geom_sf(data = sol141, aes(fill = solution), alpha = 0.5) +
  theme_minimal()

# now get suitability values across all species

# you must read in the solutions file as a data.frame; solution number is the solution to plot; meta_filepath is the filepath to the solutions.meta file
# this returns a data.frame with unit_id, solution number, selection order of the unit_id, and species suitability values. All 0s are NAs unless you convert them.
selected_solution_suitability_values <- function(all_solutions_df, solution_number, meta_filepath) {
  suitability <- jsonlite::fromJSON(meta_filepath)$suitability
  sol_subset <- all_solutions_df |>
    filter(all_solutions_df$solution==solution_number)
  sol_subset <- sol_subset |>
    select(unit_id, solution, select_order) %>%
    left_join(., suitability, by = c("unit_id" = colnames(suitability)[1]))
  return(sol_subset)
}
# example of function:
test <- selected_solution_suitability_values(sol1_result, 141, "C:/Christina_learning_2022/greedyOpt_testing/output/solutions.meta")

# now do calcs
# get mean suitability across all species (0s not counted). Must list columns species occur within. If using helper function 'selected_solution_suitability_values' it begins with col 4 and will be last column in the function's output data.frame
mean(as.matrix(test[,4:39]), na.rm = T)
# [1] 0.5457114

# get suitability values for each species selected for the PU only (when a species is 0 for a given PU, it's not included)
# go back to dataframe with the single solution of interest with 1 and 0 indicating units for which they were selected; here it's sol141

# remove everything excepting unit_id column, which must be first, and all targets (values = 0 or 1 only from solution)
# take a look; could be different based on whether you added spatial data and associated columns; unit_id must be first
sol141_spp_sol <- sol141|>
  as.data.frame(.) %>%
  select(., -c(2:4))

# make a function
feature_suit_selected_units <- function(solution, meta_filepath){
  sol_long <- solution |>
    pivot_longer(-all_of(names(solution)[1]), names_to = "Species", values_to = "Suitability")
  sol_long$Suitability[sol_long$Suitability == 0] <- NA
  # double-check just 1s (selected for the unit, and NAs, not selected for but could occur within the unit)
  # unique(sol141_long$Suitability)
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

selected_spp_suit_values141 <- feature_suit_selected_units(sol141_spp_sol, "C:/Christina_learning_2022/greedyOpt_testing/output/solutions.meta")

### now let's generate some stats about solutions
# read in variables that are aggregated at the unit_id level; again, unit_id must be first column
# dataset must be read-in for use as a data.frame
PU_stats <- read_sf("./greedyOpt_testing/testData/solutions_matrix_20220301_21.gpkg") |>
  as.data.frame()
# get rid of anything unneccessary and keep track of order/naming
PU_stats_eco_data <- PU_stats |>
  select(1, 3:25, 315)
# will merge to get all summary data and solution info, and spatial data
PU_stats_sol141 <- merge(sol141, PU_stats_eco_data, by = "PU_num")

## do this for all solutions so can pull top solutions across metrics
# for this one, only keep unit_id and solution as columns 1 and 2, and columns for getting summary stats (elevation, area, etc.)
# in function sum_stat_means include the cleaned-up dataframe "solutions_means_prep"

sol1_all_stats <- merge(sol1, PU_stats_eco_data, by = "PU_num")
# only keep unit_id and solution as columns 1 and 2, and columns for getting summary stats (elevation, area, etc.)
all_sols_prep <- as.data.frame(sol1_all_stats) |>
  select(-c(3:40)) |>
  select(-geometry)
# check no geometry or geom column
all_sols_stats <- all_sols_prep |>
  group_by(solution) |>
  summarise(
    total_area_kmsq = sum(area_km2, na.rm = TRUE),
    total_good_hab_kmsq = sum(good_hab_kmsq, na.rm = TRUE),
    mean_good_hab_pct = (sum(good_hab_kmsq, na.rm = TRUE) / sum(area_km2, na.rm = TRUE)) * 100
  )

#
# this only gets means at the PU level - doesn't tell you about total footprint area, etc.
# sum_stat_means <- function(solutions_prep){
#   mean_values <- solutions_prep |>
#   group_by(solutions_prep[[2]]) |>
#   summarise(across(-1, mean, na.rm = TRUE)) |>
#   select(-1)
#   return(mean_values)
#   }
# example
# test_sum_stat_means <- sum_stat_means(all_sols_means_prep)

# now identify summaries you want by solution. Example uses output from sum_stat_means function.
min_max_solutions <- function(data, metrics, summary_type = "max") {
  # Check if extreme_type is either "max" or "min"
  if (!(summary_type %in% c("max", "min"))) {
    stop("summary_type must be either 'max' or 'min'")
  }

  # Group by solution and find extreme values for each metric
  summary_values <- data %>%
    group_by(solution) %>%
    summarise(across(all_of(metrics), .fns = if (summary_type == "max") max else min)) %>%
    ungroup()

  # Empty dataframe to store summary solution rows
  summary_df <- data[0, ]

  # Loop through each metric
  for (metric in metrics) {
    # Extract the solution IDs corresponding to the summary value for this metric
    if (summary_type == "max") {
      summary_solution_ids <- summary_values$solution[which.max(summary_values[[metric]])]
    } else {
      summary_solution_ids <- summary_values$solution[which.min(summary_values[[metric]])]
    }

    # Filter the original data to get rows corresponding to summary solution IDs
    summary_df <- bind_rows(summary_df, data %>% filter(solution %in% summary_solution_ids))
  }
  return(summary_df)
}
metrics <- colnames(all_sols_stats)[2:4]
min_rows <- min_max_solutions(all_sols_stats, metrics, summary_type = "min")
max_rows <- min_max_solutions(all_sols_stats, metrics, summary_type = "max")
# Combine min and max dataframes; remove columns from either first if desired
output_df <- bind_rows(max_rows, min_rows)


## now try to plot a top solution
# Identify solution to plot where we show where the solution falls compared to the rest of the values across all solutions, and the top 10%
# This just pulls the 2 values for good_hab_kmsq, not pulling from solution later on

solution_value <- all_sols_stats$total_good_hab_kmsq[all_sols_stats$solution == 25]
variable_assessed <- all_sols_stats$total_good_hab_kmsq
top_10_percent <- quantile(all_sols_stats$total_good_hab_kmsq, 0.9)

# Plot
ggplot(all_sols_stats, aes(x = total_good_hab_kmsq)) +
  geom_histogram(aes(fill = ..x.. > top_10_percent), color = "black", bins = 33) +
  scale_fill_manual(values = c("gray80", "skyblue"),
                    labels = c("Solutions", "Top 10%"),
                    name = "") +
  geom_vline(xintercept = solution_value, color = "red", linetype = "dashed") +
  labs(title = "Distribution of good_hab_kmsq",
       x = "good_hab_kmsq",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #  theme_minimal() + # overridden by below them stuff, added transparent below
  theme(panel.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

vars <- colnames(all_sols_stats)[2:4] # same as metrics here
sol_to_plot <- all_sols_stats$total_good_hab_kmsq[all_sols_stats$solution == 100]

all_long <- all_sols_stats |>
  dplyr::select(solution, dplyr::all_of(vars)) |>
  tidyr::pivot_longer(-solution, names_to = "stat", values_to = "value") |>
  mutate(d = "All Solutions")
top_10_pct <- all_long |>
  dplyr::filter(
    value > quantile(value, 0.9),
    .by = "stat"
  ) |>
  mutate(d = "Top 10%")
all_long_selected_sol <- all_long |>
  filter(solution == 100)
ggplot(bind_rows(all_long, top_10_pct)) +
  aes(x = value, fill = d) +
  geom_histogram(color = "black", bins = 33) +
  scale_fill_manual(values = c("gray80", "skyblue"),
                    labels = c("Solutions", "Top 10%"),
                    name = "") +
  geom_vline(data = all_long_selected_sol, aes(xintercept = value), color = "red", linetype = "dashed") +
  facet_wrap(~stat, scales = "free", ncol=3) +
  ggthemes::theme_few()




