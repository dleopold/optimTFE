#' #' Find one solution based on input parameters; internal to optimTFE_algorithm.
#' #'
#' #' @param idx index of solution to fetch
#' #' @param spp_suit matrix of suitability scores for each species in each planning unit (PU)
#' #' @param spp_names Vector of species names
#' #' @param spp_pops matrix of known population for each species in each unit
#' #' @param spp_goals named list of the number of occurrences to select for each
#' #'   species
#' #' @param max_candidate_units maximum number of candidate units to consider for a
#' #'   species at each round of selection
#' #' @param rand_tolerance the range of species richness, from maximum, to
#' #'  consider for selection at each iteration
#' #' @param max_spp_selected maximum number of species to select in each location
#' #' @param prioritize_known_pops maximum number of species to select in each location
#' #' @param single_pu_pop only one location (ie unit) selected per delineated
#' #'   population (only when prioritize_known_pops = TRUE)
#' #'
#' #' @export
#' #'
#' get_solution <- function(
#'     idx,
#'     goals,
#'     # Config parameters
#'     max_candidate_units,
#'     rand_tolerance,
#'     max_spp_selected,
#'     prioritize_known_pops,
#'     single_pu_pop) {
#'
#'   # Use collapse for faster data manipulations
#'   suppressPackageStartupMessages({
#'     require(dplyr)
#'     suppressWarnings({
#'       require(collapse)
#'     })
#'     set_collapse(mask = "manip")
#'   })
#'
#'   spp_names <- funique(goals$species)
#'   solution <- list()
#'   i <- 1
#'   while (nrow(goals) > 0 ) {
#'
#'     # Filter based on populations
#'     if(prioritize_known_pops){
#'       goals_summary <- goals |>
#'         group_by(species, region) |>
#'         summarise(
#'           total = ffirst(total),
#'           min = ffirst(min),
#'           max = ffirst(max),
#'           populations = fndistinct(population)
#'         ) |>
#'         # limit to pops within a region if #pop >= max possible selections
#'         mutate(
#'           pop_limit = populations >= max
#'         ) |>
#'         #limit to pops globally when # pops >= total remaining
#'         group_by(species) |>
#'         mutate(
#'           pop_limit = pop_limit | fsum(populations) >= ffirst(total)
#'         ) |>
#'         # ensure we don't remove units from regions when min needed > # pops
#'         ungroup() |>
#'         mutate(
#'           pop_limit = dplyr::case_when(
#'             min <= populations ~ pop_limit,
#'             T ~ FALSE
#'           )
#'         )
#'       goals <- goals |>
#'         left_join(
#'           goals_summary |>
#'             select(species, region, pop_limit),
#'           by = c("species", "region")
#'         ) |>
#'         filter(
#'           !pop_limit | (pop_limit & !is.na(population))
#'         ) |>
#'         select(-pop_limit)
#'     }
#'
#'     # Apply max # top candidate units to consider for each species (but don't
#'     # exclude known populations)
#'     if(max_candidate_units < Inf){
#'       goals <- goals |>
#'         dplyr::slice_sample(by = species, prop = 1) |> # prevent input order effects
#'         arrange(
#'           species, desc(suitability)
#'         ) |>
#'         group_by(species) |>
#'         mutate(
#'           consider = seq_along(unit_id) <= max_candidate_units | !is.na(population)
#'         ) |>
#'         ungroup()
#'     }
#'
#'     # Select Unit
#'     unit_summary <- goals |>
#'       filter(consider == TRUE) |> # Only consider top max_candidate_units
#'       group_by(unit_id) |>
#'       summarise(
#'         richness = fndistinct(species),
#'         mean_suitability = fmean(suitability),
#'         region = ffirst(region)
#'       ) |>
#'       # Limit options based on richness / tolerance
#'       filter(
#'         richness > (fmax(richness) - rand_tolerance)
#'       )
#'     sel <- which.max(runif(fnrow(unit_summary)^(1/unit_summary$mean_suitability)))
#'     selected_unit <- unit_summary$unit_id[sel]
#'     selected_region <- unit_summary$region[sel]
#'
#'     # Find selected species
#'     selected_spp <-  goals |>
#'       filter(unit_id == selected_unit & consider == TRUE) |>
#'       arrange(
#'         desc(suitability)
#'       ) |>
#'       # Apply max # species selected (prevent species packing)
#'       # But always include prioritized populations
#'       # Also always include species when too few units would remain otherwise
#'       filter(
#'         (row_number() <= max_spp_selected) | !is.na(population) | n_units <= total
#'       ) |>
#'       pull(species)
#'
#'     # Zero-out locations associated with additional part of delineated
#'     # populations if just selected.
#'     if (prioritize_known_pops && single_pu_pop) {
#'       goals <- goals |>
#'         dplyr::anti_join(
#'           {
#'             goals |>
#'               filter(unit_id == selected_unit & species %in% selected_spp & !is.na(population)) |>
#'               select(species, population)
#'           }, by = c("species", "population")
#'         )
#'     }
#'
#'     # Update remaining spp goals
#'     goals <- bind_rows(
#'       filter(goals, !species %in% selected_spp),
#'       filter(goals, species %in% selected_spp) |>
#'         tidyr::nest(data=-species) |>
#'         purrr::pmap_dfr(function(species, data){
#'           # Update species total remaining and local min
#'           data <- data |>
#'             mutate(
#'               total = total - 1,
#'               min = case_when(
#'                 region == selected_region ~ pmax(min - 1, 0),
#'                 .default = min
#'               )
#'             )
#'           # Update regional max
#'           data |>
#'             group_by(region) |>
#'             summarise(required = ffirst(min)) |>
#'             ungroup() |>
#'             summarise(required = fsum(required)) |>
#'             cross_join(data) |>
#'             mutate(
#'               species = species,
#'               n_units = fnrow(data),
#'               max = total - required + min
#'             ) |>
#'             select(-required)
#'         })
#'     ) |>
#'       filter(
#'         max > 0 & unit_id != selected_unit
#'       )
#'
#'     # Add selected PU to solution
#'     solution[[selected_unit]] <- sapply(spp_names, function(x) as.numeric(x %in% selected_spp))
#'     i <- i + 1
#'   }
#'
#'   # Return final solution
#'   dplyr::bind_rows(solution, .id = "unit_id") |>
#'     dplyr::mutate(
#'       solution = idx,
#'       select_order = dplyr::row_number(),
#'       .after = "unit_id"
#'     )
#'
#' }
#' #
