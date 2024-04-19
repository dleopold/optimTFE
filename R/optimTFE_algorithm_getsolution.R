#' Find one solution based on input parameters; internal to optimTFE_algorithm.
#'
#' @param idx index of solution to fetch
#' @param spp_suit matrix of suitability scores for each species in each unit (pu)
#' @param spp_names Vector of species names
#' @param spp_pops matrix of known population for each species in each unit
#' @param spp_occ matrix of known occurrence for each species in each unit
#' @param spp_goals named list of the number of occurrences to select for each
#'   species
#' @param max_candidate_pops maximum number of candidate units to consider for a
#'   species at each round of selection
#' @param rand_tolerance the range of species richness, from maximum, to
#'  consider for selection at each iteration
#' @param max_spp_selected maximum number of species to select in each location
#' @param prioritize_known_pops maximum number of species to select in each location
#' @param prioritize_known_occ should known occurrence locations be prioritized
#' @param single_pu_pop only one location (ie unit) selected per delineated
#'   population (only when prioritize_known_pops = TRUE)
#'
#' @export
#'
get_solution <- function(
    idx,
    goals,
    # Config parameters
    max_candidate_pops,
    rand_tolerance,
    max_spp_selected,
    prioritize_known_pops,
    prioritize_known_occ,
    single_pu_pop) {

  # Use collapse for faster data manipulations
  suppressMessages({
    require(dplyr)
    require(collapse)
    set_collapse(mask = "manip")
  })

  spp_names <- unique(goals$species)

  solution <- list()

  i <- 1
  while (nrow(goals) > 0) {

    # TODO add early stopping criteria (escape hatch)

    # Filter based on population and occurrence data
    if(prioritize_known_pops | prioritize_known_occ){
      goals_summary <- goals |>
        group_by(species, region) |>
        summarise(
          total = unique(total),
          min = unique(min),
          max = unique(max),
          populations = sum(!is.na(unique(population))),
          occurrences = sum(occurrence, na.rm = T)
          # .by = c("species", "region")
        ) |>
        group_by(species) |>
        mutate(
          pop_limit = sum(populations) > max(total),
          occ_limit = !pop_limit & sum(occurrences) > max(total)
          # .by = "species"
        ) |>
        ungroup() |>
        mutate(
          pop_limit = dplyr::case_when(
            min <= populations ~ pop_limit,
            T ~ FALSE
          ),
          occ_limit = dplyr::case_when(
            min <= occurrences ~ occ_limit,
            T ~ FALSE
          )
        )
      goals <- goals |>
        left_join(
          goals_summary |>
            select(species, region, pop_limit, occ_limit),
          by = c("species", "region")
        ) |>
        filter(
          (!pop_limit & !occ_limit) |
            (pop_limit & !is.na(population)) |
            (occ_limit & !is.na(population) & !is.na(occurrence))
        ) |>
        select(-pop_limit, -occ_limit)
    }

    # Apply max # candidate populations
    goals <- goals |>
      arrange(
        species, desc(suitability)
      ) |>
      dplyr::mutate(
        consider = dplyr::row_number() <= max_candidate_pops,
        species_units = dplyr::n(),
        .by = "species"
      )

    # Select Unit
    unit_summary <- goals |>
      filter(consider == TRUE) |> # Only consider top max_candidate_pops
      group_by(unit_id) |>
      summarise(
        richness = length(unique(species)),
        mean_suitability = mean(suitability)
        # .by = "unit_id"
      ) |>
      # Limit options based on richness / tolerance
      filter(
        richness > (max(richness) - rand_tolerance)
      )
    selected_unit <- sample(unit_summary$unit_id, 1, prob = unit_summary$mean_suitability)

    # Find selected species
    selected_spp <-  goals |>
      filter(unit_id == selected_unit & consider == TRUE) |>
      arrange(
        desc(suitability)
      ) |>
      # Apply max # species selected (prevent species packing)
      # But always include prioritized populations / occurrences
      # Also always include species when too few units would remain otherwise
      filter(
        (row_number() <= max_spp_selected) | !is.na(population) | !is.na(occurrence) | species_units <= total
      ) |>
      pull(species)

    # Zero-out locations associated with additional part of delineated
    # populations if just selected.
    if (single_pu_pop) {
      goals <- goals |>
        dplyr::anti_join(
          {
            goals |>
              filter(unit_id == selected_unit & consider == TRUE & !is.na(population)) |>
              select(species, population)
          }, by = c("species", "population")
        )
    }

    # Update remaining spp goals
    goals <- goals |>
      dplyr::mutate(
        max = dplyr::case_when(
          species %in% selected_spp ~ max - 1,
          T ~ max
        ),
      ) |>
      dplyr::filter(
        unit_id != selected_unit & max > 0
      )

    # Add selected PU to solution
    solution[[selected_unit]] <- sapply(spp_names, function(x) as.numeric(x %in% selected_spp))
    i <- i + 1
  }

  # Return final solution
  dplyr::bind_rows(solution, .id = "unit_id") |>
    dplyr::mutate(
      solution = idx,
      select_order = dplyr::row_number(),
      .after = "unit_id"
    )

}
