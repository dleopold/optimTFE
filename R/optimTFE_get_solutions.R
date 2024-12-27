#' Find one solution based on input parameters; internal to optimTFE_algorithm.
#'
#' @param idx index of solution to fetch
#' @param spp_suit matrix of suitability scores for each species in each planning unit (PU)
#' @param spp_names Vector of species names
#' @param spp_pops matrix of known population for each species in each unit
#' @param spp_goals named list of the number of occurrences to select for each
#'   species
#' @param max_candidate_units maximum number of candidate units to consider for a
#'   species at each round of selection
#' @param rand_tolerance the range of species richness, from maximum, to
#'  consider for selection at each iteration
#' @param max_spp_selected maximum number of species to select in each location
#' @param prioritize_known_pops maximum number of species to select in each location
#' @param single_pu_pop only one location (ie unit) selected per delineated
#'   population (only when prioritize_known_pops = TRUE)
#'
#' @import data.table
#' @export
#'
get_solution <- function(
    idx,
    goals,
    # Config parameters
    max_candidate_units,
    rand_tolerance,
    max_spp_selected,
    prioritize_known_pops,
    single_pu_pop) {
  # Use collapse for faster data manipulations
  suppressPackageStartupMessages({
    require(data.table)
    suppressWarnings({
      require(collapse)
    })
  })

  spp_names <- funique(goals$species)
  solution <- list()
  i <- 1
  while (nrow(goals) > 0) {
    # Filter based on populations
    if (prioritize_known_pops) {
      goals_summary <- goals |>
        fgroup_by(species, region) |>
        fsummarise(
          total = ffirst(total),
          min = ffirst(min),
          max = ffirst(max),
          populations = fndistinct(population)
        ) |>
        # limit to pops within a region if #pop >= max possible selections
        fmutate(
          pop_limit = populations >= max
        ) |>
        # limit to pops globally when # pops >= total remaining
        fgroup_by(species) |>
        fmutate(
          pop_limit = pop_limit | fsum(populations) >= ffirst(total)
        ) |>
        # ensure we don't remove units from regions when min needed > # pops
        fungroup()
      goals_summary[, pop_limit := fcase(
        min <= populations, pop_limit,
        default = FALSE
      )]
      goals <- goals[fselect(goals_summary, species, region, pop_limit), on = .(species, region)] |>
        fsubset(
          !pop_limit | (pop_limit & !is.na(population))
        ) |>
        fselect(-pop_limit)
    }

    # Apply max # top candidate units to consider for each species (but don't
    # exclude known populations)
    if (max_candidate_units < Inf) {
      # shuffle to prevent input order effects
      goals[, random_order := frank(.I, ties.method = "random"), by = species]
      setorder(goals, species, random_order)
      goals[, random_order := NULL]
      setorder(goals, species, -suitability)

      goals <- goals |>
        fgroup_by(species) |>
        fmutate(
          consider = seq_along(unit_id) <= max_candidate_units | !is.na(population)
        ) |>
        fungroup()
    }

    # Select Unit
    unit_summary <- goals |>
      fsubset(consider == TRUE) |>
      fgroup_by(unit_id) |>
      fsummarise(
        richness = fndistinct(species),
        mean_suitability = fmean(suitability),
        region = ffirst(region)
      ) |>
      # Limit options based on richness / tolerance
      fsubset(
        richness > (fmax(richness) - rand_tolerance)
      )
    # Random weighted sample
    sel <- which.max(runif(fnrow(unit_summary)^(1 / unit_summary$mean_suitability)))
    selected_unit <- unit_summary$unit_id[sel]
    selected_region <- unit_summary$region[sel]

    # Find selected species
    if (max_spp_selected > Inf) {
      selected_spp <- goals |>
        fsubset(unit_id == selected_unit & consider == TRUE)
      setorder(selected_spp, -suitability)
      selected_spp[, row_n := .I]
      selected_spp <- selected_spp[row_n <= max_spp_selected | !is.na(population) | n_units <= total, species]
    } else {
      selected_spp <- goals[unit_id == selected_unit, species]
    }

    # Zero-out locations associated with additional part of delineated
    # populations if just selected.
    if (prioritize_known_pops && single_pu_pop) {
      selected_pops <- goals[unit_id == selected_unit & species %in% selected_spp & !is.na(population), species, population]
      if (fnrow(selected_pops) > 0L) {
        setkey(selected_pops, species, population)
        goals <- goals[!selected_pops, on = .(species, population)]
      }
    }

    # Update remaining spp goals
    goals[
      species %in% selected_spp,
      `:=`(
        total = total - 1,
        min = ifelse(region == selected_region & min > 0, min - 1, min)
      )
    ]
    required_remaining <- goals[, .(required = first(min)), by = .(species, region)][
      , .(required = sum(required)),
      by = species
    ]
    goals[required_remaining, required := i.required, on = "species"]
    goals[, max := total - required + min, ]
    goals <- goals[goals[, .I[max > 0 & unit_id != selected_unit]]][
      , n_units := .N,
      by = species
    ]

    # Add selected planning unit to solution
    solution[[selected_unit]] <- sapply(spp_names, function(x) as.numeric(x %in% selected_spp))
    i <- i + 1
  }

  # Return final solution
  dplyr::bind_rows(solution, .id = "unit_id") |>
    fmutate(
      solution = idx,
      select_order = seq_along(solution)
    ) |>
    colorder(unit_id, solution, select_order)
}
