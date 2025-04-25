#' optimTFE algorithm
#'
#' Generate a suite of conservation footprint 'solutions' based on feature
#' suitability values and a list of targets for all features.
#'
#' The function generates conservation footprints based on species/feature
#' habitat suitability scores within planning units, and a target number of
#' populations per species. A greedy algorithm iteratively selects planning
#' units with the highest number of remaining species targets until all targets
#' are met. To mitigate known pitfalls of richness-based selection at each
#' iteration, stochasticity is introduced where one planning unit is randomly
#' selected from a pool of planning units within a set number of targets of the
#' maximum for that iteration. To maximize species suitability scores in
#' selected units the probability of selection is weighted by the mean
#' suitability scores of remaining targets. Constraints, such as hybridization,
#' can be introduced to specifically prohibit the algorithm from selecting the
#' same planning unit for two taxa. This process is then repeated to generate
#' many spatially efficient solutions that meet all targets for each species.
#'
#' @param dir working directory
#' @param targets species targets file - path to a csv/tsv file or a
#'   pre-loaded data frame. First 2 columns should be species names and total
#'   targets populations, respectively. Additional columns must be provided when
#'   using sub-region targets. If specifying subregion targets, column names
#'   must match the sub-region names provided in `subregions_in` and values
#'   should be the minimum number of populations required in the sub-region.
#' @param suitability_in species suitability matrix - path to a csv/tsv file or
#'   a pre-loaded data frame. The first column must be the planning unit id /
#'   number and following columns are species. Values indicate the suitability
#'   scores for each species / taxon in each planning unit.
#' @param subregions_in (optional) path to a csv/tsv file or a pre-loaded data
#'   frame defining sub-regions within the set of planning units. Must include 1
#'   row for each planning unit with binary values indicating the sub-region
#'   membership of each planning unit. Sub-region column names must match those
#'   in the `targets_in`.
#' @param populations_in (optional) path to a csv/tsv file or a pre-loaded
#'   data frame of delineated populations. Must have columns unit_id, species,
#'   and population. Values in the population column will be interpreted and
#'   unique populations for each species, and values can be repeated across
#'   species (i.e.,  population '1' for species A will not conflict with
#'   population '1' for species B).
#' @param single_pu_pop should only one location (ie unit) be selected per
#'   delineated population (default=TRUE). Only applies if a know population
#'   file (`populations_in`) is provided.
#' @param rand_tolerance  the range of species richness, from maximum, to
#'   consider for selection at each iteration(default = 5)
#' @param max_spp_selected maximum number of species to select in each location
#'   (default = Inf) to reduce 'species packing' within units. This parameter
#'   could cause species to be unable to meet targets.
#' @param min_spp_suit_score minimum suitability score for a species to be
#'   considered in a location (default = 0)
#' @param max_candidate_units maximum number of candidate units to consider for
#'   a species at each round of selection (default = Inf). This will subset
#'   species data by highest suitability score to the number of units listed,
#'   but will not exclude known populations.
#' @param seed seed for reproducible output (optional)
#' @param n number of solutions to generate
#' @param cores number of cores to use (default = all available)
#' @param batch_size when parallel processing is used, this parameter can be
#'   used to process solution in batches for improved efficiency (default =
#'   NULL). If not provided (NULL), batch size will be calculated automatically
#'   as `ceiling(n/(cores*4))`.
#' @param max_batch_size limit batch size for parallel processing (default =
#'   1000). Smaller values will allow the progress bar to update more
#'   frequently, but at the cost of more disk writes.
#' @param min_batch_size default = 100.
#' @param progress show progress bar
#' @param output_prefix prefix for output files (default = 'solutions')
#' @param output_dir location to write outputs (default = `file.path(dir,
#'   'output')`). Created for user if does not exist.
#' @param output_csv Should the solutions be written to a single csv file
#'   (default = TRUE)
#' @param force_overwrite overwrite existing output files (default = FALSE)
#' @param return_df return all generated solutions as a data frame (default =
#'   FALSE)
#'
#' @import progressr
#'
#' @export
#'
optimTFE <- function(
  # Data Inputs
  targets = optimTFE::example_targets,
  suitability = optimTFE::example_suitability,
  subregions = optimTFE::example_subregions,
  populations = optimTFE::example_populations,
  incompatibility = optimTFE::example_incompatibility,
  # Config parameters,
  min_spp_suit_score = 0.25,
  rand_tolerance = 5,
  max_spp_selected = -1,
  single_pu_pop = TRUE,
  # Compute parameters,
  n = 100,
  cores = NULL,
  progress = TRUE,
  batch_size = NULL,
  max_batch_size = 250,
  seed = NULL,
  # Output parameters,
  output_dir = NULL,
  output_prefix = "solutions",
  output_csv = TRUE,
  force_overwrite = FALSE,
  return_df = FALSE
) {
  message("Beginning optimTFE...")

  start_time <- Sys.time()
  output_dir <- output_dir %||% file.path(".", "output")
  cores <- cores %||% future::availableCores()

  # Determine if progress bar should be used
  # if (isTRUE(progress) &&
  #   interactive() &&
  #   !isTRUE(getOption("knitr.in.progress")) &&
  #   !isTRUE(getOption("rstudio.notebook.executing"))) {
  #   progressr::handlers(global = TRUE)
  # } else if (interactive() &&
  #   !isTRUE(getOption("knitr.in.progress")) &&
  #   !isTRUE(getOption("rstudio.notebook.executing"))) {
  #   progressr::handlers(global = FALSE)
  # }

  # Output Files ----
  # if (length(output_prefix) == 0L || nchar(output_prefix) == 0L) {
  #   stop(crayon::bold(crayon::red("output_prefix must be a non-empty string")))
  # }
  # if (dir.exists(file.path(output_dir, output_prefix))) {
  #   if (!force_overwrite) {
  #     stop(crayon::bold(crayon::red(glue::glue("Output with the prefix '{output_prefix}' already exists: "))) |>
  #       paste0(crayon::bgBlue(crayon::white(output_dir)), crayon::bold(crayon::red(output_prefix))))
  #   }
  #   unlink(
  #     list.files(output_dir, pattern = output_prefix),
  #     recursive = TRUE,
  #     force = TRUE
  #   )
  # }

  # Read inputs ----
  ## Load species targets ----
  if (is.data.frame(targets)) {
    targets <- as.matrix(targets)
  }
  if (is.character(targets) && file.exists(targets)) {
    targets <- tryCatch(
      read.csv(targets, row.names = 1) |>
        as.matrix(),
      error = function(e) NULL
    )
  }
  if (!is.matrix(targets)) {
    stop(crayon::bold(crayon::red(
      "Species targets input not found."
    )))
  }
  if (any((targets %% 1) > 0L) || any(targets < 0L)) {
    stop(crayon::bold(crayon::red(
      "Species targets must be positive integers."
    )))
  }
  if (ncol(targets) > 1 && is.null(subregions)) {
    stop(crayon::bold(crayon::red(
      "Targets input must only specify the total if subregions input is not provided."
    )))
  }
  if (
    ncol(targets) > 1 & any(targets[, 1] < rowSums(targets[, -1, drop = FALSE]))
  ) {
    stop(crayon::bold(crayon::red(
      "Subregion targets can not be greater than total targets."
    )))
  }

  n_spp <- nrow(targets)
  spp_names <- rownames(targets)
  spp_targets <- targets[, 1]

  message(crayon::cyan(glue::glue(
    "Species targets loaded: {length(spp_names)}"
  )))

  ## Load Suitability matrix ----
  if (is.data.frame(suitability)) {
    suitability <- as.matrix(suitability)
  }
  if (is.character(suitability) && file.exists(suitability)) {
    suitability <- tryCatch(
      read.csv(suitability, row.names = 1) |>
        as.matrix(),
      error = function(e) NULL
    )
  }
  if (!is.matrix(suitability)) {
    stop(crayon::bold(crayon::red(
      "Could not the the required species suitability matrix."
    )))
  }

  # Validate species names in suitability matrix
  if (!identical(sort(spp_names), sort(colnames(suitability)))) {
    stop(crayon::bold(crayon::red(
      "Species names in suitability matrix do not match species targets."
    )))
  }

  suitability <- suitability[, spp_names, drop = FALSE]
  n_units <- nrow(suitability)
  unit_ids <- rownames(suitability)

  message(crayon::cyan(glue::glue(
    "Species suitability matrix loaded: {nrow(suitability)} planning units"
  )))

  # Subregions ----
  if (!is.null(subregions)) {
    if (is.character(subregions) && file.exists(subregions)) {
      subregions <- tryCatch(
        read.csv(subregions, row.names = 1),
        error = function(e) NULL
      )
    }
    if (is.data.frame(subregions)) {
      subregions <- as.matrix(subregions)
    }
    if (!is.matrix(subregions)) {
      stop(crayon::bold(crayon::red(
        "Invalid subregions input."
      )))
    }
    if (any(duplicated(rownames(subregions))) || any(rowSums(subregions) > 1)) {
      stop(crayon::bold(crayon::red(
        "Invalid subregions input. Planning units must only be allocated to one subregion."
      )))
    }
    if (!all(subregions %in% 0:1)) {
      stop(crayon::bold(crayon::red(
        "Invalid subregions input. Subregions input should be binary (ie, 0 or 1)."
      )))
    }
    if (!all(rownames(subregions) %in% unit_ids)) {
      stop(crayon::bold(crayon::red(
        "Unit_ids  in subregion input do not match the suitability matrix."
      )))
    }
    if (!identical(sort(colnames(targets)[-1]), sort(colnames(subregions)))) {
      stop(crayon::bold(crayon::red(
        "Subregion names must match columns in species targets input."
      )))
    }

    # Add missing units
    unspecified_units <- setdiff(unit_ids, rownames(subregions))
    if (length(unspecified_units) > 0) {
      subregions <- rbind(
        subregions,
        matrix(0, nrow = length(unspecified_units), ncol = ncol(subregions)),
        dimnames = list(unspecified_units, region_idss)
      )
    }

    subregions <- subregions[unit_ids, , drop = FALSE]
    n_regions <- ncol(subregions)
    region_ids <- colnames(subregions)

    message(crayon::cyan(glue::glue(
      "Subregion data loaded: {n_regions} subregions"
    )))

    # Add default region if needed (some input units not allocated to any subregion)
    if (any(rowSums(subregions) == 0)) {
      if ('default' %in% region_ids) {
        stop(crayon::bold(crayon::red(
          "Subregions input includes a 'default' region, but some unit_ids are missing."
        )))
      }
      subregions <- cbind(
        subregions,
        default = rep(0, n_units)
      )
      n_regions <- n_regions + 1
      region_ids <- c(region_ids, "default")
      subregions[rowSums(subregions) == 0, 'default'] <- 1
    }

  } else {
    # If no subregions are provided, assume all units are in one region
    region_ids <- "default"
    subregions <- matrix(
      1,
      nrow = n_units,
      ncol = 1,
      dimnames = list(unit_ids, region_ids)
    )
  }

  # Map units to regions
  unit_regions <- apply(subregions, 1, \(x) which(x == 1))

  # Build regional targets (minimums)
  targets_min <- matrix(
    as.integer(0),
    nrow = n_spp,
    ncol = n_regions,
    dimnames = list(spp_names, region_ids)
  )
  for (i in seq_len(n_regions)) {
    if (region_ids[i] != "default") {
      targets_min[, i] <- as.integer(targets[, region_ids[i]])
    }
  }

  # Find max possible unit selections for each region
  targets_max <- targets_min
  for (i in seq_len(n_spp)) {
    for (j in seq_len(n_regions)) {
      targets_max[i, j] <- spp_targets[i] - sum(targets_min[i, -j])
    }
  }

  ## Load known populations matrix ----
  if (!is.null(populations)) {
    if (is.character(populations) && file.exists(populations)) {
      populations <- tryCatch(
        read.csv(populations),
        error = function(e) NULL
      )
    }
    if (is.data.frame(populations)) {
      populations <- as.matrix(populations)
    }
    if (is.null(populations) || !is.matrix(populations)) {
      stop(crayon::bold(crayon::red(
        "Invalid input prodived for populations input."
      )))
    }

    # validate populations data
    if (!all(rownames(populations) %in% unit_ids)) {
      stop(crayon::bold(crayon::red(
        "Populations input includes unit_ids not present in spp suitability matrix."
      )))
    }
    if (!all(colnames(populations) %in% spp_names)) {
      stop(crayon::bold(crayon::red(
        "Species names in populations input do not match species targets."
      )))
    }

    populations <- populations[unit_ids, spp_names] |>
      apply(
        1:2,
        \(x) ifelse(is.na(x) || x == 0, NA_character_, as.character(x))
      )

    # Check that populations do not span sub-regions
    if (length(region_ids) > 1) {
      for (sp in spp_names) {
        pops <- unique(na.omit(populations[, sp]))
        for (pop in pops) {
          pop_regions <- unique(unit_regions[which(populations[, sp] == pop)])
          if (length(pop_regions) > 1) {
            stop(crayon::bold(crayon::red(
              "Known populations must not span multiple subregions."
            )))
          }
        }
      }
    }

    message(crayon::cyan("Known population matrix loaded."))
  }

  # Set populations with 0 suitability to min suitability for species
  for (i in seq_along(spp_names)) {
    unsuitable_pops <- which(!is.na(populations[, i]) & suitability[, i] == 0)
    if(length(unsuitable_pops) > 0) {
      suitability[unsuitable_pops, i] <- min(suitability[suitability[, i] > 0, i])
    }
  }

  # Regional population counts
  available_populations <- matrix(
    as.integer(0),
    nrow = n_spp,
    ncol = n_regions
  )

  for (i in seq_along(spp_names)) {
    for (j in seq_along(region_ids)) {
      regional_pops[i, j] <- length(unique(population_mx[unit_regions == j, i]))
      # Zero out suitability if population count is greater than or equal to minimum regional target
      if (regional_pops[i, j] >= targets_min[i, j]) {
        suitability[unit_regions == j & is.na(populations[, i]), i] <- 0
      }
    }
  }

  # Apply minimum species suitability score
  suitability[suitability < min_spp_suit_score & !is.na(populations)] <- 0

  # Validate all targets can be met
  impossible_targets <- NULL
  for (i in seq_along(spp_names)) {
    # Check global target
    unit_count <- sum(suitability_mx[, i] > 0)
    if (single_pu_pop) {
      pops <- population_mx[, i]
      unit_count <- unit_count - sum(duplicated(pops[pops != 0]))
    }
    # Check regional targets
    for (j in seq_along(region_ids)) {
      unit_count <- sum(suitability_mx[unit_regions == j, i] > 0)
      # If single_pu_pop is TRUE, remove duplicate population counts
      if (single_pu_pop) {
        pops <- population_mx[unit_regions == j, i]
        unit_count <- unit_count - sum(duplicated(pops[pops != 0]))
      }
      if (unit_count < regional_min[i, j]) {
        impossible_targets <- c(
          impossible_targets,
          paste(spp_names[i], region_ids[j], sep = "-")
        )
      }
    }
  }

  n <- 100000
  workers <- 12
  batch_size <- min(ceiling(n / (4 * workers)), 1000)
  btchs <- seq_len(n) |>
    {
      \(x) split(x, ceiling(x / batch_size))
    }()

  future::plan(future::multicore, workers = workers)
  library(tictoc)
  tic()
  furrr::future_walk(
    btchs,
    ~ {
      solutions_gen_parquet(
        .x,
        suitability = suitability_mx,
        targets = spp_targets,
        spp_names = spp_names,
        output_folder = "output",
        rand_tolerance = rand_tolerance
      )
    },
    .options = furrr::furrr_options(
      seed = TRUE,
      globals = list(
        suitability_mx = suitability_mx,
        spp_targets = spp_targets,
        spp_names = spp_names,
        rand_tolerance = rand_tolerance
      )
    )
  )
  toc()

  solutions_gen_parquet(
    1:1000,
    suitability = suitability_mx,
    targets = spp_targets,
    rand_tolerance = rand_tolerance,
    spp_names = spp_names,
    output_folder = "output"
  )

  library(tictoc)
  tic()
  purrr::walk(
    btchs,
    ~ {
      solutions_gen(
        .x,
        suitability = suitability_mx,
        targets = spp_targets,
        rand_tolerance = rand_tolerance,
        file.path(normalizePath("output"), paste0(Sys.getpid(), ".csv"))
      )
    }
  )
  toc()

  tic()
  solutions_gen(
    seq_len(n),
    suitability = suitability_mx,
    targets = spp_targets,
    rand_tolerance = rand_tolerance,
    file.path(normalizePath("output"), paste0(Sys.getpid(), ".csv"))
  )
  toc()

  foo <- solution_gen(
    suitability = suitability_mx,
    targets = spp_targets,
    rand_tolerance = rand_tolerance
  )
  solutions_gen(
    1:10,
    suitability = suitability_mx,
    targets = spp_targets,
    rand_tolerance = rand_tolerance,
    file.path(normalizePath("output"), "test.csv")
  )
  foos <- purrr::map(
    1:1000,
    ~ {
      print(.x)
      solution_gen(
        solution_id = .x,
        suitability = suitability_mx,
        targets = spp_targets,
        rand_tolerance = rand_tolerance
      )
    }
  )

  tic()
  foos <- furrr::future_map(
    1:100000,
    ~ {
      solution_gen(
        suitability = suitability_mx,
        targets = spp_targets,
        rand_tolerance = rand_tolerance
      )
    },
    .options = furrr::furrr_options(
      seed = TRUE,
      globals = list(
        suitability_mx = suitability_mx,
        spp_targets = spp_targets,
        rand_tolerance = rand_tolerance
      )
    )
  )
  toc()

  solutions_gen(
    suitability = suitability_mx,
    total_targets = spp_targets,
    rand_tolerance = rand_tolerance,
    n = 1000,
    workers = 8,
    folder = normalizePath("output")
  )

  # Initialize overall goals ----
  goals <- data.table::melt(
    suitability,
    id.vars = "unit_id",
    variable.name = "species",
    value.name = "suitability"
  )[
    targets[, .(
      species = species,
      total = total,
      region = "default",
      population = NA_character_,
      max = total,
      min = 0L,
      consider = TRUE
    )],
    on = "species"
  ][
    suitability > 0 & suitability >= min_spp_suit_score
  ]

  ## Validate that overall goals can be met ----
  validate_goals <- goals[, .(count = .N), by = species][
    targets[, .(species, total)],
    on = "species"
  ][total > count]
  if (nrow(validate_goals) > 0L) {
    message(crayon::red("Some overall species goals can not be met:"))
    for (i in seq_len(nrow(validate_goals))) {
      glue::glue(
        "{validate_goals$species[i]}: {validate_goals$count[i]} units available, {validate_goals$total[i]} required."
      ) |>
        crayon::white() |>
        message()
    }
    return(invisible())
  }

  # Subregions ----
  use_subregion_targets <- FALSE
  subregions <- NULL
  if (!is.null(subregions_in)) {
    use_subregion_targets <- TRUE
    if (is.data.frame(subregions_in)) {
      subregions <- data.table::as.data.table(subregions_in)
    }
    if (is.character(subregions_in) && file.exists(subregions_in)) {
      subregions <- tryCatch(
        data.table::fread(subregions_in),
        error = function(e) NULL
      )
    }
    if (is.null(subregions)) {
      stop(crayon::bold(crayon::red(
        "Invalid input prodived for subregions_in."
      )))
    }
    colnames(subregions)[1] <- "unit_id"
    subregions[, unit_id := as.character(unit_id)]

    ## Validate data ----
    if (
      any(is.na(subregions)) ||
        !all(subregions[,
          lapply(.SD, function(col) all(col %in% 0:1)),
          .SDcols = setdiff(names(subregions), "unit_id")
        ])
    ) {
      stop(crayon::bold(crayon::red("Incorrect subregion delineation format.")))
    }
    if (
      ncol(subregions) > 2 &&
        any(subregions[,
          rowSums(.SD) > 1,
          .SDcols = setdiff(names(subregions), "unit_id")
        ])
    ) {
      stop(crayon::bold(crayon::red(
        "Planning units must not span subregion boundaries."
      )))
    }
    if (!all(subregions[["unit_id"]] %in% suitability[["unit_id"]])) {
      stop(crayon::bold(crayon::red(
        "subregions include unit_ids not present in the suitability matrix."
      )))
    }
    if (any(duplicated(subregions[["unit_id"]]))) {
      stop(crayon::bold(crayon::red(
        "Duplicated unit_ids detected in the subregions."
      )))
    }
    if (
      !all.equal(sort(colnames(targets)[-1:-2]), sort(colnames(subregions)[-1]))
    ) {
      stop(crayon::bold(crayon::red(
        "Subregion names do not match species targets."
      )))
    }
    if (
      nrow(targets[,
        .(check = rowSums(.SD), total = total),
        .SDcols = colnames(subregions)[-1]
      ][check > total]) >
        0L
    ) {
      stop(crayon::bold(crayon::red(
        "Some subregion targets greater than total target."
      )))
    }

    ## Add subregions to goals ----
    goals[
      data.table::melt(
        subregions,
        id.vars = "unit_id",
        variable.name = "region",
        value.name = "included",
        variable.factor = FALSE
      )[included == 1L][, .SD, .SDcols = !"included"],
      on = "unit_id",
      region := data.table::fcoalesce(i.region, region)
    ]

    ## Validate subregion targets ----
    validate_goals <- goals[, .(count = .N), by = .(species, region)][
      data.table::melt(
        targets[, .SD, .SDcols = c("species", colnames(subregions)[-1])],
        id.vars = "species",
        variable.name = "region",
        value.name = "region_min",
        variable.factor = FALSE
      )[region_min > 0L],
      on = .(species, region),
      nomatch = NA
    ][is.na(count), count := 0][region_min > count]
    if (nrow(validate_goals) > 0) {
      message(crayon::red("Some subregion targets can not be met:"))
      for (i in seq_len(nrow(validate_goals))) {
        glue::glue(
          "{validate_goals$species[i]} - {validate_goals$region[i]}: {validate_goals$count[i]} units available, {validate_goals$region_min[i]} requested."
        ) |>
          crayon::white() |>
          message()
      }
      return(invisible())
    }

    ## Update subregion goals ----
    goals[
      data.table::melt(
        targets,
        id.vars = c("species", "total"),
        variable.name = "region",
        value.name = "min",
        variable.factor = FALSE
      )[, max := total - (sum(min) - min), by = species][, total := NULL],
      on = c("species", "region"),
      `:=`(
        min = data.table::fcoalesce(i.min, min),
        max = data.table::fcoalesce(i.max, total)
      )
    ]

    message(crayon::cyan(glue::glue(
      "Subregion data loaded: {paste(colnames(subregions)[-1], collapse=', ')}"
    )))
  }

  ## Load known populations matrix ----
  prioritize_known_pops <- FALSE
  populations <- NULL
  if (!is.null(populations_in)) {
    prioritize_known_pops <- TRUE
    if (is.data.frame(populations_in)) {
      populations <- data.table::as.data.table(populations_in)
    }
    if (is.character(populations_in) && file.exists(populations_in)) {
      populations <- tryCatch(
        data.table::fread(populations_in),
        error = function(e) NULL
      )
    }
    if (is.null(populations)) {
      stop(crayon::bold(crayon::red(
        "Invalid input prodived for populations_in."
      )))
    }
    colnames(populations) <- c("unit_id", "species", "population")
    populations[, names(populations) := lapply(.SD, as.character)]

    # validate data
    if (!all(populations[["unit_id"]] %in% suitability[["unit_id"]])) {
      stop(crayon::bold(crayon::red(
        "populations_file includes unit_ids not present in spp suitability matrix."
      )))
    }
    if (!all(unique(populations[["species"]]) %in% spp_names)) {
      stop(crayon::bold(crayon::red(
        "Species names in suitability matrix do not match species targets."
      )))
    }
    # validate all populations have suitability
    validate_pops <- populations[!goals, on = .(species, unit_id)]
    if (nrow(validate_pops) > 0) {
      message(crayon::red(
        "Some known populations occur in units with no suitability:"
      ))
      for (i in seq_len(nrow(validate_pops))) {
        glue::glue(
          "{validate_pops$species[i]} - population {validate_pops$population[i]}: unit {validate_pops$unit_id[i]} has no suitability."
        ) |>
          crayon::white() |>
          message()
      }
      return(invisible())
    }

    # Add populations to goals
    goals[
      populations,
      on = .(unit_id, species),
      population := data.table::fcoalesce(i.population, population)
    ]

    # Check that populations do not span sub-regions
    if (use_subregion_targets) {
      validate_pops <- goals[
        !is.na(population) &
          !duplicated(goals, by = c("species", "region", "population")),
        .(species, region, population)
      ][,
        .(count = .N),
        by = .(species, population)
      ][
        count > 1
      ]
      if (nrow(validate_pops) > 0L) {
        message(crayon::bold(crayon::red(
          "Known populations must not span subregion boundaries:"
        )))
        for (i in seq_len(nrow(validate_pops))) {
          glue::glue(
            "{validate_pops$species[i]} - population {validate_pops$population[i]}: found in {paste(validate_pops$count[i])} regions."
          ) |>
            crayon::white() |>
            message()
        }
        return(invisible())
      }
    }

    # TODO! - if single_pu_pop=TRUE check that goals can still be met

    message(crayon::cyan("Known population matrix loaded."))
  }

  # Generate Solutions ----
  # Batch
  if (is.null(batch_size)) {
    batch_size <- min(ceiling(n / cores), max_batch_size)
    if (!is.null(min_batch_size)) {
      batch_size <- max(batch_size, min_batch_size)
    }
  }
  btchs <- seq_len(n) |>
    {
      \(x) split(x, ceiling(x / batch_size))
    }()
  message(crayon::cyan(glue::glue(
    "Generating {n} solutions in {length(btchs)} batches..."
  )))
  # Set up future backend
  if (parallelly::supportsMulticore()) {
    future_mode <- future::multicore
  } else {
    future_mode <- future::multisession
  }
  future::plan(future_mode, workers = min(cores, length(btchs)))
  p <- progressor(along = btchs) # Init progress bar
  # p <- progressor(n)
  dir.create(
    solutions_dir <- file.path(output_dir, output_prefix),
    showWarnings = FALSE,
    recursive = TRUE
  )
  fns <- file.path(
    solutions_dir,
    paste0(uuid::UUIDgenerate(n = length(btchs)), ".parquet")
  )

  furrr::future_walk2(
    btchs,
    fns,
    .options = furrr::furrr_options(
      seed = ifelse(is.null(seed), TRUE, seed),
      globals = list(
        goals = goals,
        rand_tolerance = rand_tolerance,
        max_candidate_units = max_candidate_units,
        max_spp_selected = max_spp_selected,
        prioritize_known_pops = prioritize_known_pops,
        single_pu_pop = single_pu_pop
      )
    ),
    ~ {
      # Find batch of solutions and write to tmp csv
      purrr::map_dfr(
        .x,
        ~ {
          # p()
          optimTFE::get_solution(
            idx = .x,
            goals = goals,
            rand_tolerance = rand_tolerance,
            max_candidate_units = max_candidate_units,
            max_spp_selected = max_spp_selected,
            prioritize_known_pops = prioritize_known_pops,
            single_pu_pop = single_pu_pop
          )
        }
      ) |>
        arrow::write_parquet(
          .y,
          compression = ifelse(
            arrow::codec_is_available("snappy"),
            "snappy",
            "uncompressed"
          )
        )
      p() # update progress bar
      return()
    }
  )

  # Save outputs ----
  data <- arrow::open_dataset(solutions_dir, format = "parquet")
  if (output_csv) {
    csv_file <- file.path(output_dir, paste0(output_prefix, ".csv"))
    glue::glue(
      "{crayon::cyan('Saving csv output: ')}",
      "{crayon::bgBlue(crayon::white(csv_file))}"
    ) |>
      message()
    arrow::write_csv_arrow(data, csv_file)
  }
  if (return_df) {
    message(crayon::cyan("Preparing results as tibble"))
    out <- dplyr::as_tibble(data)
  }

  # Write metadata ----
  elapsed_time <- Sys.time() - start_time
  list(
    time_elapsed = glue::glue(
      "{round(elapsed_time, 1)} {attr(elapsed_time, 'units')}"
    ),
    rand_tolerance = rand_tolerance,
    max_candidate_units = max_candidate_units,
    max_spp_selected = max_spp_selected,
    prioritize_known_pops = prioritize_known_pops,
    single_pu_pop = single_pu_pop,
    min_spp_suit_score = min_spp_suit_score,
    batch_size = batch_size,
    cores = cores,
    seed = seed,
    targets = seq_len(nrow(targets)) |>
      setNames(targets$species) |>
      purrr::imap(
        ~ {
          targets <- targets[.x, -1] |> as.list()
          targets[targets > 0]
        }
      ),
    suitability = suitability |>
      purrr::pmap(function(...) {
        suits <- list(...)
        suits[suits != 0]
      }),
    popultaions = populations,
    subregions = subregions
  ) |>
    purrr::compact() |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    jsonlite::prettify() |>
    write(file.path(output_dir, paste0(output_prefix, ".meta")))

  # Report Time ----
  glue::glue(
    "Time elapsed: {round(elapsed_time, 1)} {attr(elapsed_time, 'units')}"
  ) |>
    message()

  # Return results as data frame if requested
  if (return_df) {
    return(out)
  }

  return(invisible())
}
