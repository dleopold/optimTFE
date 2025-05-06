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
#' @param targets species targets file - path to a csv file or a pre-loaded data
#'   frame or matrix. If loading from file, the first column should contain the
#'   species names, otherwise species names should be included as row names. The
#'   column direclty following the species names should contain the total
#'   targets for each species. Additional columns should specify the (optional)
#'   subregion targets, with values specifying the minimum targets required in
#'   each subregion.
#' @param suitability species suitability matrix - path to a csv/tsv file or a
#'   pre-loaded data frame or matrix. The first column must be the planning unit
#'   id / number and following columns are species. If using a preloaded matrix
#'   or data frame, unit ids should be set as row names, not in the first
#'   column. Values indicate the suitability scores for each species / taxon in
#'   each planning unit.
#' @param subregions (optional) path to a csv/tsv file or a pre-loaded data
#'   frame or matrix defining sub-regions within the set of planning units. Must
#'   include 1 row for each planning unit with binary values indicating the
#'   sub-region membership of each planning unit. Sub-region column names must
#'   match those provided in the `targets` input.
#' @param populations (optional) path to a csv/tsv file or a pre-loaded data
#'   frame or matrix of delineated populations. When loading from file data can
#'   be in long or wide format. Long format should consist of 3 columns,
#'   unit_id, species, and population. Wide format should have one column per
#'   species and one row per unit_id. If providing a data frame or matrix
#'   directly, only wide formats is accepted and unit ids should be set as
#'   rownames, not as a column.
#' @param incompatibility (optional) path to a csv/tsv file or a pre-loaded data
#'   frame or matrix specifying species that cannot share the same planning
#'   unit. Must be a symmetric matrix with species names as row and column
#'   names, where a value of 1 indicates the species are incompatible (cannot be
#'   in the same unit).
#' @param spatial (optional) path to a spatial file data file (.shp or .gpkg) or
#'   a pre-loaded sf object containing the planning unit polygons. Must have the
#'   same unit IDs as the suitability matrix included as the first column of the
#'   attribute table.
#' @param min_spp_suit_score minimum suitability score for a species to be
#'   considered in a location (default = 0.25)
#' @param rand_tolerance the range of species richness, from maximum, to
#'   consider for selection at each iteration (default = 5)
#' @param max_spp_selected maximum number of species to select in each location
#'   (default = -1) to reduce 'species packing' within units. This parameter
#'   could cause species to be unable to meet targets. Use -1 for no limit.
#' @param single_pu_pop should only one location (ie unit) be selected per
#'   delineated population (default = TRUE). Only applies if a known population
#'   file (`populations`) is provided.
#' @param n number of solutions to generate (default = 100)
#' @param cores number of cores to use (default = NULL, uses all available - 1)
#' @param progress show progress bar (default = TRUE)
#' @param batch_size when parallel processing is used, this parameter can be
#'   used to process solution in batches for improved efficiency (default =
#'   NULL). If not provided (NULL), batch size will be calculated automatically.
#' @param max_batch_size limit batch size for parallel processing (default =
#'   1000). Smaller values will allow the progress bar to update more
#'   frequently, but at the cost of more disk writes.
#' @param seed seed for reproducible output (optional)
#' @param out_dir location to write outputs (default = "."). If NULL, results
#'   will be returned as a data frame and temporary files will be used.
#' @param run_id prefix for output files (default = "optimTFE")
#' @param output_csv Should the solutions be compiled to a single csv file
#'   (default = TRUE)
#' @param return_df return all generated solutions as a data frame in the
#'   current R session (default = FALSE)
#' @param force_overwrite overwrite existing output files (default = FALSE)
#' @param summary generate summary statistics and metrics for solutions (default
#'   = TRUE)
#' @param spatial_projection optional projection string or epsg code to use for
#'   spatial data. Only applicable if spatial data is provided and summary =
#'   TRUE. A equal area projection is needed to calculate spatial metrics such
#'   as area and perimeter.
#'
#' @import progressr
#'
#' @family main
#' @export
#'
optimTFE <- function(
  # Data Inputs
  targets = NULL,
  suitability = NULL,
  subregions = NULL,
  populations = NULL,
  incompatibility = NULL,
  spatial = NULL,
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
  max_batch_size = 1000,
  seed = NULL,
  # Output parameters,
  out_dir = ".",
  run_id = "optimTFE",
  output_csv = TRUE,
  return_df = FALSE,
  force_overwrite = FALSE,
  summary = TRUE,
  spatial_projection = NULL
) {
  message("Beginning optimTFE...")

  # DEBUG ----
  list2env(as.list(environment()), envir = .GlobalEnv)
  return()

  start_time <- Sys.time()
  cores <- cores %||% (future::availableCores() - 1)
  meta <- list(
    rand_tolerance = rand_tolerance,
    max_spp_selected = max_spp_selected,
    min_spp_suit_score = min_spp_suit_score,
    batch_size = batch_size,
    cores = cores,
    seed = seed
  )

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

  # Output Files ----
  ## If no output directory provided, return all solutions as a data frame
  if (is.null(out_dir)) {
    return_df = TRUE
    out_dir <- tempdir()
    run_id <- uuid::UUIDgenerate()
    on.exit({
      unlink(
        list.files(out_dir, pattern = run_id),
        recursive = TRUE,
        force = TRUE
      )
    })
  }

  # Prevent overwriting existing output ----
  if (dir.exists(file.path(out_dir, run_id))) {
    if (!force_overwrite) {
      stop(
        crayon::bold(crayon::red(
          "Output for the run_id, {run_id}, already exists: "
        )) |>
          paste0(
            crayon::bgBlue(crayon::white(out_dir)),
            crayon::bold(crayon::red(run_id))
          )
      )
    }
    unlink(
      list.files(out_dir, pattern = run_id),
      recursive = TRUE,
      force = TRUE
    )
  }

  # Read inputs ----
  ## Load species targets ----
  if (is.character(targets) && file.exists(targets)) {
    targets <- tryCatch(
      read.csv(targets, row.names = 1),
      error = function(e) NULL
    )
  }
  if (is.data.frame(targets)) {
    targets <- as.matrix(targets)
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

  targets <- targets |>
    apply(1:2, as.integer)

  n_spp <- nrow(targets)
  spp_names <- rownames(targets)
  spp_targets <- targets[, 1]
  meta$spp_names <- spp_names
  meta$targets <- targets

  message(crayon::cyan(glue::glue(
    "Species targets loaded: {length(spp_names)}"
  )))

  ## Load Suitability matrix ----
  if (is.data.frame(suitability)) {
    suitability <- as.matrix(suitability)
  }
  if (is.character(suitability) && file.exists(suitability)) {
    suitability <- tryCatch(
      read.csv(suitability, row.names = 1, na.strings = c("", "NA", "N/A")) |>
        as.matrix(),
      error = function(e) NULL
    )
  }
  if (!is.matrix(suitability)) {
    stop(crayon::bold(crayon::red(
      "Could not load the the required species suitability matrix."
    )))
  }

  # Validate species names in suitability matrix
  if (!identical(sort(spp_names), sort(colnames(suitability)))) {
    stop(crayon::bold(crayon::red(
      "Species names in suitability matrix do not match species targets."
    )))
  }

  suitability[is.na(suitability)] <- 0
  suitability <- suitability[, spp_names, drop = FALSE]
  n_units <- nrow(suitability)
  unit_ids <- rownames(suitability)
  suitability_mx <- suitability # Local copy for modifications
  meta$suitability <- suitability
  meta$unit_ids <- unit_ids

  message(crayon::cyan(glue::glue(
    "Species suitability matrix loaded: {nrow(suitability)} planning units"
  )))

  # Subregions ----
  if (!is.null(subregions)) {
    if (is.character(subregions) && file.exists(subregions)) {
      regions <- tryCatch(
        read.csv(subregions, row.names = 1),
        error = function(e) NULL
      )
    }
    if (is.data.frame(subregions)) {
      regions <- as.matrix(subregions)
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

    regions <- subregions |>
      apply(1:2, as.integer)

    # Add missing units
    unspecified_units <- setdiff(unit_ids, rownames(regions))
    if (length(unspecified_units) > 0) {
      regions <- rbind(
        regions,
        matrix(
          as.integer(0),
          nrow = length(unspecified_units),
          ncol = ncol(regions)
        ),
        dimnames = list(unspecified_units, colnames(regions))
      )
    }

    regions <- regions[unit_ids, , drop = FALSE]
    n_regions <- ncol(regions)
    region_ids <- colnames(regions)

    message(crayon::cyan(glue::glue(
      "Subregion data loaded: {n_regions} subregions"
    )))

    # Add default region if needed (some input units not allocated to any subregion)
    if (any(rowSums(regions) == 0)) {
      if (!'default' %in% region_ids) {
        regions <- cbind(
          regions,
          default = rep(0, n_units)
        )
        n_regions <- n_regions + 1
        region_ids <- c(region_ids, "default")
      }
      regions[rowSums(regions) == 0, 'default'] <- 1
    }
  } else {
    # If no subregions are provided, assume all units are in one region
    region_ids <- "default"
    regions <- matrix(
      as.integer(1),
      nrow = n_units,
      ncol = 1,
      dimnames = list(unit_ids, "default")
    )
  }

  meta$regions <- regions
  # Map units to regions
  unit_regions <- apply(regions, 1, \(x) which(x == 1))

  # Build regional targets (minimums)
  regional_min <- matrix(
    as.integer(0),
    nrow = n_spp,
    ncol = n_regions,
    dimnames = list(spp_names, region_ids)
  )
  for (region in region_ids) {
    if (region %in% colnames(targets)) {
      regional_min[, region] <- targets[, region]
    }
  }

  # Find max possible unit selections for each region
  regional_max <- regional_min
  for (sp in spp_names) {
    for (region in region_ids) {
      regional_max[sp, region] <- spp_targets[sp] -
        sum(regional_min[sp, setdiff(region_ids, region)])
    }
  }

  ## Load known populations matrix ----
  if (!is.null(populations)) {
    if (
      length(populations) == 1L &&
        is.character(populations) &&
        file.exists(populations)
    ) {
      populations <- tryCatch(
        read.csv(populations),
        error = function(e) NULL
      )
    }
    if (is.data.frame(populations)) {
      if (
        all(populations[, 1] %in% unit_ids) &&
          all(colnames(populations)[-1] %in% spp_names)
      ) {
        populations <- populations |>
          tibble::column_to_rownames(colnames(populations)[1]) |>
          as.matrix()
      }
      if (
        all(populations[, 1] %in% spp_names) &&
          all(populations[, 2] %in% unit_ids)
      ) {
        if (ncols(populations) > 3) {
          stop(crayon::bold(crayon::red(
            "Long-format populations input must only have 3 columns: unit id, species, and population."
          )))
        }
        if (any(duplicated(populations[, 1:2]))) {
          stop(crayon::bold(crayon::red(
            "Species can not have more than one population per planning unit."
          )))
        }
        populations <- populations |>
          (\(x) {
            cols <- colnames(x)
            tidyr::pivot_wider(
              x,
              names_from = cols[1],
              values_from = cols[3],
              values_fill = NA
            ) |>
              tibble::column_to_rownames(cols[2])
          })() |>
          as.matrix()
      }
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

    # Coearse to character matrix
    populations <- populations |>
      apply(
        1:2,
        \(x) ifelse(is.na(x) || x == 0, NA_character_, as.character(x))
      )

    # Expand to add any missing spp or units
    missing_spp <- setdiff(spp_names, colnames(populations))
    if (length(missing_spp) > 0) {
      populations <- cbind(
        populations,
        matrix(
          NA_character_,
          nrow(populations),
          length(missing_spp),
          dimnames = list(rownames(populations), missing_spp)
        )
      )
    }
    missing_units <- setdiff(unit_ids, rownames(populations))
    if (length(missing_units) > 0) {
      populations <- cbind(
        populations,
        matrix(
          NA_character_,
          length(missing_units),
          ncol(populations),
          dimnames = list(missing_units, colnames(populations))
        )
      )
    }

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

    # Generate population integer mx and key
    populations <- populations[unit_ids, spp_names]
    populations_mx <- matrix(
      as.integer(0),
      nrow = n_units,
      ncol = n_spp,
      dimnames = list(unit_ids, spp_names)
    )
    population_names <- list()
    pop_idx <- 1
    for (sp in spp_names) {
      for (unit in unit_ids) {
        if (is.na(populations[unit, sp])) next
        pop <- paste(sp, populations[unit, sp], sep = "-")
        if (pop %in% names(population_names)) {
          populations_mx[unit, sp] <- as.integer(population_names[[pop]])
          next
        }
        population_names[[pop]] <- populations_mx[unit, sp] <- as.integer(
          pop_idx
        )
        pop_idx <- pop_idx + 1
      }
    }

    meta$populations <- populations_mx
    meta$population_names <- population_names
    meta$single_pu_pop <- single_pu_pop
    message(crayon::cyan("Known population matrix loaded."))
  }

  # Generate empty populations matrix if needed
  if (is.null(populations)) {
    populations <- matrix(
      as.integer(0),
      nrow = n_units,
      ncol = n_spp,
      dimnames = list(unit_ids, spp_names)
    )
  }

  # Set populations with 0 suitability to min suitability for species
  for (sp in spp_names) {
    unsuitable_pops <- which(
      !is.na(populations[, sp]) & suitability_mx[, sp] == 0
    )
    if (length(unsuitable_pops) > 0) {
      message(crayon::red(glue::glue(
        "{length(unsuitable_pops)} population in units with 0 suitability detected for {sp} - setting to mean suitability."
      )))
      for (unit in unsuitable_pops) {
        suitability_mx[unit, sp] <- mean(suitability_mx[
          unit,
          suitability_mx[unit, ] >= min_spp_suit_score
        ])
      }
    }
  }

  # Avoid checking for populations spanning multiple regions if non exist
  if (single_pu_pop) {
    single_pu_pop <- any(apply(
      populations,
      2,
      \(x) any(duplicated(na.omit(x)))
    ))
  }

  # Regional population counts
  population_counts <- matrix(
    as.integer(0),
    nrow = n_spp,
    ncol = n_regions,
    dimnames = list(spp_names, region_ids)
  )
  for (sp in spp_names) {
    for (region in region_ids) {
      region_idx <- unit_regions == which(region_ids == region)
      if (single_pu_pop) {
        # Dont double count populations that span units
        population_counts[sp, region] <- populations[region_idx, sp] |>
          unique() |>
          na.omit() |>
          length()
      } else {
        # Count all population units independently
        population_counts[sp, region] <- populations[region_idx, sp] |>
          na.omit() |>
          length()
      }
      # Zero out suitability for non-population units if overall population count is
      # greater than or equal to the total species target and the regional population
      # count is greater than or equal to min regional target
      if (
        sum(population_counts[sp, ]) >= spp_targets[sp] &&
          population_counts[sp, region] >= regional_min[sp, region]
      ) {
        suitability_mx[region_idx & is.na(populations[, sp]), sp] <- 0
      }
      if (
        population_counts[sp, region] > 0 &&
          population_counts[sp, region] >= regional_max[sp, region]
      ) {
        suitability_mx[region_idx & is.na(populations[, sp]), sp] <- 0
      }
    }
  }

  # Apply minimum species suitability score
  suitability_mx[suitability_mx < min_spp_suit_score & is.na(populations)] <- 0

  # Count available units
  unit_counts <- matrix(
    as.integer(0),
    nrow = n_spp,
    ncol = n_regions,
    dimnames = list(spp_names, region_ids)
  )
  for (sp in spp_names) {
    for (region in region_ids) {
      region_idx <- unit_regions == which(region_ids == region)
      unit_counts[sp, region] <- sum(suitability_mx[region_idx, sp] > 0)
      if (single_pu_pop) {
        duplicates <- populations[region_idx, sp] |>
          na.omit() |>
          duplicated() |>
          sum()
        unit_counts[sp, region] <- unit_counts[sp, region] - duplicates
      }
      if (regional_max[sp, region] > unit_counts[sp, region]) {
        regional_max[sp, region] <- unit_counts[sp, region]
      }
    }
  }

  # Validate all targets can be met
  impossible_targets <- NULL
  for (sp in spp_names) {
    # Check global target
    if (sum(unit_counts[sp, ]) < spp_targets[sp]) {
      impossible_targets <- c(
        impossible_targets,
        paste(sp, "global", sep = "-")
      )
      next
    }
    # Check regional targets
    for (region in region_ids) {
      if (unit_counts[sp, region] < regional_min[sp, region]) {
        impossible_targets <- c(
          impossible_targets,
          paste(sp, region, sep = "-")
        )
      }
    }
  }
  if (length(impossible_targets) > 0) {
    stop(crayon::bold(crayon::red(
      "Not enough units to meet all targets: ",
      paste(impossible_targets, collapse = ", ")
    )))
  }

  # Load optional species incompatibility data ----
  if (!is.null(incompatibility)) {
    if (
      length(incompatibility) == 1L &&
        is.character(incompatibility) &&
        file.exists(incompatibility)
    ) {
      incompatibility <- tryCatch(
        read.csv(incompatibility, row.names = 1),
        error = function(e) NULL
      )
    }
    if (is.data.frame(incompatibility)) {
      incompatibility <- as.matrix(incompatibility)
    }
    if (is.null(incompatibility) || !is.matrix(incompatibility)) {
      stop(crayon::bold(crayon::red(
        "Invalid input prodived for species incompatibility input."
      )))
    }
    if (!isSymmetric(incompatibility) || !all(incompatibility %in% 0:1)) {
      stop(crayon::bold(crayon::red(
        "Species incompatibility input must be a symmetric matrix of 0/1 values."
      )))
    }
    if (
      !all(colnames(incompatibility) %in% spp_names) ||
        !all(rownames(incompatibility) %in% spp_names)
    ) {
      stop(crayon::bold(crayon::red(
        "Species mismatch detected in incompatibility input."
      )))
    }

    # Expand incompatibility matrix to cover all species
    missing_spp <- setdiff(spp_names, colnames(incompatibility))
    if (length(missing_spp) > 0) {
      incompatibility <- cbind(
        incompatibility,
        matrix(
          0,
          nrow(incompatibility),
          length(missing_spp),
          dimnames = list(rownames(incompatibility), missing_spp)
        )
      )
      incompatibility <- rbind(
        incompatibility,
        matrix(
          0,
          length(missing_spp),
          ncol(incompatibility),
          dimnames = list(missing_spp, colnames(incompatibility))
        )
      )
    }

    incompatibility <- incompatibility[spp_names, spp_names] |>
      apply(1:2, as.integer)

    meta$incompatibility <- incompatibility
    message(crayon::cyan("Loaded species incompatibility data."))

    # TODO - add some validation that there are not too many incompatibilities
  }

  # Load spatial data if provided ----
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
    if (!all(sf::st_is(spatial, "POLYGON"))) {
      stop(crayon::bold(crayon::red(
        "Spatial input must only contain POLYGON geometries."
      )))
    }
    if (!identical(sort(dplyr::pull(spatial, 1)), sort(unit_ids))) {
      stop(crayon::bold(crayon::red(
        "Spatial input unit_ids do not other inputs."
      )))
    }
  }

  # DEBUG ----
  # list2env(as.list(environment()), envir = .GlobalEnv)
  # return()

  # Generate Solutions ----
  # Batch
  if (is.null(batch_size)) {
    batch_size <- min(ceiling(n / cores), max_batch_size)
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

  # Create output file names
  dir.create(
    solutions_dir <- file.path(out_dir, run_id, "solutions"),
    showWarnings = FALSE,
    recursive = TRUE
  )
  fns <- file.path(
    solutions_dir,
    paste0(uuid::UUIDgenerate(n = length(btchs)), ".parquet")
  )

  # Generate solutions
  p <- progressor(along = btchs) # Init progress bar
  furrr::future_walk2(
    btchs,
    fns,
    ~ {
      solutions_gen_df(
        solution_ids = .x,
        suitability = suitability_mx,
        spp_targets = spp_targets,
        unit_regions = unit_regions,
        unit_counts = unit_counts,
        regional_min = regional_min,
        regional_max = regional_max,
        populations = populations_mx,
        population_counts = population_counts,
        single_pu_pop = single_pu_pop,
        rand_tolerance = rand_tolerance,
        max_spp_selected = max_spp_selected,
        spp_names = spp_names,
        incompat = incompatibility,
        seed = sample.int(.Machine$integer.max, 1)
      ) |>
        arrow::write_parquet(.y)
      p()
    },
    .options = furrr::furrr_options(
      seed = ifelse(is.null(seed), TRUE, seed),
      globals = c(
        "suitability_mx",
        "spp_targets",
        "unit_regions",
        "unit_counts",
        "regional_min",
        "regional_max",
        "populations_mx",
        "population_counts",
        "single_pu_pop",
        "rand_tolerance",
        "max_spp_selected",
        "spp_names",
        "incompatibility"
      )
    )
  )

  elapsed_time <- Sys.time() - start_time
  meta$elapsed_time <- glue::glue(
    "{round(elapsed_time, 1)} {attr(elapsed_time, 'units')}"
  )
  message(crayon::cyan(glue::glue(
    "Finished generating {n} solutions in {meta$elapsed_time}."
  )))

  # write metadata ----
  meta |>
    purrr::compact() |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    # jsonlite::prettify() |>
    write(file.path(out_dir, paste0(run_id, ".meta")))

  # Load solutions ----
  solutions <- arrow::open_dataset(solutions_dir) |>
    dplyr::collect()

  # Write output to csv ----
  if (output_csv) {
    arrow::write_csv_arrow(
      solutions |>
        dplyr::arrange(solution),
      file = file.path(out_dir, run_id, paste0(run_id, ".solutions.csv"))
    )
  }

  # Generate summary data ----
  if (isTRUE(summary)) {
    message(crayon::cyan("Generating summary data for {n} solutions..."))
    generate_summary(
      data = dplyr::collect(solutions),
      out_dir = out_dir,
      run_id = run_id,
      suitability_mx = suitability_mx,
      unit_ids = unit_ids,
      spatial = spatial,
      spatial_projection = spatial_projection
    )
  }

  # DEBUG ----
  # list2env(as.list(environment()), envir = .GlobalEnv)
  # return()

  if (return_df) {
    return(solutions)
  }
  return(invisible())
}
