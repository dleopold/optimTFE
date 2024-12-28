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
#' @param targets_in species targets file - path to a csv/tsv file or a
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
#'   matrix of delineated populations. Row / column names must match the
#'   suitability matrix. Missing values or 0s will be interpreted as no know
#'   population in the planning unit and other unique values will be used to
#'   identify known populations such that values for a given species that are
#'   shared across planning units indicate a population that extends across
#'   multiple units.
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
#' @param min_batch_size default = 10.
#' @param progress show progress bar
#' @param output_prefix prefix for output files (default = 'solutions')
#' @param output_dir location to write outputs (default = `file.path(dir,
#'   'output')`). Created for user if does not exist.
#' @param output_csv Should the solutions be written to a single csv file
#'   (default = TRUE)
#' @param output_parquet Should the solutions be written to a parquet file
#'   (default = FALSE)
#' @param force_overwrite overwrite existing output files (default = FALSE)
#' @param delete_tmp_files should temporary files be deleted after processing
#'   (default = TRUE)
#' @param return_df return all generated solutions as a data frame (default =
#'   FALSE)
#'
#' @import progressr
#'
#' @export
#'
optimTFE <- function(
    # Data Inputs,
    dir = ".",
    targets_in = optimTFE::example_targets,
    suitability_in = optimTFE::example_suitability,
    subregions_in = optimTFE::example_subregions,
    populations_in = optimTFE::example_populations,
    # Config parameters,
    min_spp_suit_score = 0,
    max_candidate_units = Inf,
    rand_tolerance = 5,
    max_spp_selected = Inf,
    single_pu_pop = TRUE,
    # Compute parameters,
    n = 100,
    cores = NULL,
    progress = TRUE,
    batch_size = NULL,
    max_batch_size = 1000,
    min_batch_size = 10,
    seed = NULL,
    # Output parameters,
    output_dir = NULL,
    output_prefix = "solutions",
    output_csv = TRUE,
    output_parquet = FALSE,
    force_overwrite = FALSE,
    delete_tmp_files = TRUE,
    return_df = FALSE) {
  message("Beginning optimTFE...")

  start_time <- Sys.time()
  output_dir <- output_dir %||% file.path(dir, "output")
  cores <- cores %||% future::availableCores()

  # Determine if progress bar should be used
  if (isTRUE(progress) &&
    interactive() &&
    !isTRUE(getOption("knitr.in.progress")) &&
    !isTRUE(getOption("rstudio.notebook.executing"))) {
    progressr::handlers(global = TRUE)
  } else if (interactive() &&
    !isTRUE(getOption("knitr.in.progress")) &&
    !isTRUE(getOption("rstudio.notebook.executing"))) {
    progressr::handlers(global = FALSE)
  }

  # Output Files ----
  if (length(output_prefix) == 0L || nchar(output_prefix) == 0L) {
    stop(crayon::bold(crayon::red("output_prefix must be a non-empty string")))
  }
  if (output_csv) {
    csv_file <- file.path(output_dir, paste0(output_prefix, ".csv"))
    if (!force_overwrite && file.exists(csv_file)) {
      glue::glue(
        "{crayon::bold(crayon::red('Output file already exists: '))}",
        crayon::bgBlue(crayon::white(csv_file))
      ) |>
        message()
      stop(crayon::red("Delete existing outputs, change the solution prefix, or set force_overwrite = TRUE."))
    }
  }
  if (output_parquet) {
    parquet_chk <- file.path(output_dir, output_prefix) |>
      list.files(pattern = ".parquet$")
    if (!force_overwrite && length(parquet_chk) > 0L) {
      glue::glue(
        "{crayon::bold(crayon::red('Output files already exist: '))}",
        crayon::bgBlue(crayon::white(file.path(output_dir, output_prefix, "part-{i}.parquet")))
      ) |>
        message()
      stop(crayon::red("Delete existing outputs, change the solution prefix, or set force_overwrite = TRUE."))
    }
  }

  # Temp files ----
  tmp_dir <- file.path(output_dir, paste0(output_prefix, "_tmp_", as.numeric(Sys.time())))
  # add timestamp to tmp_dir to create unique
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  if (!force_overwrite && length(list.files(tmp_dir) > 0L)) {
    glue::glue(
      "{crayon::bold(crayon::red('Temporary file directory is not empty: '))}",
      crayon::bgBlue(crayon::white(tmp_dir))
    ) |>
      message()
    stop(crayon::red("Delete existing outputs, change the solution prefix, or set force_overwrite = TRUE."))
  }

  # Read inputs ----
  ## Load species targets ----
  targets <- NULL
  if (is.data.frame(targets_in)) {
    targets <- data.table::as.data.table(targets_in)
  }
  if (is.character(targets_in) && file.exists(targets_in)) {
    targets <- tryCatch(
      data.table::fread(targets_in),
      error = function(e) NULL
    )
  }
  if (is.null(targets)) {
    stop(crayon::bold(crayon::red("A species targets input is required (targets_in).")))
  }
  if (any((targets[, -1] %% 1) > 0L) || any(targets[, -1] < 0L)) {
    stop(crayon::bold(crayon::red("Species targets must be positive integers.")))
  }
  colnames(targets)[1:2] <- c("species", "total")
  spp_names <- sort(targets[["species"]])
  message(crayon::cyan(glue::glue("Species targets loaded: {length(spp_names)}")))

  ## Load Suitability matrix ----
  suitability <- NULL
  if (is.data.frame(suitability_in)) {
    if (inherits(suitability_in, "sf")) {
      suitability_in <- sf::st_drop_geometry(suitability_in)
    }
    suitability <- data.table::as.data.table(suitability_in)
  }
  if (is.character(suitability_in) && file.exists(suitability_in)) {
    suitability <- tryCatch(
      data.table::fread(suitability_in),
      error = function(e) NULL
    )
  }
  if (is.null(suitability)) {
    stop(crayon::bold(crayon::red("A species suitability matrix is required (suitability_in).")))
  }
  colnames(suitability)[1] <- "unit_id"
  suitability[, unit_id := as.character(unit_id)]

  # Validate species names in suitability matrix
  if (!all(spp_names %in% colnames(suitability)[-1])) {
    stop(crayon::bold(crayon::red("Species names in suitability matrix do not match species targets.")))
  }
  suitability <- suitability[, c("unit_id", spp_names), with = FALSE]
  message(crayon::cyan(glue::glue("Species suitability matrix loaded: {nrow(suitability)} planning units")))

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
    for (i in seq_len(nrow(impossible_goals))) {
      glue::glue(
        "{impossible_goals$species[i]}: {impossible_goals$available_units[i]} units available, {impossible_goals$total[i]} required."
      ) |>
        crayon::white() |>
        message()
    }
    return(invisible())
  }

  # Subregions ----
  use_subregion_targets <- FALSE
  if (!is.null(subregions_in)) {
    use_subregion_targets <- TRUE
    subregions <- NULL
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
      stop(crayon::bold(crayon::red("Invalid input prodived for subregions_in.")))
    }
    colnames(subregions)[1] <- "unit_id"
    subregions[, unit_id := as.character(unit_id)]

    ## Validate data ----
    if (any(is.na(subregions)) || !all(subregions[, lapply(.SD, function(col) all(col %in% 0:1)), .SDcols = setdiff(names(subregions), "unit_id")])) {
      stop(crayon::bold(crayon::red("Incorrect subregion delineation format.")))
    }
    if (ncol(subregions) > 2 && any(subregions[, rowSums(.SD) > 1, .SDcols = setdiff(names(subregions), "unit_id")])) {
      stop(crayon::bold(crayon::red("Planning units must not span subregion boundaries.")))
    }
    if (!all(subregions[["unit_id"]] %in% suitability[["unit_id"]])) {
      stop(crayon::bold(crayon::red("subregions include unit_ids not present in the suitability matrix.")))
    }
    if (any(duplicated(subregions[["unit_id"]]))) {
      stop(crayon::bold(crayon::red("Duplicated unit_ids detected in the subregions.")))
    }
    if (!all.equal(sort(colnames(targets)[-1:-2]), sort(colnames(subregions)[-1]))) {
      stop(crayon::bold(crayon::red("Subregion names do not match species targets.")))
    }
    if (nrow(targets[, .(check = rowSums(.SD), total = total), .SDcols = colnames(subregions)[-1]][check > total]) > 0L) {
      stop(crayon::bold(crayon::red("Some subregion targets greater than total target.")))
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
    validate_goals <- goals[, .(count = .N), by = .(species,region)][
      data.table::melt(
        targets[, .SD, .SDcols = c("species", colnames(subregions)[-1])],
        id.vars = "species",
        variable.name = "region",
        value.name = "region_min",
        variable.factor = FALSE
      )[region_min > 0L],
      on = .(species,region),
      nomatch = NA
    ][is.na(count), count := 0][region_min > count]
    if (nrow(validate_goals) > 0) {
      message(crayon::red("Some sub-region species goals can not be met:"))
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

    message(crayon::cyan("Subregion data loaded: ", paste(unique(subregions$region), collapse = ", ")))
  }

  ## Load known populations matrix ----
  prioritize_known_pops <- FALSE
  if (!is.null(populations_in)) {
    prioritize_known_pops <- TRUE
    populations <- NULL
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
      stop(crayon::bold(crayon::red("Invalid input prodived for populations_in.")))
    }
    colnames(populations)[1] <- "unit_id"
    populations[, unit_id := as.character(unit_id)]

    # validate data
    if (!all(populations[['unit_id']] %in% suitability[['unit_id']])) {
      stop(crayon::bold(crayon::red("populations_file includes unit_ids not present in spp suitability matrix.")))
    }
    if (any(duplicated(populations[['unit_id']]))) {
      stop(crayon::bold(crayon::red("Duplicated unit_ids detected in the populations_file.")))
    }
    if (!all(spp_names %in% colnames(populations))) {
      stop(crayon::bold(crayon::red("Species names in suitability matrix do not match species targets.")))
    }


    foo <- goals[
      data.table::melt(
        populations[, c("unit_id", spp_names), with = FALSE],
        id.vars = "unit_id",
        variable.name = "species",
        value.name = "population",
        variable.factor = FALSE
      )[,
        population := as.character(population)
      ],
      on = .(unit_id, species),
      population := data.table::fcoalesce(i.population, population)
    ]

    message(crayon::cyan("Known population matrix loaded."))
  }

  # Add known populations to goals b----
  if (prioritize_known_pops) {
    spp_pops <- spp_pops |>
      tidyr::pivot_longer(
        -unit_id,
        names_to = "species",
        values_to = "population"
      ) |>
      dplyr::filter(
        population > 0
      )

    goals <- goals |>
      dplyr::full_join(
        spp_pops,
        by = c("unit_id", "species")
      ) |>
      dplyr::mutate(
        suitability = dplyr::case_when(
          !is.na(population) & suitability == 0 ~ max(min_spp_suit_score, 1E-5),
          .default = suitability
        )
      )
  }

  # Check that populations do not span sub-regions
  if (prioritize_known_pops && use_subregion_targets) {
    validatation <- goals |>
      dplyr::select(species, region, population) |>
      dplyr::distinct() |>
      tidyr::drop_na() |>
      dplyr::summarise(
        cnt = dplyr::n(),
        region = paste(region, collapse = ", "),
        .by = c(species, population)
      ) |>
      dplyr::filter(cnt > 1)
    if (nrow(validatation) > 0L) {
      message(crayon::bold(crayon::red("Known populations must not span sub-region boundaries:")))
      purrr::pwalk(validatation, function(species, population, cnt, region) {
        message(glue::glue("{crayon::italic(species)}, population {population} found in: {region}"))
      })
      return(invisible())
    }
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
  message(crayon::cyan(glue::glue("Generating {n} solutions in {length(btchs)} batches...")))
  # Set up future backend
  if (parallelly::supportsMulticore()) {
    future_mode <- future::multicore
  } else {
    future_mode <- future::multisession
  }
  future::plan(future_mode, workers = cores)
  p <- progressor(along = btchs) # Init progress bar

  tmp_files <- file.path(
    tmp_dir,
    paste0(uuid::UUIDgenerate(n = length(btchs)), ".csv")
  )

  furrr::future_walk2(
    btchs,
    tmp_files,
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
      purrr::map_dfr(.x, ~ {
        optimTFE::get_solution(
          idx = .x,
          goals = data.table::as.data.table(goals),
          rand_tolerance = rand_tolerance,
          max_candidate_units = max_candidate_units,
          max_spp_selected = max_spp_selected,
          prioritize_known_pops = prioritize_known_pops,
          single_pu_pop = single_pu_pop
        )
      }) |>
        readr::write_csv(.y, num_threads = 1, progress = FALSE)
      p() # update progress bar
      return()
    }
  )

  # Save outputs ----
  data <- arrow::open_dataset(tmp_dir, format = "csv")
  if (output_csv) {
    glue::glue(
      "{crayon::cyan('Saving csv output: ')}",
      "{crayon::bgBlue(crayon::white(csv_file))}"
    ) |>
      message()
    arrow::write_csv_arrow(data, csv_file)
  }
  if (output_parquet) {
    pq_path <- file.path(output_dir, output_prefix)
    glue::glue(
      "{crayon::cyan('Saving parquet output: ')}",
      "{crayon::bgBlue(crayon::white(pq_path))}"
    ) |>
      message()
    arrow::write_parquet(data, pq_path)
  }
  if (return_df) {
    message(crayon::cyan("Preparing results as tibble"))
    out <- dplyr::as_tibble(data)
  }

  # Write metadata ----
  elapsed_time <- Sys.time() - start_time
  meta <- list(
    time_elapsed = glue::glue(
      "{round(elapsed_time, 1)} {attr(elapsed_time, 'units')}"
    ),
    targets = seq_len(nrow(targets)) |>
      setNames(targets$species) |>
      purrr::imap(~ {
        targets <- targets[.x, -1] |> as.list()
        targets[targets > 0]
      }),
    rand_tolerance = rand_tolerance,
    max_candidate_units = max_candidate_units,
    max_spp_selected = max_spp_selected,
    prioritize_known_pops = prioritize_known_pops,
    single_pu_pop = single_pu_pop,
    min_spp_suit_score = min_spp_suit_score,
    batch_size = batch_size,
    cores = cores,
    seed = seed,
    suitability = spp_suit |>
      purrr::pmap(function(...) {
        suits <- list(...)
        suits[suits != 0]
      })
  ) |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    jsonlite::prettify() |>
    write(file.path(output_dir, paste0(output_prefix, ".meta")))

  # Delete temp files ----
  if (delete_tmp_files) {
    message(crayon::cyan("Cleaning up temp files"))
    unlink(tmp_dir, force = TRUE, recursive = TRUE)
  }

  # Report Time ----
  glue::glue(
    "Time elapsed: {round(elapsed_time, 1)} {attr(elapsed_time, 'units')}"
  ) |> message()

  # Return results as data frame if requested
  if (return_df) {
    return(out)
  }

  return(invisible())
}
