#' optimTFE algorithm: generate a suite of conservation footprint 'solutions'
#' based on Feature suitability values and a list of Targets for all Features.
#'
#' @param dir working directory
#' @param spp_targets_fn .csv of species targets. First 2 columns should be
#'  species names and total targets populations, respectively. Additional
#'  columns should be provided when using subregion targets. Column names must
#'  match the sub-region names provided in the sub_regions_fn input and values
#'  should be the minimum number of populations required in the the subregion.
#' @param spp_suit_fn input species suitability matrix. Rows are locations and
#'  columns are species. Must include species names and location ids in the
#'  column and row names, respectively.
#' @param spp_pops_fn (optional) .csv input matrix of delineated populations. Row / column names
#'  must match suitability matrix
#' @param spp_occ_fn (optional) input matrix of known occurrence locations. Row / column
#'  names must match suitability matrix
#' @param sub_regions_fn (optional) A csv input defining sub-regions. Should
#'  include 1 row for each planning unit with binary values indicating the
#'  sub-region membership of each planning unit.
#' @param rand_tolerance  the range of species richness, from maximum, to
#'  consider for selection at each iteration(default = 5)
#' @param max_spp_selected maximum number of species to select in each location
#'  (default = Inf) to reduce 'species packing' within units. This parameter
#'  could cause species to be unable to meet targets.
#' @param min_spp_suit_score minimum suitability score for a species to be
#'  considered in a location (default = 0)
#' @param use_subregion_targets should subregion targets be used (default = TRUE
#'  if a valid sub_regions_fn is provided).
#' @param prioritize_known_pops should known delineated populations be
#'  prioritized (default = TRUE if a valid spp_pops_fn is provided)
#' @param prioritize_known_occ should known occurrence locations be prioritized?
#'  (default = TRUE if a valid spp_occ_fn is provided)
#' @param single_pu_pop only one location (ie unit) selected per delineated
#'  population (only when prioritize_known_pops = TRUE)
#' @param n number of solutions to generate
#' @param cores number of cores to use (default = all available)
#' @param batch_size when parallel processing is used, this parameter can be used
#'  to process solution in batches for improved efficiency (default = NULL). If
#'  not provided (NULL), batch size will be calculated automatically as
#'  `ceiling(n/(cores*4))`.
#' @param max_batch_size limit batch size for parallel processing (default =
#'  1000). Smaller values will allow the progress bar to update more frequently,
#'  but at the cost of more disk writes.
#'  @param progress show progress bar
#' @param max_candidate_pops maximum number of candidate units to consider for a
#'  species at each round of selection (default = Inf). This will subset species
#'  data by highest suitability score to the number of units listed.
#' @param seed seed for reproducible output (optional)
#' @param output_prefix prefix for output files (default = 'solutions').
#' @param output_csv Should the solutions be written to a single csv file
#'  (default = TRUE)
#' @param output_parquet Should the solutions be written to a parquet file
#'  (default = FALSE)
#' @param delete_tmp_files should temporary files be deleted after processing
#'  (default = TRUE)
#' @param return_df return all generated solutions as a data frame (default =
#'  FALSE)
#'
#' @import progressr
#'
#' @export
#'
optimTFE <- function(
    # Data Inputs
    dir = here::here(),
    spp_targets_fn = NULL,
    spp_suit_fn = NULL,
    sub_regions_fn = NULL,
    spp_pops_fn = NULL,
    spp_occ_fn = NULL,
    # Config parameters
    min_spp_suit_score = 0,
    max_candidate_pops = Inf,
    rand_tolerance = 5,
    max_spp_selected = Inf,
    use_subregion_targets = !is.null(sub_regions_fn) && file.exists(sub_regions_fn),
    prioritize_known_pops = !is.null(spp_pops_fn) && file.exists(spp_pops_fn),
    prioritize_known_occ = !is.null(spp_occ_fn) && file.exists(spp_occ_fn),
    single_pu_pop = TRUE,
    # Compute parameters
    n = 100,
    cores = future::availableCores(),
    progress = TRUE,
    batch_size = NULL,
    max_batch_size = 1000,
    seed = NULL,
    # Output parameters
    output_prefix = "solutions",
    output_csv = TRUE,
    output_parquet = FALSE,
    delete_tmp_files = TRUE,
    return_df = FALSE) {
  message("Begining optimTFE...")
  start_time <- Sys.time()

  # Determine if progress bar should be used
  if(isTRUE(progress) &&
     interactive() &&
     !isTRUE(getOption('knitr.in.progress')) &&
     !isTRUE(getOption("rstudio.notebook.executing"))){
    progressr::handlers(global=TRUE)
  }else if(interactive() &&
           !isTRUE(getOption('knitr.in.progress')) &&
           !isTRUE(getOption("rstudio.notebook.executing"))){
    progressr::handlers(global=FALSE)
  }

  # Output Files ----
  if (length(output_prefix) == 0L || nchar(output_prefix) == 0L) {
    stop("output_prefix must be a non-empty string")
  }
  if (output_csv) {
    csv_file <- file.path(dir, "output", paste0(output_prefix, ".csv"))
    if (file.exists(csv_file)) {
      message(crayon::red("Output file already exists:"))
      message(crayon::white("  ", csv_file))
      message(crayon::red("Please delete existing outputs or change the solution prefix."))
      return(invisible())
    }
  }
  if (output_parquet) {
    parquet_chk <- file.path(dir, "output", output_prefix) |>
      list.files(pattern = ".parquet$")
    if (length(parquet_chk) > 0L) {
      message(crayon::red("Output parquet file(s) already exists:"))
      message(crayon::white("  ", file.path(dir, "output", output_prefix, "part-{i}.parquet")))
      message(crayon::red("Please delete existing outputs or change the solution prefix."))
      return(invisible())
    }
  }

  # Temp files ----
  tmp_dir <- file.path(dir, "output", paste0(output_prefix, "_tmp"))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  if (length(list.files(tmp_dir) > 0L)) {
    message(crayon::red("Temporary file directory is not empty:"))
    message(crayon::white("  ", tmp_dir))
    return(invisible())
  }

  # Read inputs ----
  ## Load species targets ----
  if (length(spp_targets_fn) == 0L || !file.exists(spp_targets_fn)) {
    message(crayon::red("A species targets input is required (spp_targets_fn)."))
    return(invisible())
  }
  targets_df <- read.csv(spp_targets_fn)
  colnames(targets_df)[1:2] <- c("species", "total")
  spp_names <- targets_df[, 1] |> sort()
  if (any((targets_df[, -1] %% 1) > 0L) || any(targets_df[, -1] < 0L)) {
    message(crayon::red("Species targets must be positive integers."))
    return(invisible())
  }
  message(crayon::white(glue::glue("Species targets loaded: {length(spp_names)}")))

  ## Load Suitability matrix ----
  if (length(spp_suit_fn) == 0L || !file.exists(spp_suit_fn)) {
    message(crayon::red("A species suitability matrix is required (spp_suit_fn)."))
    return(invisible())
  }
  spp_suit <- read.csv(spp_suit_fn)
  colnames(spp_suit)[1] <- "unit_id"

  # Validate species names in suitability matrix
  if (!all.equal(spp_names, sort(colnames(spp_suit)[-1]))) {
    message(crayon::red("Species names in suitability matrix do not match species targets."))
    return(invisible())
  }
  message(crayon::white("Species suitability matrix loaded"))

  ## Load sub-region delineations ----
  if (length(sub_regions_fn) == 1L && file.exists(sub_regions_fn)) {
    sub_regions <- read.csv(sub_regions_fn)
    colnames(sub_regions)[1] <- "unit_id"
    if (!all.equal(sort(colnames(targets_df)[-1:-2]), sort(colnames(sub_regions)[-1]))) {
      message(crayon::red("Sub-region names do not match species targets."))
      return(invisible())
    }
    # TODO - need to validate binary data
    sub_regions <- sub_regions |>
      tidyr::pivot_longer(
        -unit_id,
        names_to = "region",
        values_to = "included"
      ) |>
      dplyr::filter(
        included == 1
      )
    message(crayon::white("Sub-region data loaded: ", paste(unique(sub_regions$region), collapse = ", ")))

    # Validate sub region goal compatible with total goal for each species
    subRegions_possible <- purrr::map2_lgl(targets_df[, 2], rowSums(targets_df[, -1:-2]), ~ .x >= .y)
    if (any(!subRegions_possible)) {
      message(crayon::red("Sub-region targets greater that total targets for some species:"))
      message(crayon::white(paste(targets_df$species[!subRegions_possible], collapse = ", ")))
      return(invisible())
    }
  }

  ## Load known populations matrix ----
  if (prioritize_known_pops) {
    spp_pops <- read.csv(spp_pops_fn)
    colnames(spp_pops)[1] <- "unit_id"
    message(crayon::white("Known population matrix loaded."))
    # validate planning units
    if (!all(spp_pops[, 1] %in% spp_suit[, 1])) {
      message(crayon::red("Known population matrix includes unit_ids not present in spp suitability matrix."))
      return(invisible())
    }
    # validate species
    if (!all(colnames(spp_pops)[-1] %in% targets_df[, 1])) {
      message(crayon::red("Known population matrix includes unexpected species names."))
      return(invisible())
    }
  } else {
    spp_pops <- spp_suit
    spp_pops[, -1] <- 0
  }

  ## Load known occurrences matrix ----
  if (prioritize_known_occ) {
    spp_occ <- read.csv(spp_occ_fn)
    colnames(spp_occ)[1] <- "unit_id"
    message(crayon::white("Known occurance matrix loaded."))
    # validate planning units
    if (!all(spp_occ[, 1] %in% spp_suit[, 1])) {
      message(crayon::red("Known occurance matrix includes unit_ids not present in spp suitability matrix."))
      return(invisible())
    }
    # validate species
    if (!all(colnames(spp_occ)[-1] %in% targets_df[, 1])) {
      message(crayon::red("Known occurance matrix includes unexpected species names."))
      return(invisible())
    }
  } else {
    spp_occ <- spp_suit
    spp_occ[, -1] <- 0
  }

  # Compile species goals ----
  goals <- purrr::map_dfr(spp_names, \(spp){
    spp_goals <- spp_suit |>
      dplyr::select(unit_id, dplyr::sym(spp)) |>
      tidyr::pivot_longer(
        -unit_id,
        names_to = "species",
        values_to = "suitability"
      ) |>
      dplyr::left_join(
        dplyr::transmute(
          targets_df,
          species = species,
          total = total,
          region = "default",
          max = total,
          min = 0
        ),
        by = "species"
      )

    if (use_subregion_targets) {
      spp_subregions <- targets_df |>
        dplyr::filter(species == spp) |>
        tidyr::pivot_longer(
          -species,
          names_to = "region",
          values_to = "min"
        ) |>
        dplyr::filter(
          region != "total" & min > 0
        )
      for (i in seq_along(spp_subregions$region)) {
        subregion_target <- spp_subregions$min[i]
        subregion <- spp_subregions$region[i]
        units <- sub_regions |>
          dplyr::filter(region == subregion) |>
          dplyr::pull(unit_id)
        spp_goals <- spp_goals |>
          dplyr::mutate(
            region = dplyr::case_when(
              unit_id %in% units ~ subregion,
              .default = region
            ),
            max = dplyr::case_when(
              !unit_id %in% units ~ max - subregion_target,
              .default = max
            ),
            min = dplyr::case_when(
              unit_id %in% units ~ subregion_target,
              .default = min
            )
          )
      }
    }
    return(spp_goals)
  })

  # Add known populations to goals ----
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
  } else {
    goals$population <- NA
  }

  # Add known occurrences to goals ----
  if (prioritize_known_occ) {
    spp_occ <- spp_occ |>
      tidyr::pivot_longer(
        -unit_id,
        names_to = "species",
        values_to = "occurrence"
      ) |>
      dplyr::filter(
        occurrence > 0
      )

    goals <- goals |>
      dplyr::left_join(
        spp_occ,
        by = c("unit_id", "species")
      ) |>
      dplyr::mutate(
        suitability = dplyr::case_when(
          !is.na(occurrence) & suitability == 0 ~ max(min_spp_suit_score, 1E-5),
          .default = suitability
        )
      )
  } else {
    goals$occurrence <- NA
  }

  # Apply minimum suitability score ----
  goals <- goals |>
    dplyr::filter(
      max > 0,
      suitability > 0 & suitability >= min_spp_suit_score
    )

  # Ensure unit_id is character ----
  goals$unit_id <- as.character(goals$unit_id)

  # Validate that overall goals can be met ----
  impossible_goals <- goals |>
    dplyr::summarise(
      available_units = dplyr::n(),
      .by = "species"
    ) |>
    dplyr::full_join(
      targets_df,
      by = "species"
    ) |>
    tidyr::replace_na(list(available_units = 0)) |>
    dplyr::filter(
      available_units < total
    )
  if (nrow(impossible_goals) > 0) {
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

  # Validate that sub-region goals can be met ----
  impossible_goals <- goals |>
    dplyr::summarise(
      available_units = dplyr::n(),
      min = unique(min),
      .by = c("species", "region")
    ) |>
    dplyr::left_join(
      targets_df,
      by = "species"
    ) |>
    dplyr::filter(
      available_units < min
    )
  if (nrow(impossible_goals) > 0) {
    message(crayon::red("Some sub-region species goals can not be met:"))
    for (i in seq_len(nrow(impossible_goals))) {
      glue::glue(
        "{impossible_goals$species[i]} - {impossible_goals$region[i]}: {impossible_goals$available_units[i]} units available, {impossible_goals$min[i]} required."
      ) |>
        crayon::white() |>
        message()
    }
    return(invisible())
  }

  # Generate Solutions ----
  message(crayon::white("Generating solutions..."))
  # Batch
  if (is.null(batch_size)) {
    batch_size <- min(ceiling(n / cores), max_batch_size)
  }
  btchs <- seq_len(n) |>
    {
      \(x) split(x, ceiling(x / batch_size))
    }()
  # Set up future backend
  if(parallelly::supportsMulticore()){
    future_mode <- future::multicore
  }else{
    future_mode <- future::multisession
  }
  future::plan(future_mode, workers = cores)
  p <- progressor(along = btchs) # Init progress bar

  tmp_files <- furrr::future_map(btchs, ~ {
    # TEMP file to write batch results
    tmp_file <- tempfile(tmpdir = tmp_dir, fileext = ".csv")

    # Find solutions batch and write to tmp csv
    purrr::map_dfr(.x, ~ {
      get_solution(
        idx = .x,
        goals = goals,
        rand_tolerance = rand_tolerance,
        max_candidate_pops = max_candidate_pops,
        max_spp_selected = max_spp_selected,
        prioritize_known_pops = prioritize_known_pops,
        prioritize_known_occ = prioritize_known_occ,
        single_pu_pop = single_pu_pop
      )
    }) |>
      readr::write_csv(tmp_file, num_threads = 1, progress = FALSE)

    p() # update progress bar
    return(tmp_file)
  }, .options = furrr::furrr_options(seed = ifelse(is.null(seed), TRUE, seed)))

  # Save outputs ----
  data <- arrow::open_dataset(tmp_dir, format = "csv")
  if (output_csv) {
    csv_path <- file.path(dir, "output", paste0(output_prefix, ".csv"))
    glue::glue(
      "Saving csv output: {crayon::underline(csv_path)}"
    ) |>
      crayon::white() |>
      message()
    arrow::write_csv_arrow(data, csv_path)
  }
  if (output_parquet) {
    pq_path <- file.path(dir, "output", output_prefix)
    glue::glue(
      "Saving parquet output: {crayon::underline(pq_path)}"
    ) |>
      crayon::white() |>
      message()
    arrow::write_parquet(data, pq_path)
  }
  if (return_df) {
    message(crayon::white("Preparing results as tibble"))
    out <- dplyr::as_tibble(data)
  }

  # Write metadata ----
  elapsed_time <- Sys.time() - start_time
  meta <- list(
    time_elapsed = glue::glue(
      "{round(elapsed_time, 1)} {attr(elapsed_time, 'units')}"
    ),
    targets = seq_len(nrow(targets_df)) |>
      setNames(targets_df$species) |>
      purrr::imap(~ {
        targets <- targets_df[.x, -1] |> as.list()
        targets[targets > 0]
      }),
    rand_tolerance = rand_tolerance,
    max_candidate_pops = max_candidate_pops,
    max_spp_selected = max_spp_selected,
    prioritize_known_pops = prioritize_known_pops,
    prioritize_known_occ = prioritize_known_occ,
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
    write(file.path(dir, "output", paste0(output_prefix, ".meta")))

  # Delete temp files ----
  if (delete_tmp_files) {
    message(crayon::white("Cleaning up temp files"))
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
