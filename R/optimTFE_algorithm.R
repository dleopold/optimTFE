#' optimTFE algorithm: generate a suite of conservation footprint 'solutions'
#' based on Feature suitability values and a list of Targets for all Features.
#'
#' @param dir working directory
#' @param spp_targets_fn .csv of species targets. First 2 columns should be
#'   species names and total targets populations, respectively. Additional
#'   columns should be provided when using subregion targets. Column names must
#'   match the sub-region names provided in the sub_regions_fn input and values
#'   should be the minimum number of populations required in the the subregion.
#' @param spp_suit_fn input species suitability matrix. Rows are locations and
#'   columns are species. Must include species names and location ids in the
#'   column and row names, respectively.
#' @param sub_regions_fn (optional) A csv input defining sub-regions. Should
#'   include 1 row for each planning unit with binary values indicating the
#'   sub-region membership of each planning unit.
#' @param use_subregion_targets should subregion targets be used (default = TRUE
#'   if a valid sub_regions_fn is provided).
#' @param spp_pops_fn (optional) .csv input matrix of delineated populations.
#'   Row / column names must match suitability matrix
#' @param prioritize_known_pops should known delineated populations be
#'   prioritized (default = TRUE if a valid spp_pops_fn is provided)
#' @param single_pu_pop only one location (ie unit) selected per delineated
#'   population (only when prioritize_known_pops = TRUE)
#' @param rand_tolerance  the range of species richness, from maximum, to
#'   consider for selection at each iteration(default = 5)
#' @param max_spp_selected maximum number of species to select in each location
#'   (default = Inf) to reduce 'species packing' within units. This parameter
#'   could cause species to be unable to meet targets.
#' @param min_spp_suit_score minimum suitability score for a species to be
#'   considered in a location (default = 0)
#' @param max_candidate_units maximum number of candidate units to consider for a
#'   species at each round of selection (default = Inf). This will subset
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
#'   'output')`)
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
    # Data Inputs
    dir = here::here(),
    spp_targets_fn = NULL,
    spp_suit_fn = NULL,
    sub_regions_fn = NULL,
    spp_pops_fn = NULL,
    # Config parameters
    min_spp_suit_score = 0,
    max_candidate_units = Inf,
    rand_tolerance = 5,
    max_spp_selected = Inf,
    use_subregion_targets = !is.null(sub_regions_fn) && file.exists(sub_regions_fn),
    prioritize_known_pops = !is.null(spp_pops_fn) && file.exists(spp_pops_fn),
    single_pu_pop = TRUE,
    # Compute parameters
    n = 100,
    cores = future::availableCores(),
    progress = TRUE,
    batch_size = NULL,
    max_batch_size = 1000,
    min_batch_size = 10,
    seed = NULL,
    # Output parameters
    output_dir = file.path(dir, "output"),
    output_prefix = "solutions",
    output_csv = TRUE,
    output_parquet = FALSE,
    force_overwrite = FALSE,
    delete_tmp_files = TRUE,
    return_df = FALSE) {
  message("Beginning optimTFE...")
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
  tmp_dir <- file.path(output_dir, paste0(output_prefix, "_tmp"))
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
  if (length(spp_targets_fn) == 0L || !file.exists(spp_targets_fn)) {
    stop(crayon::bold(crayon::red("A species targets input is required (spp_targets_fn).")))
  }
  targets_df <- read.csv(spp_targets_fn)
  colnames(targets_df)[1:2] <- c("species", "total")
  spp_names <- targets_df[, 1] |> sort()
  if (any((targets_df[, -1] %% 1) > 0L) || any(targets_df[, -1] < 0L)) {
    stop(crayon::bold(crayon::red("Species targets must be positive integers.")))
  }
  message(crayon::cyan(glue::glue("Species targets loaded: {length(spp_names)}")))

  ## Load Suitability matrix ----
  if (length(spp_suit_fn) == 0L || !file.exists(spp_suit_fn)) {
    stop(crayon::bold(crayon::red("A species suitability matrix is required (spp_suit_fn).")))
  }
  spp_suit <- read.csv(spp_suit_fn)
  colnames(spp_suit)[1] <- "unit_id"

  # Validate species names in suitability matrix
  if (!all.equal(spp_names, sort(colnames(spp_suit)[-1]))) {
    stop(crayon::bold(crayon::red("Species names in suitability matrix do not match species targets.")))
  }
  message(crayon::cyan("Species suitability matrix loaded"))

  ## Load sub-region delineations ----
  if (length(sub_regions_fn) == 1L && file.exists(sub_regions_fn)) {
    sub_regions <- read.csv(sub_regions_fn)
    colnames(sub_regions)[1] <- "unit_id"
    if (!all.equal(sort(colnames(targets_df)[-1:-2]), sort(colnames(sub_regions)[-1]))) {
      stop(crayon::bold(crayon::red("Sub-region names do not match species targets.")))
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
    message(crayon::cyan("Sub-region data loaded: ", paste(unique(sub_regions$region), collapse = ", ")))

    # Validate sub region goal compatible with total goal for each species
    subRegions_possible <- purrr::map2_lgl(targets_df[, 2], rowSums(targets_df[, -1:-2]), ~ .x >= .y)
    if (any(!subRegions_possible)) {
      message(crayon::bold(crayon::red("Sub-region targets greater that total targets for some species:")))
      targets_df$species[!subRegions_possible] |>
        purrr::walk(~{
          message(crayon::white(crayon::bgRed(.x)))
        })
      stop()
    }
  }

  ## Load known populations matrix ----
  if (prioritize_known_pops) {
    spp_pops <- read.csv(spp_pops_fn)
    colnames(spp_pops)[1] <- "unit_id"
    message(crayon::cyan("Known population matrix loaded."))
    # validate planning units
    if (!all(spp_pops[, 1] %in% spp_suit[, 1])) {
      stop(crayon::bold(crayon::red("Known population matrix includes unit_ids not present in spp suitability matrix.")))
    }
    # validate species
    if (!all(colnames(spp_pops)[-1] %in% targets_df[, 1])) {
      stop(crayon::bold(crayon::red("Known population matrix includes unexpected species names.")))
    }
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
          min = 0,
          consider = TRUE
        ),
        by = "species"
      )

    if (use_subregion_targets) {
      region_key <- sub_regions$region |> setNames(sub_regions$unit_id)
      spp_goals <- spp_goals |>
        dplyr::select(-region) |>
        dplyr::left_join(
          dplyr::select(
            sub_regions,
            unit_id,
            region
          ),
          by = "unit_id"
        ) |>
        tidyr::replace_na(
          list(region = "default")
        )
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
    spp_goals |>
      dplyr::mutate(
        n_units = dplyr::n()
      )
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

  # Check that populations do not span sub-regions
  if(prioritize_known_pops && use_subregion_targets){
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
    if(nrow(validatation) > 0L){
      message(crayon::bold(crayon::red("Known populations must not span sub-region boundaries:")))
      purrr::pwalk(validatation, function(species, population, cnt, region){
        message(glue::glue("{crayon::italic(species)}, population {population} found in: {region}"))
      })
      stop()
    }
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
  # Batch
  if (is.null(batch_size)) {
    batch_size <- min(ceiling(n / cores), max_batch_size)
    if(!is.null(min_batch_size)){
      batch_size <- max( batch_size, min_batch_size )
    }
  }
  btchs <- seq_len(n) |>
    {
      \(x) split(x, ceiling(x / batch_size))
    }()
  message(crayon::cyan(glue::glue("Generating {n} solutions in {length(btchs)} batches...")))
  # Set up future backend
  if(parallelly::supportsMulticore()){
    future_mode <- future::multicore
  }else{
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
        rand_tolerance=rand_tolerance,
        max_candidate_units=max_candidate_units,
        max_spp_selected=max_spp_selected,
        prioritize_known_pops=prioritize_known_pops,
        single_pu_pop=single_pu_pop
        )
      ),
    ~{
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
    targets = seq_len(nrow(targets_df)) |>
      setNames(targets_df$species) |>
      purrr::imap(~ {
        targets <- targets_df[.x, -1] |> as.list()
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
