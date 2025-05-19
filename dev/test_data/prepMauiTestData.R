dat <- sf::st_read("~/Downloads/input_metrics_EMaui_2025.gpkg")

dat |> dplyr::select(PU_num) |> dplyr::rename(unit_id = PU_num) |> saveRDS("output/spatial.rds")

targets <- read.csv("dev/targets2.csv")

suitability <- dat |>
  sf::st_drop_geometry() |>
  dplyr::select(PU_num, dplyr::starts_with("suit_")) |>
  dplyr::mutate(PU_num = as.integer(PU_num))
colnames(suitability) <- c("uniqueId", targets$species)

known_pops <- dat |>
  sf::st_drop_geometry() |>
  dplyr::select(PU_num, dplyr::starts_with("PtCt_")) |>
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("PtCt_"), ~ as.integer(.x > 0))) |>
  dplyr::mutate(PU_num = as.integer(PU_num))
colnames(known_pops) <- c("uniqueId", targets$species)
known_pops <- tidyr::pivot_longer(
  known_pops,
  -1,
  names_to = "taxon",
  values_to = "population"
) |>
  dplyr::mutate(
    population = stringr::str_squish(population) |> as.numeric()
  ) |>
  dplyr::filter(population != 0) |>
  dplyr::mutate(
    population = seq_along(population),
    .by = "taxon"
  )


for(i in 1:nrow(known_pops)){

  suit <- suitability[suitability$uniqueId == known_pops$uniqueId[i], known_pops$taxon[i]]
  if(suit < 0.25){
    suitability[suitability$uniqueId == known_pops$uniqueId[i], known_pops$taxon[i]] <- 0.25
  }

}

write.csv(known_pops, "dev/populations2.csv", row.names = FALSE)
write.csv(suitability, "dev/suitability2.csv", row.names = FALSE)

optimTFE(
  dir = ".",
  targets_in = "dev/targets2.csv",
  suitability_in = "dev/suitability2.csv",
  populations_in = "dev/populations2.csv",
  rand_tolerance = 5,
  min_spp_suit_score = 0.25,
  n = 20000,
  cores = 12
)

solution <- readr::read_csv("output/solutions.csv", show_col_types = F) |>
  dplyr::filter(solution == 12207)

# Minimal footprint inputs
solution <- read.csv("../optimTFE.footprint/inst/output/solutions.csv") |>
  dplyr::filter(solution == "Complete")
spp <- colnames(solution)[-1:-3]
suitability <- read.csv("dev/suitability2.csv")
for(sp in spp){
  units <- solution$unit_id[solution[[sp]] == 1]
  cur <- suitability[[sp]]
  cur[!suitability$uniqueId %in% units] <- 0
  suitability[[sp]] <- cur
  any(cur[suitability$uniqueId %in% units] == 0) |> print()
}

populations <- read.csv("dev/populations2.csv") |>
  dplyr::filter(uniqueId %in% solution$unit_id)
for (sp in spp) {
  units <- solution$unit_id[solution[[sp]] == 1]
  exclude <- !(populations$uniqueId %in% units) & populations$taxon == sp
  populations <- populations[!exclude, ]
}

write.csv(suitability, "dev/suitability3.csv", row.names = FALSE)
write.csv(populations, "dev/populations3.csv", row.names = FALSE)

optimTFE(
  dir = ".",
  output_dir = "output2",
  targets_in = "dev/targets3.csv",
  suitability_in = "dev/suitability3.csv",
  populations_in = "dev/populations3.csv",
  rand_tolerance = 10,
  n = 20000,
  cores = 12
)

foo <- jsonlite::fromJSON("output/solutions.meta")
spp <- foo$targets |> names()
for(sp in spp){
  foo$targets[[sp]]$total <- sum(dat[[sp]])
}
foo$suitability = suitability |>
  purrr::pmap(function(...) {
    suits <- list(...)
    suits[suits != 0]
  })

jsonlite::toJSON(foo,auto_unbox = TRUE) |>
  jsonlite::prettify() |>
  write("solutions.meta")
