library(sf)
set.seed(34534)

# Generate spatial data (hexagonal grid)
bbox_poly <- st_sfc(
  st_polygon(
    list(
      rbind(
        c(0, 0),
        c(50, 0),
        c(50, 50),
        c(0, 50),
        c(0, 0)
      )
    )
  )
)
hex_grid <- st_make_grid(
  bbox_poly,
  cellsize = 2,
  square = FALSE
)
hex_sf <- st_sf(
  unit_id = paste0("unit_", seq_along(hex_grid)),
  geometry = hex_grid
)

# Define a Lambert Azimuthal Equal‐Area projection centered on the hex grid
laea_crs <- "+proj=laea +lat_0=25 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
hex_sf <- st_set_crs(hex_sf, laea_crs)

# Generate speies testing data for 30 species
n_units <- nrow(hex_sf)
unit_names <- hex_sf$unit_id
n_species <- 30
spp_names <- paste0("sp_", 1:n_species)

## Random suitability decay rates
rate_range <- c(0.01, 0.2)
rates <- runif(n_species, rate_range[1], rate_range[2])

# Unit centroids for computing distances
centroids <- st_centroid(hex_sf)
coords <- st_coordinates(centroids)

# Initialize suitability matrix
suitability_mx <- matrix(
  NA_real_,
  n_units,
  n_species,
  dimnames = list(unit_names, spp_names)
)

# Define Regions
region_mx <- matrix(0, n_units, 2, dimnames = list(unit_names, c("left", "right")))
region_mx[
  coords[, 1] <= 25 & coords[, 2] >= 25,
  'left'
] <- 1
region_mx[
  coords[, 1] >= 25 & coords[, 2] <= 25,
  'right'
] <- 1
left_core <- which(coords[, 1] > 5 & coords[, 1] < 20 & coords[, 2] > 30 & coords[, 2] < 45)
right_core <- which(coords[, 1] > 30 & coords[, 1] < 45 & coords[, 2] > 5 & coords[, 2] < 20)

# Prepare targets matrix
targets_mx <- matrix(0, n_species, 3, dimnames = list(spp_names, c("total", "left", "right")))
targets_mx[, "total"] <- 10

# Intialize population matrix
population_mx <- matrix(
  NA_character_,
  n_units,
  n_species,
  dimnames = list(unit_names, spp_names)
)
# find touching units for merging neighboring population units
touches <- st_touches(hex_sf)

# Loop over species to populate syntheic data
for (j in seq_len(n_species)) {

  print(spp_names[j])

  # choose 1 or 2 regions
  n_regions <- sample(1:2, 1)
  regions <- sample(c("left", "right"), n_regions, replace = FALSE)

  # Find a core unit for each region
  cores_idx <- purrr::map_dbl(regions, ~{
    if(.x == "left"){
      opts <- left_core
    } else {
      opts <- right_core
    }
    sample(opts, 1)
    })
  core_xy <- coords[cores_idx, , drop = FALSE]

  # compute distance from every cell to each core
  dists <- sapply(seq_len(n_regions), function(k) {
    sqrt(
      (coords[, 1] - core_xy[k, 1])^2 +
        (coords[, 2] - core_xy[k, 2])^2
    )
  })

  # collapse to minimum distance (when 2 cores)
  if (n_regions > 1){
    dists <- apply(dists, 1, min)
  }

  # compute suitability using exponantial decay with distance
  suitability_mx[, j] <- exp(-rates[j] * dists)

  # select regional targets for species found in 2 regions
  if(n_regions == 2){
    targets_mx[j, "left"] <- sample(1:5, 1)
    targets_mx[j, "right"] <- sample(1:5, 1)
  }

  # select populations
  if(runif(1) < 0.2) next
  pop_opts <- which(suitability_mx[, j] > 0.2)
  pop_cnt <- min(ceiling(length(pop_opts) / 2), sample(0:20, 1))
  pops <- sample(pop_opts, pop_cnt, replace = FALSE)
  pop_idx <- 1
  for(pop in pops){
    pops_remaining <- setdiff(pops, which(population_mx[, j] > 0))
    if(!pop %in% pops_remaining) next
    population_mx[pop, j] <- LETTERS[pop_idx]
    pops_remaining <- setdiff(pops, which(population_mx[, j] > 0))
    # Find and merge neighbors
    neighbors <- intersect(st_touches(hex_sf)[[pop]], pops_remaining)
    while(length(neighbors) > 0){
      population_mx[neighbors, j] <- LETTERS[pop_idx]
      pops_remaining <- setdiff(pops, which(population_mx[, j] > 0))
      neighbors <- intersect(unlist(unique(touches[neighbors])), pops_remaining)
    }
    pop_idx <- pop_idx + 1
  }

}

# Create incompatibility matrix
incompat_mx <- matrix(0, n_species, n_species, dimnames = list(spp_names, spp_names))
# Randomly select 5 incomptable groupings of 2-3 spp
for(i in 1:5){
  n <- sample(3:2, 1)
  spp <- sample(spp_names, n, replace = FALSE)
  print(spp)
  for(sp in spp){
    incompat_mx[sp, setdiff(spp, sp)] <- 1
  }
}

# Save as pkg data
example_spatial <- hex_sf
usethis::use_data(example_spatial, overwrite = TRUE)
example_suitability <- suitability_mx
usethis::use_data(example_suitability, overwrite = TRUE)
example_targets <- targets_mx
usethis::use_data(example_targets, overwrite = TRUE)
example_subregions <- region_mx
usethis::use_data(example_subregions, overwrite = TRUE)
example_populations <- population_mx
usethis::use_data(example_populations, overwrite = TRUE)
example_incompatibility <- incompat_mx
usethis::use_data(example_incompatibility, overwrite = TRUE)
