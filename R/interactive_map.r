#' Create Interactive Map
#'
#' Generates an interactive map visualizing conservation footprints and
#' additional layers specified by the user.
#'
#' @param solution_idx Integer, index of the solution to visualize.
#' @param dir Character, directory containing the optimTFE output. Default is current directory.
#' @param run_id Character, identifier for the optimTFE run. Default is "optimTFE".
#' @param spatial SF or Spatial object, containing planning unit polygons with unit_id field.
#' @param map_tiles Character, name of the leaflet provider background tiles. Default is
#'   "Esri.WorldTopoMap". Set to NULL to use no background map tiles.
#' @param spp_layers Character vector, names of species to add as separate overlay layers.
#' @param auxiliary_layers Named list, paths to additional spatial files to add as overlay layers.
#' @param html_out Character, file path to save a self contained, sharable HTML file.
#'
#' @return A leaflet map object.
#'
#' @import leaflet sf htmltools
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr left_join
#' @importFrom purrr map pmap
#' @importFrom glue glue
#' @importFrom htmlwidgets saveWidget
#'
#' @family utils
#' @export
#'
create_interactive_map <- function(
  solution_idx = NULL,
  dir = ".",
  run_id = "optimTFE",
  spatial = NULL,
  map_tiles = "Esri.WorldTopoMap",
  spp_layers = NULL,
  auxiliary_layers = NULL,
  html_out = NULL
) {
  # Load footprint ----
  if (length(solution_idx) != 1 && !is.numeric(solution_idx)) {
    return()
  }
  meta <- jsonlite::fromJSON(file.path(dir, run_id, paste0(run_id, ".meta")))
  solution <- extract_solutions(
    dir = dir,
    run_id = run_id,
    solutions = solution_idx,
    columns = c("unit_id", "richness", meta$spp_names)
  )
  if (nrow(solution) == 0L || length(meta) == 0L) {
    return()
  }

  # Load spatial data ---
  if (is.character(spatial) && file.exists(spatial)) {
    spatial <- tryCatch(
      read_sf(spatial),
      error = function(e) NULL
    )
  }
  if (!inherits(spatial, "sf") && !inherits(spatial, "Spatial")) {
    stop(crayon::bold(crayon::red(
      "Invalid spatial data input."
    )))
  }
  if (!all(st_is(spatial, "POLYGON") | st_is(spatial, "MULTIPOLYGON"))) {
    stop(crayon::bold(crayon::red(
      "Spatial input must only contain POLYGON geometries."
    )))
  }
  if (!all(meta$unit_ids %in% spatial[[1]])) {
    stop(crayon::bold(crayon::red(
      "Spatial input must include all unit ids from the solutions data."
    )))
  }
  # Force to WGS84 for consistency with most provider tiles
  spatial <- st_transform(spatial, "EPSG:4326")
  solution <- dplyr::left_join(
    spatial,
    solution,
    by = "unit_id"
  )

  # Generate hover labels ----
  labels <- purrr::map(
    solution$unit_id,
    ~ {
      span(
        style = "display: flex; gap : 1em;",
        tags$strong("Unit ID: "),
        .x
      ) |>
        as.character() |>
        htmltools::HTML()
    }
  )

  # Generate popups ----
  popups <- purrr::pmap(solution, function(...) {
    cur <- list(...)
    possible_richness <- meta$suitability[meta$unit_ids == cur$unit_id, ] >=
      meta$min_spp_suit_score
    if (!is.null(meta$populations)) {
      possible_richness <- possible_richness |
        meta$populations[meta$unit_ids == cur$unit_id, ] > 0
    }
    targets <- cur[meta$spp_names]
    targets[is.na(targets)] <- 0
    tagList(
      span(
        style = "display: flex; gap : 1em;",
        tags$strong("Unit ID: "),
        cur$unit_id
      ),
      hr(),
      tags$details(
        tags$summary(
          glue::glue(
            "Target species: {cur$richness %|NA|% 0}"
          )
        ),
        div(
          style = "display: flex; flex-flow: column nowrap; max-height: 8em; overflow-y: auto;",
          purrr::map(names(targets)[targets > 0], ~ span(.x))
        )
      ),
      tags$details(
        tags$summary(
          glue::glue(
            "Possible species: {sum(possible_richness)}"
          )
        ),
        div(
          style = "display: flex; flex-flow: column nowrap; max-height: 8em; overflow-y: scroll;",
          purrr::map(meta$spp_names[possible_richness], ~ span(.x))
        )
      )
    ) |>
      as.character() |>
      htmltools::HTML()
  })

  # Generate popup ----

  # Base Map ----
  bounds <- st_bbox(solution)
  map <- leaflet(
    options = leafletOptions(
      attributionControl = FALSE
    )
  ) |>
    addMapPane("units", zIndex = 500) |>
    addPolygons(
      data = solution,
      weight = 1,
      opacity = 0.5,
      color = "grey",
      fillOpacity = 0,
      popup = popups,
      label = labels,
      labelOptions = labelOptions(
        opacity = 0.65,
        style = list(
          "border-color" = "grey"
        ),
        textsize = "15px",
        direction = "auto"
      ),
      highlight = highlightOptions(
        weight = 5,
        bringToFront = FALSE
      ),
      options = pathOptions(
        pane = "units",
        group = "Planning units"
      )
    ) |>
    addPolygons(
      data = solution[!is.na(solution$richness), ],
      weight = 2,
      color = "black",
      fillColor = "green",
      fillOpacity = 1,
      options = pathOptions(
        pane = "units",
        group = "Conservation footprint",
        clickable = FALSE
      )
    )

  # Add sinlge spp layer ----
  single_sp_PU <- solution[!is.na(solution$richness), ]
  single_sp_PU <- single_sp_PU[
    rowSums(st_drop_geometry(single_sp_PU)[meta$spp_names]) == 1,
  ]
  map <- map |>
    addPolygons(
      data = single_sp_PU,
      weight = 2,
      color = "black",
      fillColor = "red",
      fillOpacity = 1,
      options = pathOptions(
        pane = "units",
        group = "Single species PU",
        clickable = FALSE
      )
    ) |>
    hideGroup("Single species PU")

  # Add target spp richness layer ----
  pal_richness <- colorNumeric("YlOrRd", solution$richness)
  map <- map |>
    addPolygons(
      data = solution[!is.na(solution$richness), ],
      weight = 2,
      color = "black",
      fillColor = ~ pal_richness(richness),
      fillOpacity = 1,
      options = pathOptions(
        pane = "units",
        group = "Target species richness",
        clickable = FALSE
      )
    ) |>
    hideGroup("Target species richness")

  # Add background tiles ----
  if (!is.null(map_tiles)) {
    map <- map %>%
      addProviderTiles(
        providers[[map_tiles]],
        group = "Contour background"
      )
  }

  # Default layer groups ----
  groups <- c(
    "Target species richness",
    "Single species PU",
    "Conservation footprint",
    "Planning units"
  )

  # Species layers ----
  if (length(spp_layers) > 0) {
    map <- map |>
      addMapPane("spp", zIndex = 300)
    for (spp in spp_layers) {
      spp_data <- solution[!is.na(solution$richness), ]
      col <- ifelse(spp_data[[spp]] > 0, "orange", "green")
      map <- map %>%
        addPolygons(
          data = spp_data,
          weight = 2,
          col = col,
          fillColor = "orange",
          fillOpacity = 1,
          options = pathOptions(
            pane = "aux",
            group = spp,
            clickable = FALSE
          )
        ) |>
        hideGroup(spp)
      groups <- c(groups, spp)
    }
  }

  # Aux layers ----
  if (length(auxiliary_layers) > 0) {
    map <- map |>
      addMapPane("aux", zIndex = 400)
    for (i in seq_along(auxiliary_layers)) {
      layer <- tryCatch(
        read_sf(auxiliary_layers[i]) |>
          st_transform("EPSG:4326"),
        error = function(e) {
          return(NULL)
        }
      )
      if (is.null(layer)) {
        message("failed to load auxiliary layer")
        next
      }
      layer_name <- names(auxiliary_layers)[i] %||%
        paste("Auxiliary layer", i)
      map <- map |>
        addPolygons(
          data = layer,
          weight = 1,
          col = "black",
          fillColor = "blue",
          fillOpacity = 0.25,
          options = pathOptions(
            pane = "aux",
            group = layer_name,
            clickable = FALSE
          )
        ) |>
        hideGroup(layer_name)
      groups <- c(groups, layer_name)
    }
  }

  # Layers control ----
  map <- map |>
    addLayersControl(
      overlayGroups = groups,
      baseGroups = c("Contour background"),
      options = layersControlOptions(
        collapsed = FALSE,
        hideSingleBase = T,
        sortLayers = F
      )
    ) |>
    addLegend(
      pal = pal_richness,
      values = solution$richness[!is.na(solution$richness)],
      opacity = 1,
      title = NULL,
      position = "bottomright",
      layerId = "legend_richness",
      group = "Target species richness"
    ) |>
    removeControl("legend_richness")

  if (!is.null(html_out)) {
    saveWidget(map, file = html_out)
  }

  return(map)
}
