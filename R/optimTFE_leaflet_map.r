#################################################
#### webmap!
#################################################
#' Create Interactive Map
#'
#' Generates an interactive map visualizing conservation footprints and
#' additional layers specified by the user.
#'
#' @param solution_result_file A .csv optimTFE output dataframe containing the solution results.
#' @param original_PU_polygons Spatial dataframe with unit ids and species scores.
#' @param selected_sol_index Integer, index of the solution to map.
#' @param auxiliaryLayers (Optional) List of auxiliary layers to add to the map.
#' @param map_individual_spp (Optional) List of species to add to the map.
#' @param save_HTML_loc (Optional) save location of html file for sharing with collaborators.
#'
#' @return An interactive leaflet map with solutions from the optimTFE alogrithm.
#' @export
#'

create_interactive_map <- function(solution_result_file, original_PU_polygons,
                                   selected_sol_index, auxiliaryLayers = NULL, map_individual_spp = NULL, save_HTML_loc = NULL) {
  solution <- subset(solution_result_file, solution == selected_sol_index) # subset specific solution
  sol1 <- original_PU_polygons %>%
    select(c(1, ncol(original_PU_polygons)))
  join_by_cols <- setNames("unit_id", names(sol1)[1])
  solution <- left_join(sol1, solution, by = join_by_cols) |>
    filter(!is.na(solution))
  if (nrow(solution) > 0) {
    spp_col_indices <- c((which(colnames(solution) == "select_order") + 1):ncol(solution)) # this assumes that species columns occur after "select_order" column
    spp_score_columns <- names(solution)[spp_col_indices]
    spp_names <- gsub("_", " ", spp_score_columns)

    PU_data_spp_score <- as.data.frame(original_PU_polygons) |>
      select(c(1:(ncol(original_PU_polygons) - 1)))

    solution$n_targets <- apply(as.data.frame(solution)[, spp_score_columns], 1, sum, na.rm = T)
    solution$single_spp_PU <- solution$n_targets == 1
    single_sp_PU <- solution[solution$single_spp_PU == 1, ]

    ########################################
    # create mouse over labels
    ########################################
    # labels for single species target layer
    spp_cols <- which(names(single_sp_PU) %in% spp_score_columns) # c(1:(dim(single_sp_PU)[2]-5))
    single_spp_picked <- spp_names[apply(as.data.frame(single_sp_PU)[, spp_cols], 1, function(x) which(x == 1))]
    single_sp_PU$single_sp_picked <- single_spp_picked
    labels <- sprintf("Single species picked:<br/><strong>%s</strong>", single_spp_picked) %>% lapply(htmltools::HTML)
    if (length(labels) == 0) labels <- NULL
    # label for conservation footprint layer
    target_labels <- c()
    for (i in c(1:dim(solution)[1])) {
      tmp <- as.data.frame(solution)[i, spp_cols]
      species_targets <- which(tmp == 1)
      species_targets <- spp_names[species_targets]

      PU_top_label <- paste0("PU ID: <strong>", as.data.frame(solution)[i, 1], " </strong><br/>")

      PU_label <- paste(c(PU_top_label, "TARGET SPECIES<br/>", species_targets), collapse = " <br/> ")
      target_labels <- c(target_labels, PU_label)
    }
    target_labels <- target_labels %>% lapply(htmltools::HTML)

    # label for all planning unit layer
    PU_ID_labels <- c()
    for (i in c(1:dim(original_PU_polygons)[1])) {
      tmp <- as.data.frame(original_PU_polygons)[i, 1]
      PU_top_label <- paste0("PU ID: <strong>", tmp, " </strong> <br/>") #<br/>

      tmp <- PU_data_spp_score[i, -1]
      species_habitat <- which(tmp > 0)
      species_habitat <- spp_names[species_habitat]
      if (length(species_habitat) == 0) species_habitat <- "None"
      PU_label <- paste(c(PU_top_label, "POTENTIAL SPECIES<br/>", species_habitat), collapse = " <br/> ")
      PU_ID_labels <- c(PU_ID_labels, PU_label)
    }
    PU_ID_labels <- PU_ID_labels %>% lapply(htmltools::HTML)

    ########################################
    # color scheme for number of target species layer
    if (!all(is.na(solution$n_targets))) {
      max_targets <- max(solution$n_targets, na.rm = TRUE)
      if (max_targets <= 40) {
        num_bins <- ceiling(max_targets / 5)
        bins <- seq(0, num_bins * 5, 5)
      } else {
        num_bins <- ceiling(max_targets / 10)
        bins <- seq(0, num_bins * 10, 10)
      }
    } else {
      bins <- numeric(0) # Or assign a default value if needed
    }
    # bins <- c(1,2,3, seq(5, ceiling(max(solution$n_targets)/5)*5, 5)) #modify!
    pal <- colorBin("YlOrRd", domain = as.data.frame(solution)$n_targets, bins = bins)

    # define name of default overlay groups, map CRS
    OG <- c("Number of target species", "Single species PU", "Conservation footprint", "All planning units")
    desiredCrs <- st_crs(original_PU_polygons)

    ########################################
    # start building map now that all elements are ready!
    map <- leaflet(original_PU_polygons) %>% addTiles() %>% # start adding the solution data
      # Base groups
      # addProviderTiles(providers$Stadia.StamenTerrainBackground, group = "Terrain background") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Contour background") %>%
      # add base map of all PUs
      addPolygons(
        data = original_PU_polygons, weight = 1, col = "grey", fillColor = "yellow", fillOpacity = 0,
        highlight = highlightOptions(
          weight = 5,
          bringToFront = TRUE
        ),
        label = PU_ID_labels,
        labelOptions = labelOptions(
          opacity = 0.5,
          style = list("font-weight" = "normal", padding = "3px 8px", "border-color" = "grey"),
          textsize = "15px",
          direction = "auto"
        ),
        group = "All planning units"
      ) %>%
      # add solution footprint
      addPolygons(
        data = solution, weight = 2, fillColor = "green", fillOpacity = 0.4,
        highlight = highlightOptions(
          weight = 5,
          fillOpacity = 0.4,
          bringToFront = TRUE
        ),
        label = target_labels,
        labelOptions = labelOptions(
          opacity = 0.5,
          style = list("font-weight" = "normal", padding = "3px 8px", "border-color" = "blue"),
          textsize = "15px",
          direction = "auto"
        ),
        group = "Conservation footprint"
      ) %>%
      # add single sp PUs
      addPolygons(
        data = single_sp_PU, weight = 2, fillColor = "red", fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          opacity = 0.5,
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        group = "Single species PU"
      ) %>%
      # add map of met targets per planning unit (PU)
      addPolygons(data = solution[, "n_targets"], weight = 1, col = "black", fillColor = ~ pal(as.data.frame(solution)$n_targets), fillOpacity = 1, group = "Number of target species")

    # Dynamically add species layers if any specified
    if (!is.null(map_individual_spp)) {
      for (layer in map_individual_spp) {
        layerData <- solution[as.data.frame(solution)[, layer$species] > 0, ]
        map <- map %>%
          addPolygons(data = layerData, weight = 1, col = "black", fillColor = "orange", fillOpacity = 0.4, group = layer$species)
        if (!is.null(layer$hidden) && layer$hidden) {
          map <- map %>% hideGroup(layer$species)
        }
        OG <- c(OG, layer$species)
      }
    }

    # Dynamically add auxiliary layers if any
    if (!is.null(auxiliaryLayers)) {
      for (layer in auxiliaryLayers) {
        layerData <- read_sf(layer$filename)
        # Check if the layer's projection matches the desired CRS
        if (st_crs(layerData) != desiredCrs) {
          cat("projecting auxiliary data \n")
          # Reproject the layer to match the desired CRS
          layerData <- st_transform(layerData, desiredCrs)
        }
        map <- map %>%
          addPolygons(data = layerData, weight = 1, col = "black", fillColor = "blue", fillOpacity = 0.1, group = layer$name)
        if (!is.null(layer$hidden) && layer$hidden) {
          map <- map %>% hideGroup(layer$name)
        }
        OG <- c(OG, layer$name)
      }
    }

    # set map bounds
    bounds <- st_bbox(original_PU_polygons)
    map <- map %>% fitBounds(
      lng1 = as.numeric(bounds$xmin), lat1 = as.numeric(bounds$ymin),
      lng2 = as.numeric(bounds$xmax), lat2 = as.numeric(bounds$ymax)
    )
    # Layers control
    map <- map %>%
      addLayersControl(
        overlayGroups = OG,
        baseGroups = c("Contour background"),
        options = layersControlOptions(collapsed = FALSE, hideSingleBase = T, sortLayers = F)
      ) %>%
      hideGroup("Single species PU") %>% # turn of these layers by default
      hideGroup("Number of target species") %>% # turn of these layers by default
      addLegend(pal = pal, values = ~ as.data.frame(solution)$n_targets, opacity = 0.7, title = NULL, position = "bottomright", group = "Number of target species")

    if (!is.null(save_HTML_loc)) {
      saveWidget(map, file = save_HTML_loc) # edit html doc title (defaults to 'leaflet')
    }
    return(map)
  } else {
    cat("Solution selected does not exist or is empty \n")
  }
}
