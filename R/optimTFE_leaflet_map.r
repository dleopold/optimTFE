rm(list = ls()) #remove all past worksheet variables #run this on local
library(sf)
library(dplyr)
library(tidyverse)
library(leaflet)
library(leaflet.providers)
library(htmltools)
library(htmlwidgets)

wds <- c(
  "C:/Christina_learning_2022/greedyOpt_testing/",
  "D:/projects/2023_maui_nui_optim/git_repos/GreedyOptTests/",
  "/hdd/git_repos/GreedyOptTests/"
)
wd <- wds[which(dir.exists(wds))][1] # grab filepath to directory that exists on local device
setwd(wd)

#################################################
####webmap!
#################################################
#' Create Solution Interactive Map
#'
#' Generates an interactive map visualizing conservation solutions and
#' additional layers specified by the user.
#'
#' @param solution_result_file A optimTFE output dataframe containing the solution results.
#' @param original_PU_polygons Spatial dataframe with unit ids and species scores.
#' @param selected_sol_index Integer, index of the solution to map.
#' @param auxiliaryLayers (Optional) List of auxiliary layers to add to the map.
#' @param map_individual_spp (Optional) List of species to add to the map.
#'
#' @return An interactive leaflet map.
#' @export
#'
#' @examples
#' solution_result_file <- read.csv("output/solutions.csv")
#' original_PU_polygons <- read_sf("testData/PU_data_sample.gpkg")
#' selected_sol_index <- 141
#' # Assume auxiliaryLayers and map_individual_spp are defined as shown above
#' create_solution_interactive_map(solution_result_file, original_PU_polygons, selected_sol_index,
#'                                 auxiliaryLayers, map_individual_spp)

#MAIN vars for function:
solution_result_file <- read.csv("output/solutions.csv") #the csv with all solution results
original_PU_polygons <- read_sf("testData/PU_data_sample.gpkg") #spatial data with PU ids and species scores
selected_sol_index=141 #number of solution to map
auxiliaryLayers <- list( #last optional arg is a list of auxiliary layers to add to webmap and their names
  list(name = "Fenced units", filename = "/hdd/git_repos/GreedyOptTests/leaflet_web_map_full_page/ung_units_reproj.shp", hidden=T),
  list(name = "Native habitat", filename = "/hdd/git_repos/GreedyOptTests/leaflet_web_map_full_page/native_habitat_polygons.gpkg", hidden=T),
  list(name = "Conservation lands", filename = "/hdd/git_repos/GreedyOptTests/leaflet_web_map_full_page/reserves_reproj.shp", hidden=T)
)
map_individual_spp <- list( #last optional arg is a list of species to add to webmap
  list(species = "Cyanea_duvalliorum",  hidden=T),
  list(species = "Bidens_campylotheca_waihoienss", hidden=T),
  list(species = "Phyllostegia_mannii", hidden=T)
)


create_solution_interactive_map=function(solution_result_file, original_PU_polygons,
                                         selected_sol_index, auxiliaryLayers=NULL, map_individual_spp=NULL, save_HTML_loc=NULL){
  sol1 <- original_PU_polygons %>%
    select(c(1, ncol(original_PU_polygons)))
  join_by_cols <- setNames(names(solution_result_file)[1], names(sol1)[1])
  sol1 <- left_join(sol1, solution_result_file, by = join_by_cols) |>
    filter(!is.na(solution))
  solution <- subset(sol1, solution==selected_sol_index) # subset specific solution

  spp_col_indices=c((which(colnames(solution) == "select_order")+1):ncol(solution)) #this assumes that species columns occur after "select_order" column
  spp_score_columns=names(solution)[spp_col_indices]
  spp_names=gsub("_", " ", spp_score_columns)

  PU_data_spp_score <- as.data.frame(original_PU_polygons) |>
    select(c(1:(ncol(original_PU_polygons)-1)))

  solution$n_targets=apply(as.data.frame(solution)[,spp_score_columns], 1, sum, na.rm=T)
  solution$single_spp_PU=solution$n_targets==1
  single_sp_PU=solution[solution$single_spp_PU==1,]

  ########################################
  #create mouse over labels
  ########################################
  #labels for single species target layer
  spp_cols=which(names(single_sp_PU) %in% spp_score_columns) #c(1:(dim(single_sp_PU)[2]-5))
  single_spp_picked=spp_names[apply(as.data.frame(single_sp_PU)[,spp_cols], 1, function(x) which(x==1))]
  single_sp_PU$single_sp_picked=single_spp_picked
  labels <- sprintf("Single species picked:<br/><strong>%s</strong>",single_spp_picked) %>% lapply(htmltools::HTML)

  #label for conservation footprint layer
  target_labels=c()
  i=14
  for (i in c(1:dim(solution)[1])){
    tmp=as.data.frame(solution)[i,spp_cols]
    species_targets=which(tmp==1)
    species_targets=spp_names[species_targets]

    PU_top_label=paste0("PU ID: <strong>", as.data.frame(solution)[i,1]," </strong><br/>")

    PU_label=paste(c(PU_top_label, "TARGET SPECIES<br/>", species_targets), collapse = " <br/> ")
    target_labels=c(target_labels, PU_label)
  }
  target_labels <- target_labels %>% lapply(htmltools::HTML)

  #label for all planning unit layer
  PU_ID_labels=c()
  i=20
  for (i in c(1:dim(original_PU_polygons)[1])){
    tmp=as.data.frame(original_PU_polygons)[i,1]
    PU_top_label=paste0("PU ID: <strong>", tmp," </strong> <br/>") #<br/>

    tmp=PU_data_spp_score[i,-1]
    species_habitat=which(tmp>0)
    species_habitat=spp_names[species_habitat]
    if (length(species_habitat)==0) species_habitat="None"
    PU_label=paste(c(PU_top_label, "POTENTIAL SPECIES<br/>", species_habitat), collapse = " <br/> ")
    PU_ID_labels=c(PU_ID_labels, PU_label)
  }
  PU_ID_labels <- PU_ID_labels %>% lapply(htmltools::HTML)

  ########################################
  #color scheme for number of target species layer
  bins <- c(1,2,3, seq(5, ceiling(max(solution$n_targets)/5)*5, 5)) #modify!
  pal <- colorBin("YlOrRd", domain = as.data.frame(solution)$n_targets, bins = bins)

  #define name of default overlay groups, map CRS
  OG=c("Number of target species", "Single species PU", "Conservation footprint", "All planning units")
  desiredCrs <- st_crs(original_PU_polygons)

  ########################################
  #start building map now that all elements are ready!
  map <- leaflet(original_PU_polygons) %>% addTiles() %>% #start adding the solution data
    # Base groups
    addProviderTiles(providers$Stadia.StamenTerrainBackground, group = "Terrain background") %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Contour background") %>%
    #add base map of all PUs
    addPolygons(data=original_PU_polygons, weight = 1, col="grey", fillColor = "yellow", fillOpacity = 0,
                highlight = highlightOptions(
                  weight = 5,
                  bringToFront = TRUE),
                label = PU_ID_labels,
                labelOptions = labelOptions(
                  opacity = 0.5,
                  style = list("font-weight" = "normal", padding = "3px 8px", "border-color" = "grey"),
                  textsize = "15px",
                  direction = "auto"),
                group="All planning units") %>%
    #add solution footprint
    addPolygons(data=solution, weight = 2, fillColor = "green", fillOpacity = 0.4,
                highlight = highlightOptions(
                  weight = 5,
                  fillOpacity = 0.4,
                  bringToFront = TRUE),
                label = target_labels,
                labelOptions = labelOptions(
                  opacity = 0.5,
                  style = list("font-weight" = "normal", padding = "3px 8px", "border-color" = "blue"),
                  textsize = "15px",
                  direction = "auto"),
                group="Conservation footprint") %>%
    #add single sp PUs
    addPolygons(data=single_sp_PU, weight = 2, fillColor = "red", fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight = 5,
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  opacity = 0.5,
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                group="Single species PU") %>%
    #add map of met targets per PU
    addPolygons(data=solution[,"n_targets"], weight = 1, col="black", fillColor = ~pal(as.data.frame(solution)$n_targets), fillOpacity = 1, group="Number of target species")

  #Dynamically add species layers if any specified
  if (!is.null(map_individual_spp)) {
    for (layer in map_individual_spp) {
      layerData <- solution[as.data.frame(solution)[,layer$species]>0,]
      map <- map %>%
        addPolygons(data = layerData, weight = 1, col="black", fillColor = "orange", fillOpacity = 0.4, group = layer$species)
      if (!is.null(layer$hidden) && layer$hidden) {
        map <- map %>% hideGroup(layer$species)
      }
      OG=c(OG, layer$species)
    }
  }

  # Dynamically add auxiliary layers if any
  if (!is.null(auxiliaryLayers)) {
    for (layer in auxiliaryLayers) {
      layerData <- st_read(layer$filename)
      # Check if the layer's projection matches the desired CRS
      if (st_crs(layerData) != desiredCrs) {
        cat("projecting auxiliary data \n")
        # Reproject the layer to match the desired CRS
        layerData <- st_transform(layerData, desiredCrs)
      }
      map <- map %>%
        addPolygons(data = layerData, weight = 1, col="black", fillColor = "blue", fillOpacity = 0.2, group = layer$name)
      if (!is.null(layer$hidden) && layer$hidden) {
        map <- map %>% hideGroup(layer$name)
      }
      OG=c(OG, layer$name)
    }
  }

  #set map bounds
  bounds <- st_bbox(original_PU_polygons)
  map <- map %>% fitBounds(lng1 = as.numeric(bounds$xmin), lat1 = as.numeric(bounds$ymin),
                           lng2 = as.numeric(bounds$xmax), lat2 = as.numeric(bounds$ymax))
  # Layers control
  map <- map %>% addLayersControl(
    overlayGroups = OG,
    baseGroups = c("Terrain background", "Contour background"),
    options = layersControlOptions(collapsed = FALSE, hideSingleBase=T, sortLayers=F)) %>%
    hideGroup("Single species PU") %>% #turn of these layers by default
    hideGroup("Number of target species") %>% #turn of these layers by default
    addLegend(pal = pal, values = ~as.data.frame(solution)$n_targets, opacity = 0.7, title = NULL, position = "bottomright", group = "Number of target species")

  if (!is.null(save_HTML_loc)){
    saveWidget(map, file=save_HTML_loc) #edit html doc title (defaults to 'leaflet')
  }
  return(map)
}

map_no_spp=create_solution_interactive_map(solution_result_file, original_PU_polygons,
                                    selected_sol_index, auxiliaryLayers)

map=create_solution_interactive_map(solution_result_file, original_PU_polygons,
                                             selected_sol_index, auxiliaryLayers=NULL, map_individual_spp)
map=create_solution_interactive_map(solution_result_file, original_PU_polygons,
                                    selected_sol_index, auxiliaryLayers, map_individual_spp,
                                    save_HTML_loc ="/hdd/git_repos/GreedyOptTests/leaflet_web_map_full_page/greedyOpt_leaflet_webmap2.html")


