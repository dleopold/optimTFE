#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import leaflet
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      id = ns("map_box"),
      bslib::card_header(
        class = "bg-dark",
        "Solution Map"
      ),
      bslib::card_body(
        leafletOutput(ns("map"), height = 400)
      ),
      full_screen = TRUE,
      fill = FALSE
    ) |> shinyjs::hidden()
  )
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render map ----
    output$map <- renderLeaflet({
      bounds <- sf::st_bbox(req(rv$spatial))
      shinyjs::show("map_box")
      leaflet(
        options = leafletOptions(
          attributionControl = FALSE
        )
      ) |>
        addPolygons(
          data = rv$spatial,
          color = "#000000", weight = 0.5, opacity = 0.7,
          fillOpacity = 0
        ) |>
        addMapPane("solutions", zIndex = 500) |>
        fitBounds(lng1 = bounds[[1]], lat1 = bounds[[2]], lng2 = bounds[[3]], lat2 = bounds[[4]]) |>
        addProviderTiles(providers$Stadia.StamenTerrainBackground)
    })
    outputOptions(output, "map", suspendWhenHidden = FALSE)

    # Show selected solution(s) ----
    observeEvent(rv$selected_solution, ignoreInit = TRUE, {
      req(rv$spatial)

      # clear existing solution layer
      leafletProxy("map") |>
        clearGroup("sol_layer")
      req(rv$selected_solution)

      # create new solution layer
      solution_layer <- rv$selected_solution |> purrr::map_dfr(~ {
        units <- rv$solutions |>
          dplyr::filter(solution == .x) |>
          dplyr::pull(units) |>
          jsonlite::fromJSON()
        rv$spatial |>
          dplyr::filter(PU_num %in% units)
      })

      # add new solution layer
      leafletProxy("map") |>
        addPolygons(
          data = solution_layer,
          color = "#000000",
          fillColor = "#1D2F6F",
          weight = 0.5,
          opacity = 0.7,
          group = "sol_layer",
          fillOpacity = 1 / length(rv$selected_solution),
          options = pathOptions(
            pane = "solutions", clickable = FALSE
          )
        )
    })
  })
}
