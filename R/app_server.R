#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny gargoyle
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 2000 * 1024^2)

  # Reactive values shared across modules ----
  rv <- reactiveValues(
    spatial = NULL, # Spatial data
    solutions = NULL, # Solution summary data
    selected_stats = NULL, # Currently elected summary stat columns
    observers = NULL, # list of stats that currently have slider inputs
    ranks = NULL, # current ranks of the solutions based on the selected stats
    selected_solution = NULL # currently selected solution(s) to display
  )

  # Module servers ----
  mod_sidebar_server("sidebar", rv)
  mod_load_data_server("load_data", rv)
  mod_map_server("map", rv)
  mod_histograms_server("hist", rv)

}

