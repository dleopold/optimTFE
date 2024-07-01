#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_sidebar(
      title = "optimTFE.explorer",
      theme = bslib::bs_theme(
        version = 5,
        bootswatch = "yeti"
      ),
      sidebar = bslib::sidebar(
        mod_sidebar_ui("sidebar"),
        width = 320
      ),
      mod_map_ui("map"),
      mod_histograms_ui("hist")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "optimTFE.explorer"
    ),
    shinyjs::useShinyjs()
  )
}
