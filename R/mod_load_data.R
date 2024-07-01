#' load_data Server Functions
#'
#' @noRd
mod_load_data_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Select / load data ----
    init("select_data")
    on("select_data", {

      modalDialog(
        title = "Select Input Data",
        size = "l",
        fileInput(
          inputId = ns("summary_data"),
          label = "Solution Summary:",
          placeholder = "csv",
          accept = ".csv",
          width = "100%"
        ),
        fileInput(
          inputId = ns("spatial_data"),
          label = "Spatial data:",
          placeholder = "shp/gpkg",
          accept = c(".shp", ".gpkg"),
          width = "100%"
        ),
        footer = tagList(
          actionButton(
            ns("done"),
            "Done",
          )
        )
      ) |> showModal()
    })
    # Close modal ----
    observeEvent(input$done, {
      removeModal()
    })
    # Load spatial data ----
    observeEvent(input$spatial_data, {
      rv$spatial <- sf::st_read(input$spatial_data$datapath, quiet = TRUE)
    })
    # Load summary data ----
    observeEvent(input$summary_data, {
      rv$solutions <- read.csv(input$summary_data$datapath)
    })

  })
}

