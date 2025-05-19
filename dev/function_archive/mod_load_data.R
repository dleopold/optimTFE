#' load_data Server Functions
#'
#' @noRd
mod_load_data_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
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
          accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj", ".gpkg"),
          multiple = TRUE,
          width = "100%"
        ),
        p("If uploading a shapefile, all accessory files must be selected. At a minimum, the '.shp', '.shx', and '.dbf' are required."),
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
      fns <- input$spatial_data$datapath
      # Geopackage ingest
      if (length(fns) == 1L && stringr::str_detect(fns, ".gpkg$")) {
        rv$spatial <- sf::st_read(input$spatial_data$datapath, quiet = TRUE)
        req(F)
      }
      # Shapefile ingest
      if (length(fns) > 1L && sum(stringr::str_detect(fns, ".shp$|.shx$|.dbf$")) == 3L) {
        dir <- dirname(fns)[[1]]
        name <- stringr::str_split(input$spatial_data$name[1], "\\.")[[1]][1]
        purrr::walk(fns, ~ {
          dir <- dirname(.x)
          suffix <- stringr::str_split(basename(.x), "\\.")[[1]][2]
          file.rename(.x, file.path(dir, paste(name, suffix, sep = ".")))
        })
        rv$spatial <- file.path(dir, paste0(name, ".shp")) |>
          sf::st_read(quiet = TRUE)
        req(F)
      }
      shinyjs::reset("spatial_data")
    })
    # Load summary data ----
    observeEvent(input$summary_data, {
      rv$solutions <- read.csv(input$summary_data$datapath)
    })
  })
}
