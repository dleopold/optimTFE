#' sidebar UI
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param footprints Character vector of footprint names
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
ce_sidebar_ui <- function(id, footprints) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(
      "
    .bs-select-all {
      display: none !important;
    }
    .bs-actionsbox .btn-group button {
      width: 100% !important;
    }
  "
    )),
    shinyWidgets::pickerInput(
      inputId = ns("stats"),
      label = "Choose Evaluation Criteria:",
      choices = character(0),
      multiple = TRUE
    ),
    uiOutput(ns("sliders")),
    shinyWidgets::pickerInput(
      inputId = ns("selected_solution"),
      label = "Selected Footprint(s):",
      choices = footprints,
      options = shinyWidgets::pickerOptions(
        size = 5,
        actionsBox = TRUE,
        liveSearch = TRUE,
        selectAllText = NULL,
        virtualScroll = TRUE,
        deselectAllText = "Clear Selection"
      ),
      multiple = TRUE
    )
  )
}

#' sidebar Server
#'
#' @noRd
ce_sidebar_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Stat picker ----
    observeEvent(rv$solutions, once = TRUE, {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "stats",
        choices = rv$solutions |>
          dplyr::select(-units, -solution) |>
          colnames(),
        selected = names(rv$weights)
      )
    })
    observeEvent(input$stats, ignoreNULL = F, {
      rv$selected_stats <- input$stats
      if (length(rv$selected_stats) == 0) {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "selected_solution",
          choices = rv$solutions$solution,
          selected = 1
        )
        gargoyle::trigger("update_histograms")
      }
    })

    # dynamic inputs ----
    output$sliders <- renderUI({
      req(rv$selected_stats)
      isolate({
        weights <- as.list(rv$weights)
      })
      sliders <- purrr::map(
        rv$selected_stats,
        ~ {
          bslib::card(
            sliderInput(
              ns(paste0("slider_", .x)),
              label = .x,
              min = 0,
              max = 1,
              value = weights[[.x]][['val']] %||% 1,
              step = 0.05,
              width = "100%"
            ),
            checkboxInput(
              ns(paste0("desc_", .x)),
              "Descending",
              value = (weights[[.x]][['desc']] %||% 1) == -1
            )
          )
        }
      ) |>
        tagList()
    })

    # dynamic observers ----
    observers <- reactiveValues()
    observeEvent(rv$selected_stats, ignoreNULL = F, {
      ## Delete unused observers ----
      toRemove <- setdiff(rv$observers, rv$selected_stats)
      for (i in seq_along(toRemove)) {
        rv$observers <- rv$observers[rv$observers != toRemove[i]]
        observers[[toRemove[i]]][["val"]]$destroy()
        observers[[toRemove[i]]][["desc"]]$destroy()
        observers[[toRemove[i]]] <- NULL
        # rv$weights[[toRemove[i]]] <- NULL
      }
      ## Add new observers ----
      for (i in seq_along(rv$selected_stats)) {
        stat <- rv$selected_stats[[i]]
        if (is.null(observers[[stat]])) {
          rv$observers <- c(rv$observers, stat)
          if (length(rv[["weights"]][[stat]][["val"]]) == 0) {
            rv[["weights"]][[stat]][["val"]] <- 1
          }
          # Create observers with local variables to avoid closure issues
          local({
            current_stat <- stat
            sliderId <- paste0("slider_", current_stat)
            observers[[current_stat]][["val"]] <<- observeEvent(
              input[[sliderId]],
              {
                rv[["weights"]][[current_stat]][["val"]] <- input[[sliderId]]
                gargoyle::trigger("calculate_ranks")
              }
            )
          })
          if (length(rv[["weights"]][[stat]][["desc"]]) == 0) {
            rv[["weights"]][[stat]][["desc"]] <- 1
          }
          # Create observers with local variables to avoid closure issues
          local({
            current_stat <- stat
            descId <- paste0("desc_", current_stat)
            observers[[current_stat]][["desc"]] <<- observeEvent(
              input[[descId]],
              {
                rv[["weights"]][[current_stat]][["desc"]] <- ifelse(
                  input[[descId]],
                  -1,
                  1
                )
                gargoyle::trigger("calculate_ranks")
              }
            )
          })
        }
      }
    })

    # update rank order ----
    gargoyle::init("calculate_ranks")
    gargoyle::on("calculate_ranks", {
      w <- rv$selected_stats |>
        purrr::set_names() |>
        purrr::imap(
          ~ {
            rv$weights[[.y]][["val"]] * rv$weights[[.y]][["desc"]]
          }
        )
      weighted_solutions <- req(rv$criteria) |>
        collapse::fsubset(
          variable %iin% rv$selected_stats
        ) |>
        collapse::fmutate(
          value = value * unlist(w[variable])
        ) |>
        collapse::fgroup_by(solution) |>
        collapse::fsummarize(
          score = collapse::fsum(value)
        ) |>
        collapse::fungroup() |>
        collapse::roworder(score)
      ranked <- glue::glue(
        "{weighted_solutions$solution} (Rank: ",
        "{match(weighted_solutions$score, unique(weighted_solutions$score))})"
      )

      # Update solution picker
      rv$selected_solution <- ranked[1] |>
        stringr::str_extract("^\\d+")

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "selected_solution",
        selected = ranked[1],
        choices = ranked
      )
      gargoyle::trigger("update_histograms")
    })

    # Select solution ----
    observeEvent(input$selected_solution, ignoreNULL = F, ignoreInit = T, {
      selected_solution <- input$selected_solution |>
        stringr::str_extract("^\\d+")
      if (!identical(rv$selected_solution, selected_solution)) {
        rv$selected_solution <- selected_solution
        gargoyle::trigger("update_histograms")
      }
    })
  })
}

#' map UI
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
ce_map_ui <- function(id) {
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
    )
  )
}

#' map Server Functions
#'
#' @noRd
ce_map_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render map ----
    output$map <- renderLeaflet({
      bounds <- sf::st_bbox(req(rv$spatial))
      map <- leaflet(
        options = leafletOptions(
          attributionControl = FALSE
        )
      ) |>
        addPolygons(
          data = rv$spatial,
          color = "#000000",
          weight = 0.5,
          opacity = 0.7,
          fillOpacity = 0
        ) |>
        addMapPane("solutions", zIndex = 500) |>
        fitBounds(
          lng1 = bounds[[1]],
          lat1 = bounds[[2]],
          lng2 = bounds[[3]],
          lat2 = bounds[[4]]
        )
      if (length(rv$provider_tiles) == 0L) {
        return(map)
      }
      map <- tryCatch(
        addProviderTiles(map, providers[[rv$provider_tiles]]),
        error = \(e) map
      )
    })
    outputOptions(output, "map", suspendWhenHidden = FALSE)

    # Show selected solution(s) ----
    observeEvent(rv$selected_solution, ignoreInit = TRUE, {
      req(rv$spatial)

      # clear existing solution layer
      leafletProxy("map") |>
        clearGroup("sol_layer")

      # create new solution layer
      solution_layer <- req(rv$selected_solution) |>
        purrr::map_dfr(
          ~ {
            units <- rv$solutions |>
              dplyr::filter(solution == .x) |>
              dplyr::pull(units) |>
              jsonlite::fromJSON()
            rv$spatial |>
              dplyr::filter(unit %in% units)
          }
        )

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
            pane = "solutions",
            clickable = FALSE
          )
        )
    })
  })
}

#' histograms UI
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
ce_histograms_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "display: flex; flex-flow: row wrap; width: 100%; gap: 1em;",
      uiOutput(ns("panels"))
    )
  )
}

#' histograms Server
#'
#' @import ggplot2
#'
#' @noRd
ce_histograms_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # dynamic plot outputs ----
    output$panels <- renderUI({
      req(rv$selected_solution)
      req(rv$selected_stats) |>
        purrr::map(
          ~ {
            bslib::card(
              bslib::card_header(
                class = "bg-dark",
                .x
              ),
              bslib::card_body(
                plotOutput(
                  ns(paste0("hist_", .x)),
                  height = "250px",
                  width = "250px"
                )
              ),
              fill = FALSE
            )
          }
        )
    })

    # render outputs ----
    gargoyle::init("update_histograms")
    gargoyle::on("update_histograms", {
      req(rv$selected_solution)
      req(rv$selected_stats) |>
        purrr::walk(
          ~ {
            dat <- rv$solutions |>
              dplyr::transmute(
                solution,
                stat = !!rlang::sym(.x)
              )
            plot <- dat |>
              ggplot() +
              aes(x = stat) +
              geom_histogram(
                fill = "#008cba",
                binwidth = \(x) (max(x) - min(x)) / 100,
              ) +
              geom_vline(
                data = {
                  dat |>
                    dplyr::filter(solution %in% rv$selected_solution)
                },
                aes(xintercept = stat),
                color = "#A4031F",
                linetype = "dashed"
              ) +
              ggthemes::theme_few() +
              theme(
                axis.title = element_blank(),
              )
            # if (rv$weights[[.x]][["desc"]] == -1) {
            #   plot <- plot + scale_x_reverse()
            # }
            output[[paste0("hist_", .x)]] <- renderPlot({
              plot
            })
          }
        )
    })
  })
}
