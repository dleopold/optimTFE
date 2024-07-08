#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("chooseInput"),
      label = "Select Input Files",
      width = "100%"
    ),
    div(
      id = ns("ctrls"),
      shinyWidgets::pickerInput(
        inputId = ns("stats"),
        label = "Choose Evaluation Criteria:",
        choices = character(0),
        multiple = TRUE
      ),
      conditionalPanel(
        condition = "input.stats.length > 0", ns = ns,
        uiOutput(ns("sliders")),
        shinyWidgets::pickerInput(
          inputId = ns("selected_solution"),
          label = "Selected Solution(s):",
          choices = character(0),
          options = shinyWidgets::pickerOptions(
            size = 5
          ),
          multiple = TRUE
        )
      )
    ) |> shinyjs::hidden()
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Open data loading module ----
    observeEvent(input$chooseInput, { trigger("select_data") })

    # Stat picker ----
    observeEvent(rv$solutions, {
      if(length(rv$solutions) == 0 || nrow(rv$solutions) == 0L){
        shinyjs::hide("ctrls")
        req(F)
      }
      shinyjs::show("ctrls")
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "stats",
        choices = colnames(rv$solutions)[-(1:2)],
        selected = character(0)
      )
    })
    observeEvent(input$stats, ignoreNULL = F, {
      rv$selected_stats <- input$stats
    })

    # dynamic inputs ----
    output$sliders <- renderUI({
      req(rv$selected_stats) |>
        purrr::map(~{
          bslib::card(
            sliderInput(
              ns(paste0("slider_", .x)),
              label = .x,
              min = 0,
              max = 1,
              value = 1,
              step = 0.05, # this is where we change 'percentage' for scale
              width = "100%"
            ),
            checkboxInput(
              ns(paste0("desc_", .x)),
              "Use descending rank order"
            )
          )
        }) |> tagList()
    })

    # dynamic observers ----
    observers <- reactiveValues()
    observeEvent(rv$selected_stats, ignoreNULL = F, {

      ## Delete unused observers ----
      toRemove <- setdiff(rv$observers, rv$selected_stats)
      for(i in seq_along(toRemove)){
        rv$observers <- rv$observers[rv$observers != toRemove[i]]
        observers[[toRemove[i]]][['val']]$destroy()
        observers[[toRemove[i]]][['desc']]$destroy()
        observers[[toRemove[i]]] <- NULL
        # rv$weights[[toRemove[i]]] <- NULL
      }
      ## Add new observers ----
      for(i in seq_along(rv$selected_stats)){
        stat <- rv$selected_stats[[i]]
        if(is.null(observers[[stat]])){
          rv$observers <- c(rv$observers, stat)
          sliderId <- paste0("slider_", stat)
          rv[['weights']][[stat]][['val']] <- 1
          observers[[stat]][['val']] <- observeEvent(input[[sliderId]], {
            rv[['weights']][[stat]][['val']] <- input[[sliderId]]
            trigger('calculate_ranks')
          })
          descId <- paste0("desc_", stat)
          rv[['weights']][[stat]][['desc']] <- 1
          observers[[stat]][['desc']] <- observeEvent(input[[descId]], {
            rv[['weights']][[stat]][['desc']] <- ifelse(input[[descId]], -1, 1)
            trigger('calculate_ranks')
          })
        }
      }
    })

    # update rank order ----
    init('calculate_ranks')
    on('calculate_ranks', {
      rv$ranks <- req(rv$solutions) |>
        dplyr::select(
          solution, dplyr::all_of(rv$selected_stats)
        ) |>
        tidyr::pivot_longer(
          cols = -solution,
          names_to = "stat",
          values_to = "value"
        ) |>
        dplyr::left_join(
          {
            req(rv$weights) |>
              purrr::imap_dfr(~{
                data.frame(
                  stat=.y,
                  val = .x$val,
                  desc = .x$desc)
              })
          }, by = "stat") |>
        dplyr::mutate(
          value = scale(value) * val * desc,
          .by = "stat"
        ) |>
        dplyr::summarize(
          score = sum(value),
          .by = solution
        ) |>
        dplyr::arrange(score) |>
        dplyr::mutate(
          rank = dplyr::row_number()
        )
      # Update solution picker
      rv$selected_solution <- rv$ranks$solution[1]

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "selected_solution",
        choices = rv$ranks$solution,
        choicesOpt = list(
          subtext = stringr::str_glue("(Rank: {rv$ranks$rank})")
        ),
        selected = rv$selected_solution
      )
      trigger("update_histograms")
    })

    # Select solution ----
    observeEvent(input$selected_solution, ignoreNULL = F, ignoreInit = T, {
      if(!identical(rv$selected_solution, input$selected_solution)){
        rv$selected_solution <- input$selected_solution
        trigger("update_histograms")
      }
    })

  })
}

