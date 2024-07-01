#' histograms UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_histograms_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      style = "display: flex; flex-flow: row wrap; width: 100%; gap: 1em;",
      uiOutput(ns("panels"))
    )
  )
}

#' histograms Server Functions
#'
#' @import ggplot2
#'
#' @noRd
mod_histograms_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # dynamic plot outputs ----
    output$panels <- renderUI({
      req(rv$selected_solution)
      req(rv$selected_stats) |>
        purrr::map(~{
          bslib::card(
            bslib::card_header(
              class = "bg-dark",
              .x
            ),
            bslib::card_body(
              plotOutput(ns(paste0("hist_", .x)), height = "250px", width = "250px")
            ),
            fill = FALSE
          )
        })
    })

    # render outputs ----
    init("update_histograms")
    on("update_histograms", {
      req(rv$selected_solution)
      req(rv$selected_stats) |>
        purrr::walk(~{
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
              binwidth = \(x) (max(x) - min(x))/100,
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
          if(rv$weights[[.x]][['desc']]==-1){
            plot <- plot + scale_x_reverse()
          }
          output[[paste0("hist_", .x)]] <- renderPlot({
             plot
          })
        })
    })

  })
}



