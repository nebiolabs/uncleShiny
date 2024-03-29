
##-------------------------------------------------------------------------
##  mod_spectraSpark - spectra sparklines                                --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
spectraSparksUI <- function(id) {
  ns <- NS(id)
    shiny::tagList(
      shiny::uiOutput(ns("selected")),
      shiny::br(),
      shiny::uiOutput(ns("spectra_plots"), inline = TRUE)
    )
    # shiny::fluidRow(shiny::plotOutput(ns("spectra_cowplot"), height = "75px"))
}

##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
spectraSparksServer <- function(id, grv, event_type) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      module_data <- shiny::reactive({grv$data()})
      
      if (event_type == "click") {
        event <- quote(grv$scatter$clicked)
      } else if (event_type == "hover") {
        event <- quote(grv$scatter$hovered)
      } else {
        stop("Sparkline module event_type argument error.")
      }
      
      output$selected <- shiny::renderUI({
        ns <- session$ns
        if (is.null(eval(event)[["summary_ids"]])) {
          shiny::helpText(
            glue::glue("Activated on {event_type}.")
          )
        } else {
          the_row <- module_data()[module_data()[["uncle_summary_id"]] %in% 
                                                 eval(event)[["summary_ids"]], ]
          shiny::tags$em(
            glue::glue("Spectra shown for {the_row[['product_name']]}, ",
                       "plate {the_row[['plate']]}, well {the_row[['well']]}.")
          )
        }
      })
      
      data_for_spark <- shiny::reactive({
        req(module_data(), eval(event))
        dplyr::filter(
          module_data(),
          uncle_summary_id %in% eval(event)[["summary_ids"]]
        )
      })
      
      sparkline_vars <- shiny::reactive({get_sparkline_vars(data_for_spark())})

      output$spectra_plots <- shiny::renderUI({
        ns <- session$ns
        plotOutput_list <- purrr::map(
          # `as.vector` required; named list affects `fluidRow` div assignment
          as.vector(spec_vars),
          function(var) {
            shiny::column(
              width = 2,
              shiny::plotOutput(ns(var), height = "120px")
            )
          }
        )
        do.call(shiny::fluidRow, plotOutput_list)
      })

      plot_list <- shiny::reactive({
        purrr::pmap(
          sparkline_vars(),
          ~ggspark(
            data = data_for_spark(),
            spec_var = ..1,
            spec_name = ..2,
            x_var = ..3,
            y_var = ..4,
            summary_var = ..5,
            palette_name = shiny::isolate(grv$scatter_opts$palette_global),
            color_n = ..6,
            alpha = 0.6
          )
        )
      })

      # shiny::observe({
      #   for (i in sparkline_vars()$n) {
      #     local({
      #       n <- i
      #       plot_name <- sparkline_vars()$spec_vars[n]
      #       output[[plot_name]] <- shiny::renderPlot({
      #         plot_list()[[n]]
      #       })
      #     })
      #   }
      # })
      
      shiny::observe({
        purrr::walk2(
          sparkline_vars()$n,
          sparkline_vars()$spec_vars,
          function(n, name) {
            output[[name]] <- shiny::renderPlot({
              plot_list()[[n]]
            })
          }
        )
      })
      
      # # Alternative method - generate single plot object from a list of plots
      # # using the `cowplot` package
      # output$spectra_cowplot <- shiny::renderPlot({
      #   sparkline_vars <- get_sparkline_vars(data_for_spark())
      # 
      #   plot_list <- purrr::pmap(
      #     sparkline_vars,
      #     ~ggspark(
      #       data = data_for_spark(),
      #       spec_var = ..1,
      #       spec_name = ..2,
      #       x_var = ..3,
      #       y_var = ..4,
      #       summary_var = ..5,
      #       palette_name = grv$scatter_opts$palette_global,
      #       color_n = ..6,
      #       alpha = 0.6
      #     )
      #   )
      # 
      #   cowplot::plot_grid(plotlist = plot_list, nrow = 1)
      # })
    }
  )
}
