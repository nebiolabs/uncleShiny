
##-------------------------------------------------------------------------
##  mod_spectraSpark - spectra sparklines                                --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
spectraSparksUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("spectra_plots"))
    # shiny::plotOutput(ns("spectra_cowplot"), height = "600px")
  )
}

##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
spectraSparksServer <- function(id, grv, opts_obj) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      if (use_testing_mode) {
        module_data <- shiny::reactive({test_data})
      } else {
        module_data <- shiny::reactive({grv$robj_collected_data()})
      }
      
      # require(rlang)
      
      # data <- shiny::reactive({module_data()})
      # data <- shiny::reactive({grv$testy_data()})
      # hover <- shiny::reactive({grv$scatter_hover_summary_id()})
      
      data_hovered <- shiny::reactive({
        # shiny::req(data(), hover())
        shiny::req(module_data(), grv$scatter_hover_summary_id())
        # selection <- hover()[["summary_ids"]]
        # selection <- grv$scatter_hover_summary_id()[["summary_ids"]]
        dplyr::filter(
          # data(),
          shiny::isolate(module_data()),
          # uncle_summary_id %in% c(!!!selection)
          uncle_summary_id %in% grv$scatter_hover_summary_id()[["summary_ids"]]
        )
      })
      
      sparkline_vars <- shiny::reactive({get_sparkline_vars(data_hovered())})

      output$spectra_plots <- shiny::renderUI({
        ns <- session$ns
        plotOutput_list <- purrr::map(
          sparkline_vars()$spec_vars,
          function(var) {
            shiny::plotOutput(ns(var), height = "100px")
          }
        )
        do.call(tagList, plotOutput_list)
      })

      plot_list <- shiny::reactive({
        purrr::pmap(
          sparkline_vars(),
          ~ggspark(
            data = data_hovered,
            spec_var = ..1,
            spec_name = ..2,
            x_var = ..3,
            y_var = ..4,
            summary_var = ..5,
            palette_name = opts_obj$palette_global,
            color_n = ..6,
            alpha = 0.6
          )
        )
      })

      shiny::observe({
        for (i in sparkline_vars()$n) {
          local({
            n <- i
            plot_name <- sparkline_vars()$spec_vars[n]
            output[[plot_name]] <- shiny::renderPlot({
              plot_list()[[n]]
            })
          })
        }
      })
      
      # # Alternative method - generate single plot object from a list of plots
      # # using the `cowplot` package
      # output$spectra_cowplot <- shiny::renderPlot({
      #   sparkline_vars <- get_sparkline_vars(data_hovered())
      # 
      #   plot_list <- purrr::pmap(
      #     sparkline_vars,
      #     ~ggspark(
      #       data = data_hovered,
      #       spec_var = ..1,
      #       spec_name = ..2,
      #       x_var = ..3,
      #       y_var = ..4,
      #       summary_var = ..5,
      #       palette_name = opts_obj$palette_global,
      #       color_n = ..6,
      #       alpha = 0.6
      #     )
      #   )
      # 
      #   cowplot::plot_grid(plotlist = plot_list, ncol = 1)
      # })
    }
  )
}
