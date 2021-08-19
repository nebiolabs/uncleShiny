
##-------------------------------------------------------------------------
##  mod_spectraSpark - spectra sparklines                                --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
spectraSparksUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("spectra_plots"))
  )
}

##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
spectraSparksServer <- function(id, grv, opts_obj) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      require(rlang)
      
      data <- reactive({grv$robj_collected_data()})
      # hover_id <- reactive({grv$scatter_hover_summary_id()[["summary_ids"]]})
      # palette <- reactive({opts_obj$palette_global})
      
      # data <- shiny::reactive({grv$testy_data()})
      hover <- shiny::reactive({grv$scatter_hover_summary_id()})
      
      data_hovered <- reactive({
        selection <- hover()[["summary_ids"]]
        dplyr::filter(
          data(),
          uncle_summary_id %in% c(!!!selection)
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
    }
  )
}
