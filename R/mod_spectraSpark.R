
##-------------------------------------------------------------------------
##  mod_spectraSpark - spectra sparklines                                --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
spectraSparksUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # shiny::verbatimTextOutput(ns("test_print")),# debugging
    # shiny::tableOutput(ns("test_table")),# debugging
    shiny::plotOutput(ns("dsf"), height = "100px"),
    shiny::plotOutput(ns("sls266"), height = "100px"),
    shiny::plotOutput(ns("sls473"), height = "100px"),
    shiny::plotOutput(ns("dls_int"), height = "100px"),
    shiny::plotOutput(ns("dls_mass"), height = "100px"),
    shiny::plotOutput(ns("dls_corr"), height = "100px")
    # shiny::uiOutput(ns("spectra_plots"))# TO-DO, render UI from server
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
      
      # output$test_print <- shiny::renderPrint({
      #   hover()
      # })
      
      # output$test_table <- shiny::renderTable({
      #   req(hover())
      #   data_hovered() |> dplyr::select(-tidyselect::contains("spec") | 
      #                                     -tidyselect::contains("residuals"))
      # })
      
      # output$dsf <- shiny::renderPlot({
      #   req(data_hovered())
      #   spec_var <- "specTm"
      #   summary_var <- "Tm1"
      #   x_var <- "temperature"
      #   y_var <- "bcm"
      #   ggspark(data_hovered, spec_var, x_var, y_var, summary_var)
      # })
      
      output$dsf <- shiny::renderPlot({
        req(data_hovered())
        ggspark(
          data = data_hovered,
          spec_var = "specTm",
          x_var = "temperature",
          y_var = "bcm",
          summary_var = "Tm1",
          palette_name = opts_obj$palette_global,
          color_n = 1
        )
      })
      
      output$sls266 <- shiny::renderPlot({
        req(data_hovered())
        ggspark(
          data = data_hovered,
          spec_var = "specSLS266",
          x_var = "temperature",
          y_var = "sls_266",
          summary_var = "Tagg266",
          palette_name = opts_obj$palette_global,
          color_n = 2
        )
      })

      output$sls473 <- shiny::renderPlot({
        req(data_hovered())
        ggspark(
          data = data_hovered,
          spec_var = "specSLS473",
          x_var = "temperature",
          y_var = "sls_473",
          summary_var = "Tagg473",
          palette_name = opts_obj$palette_global,
          color_n = 3
        )
      })

      output$dls_int <- shiny::renderPlot({
        req(data_hovered())
        ggspark(
          data = data_hovered,
          spec_var = "specDLS_I",
          x_var = "hydrodynamic_diameter",
          y_var = "amplitude",
          summary_var = "Z_D",
          palette_name = opts_obj$palette_global,
          color_n = 4
        )
      })

      output$dls_mass <- shiny::renderPlot({
        req(data_hovered())
        ggspark(
          data = data_hovered,
          spec_var = "specDLS_M",
          x_var = "hydrodynamic_diameter",
          y_var = "amplitude",
          summary_var = "Z_D",
          palette_name = opts_obj$palette_global,
          color_n = 5
        )
      })

      output$dls_corr <- shiny::renderPlot({
        req(data_hovered())
        ggspark(
          data = data_hovered,
          spec_var = "specDLS_C",
          x_var = "time",
          y_var = "amplitude",
          summary_var = NULL,
          palette_name = opts_obj$palette_global,
          color_n = 6
        )
      })
    }
  )
}
