
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
      
      # hoverData <- reactive({
      #   if (is.null(hover)) {
      #     return(NULL)
      #   } else {
      #     # return(data[data$sharedKey %in% hover, ][1, ])
      #     return(data %>% filter(sharedKey %in% hover))
      #   }
      # })
      # 
      # clickData <- reactive({
      #   if (is.null(click)) {
      #     return(NULL)
      #   } else {
      #     # return(data[data$sharedKey %in% click, ][1, ])
      #     return(data %>% filter(sharedKey %in% click))
      #   }
      # })
      # 
      # output$sparkyID <- renderText({
      #   # paste0(hover,"<br>",click)
      #   if (is.null(click)) {
      #     # glue("Showing spectra for:<br><em>Well</em> {hoverData()[['well']]}, <em>Uni</em> {hoverData()[['uni']]}")
      #     glue("Showing spectra for:<br><em>Well</em> {hoverData()[['well']]}, <em>Uni</em> {hoverData()[['uni']]}")
      #   } else {
      #     glue("Showing spectra for:<br><em>Well</em> {hoverData()[['well']]}, <em>Uni</em> {hoverData()[['uni']]}<br>
      #    Compared to (in grey): <br><em>Well</em> {clickData()[['well']]}, <em>Uni</em> {clickData()[['uni']]}")
      #   }
      # })
      # 
      # output$sparky <- renderPlot({
      #   req(hover)
      #   
      #   # spectra columns available to plot
      #   hoverList <- grep(pattern = "spec", colnames(hoverData()), value = TRUE)
      #   hoverToPlot <- specList[specList %in% hoverList]
      #   namesToPlot <- names(hoverToPlot)
      #   
      #   # sparkline and color creation function, important: called locally within this renderPlot evironment
      #   source("R/funs.R", local = TRUE)
      #   
      #   # color setup
      #   if (is.na(pal) | pal == "Default") {
      #     pal <- "Spectral"
      #   }
      #   colors <- mycolors(pal, length(hoverList))
      #   
      #   # generate the plots
      #   plist <- purrr::pmap(
      #     list(
      #       hoverToPlot,
      #       namesToPlot,
      #       seq_along(hoverToPlot)
      #     ),
      #     ~ ggspark(hoverData(), clickData(), ..1, ..2, ..3, specDerived)
      #   )
      #   
      #   cowplot::plot_grid(plotlist = plist, ncol = 1)
      # })
    }
  )
}
