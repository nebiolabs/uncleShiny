
##-------------------------------------------------------------------------
##  mod_scatterPlots - summary data visualization                        --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
scatterPlotsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tabsetPanel(
      shiny::tabPanel(
        title = "Summary Scatter Plots",
        icon = shiny::icon("braille"),
        shiny::fluidRow(
          shiny::column(
            width = 9,
            ##-----------------------------------------
            ##  Scatter plots                        --
            ##-----------------------------------------
            shiny::h3("Summary Data"),
            shiny::helpText("Click/drag to select for zoom.
                        Double-click to deselect.
                        Click a point to show all spectra."),
            plotly::plotlyOutput(
              ns("plot_scatter"),
              width = "100%",
              height = "500"
            ),
            ##----------------------------------------
            ##  Spectra sparklines                  --
            ##----------------------------------------
            shiny::fluidRow(
              shiny::h3("Spectra Quickview"),
              shiny::helpText("Activated on click.")
            ),
            spectraSparksUI(ns("scatter_sparks")),
            # Output for testing -----------------------------------------------
            # TODO remove before merge
            shiny::fluidRow(shiny::verbatimTextOutput(ns("test_module_data"))),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::verbatimTextOutput(ns("test_hovered"))
              ),
              shiny::column(
                width = 6,
                shiny::verbatimTextOutput(ns("test_selected"))
              )
            )
            # End section of UI to remove --------------------------------------
          ),
          shiny::column(
            width = 3,
            conditionsViewerUI(ns("scatter_conditions"))
          )
        )
      ),
      shiny::tabPanel(
        title = "Selected Spectra Ridgeline Plots",
        icon = shiny::icon("chart-area"),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::h3("Selection Zoom"),
            shiny::helpText("Activated on click/drag selection."),
            ##-----------------------------------------
            ##  Zoomed plot                          --
            ##-----------------------------------------
            plotly::plotlyOutput(
              ns("plot_zoom"),
              width = "100%",
              height = "400px"
            )
          ),
          shiny::column(
            width = 8,
            spectraViewerUI(ns("scatter_ridgeline"))
          )
        )
      )
    )
  )
}

##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
scatterPlotsServer <- function(id, grv) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      grv$scatter <- shiny::reactiveValues()
      
      munge_module_data <- function(data_input, color_input, palette_input) {
        if (is.null(data_input)) {
          NULL
        } else {
          data_input |>
            dplyr::mutate(
              Buffer_condition_name = dplyr::if_else(
                stringr::str_detect(
                  Buffer_condition_name,
                  "(Neutral Buffer)|(NB)"
                ),
                "Neutral Buffer",
                Buffer_condition_name
              )
            ) |> 
            cbind_colors(color_input, palette_input)
        }
      }
      
      # if (use_testing_mode) {
      #   module_data <- shiny::reactive({
      #     test_data |>
      #       munge_module_data(
      #         color_input = grv$scatter_opts$color_global,
      #         palette_input = grv$scatter_opts$palette_global
      #       )
      #   })
      # } else {
      #   module_data <- shiny::reactive({
      #     grv$data() |> 
      #       munge_module_data(
      #         color_input = grv$scatter_opts$color_global,
      #         palette_input = grv$scatter_opts$palette_global
      #       )
      #   })
      # }
      module_data <- shiny::reactive({
        grv$data() |> 
          munge_module_data(
            color_input = grv$scatter_opts$color_global,
            palette_input = grv$scatter_opts$palette_global
          )
      })
      
      # Output for testing
      # TODO remove before merge
      output$test_module_data <- shiny::renderPrint({
        module_data()
      })
      
      module_SharedData <- shiny::reactive({
        grv$scatter_opts$color_highlight
        crosstalk::SharedData$new(module_data(), key = ~uncle_summary_id)
      })
      
      
      ##-----------------------------------------
      ##  Left Plot (DLS)                      --
      ##-----------------------------------------
      # Reactive object of DLS plot
      plot_DLS <- shiny::reactive({
        shiny::req(module_SharedData())
        ggscatter(
          data = module_SharedData(),
          x_var = grv$scatter_opts$xvar1,
          y_var = grv$scatter_opts$yvar1,
          color_var = grv$scatter_opts$color_global,
          color_encoded = FALSE,
          palette_name = grv$scatter_opts$palette_global,
          size = grv$scatter_opts$size_points(),
          alpha = grv$scatter_opts$alpha_points(),
          show_vert_guides = grv$scatter_opts$show_guides_v1,
          vert_guides = grv$scatter_opts$guides_v1(),
          show_horiz_guides = grv$scatter_opts$show_guides_h1,
          horiz_guides = grv$scatter_opts$guides_h1(),
          x_is_log = grv$scatter_opts$xvar1_is_log,
          custom_data = "well_id",
          show_legend = FALSE
        )
      })
      
      
      ##-----------------------------------------
      ##  Right Plot (SLS/DSF)                 --
      ##-----------------------------------------
      # Reactive object of SLS & DSF plot
      plot_SLS_DSF <- shiny::reactive({
        shiny::req(module_SharedData())
        ggscatter(
          data = module_SharedData(),
          x_var = grv$scatter_opts$xvar2,
          y_var = grv$scatter_opts$yvar2,
          color_var = grv$scatter_opts$color_global,
          color_encoded = FALSE,
          palette_name = grv$scatter_opts$palette_global,
          size = grv$scatter_opts$size_points(),
          alpha = grv$scatter_opts$alpha_points(),
          show_vert_guides = grv$scatter_opts$show_guides_v2,
          vert_guides = grv$scatter_opts$guides_v2(),
          show_horiz_guides = grv$scatter_opts$show_guides_h2,
          horiz_guides = grv$scatter_opts$guides_h2(),
          x_is_log = grv$scatter_opts$xvar2_is_log,
          custom_data = "well_id",
          show_legend = TRUE
        )
      })
      
      
      ##-----------------------------------------
      ##  L/R Plotly Output                    --
      ##-----------------------------------------
      # Plotly subplot output rendering
      output$plot_scatter <- plotly::renderPlotly({
        shiny::req(module_SharedData())
        plotly::subplot(
          plot_DLS() |>
            plotly::ggplotly(source = "scatter", tooltip = "text") |> 
            plotly::event_register("plotly_selected") |>
            plotly::event_register("plotly_click") |> 
            plotly::event_register("plotly_hover"),
          plot_SLS_DSF() |>
            plotly::ggplotly(source = "scatter", tooltip = "text") |>
            plotly::event_register("plotly_selected") |>
            plotly::event_register("plotly_click") |> 
            plotly::event_register("plotly_hover"),
          nrows = 1,
          titleX = TRUE,
          titleY = TRUE,
          margin = 0.04
        ) |>
          plotly::layout(
            legend = legendList
          ) |>
          plotly::highlight(
            on = grv$scatter_opts$mode_highlight_on,
            off = grv$scatter_opts$mode_highlight_off,
            color = grv$scatter_opts$color_highlight,
            opacityDim = 0.42,
            selected = plotly::attrs_selected(
              showlegend = TRUE,
              opacity = 1
            ),
            debounce = 100
          ) |>
          plotly::config(displaylogo = FALSE)# |> 
          # plotly::event_register("plotly_selected") |>
          # plotly::event_register("plotly_click") |> 
          # plotly::event_register("plotly_hover")
      })
      
      
      ##-----------------------------------------
      ##  Plotly Callbacks                     --
      ##-----------------------------------------
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter selected plotly event        <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        # similar to a debounce to delay repeat invalidation/evaluation
        # shiny::invalidateLater(1000, session)
        
        # plotly callback
        event <- plotly::event_data(
          event = "plotly_selected",
          source = "scatter"
        )
        
        # sometimes selection returns a list if points overlap which is
        # incompatible with filtering and must be repaired
        if (rlang::is_list(event[["key"]])) {
          event[["key"]] <- as.character(unlist(event[["key"]]))
        }
        if (rlang::is_list(event[["customdata"]])) {
          event[["customdata"]] <- as.character(unlist(event[["customdata"]]))
        }
        
        # output of a list containing key and customdata values for selection
        if (is.null(event)) {
          grv$scatter$selected <- NULL
        } else {
          grv$scatter$selected <- list(
            summary_ids = bit64::as.integer64.character(event[["key"]]),
            well_ids = bit64::as.integer64.character(event[["customdata"]])
          )
        }
      }, label = "scatter_selected")
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter clicked plotly event         <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        # similar to a debounce to delay repeat invalidation/evaluation
        # shiny::invalidateLater(1000, session)
        
        # plotly callback
        event <- plotly::event_data(
          event = "plotly_click",
          source = "scatter"
        )
        
        # sometimes selection returns a list if points overlap which is
        # incompatible with filtering and must be repaired
        if (rlang::is_list(event[["key"]])) {
          event[["key"]] <- as.character(unlist(event[["key"]]))
        }
        if (rlang::is_list(event[["customdata"]])) {
          event[["customdata"]] <- as.character(unlist(event[["customdata"]]))
        }
        
        # output of a list containing key and customdata values for selection
        if (is.null(event)) {
          grv$scatter$clicked <- NULL
        } else {
          grv$scatter$clicked <- list(
            summary_ids = bit64::as.integer64.character(event[["key"]]),
            well_ids = bit64::as.integer64.character(event[["customdata"]])
          )
        }
      }, label = "scatter_clicked")
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter hovered plotly event         <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        # similar to a debounce to delay repeat invalidation/evaluation
        
        # plotly callback
        event <- plotly::event_data(
          event = "plotly_hover",
          source = "scatter"
        )
        
        # sometimes selection returns a list if points overlap which is
        # incompatible with filtering and must be repaired
        if (rlang::is_list(event[["key"]])) {
          event[["key"]] <- as.character(unlist(event[["key"]]))
        }
        if (rlang::is_list(event[["customdata"]])) {
          event[["customdata"]] <- as.character(unlist(event[["customdata"]]))
        }
        
        # output of a list containing key and customdata values for selection
        if (is.null(event)) {
          grv$scatter$hovered <- NULL
        } else {
          grv$scatter$hovered <- list(
            summary_ids = bit64::as.integer64.character(event[["key"]]),
            well_ids = bit64::as.integer64.character(event[["customdata"]])
          )
        }
        # if (rlang::is_empty(grv$scatter$hovered[["summary_ids"]])) {
        #   cat("Notice, that empty vector error happened again on hover.\n")
        # }
      }, label = "scatter_hovered")
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter selected data                <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        shiny::req(module_data(), grv$scatter$selected)
        grv$scatter$selected$data <- dplyr::filter(
          module_data(),
          uncle_summary_id %in% grv$scatter$selected[["summary_ids"]]
        )
      }, label = "scatter_selected_data")
      
      # Output for testing
      # TODO remove before merge
      output$test_selected <- shiny::renderPrint({
        # grv$scatter$selected$data
        # plotly::event_data(
        #   event = "plotly_selected",
        #   source = "scatter"
        # )
        grv$scatter$selected
      })
      
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter hovered data                <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        shiny::req(module_data(), grv$scatter$hovered)
        if (rlang::is_empty(grv$scatter$hovered[["summary_ids"]])) {
          grv$scatter$hovered$data <- NULL
        } else {
          grv$scatter$hovered$data <- dplyr::filter(
            module_data(),
            uncle_summary_id %in% grv$scatter$hovered[["summary_ids"]]
          )
        }
      }, label = "scatter_hovered_data")
      
      # Output for testing
      # TODO remove before merge
      output$test_hovered <- shiny::renderPrint({
        # grv$scatter$hovered$data
        # plotly::event_data(
        #   event = "plotly_hover",
        #   source = "scatter"
        # )
        grv$scatter$hovered
      })
      
      
      ##-----------------------------------------
      ##  Zoom Plot (selected callback)        --
      ##-----------------------------------------
      # Reactive object of SLS & DSF plot
      plot_zoom <- shiny::reactive({
        ggscatter(
          data = grv$scatter$selected$data,
          x_var = grv$scatter_opts$xvar3,
          y_var = grv$scatter_opts$yvar3,
          label = "well",
          color_var = grv$scatter_opts$color_zoom,
          color_encoded = TRUE,
          palette_name = grv$scatter_opts$palette_global,
          size = grv$scatter_opts$size_points(),
          alpha = grv$scatter_opts$alpha_points(),
          # show_vert_guides = grv$scatter_opts$show_guides_v2,
          # vert_guides = grv$scatter_opts$guides_v2(),
          # show_horiz_guides = grv$scatter_opts$show_guides_h2,
          # horiz_guides = grv$scatter_opts$guides_h2(),
          x_is_log = grv$scatter_opts$xvar3_is_log,
          custom_data = "well_id",
          show_legend = FALSE
        )
      })
      
      
      ##----------------------------------------
      ##  Zoom Plotly Output                  --
      ##----------------------------------------
      # Plotly subplot output rendering
      output$plot_zoom <- plotly::renderPlotly({
        plot_zoom() |> 
        plotly::ggplotly(source = "zoom", tooltip = "text") |>
          plotly::layout(
            legend = legendList
          ) |>
          plotly::highlight(
            on = "plotly_selected", off = "plotly_deselect",
            opacityDim = 0.15,
            selected = plotly::attrs_selected(showlegend = FALSE)
          ) |>
          plotly::config(displaylogo = FALSE) |> 
          plotly::event_register("plotly_selected") |> 
          plotly::event_register("plotly_hover")
      })
      
      
      ##////////////////////////////////////////
      ##  Sparkline module                    //
      ##////////////////////////////////////////
      spectraSparksServer(
        "scatter_sparks",
        grv,
        "click"
      )
      
      
      ##/////////////////////////////////////////
      ##  Spectra viewer module                //
      ##/////////////////////////////////////////
      spectraViewerServer(
        "scatter_ridgeline",
        shiny::reactive({grv$scatter$selected$data})
      )
      
      
      ##////////////////////////////////////////
      ##  Conditions viewer module            //
      ##////////////////////////////////////////
      conditionsViewerServer(
        "scatter_conditions",
        shiny::reactive({grv$scatter$hovered$data})
      )
    }
  )
}