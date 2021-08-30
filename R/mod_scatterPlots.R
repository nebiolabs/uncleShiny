
##-------------------------------------------------------------------------
##  mod_scatterPlots - summary data visualization                        --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
scatterPlotsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 9,
        shiny::h3("Summary Data"),
        shiny::helpText("Click/drag to select for zoom. Double-click to deselect.
                    Click a point to show all spectra."),
        ##-----------------------------------------
        ##  Scatter plots                        --
        ##-----------------------------------------
        # shiny::verbatimTextOutput(ns("data_shared_group")),
        plotly::plotlyOutput(
          ns("plot_scatter"),
          width = "100%",
          height = "600px"
        ),
        ##-----------------------------------------
        ##  Zoomed plot                          --
        ##-----------------------------------------
        shiny::h3("Selection Zoom"),
        shiny::helpText("Activated on click/drag selection."),
        plotly::plotlyOutput(
          ns("plot_zoom"),
          width = "100%",
          height = "400px"
        )
      ),
      shiny::column(
        width = 3,
        ##----------------------------------------
        ##  Spectra sparks                      --
        ##----------------------------------------
        shiny::h3("Spectra Quickview"),
        shiny::helpText("Activated on click."),
        spectraSparksUI(ns("spectraSparks"))
      )
    )#,
    # ##----------------------------------------
    # ##  Test outputs                        --
    # ##----------------------------------------
    # shiny::fluidRow(shiny::h4("Direct Output (Debugging)")),
    # shiny::fluidRow(
    #   shiny::column(
    #     width = 4,
    #     shiny::h6("Hovered:"),
    #     shiny::verbatimTextOutput(ns("test_hover_summary_id")),
    #   ),
    #   shiny::column(
    #     width = 4,
    #     shiny::h6("Clicked:"),
    #     shiny::verbatimTextOutput(ns("test_click_summary_id")),
    #   ),
    #   shiny::column(
    #     width = 4,
    #     shiny::h6("Selected:"),
    #     shiny::verbatimTextOutput(ns("test_selected_summary_ids")),
    #   )
    # )
  )
}

##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
scatterPlotsServer <- function(id, opts_obj, grv) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      if (use_testing_mode) {
        module_data <- shiny::reactive({
          test_data |> 
            cbindColors(opts_obj$color_global, opts_obj$palette_global)
        })
      } else {
        module_data <- shiny::reactive({
          grv$robj_collected_data() |> 
            cbindColors(opts_obj$color_global, opts_obj$palette_global)
        })
      }
      module_SharedData <- shiny::reactive({
        crosstalk::SharedData$new(module_data(), key = ~uncle_summary_id)
      })
      
      
      ##-----------------------------------------
      ##  Left Plot (DLS)                      --
      ##-----------------------------------------
      # Reactive object of DLS plot
      plot_DLS <- shiny::reactive({
        ggscatter(
          data = module_SharedData(),
          x_var = opts_obj$xvar1,
          y_var = opts_obj$yvar1,
          color_var = opts_obj$color_global,
          color_encoded = FALSE,
          palette_name = opts_obj$palette_global,
          size = opts_obj$size_points(),
          alpha = opts_obj$alpha_points(),
          show_vert_guides = opts_obj$show_guides_v1,
          vert_guides = opts_obj$guides_v1(),
          show_horiz_guides = opts_obj$show_guides_h1,
          horiz_guides = opts_obj$guides_h1(),
          x_is_log = opts_obj$xvar1_is_log,
          custom_data = "well_id",
          show_legend = FALSE
        )
      })
      
      
      ##-----------------------------------------
      ##  Right Plot (SLS/DSF)                 --
      ##-----------------------------------------
      # Reactive object of SLS & DSF plot
      plot_SLS_DSF <- shiny::reactive({
        ggscatter(
          data = module_SharedData(),
          x_var = opts_obj$xvar2,
          y_var = opts_obj$yvar2,
          color_var = opts_obj$color_global,
          color_encoded = FALSE,
          palette_name = opts_obj$palette_global,
          size = opts_obj$size_points(),
          alpha = opts_obj$alpha_points(),
          show_vert_guides = opts_obj$show_guides_v2,
          vert_guides = opts_obj$guides_v2(),
          show_horiz_guides = opts_obj$show_guides_h2,
          horiz_guides = opts_obj$guides_h2(),
          x_is_log = opts_obj$xvar2_is_log,
          custom_data = "well_id",
          show_legend = TRUE
        )
      })
      
      
      ##-----------------------------------------
      ##  L/R Plotly Output                    --
      ##-----------------------------------------
      # Plotly subplot output rendering
      output$plot_scatter <- plotly::renderPlotly({
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
            # annotations = list(
            #   list(
            #     x = 0.45, xref = "paper", xanchor = "right",
            #     y = 1, yref = "paper",
            #     text = "Plot L", font = list(size = 18),
            #     showarrow = F
            #   ),
            #   list(
            #     x = 1, xref = "paper", xanchor = "right",
            #     y = 1, yref = "paper",
            #     text = "Plot R", font = list(size = 18),
            #     showarrow = F
            #   )
            # ),
            legend = legendList
          ) |>
          plotly::highlight(
            on = "plotly_selected", off = "plotly_deselect",
            opacityDim = 0.15,
            selected = plotly::attrs_selected(showlegend = FALSE)
          ) |>
          plotly::config(displaylogo = FALSE)# |> 
          # plotly::event_register("plotly_selected") |> 
          # plotly::event_register("plotly_hover")
      })
      
      
      ##-----------------------------------------
      ##  Plotly Callbacks                     --
      ##-----------------------------------------
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter selected plotly event        <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # Scatter plot selected event data
      grv$scatter_selected_event <- shiny::debounce(shiny::reactive({
        plotly::event_data(event = "plotly_selected", source = "scatter")
      }), 100)
      # Scatter plot selected summary_ids (key) and well_ids (customdata)
      grv$scatter_selected_summary_ids <- shiny::debounce(shiny::reactive({
        shiny::req(grv$scatter_selected_event())
        df <- grv$scatter_selected_event()
        if (rlang::is_list(df[["key"]])) {
          df[["key"]] <- as.character(unlist(df[["key"]]))
        }
        if (rlang::is_list(df[["customdata"]])) {
          df[["customdata"]] <- as.character(unlist(df[["customdata"]]))
        }
        list(
          summary_ids = bit64::as.integer64.character(df[["key"]]),
          well_ids = bit64::as.integer64.character(df[["customdata"]])
        )
      }), 100)
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter clicked plotly event         <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # Scatter plot click event data
      grv$scatter_click_event <- shiny::debounce(shiny::reactive({
        plotly::event_data(event = "plotly_click", source = "scatter")
      }), 500)
      # Scatter plot clicked summary_id (key) and well_ids (customdata)
      grv$scatter_click_summary_id <- shiny::debounce(shiny::reactive({
        shiny::req(grv$scatter_click_event())
        df <- grv$scatter_click_event()
        if (rlang::is_list(df[["key"]])) {
          df[["key"]] <- as.character(unlist(df[["key"]]))
        }
        if (rlang::is_list(df[["customdata"]])) {
          df[["customdata"]] <- as.character(unlist(df[["customdata"]]))
        }
        list(
          summary_ids = bit64::as.integer64.character(df[["key"]]),
          well_ids = bit64::as.integer64.character(df[["customdata"]])
        )
      }), 500)
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter hovered plotly event         <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # Scatter plot hover event data
      grv$scatter_hover_event <- shiny::debounce(shiny::reactive({
        plotly::event_data(event = "plotly_hover", source = "scatter")
      }), 500)
      # Scatter plot hover summary_id (key) and well_id (customdata)
      grv$scatter_hover_summary_id <- shiny::debounce(shiny::reactive({
        shiny::req(grv$scatter_hover_event())
        df <- grv$scatter_hover_event()
        if (rlang::is_list(df[["key"]])) {
          df[["key"]] <- as.character(unlist(df[["key"]]))
        }
        if (rlang::is_list(df[["customdata"]])) {
          df[["customdata"]] <- as.character(unlist(df[["customdata"]]))
        }
        list(
          summary_ids = bit64::as.integer64.character(df[["key"]]),
          well_ids = bit64::as.integer64.character(df[["customdata"]])
        )
      }), 500)
      
      
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter selected data                <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      grv$robj_selected_data <- shiny::reactive({
        shiny::req(module_data(), grv$scatter_selected_summary_ids())
        dplyr::filter(
          module_data(),
          uncle_summary_id %in%
            grv$scatter_selected_summary_ids()[["summary_ids"]]
        )
      })
      
      ##-----------------------------------------
      ##  Zoom Plot (selected callback)        --
      ##-----------------------------------------
      # Reactive object of SLS & DSF plot
      plot_zoom <- shiny::reactive({
        ggscatter(
          data = grv$robj_selected_data(),
          x_var = opts_obj$xvar3,
          y_var = opts_obj$yvar3,
          color_var = opts_obj$color_global,
          color_encoded = TRUE,
          palette_name = opts_obj$palette_global,
          size = opts_obj$size_points(),
          alpha = opts_obj$alpha_points(),
          # show_vert_guides = opts_obj$show_guides_v2,
          # vert_guides = opts_obj$guides_v2(),
          # show_horiz_guides = opts_obj$show_guides_h2,
          # horiz_guides = opts_obj$guides_h2(),
          x_is_log = opts_obj$xvar2_is_log,
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
      
      
      
      
      # ##----------------------------------------
      # ##  Test outputs                        --
      # ##----------------------------------------
      # # Testing - selected print
      # output$test_selected_summary_ids <- shiny::renderPrint({
      #   shiny::req(grv$scatter_selected_summary_ids())
      #   grv$scatter_selected_summary_ids()
      # })
      # # Testing - clicked print
      # output$test_click_summary_id <- shiny::renderPrint({
      #   shiny::req(grv$scatter_click_summary_id())
      #   grv$scatter_click_summary_id()
      # })
      # # Testing - hover print
      # output$test_hover_summary_id <- shiny::renderPrint({
      #   shiny::req(grv$scatter_hover_summary_id())
      #   grv$scatter_hover_summary_id()
      # })
      # 
      # # # Testing - SharedData group id
      # # output$data_shared_group <- renderPrint({
      # #   data_shared$groupName()
      # # })
      
      
      
      ##////////////////////////////////////////
      ##  Sparkline Module                    //
      ##////////////////////////////////////////
      spectraSparksServer(
        "spectraSparks",
        grv,
        opts_obj,
        "click"
      )
      
    }
  )
}