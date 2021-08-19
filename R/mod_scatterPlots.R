
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
        ##-----------------------------------------
        ##  Scatter plots                        --
        ##-----------------------------------------
        shiny::fluidRow(
          shiny::h3("Summary Data"),
          shiny::verbatimTextOutput(ns("data_shared_group")),
          plotly::plotlyOutput(
            ns("plot_scatter"),
            width = "100%",
            height = "600px"
          )
        ),
        ##-----------------------------------------
        ##  Zoomed plot                          --
        ##-----------------------------------------
        shiny::fluidRow(
          shiny::h3("Selection Zoom")
          # zoomed in plot
        )
      ),
      shiny::column(
        width = 3,
        ##----------------------------------------
        ##  Spectra sparks                      --
        ##----------------------------------------
        shiny::h3("Spectra Quickview"),
        spectraSparksUI(ns("spectraSparks"))
      )
    ),
    ##-----------------------------------------
    ##  Test tables                          --
    ##-----------------------------------------
    shiny::fluidRow(shiny::h4("Direct Output (Debugging)")),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::h6("Hovered:"),
        shiny::tableOutput(ns("test_hovered"))
      ),
      shiny::column(
        width = 4,
        shiny::h6("Selected:"),
        shiny::tableOutput(ns("test_selected"))
      ),
      shiny::column(
        width = 4,
        shiny::h6("Hovered:"),
        shiny::verbatimTextOutput(ns("test_hover_summary_id")),
        shiny::h6("Selected"),
        shiny::verbatimTextOutput(ns("test_selected_summary_ids"))
      )
    )
  )
}

##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
scatterPlotsServer <- function(id, opts_obj, grv, data_shared) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      ##-----------------------------------------
      ##  Left Plot (DLS)                      --
      ##-----------------------------------------
      # Reactive object of DLS plot
      plot_DLS <- shiny::reactive({
        ggscatter(
          data = data_shared,
          x_var = opts_obj$xvar1,
          y_var = opts_obj$yvar1,
          color_var = opts_obj$color_global,
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
          data = data_shared,
          x_var = opts_obj$xvar2,
          y_var = opts_obj$yvar2,
          color_var = opts_obj$color_global,
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
      ##  Plotly Output                        --
      ##-----------------------------------------
      # Plotly subplot output rendering
      output$plot_scatter <- plotly::renderPlotly({
        plotly::subplot(
          plot_DLS() |>
            plotly::ggplotly(source = "scatter", tooltip = "text"),
          plot_SLS_DSF() |>
            plotly::ggplotly(source = "scatter", tooltip = "text") |>
            plotly::layout(
              xaxis = axisList,
              yaxis = axisList
            ),
          nrows = 1,
          titleX = TRUE,
          titleY = TRUE,
          margin = 0.04
        ) |>
          plotly::layout(
            annotations = list(
              list(
                x = 0, xref = "paper", xanchor = "right",
                y = 1.09, yref = "paper",
                text = "Plot1", font = list(size = 18),
                showarrow = F
              ),
              list(
                x = 0.54, xref = "paper", xanchor = "right",
                y = 1.09, yref = "paper",
                text = "Plot2", font = list(size = 18),
                showarrow = F
              )
            ),
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
      
      
      ##-----------------------------------------
      ##  Plotly Callbacks                     --
      ##-----------------------------------------
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter selected                    <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # Scatter plot selected event data
      grv$scatter_selected_event <- shiny::debounce(shiny::reactive({
        df <- plotly::event_data(event = "plotly_selected", source = "scatter")
        # shiny::req(df)
        df
      }), 1000)
      # Scatter plot selected summary_ids (key) and well_ids (customdata)
      grv$scatter_selected_summary_ids <- shiny::debounce(shiny::reactive({
        df <- plotly::event_data(event = "plotly_selected", source = "scatter")
        shiny::req(df)
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
      }), 1000)
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter hovered                     <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # Scatter plot hover event data
      grv$scatter_hover_event <- shiny::debounce(shiny::reactive({
        df <- plotly::event_data(event = "plotly_hover", source = "scatter")
        # shiny::req(df)
        df
      }), 1000)
      # Scatter plot hover summary_id (key) and well_id (customdata)
      grv$scatter_hover_summary_id <- shiny::debounce(shiny::reactive({
        df <- plotly::event_data(event = "plotly_hover", source = "scatter")
        shiny::req(df)
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
      }), 1000)
      
      
      ##-----------------------------------------
      ##  Testing Outputs                      --
      ##-----------------------------------------
      # Testing - selected table output
      output$test_selected <- shiny::renderTable({
        shiny::req(grv$scatter_selected_event())
        dplyr::select(grv$scatter_selected_event(), -c(curveNumber))
      })
      # Testing - selected print
      output$test_selected_summary_ids <- shiny::renderPrint({
        shiny::req(grv$scatter_selected_summary_ids())
        grv$scatter_selected_summary_ids()
      })
      # Testing - hover table output
      output$test_hovered <- shiny::renderTable({
        shiny::req(grv$scatter_hover_event())
        dplyr::select(grv$scatter_hover_event(), -c(curveNumber))
      })
      # Testing - hover print
      output$test_hover_summary_id <- shiny::renderPrint({
        shiny::req(grv$scatter_hover_summary_id())
        grv$scatter_hover_summary_id()
      })
      
      # Testing - SharedData group id
      output$data_shared_group <- renderPrint({
        data_shared$groupName()
      })
      
      
      
      ##////////////////////////////////////////
      ##  Sparkline Module                    //
      ##////////////////////////////////////////
      spectraSparksServer(
        "spectraSparks",
        grv,
        opts_obj
      )
      
    }
  )
}