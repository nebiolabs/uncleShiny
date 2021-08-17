
##-------------------------------------------------------------------------
##  mod_scatterPlots - summary data visualization                        --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI components                                      --
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
          plotly::plotlyOutput(ns("plot_scatter"), width = "100%", height = "600px")
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
        shiny::verbatimTextOutput(ns("data_shared_group"))
      )
    ),
    ##-----------------------------------------
    ##  Test tables                          --
    ##-----------------------------------------
    shiny::fluidRow(shiny::h3("Direct Output (Debugging)")),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::h5("Selected:"),
        DT::DTOutput(ns("test_selected"))
      ),
      shiny::column(
        width = 6,
        shiny::h5("Hovered:"),
        DT::DTOutput(ns("test_hovered"))
      )
    )
  )
}

##-------------------------------------------------------
##  Server function                                    --
##-------------------------------------------------------
scatterPlotsServer <- function(id, opts_obj, grv, data_shared) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
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
      
      # Scatter plot selected event data
      grv$scatter_selected_event <- shiny::debounce(shiny::reactive({
        plotly::event_data(event = "plotly_selected", source = "scatter")
      }), 1000)
      
      # Scatter plot hover event data
      grv$scatter_hover_event <- shiny::debounce(shiny::reactive({
        plotly::event_data(event = "plotly_hover", source = "scatter")
      }), 1000)
      
      output$test_selected <- DT::renderDT({
        shiny::req(grv$scatter_selected_event())
        DT::datatable(
          data = dplyr::select(grv$scatter_selected_event(), -c(curveNumber)),
          options = list(
            dom = "it",
            scrollX = TRUE,
            scrollY = "100px",
            scrollCollapse = TRUE
          )
        )
      })
      output$test_hovered <- DT::renderDT({
        shiny::req(grv$scatter_hover_event())
        DT::datatable(
          data = dplyr::select(grv$scatter_hover_event(), -c(curveNumber)),
          options = list(
            dom = "it",
            scrollX = TRUE,
            scrollY = "100px",
            scrollCollapse = TRUE
          )
        )
      })
      
      output$data_shared_group <- renderPrint({
        data_shared$groupName()
      })
      
      output$data_shared_selected <- DT::renderDT({
        DT::datatable(
          data = data_shared,
          selection = "none",
          # extensions = c("FixedColumns"),
          options = list(
            dom = "tip",
            # i - information
            # f - filter
            # searchHighlight = TRUE,
            # p - pagination
            scrollX = TRUE,
            scrollY = "200px",
            paging = FALSE,
            # pageLength = 80,
            scrollCollapse = TRUE#,
            # t - table
            # fixedColumns = list(leftColumns = 5),
            # order = list(list(3, "asc")),
            # columnDefs = list(list(visible = FALSE, targets = c(1, 2)))
          )
        )
      })
    }
  )
}