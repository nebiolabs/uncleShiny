
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
              height = "380px"
            )
          ),
          shiny::column(
            width = 3,
            ##-----------------------------------------
            ##  Scatter conditions viewer            --
            ##-----------------------------------------
            conditionsViewerUI(ns("scatter_conditions"))
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 9,
            ##----------------------------------------
            ##  Spectra sparklines                  --
            ##----------------------------------------
            shiny::h3("Spectra Quickview"),
            spectraSparksUI(ns("scatter_sparks"))
          ),
          shiny::column(
            width = 3,
            ##-----------------------------------------
            ##  Wordcloud                            --
            ##-----------------------------------------
            wordcloudUI(ns("scatter_wordcloud"))
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
              height = "380px"
            ),
            ##----------------------------------------
            ##  Zoom conditions viewer              --
            ##----------------------------------------
            conditionsViewerUI(ns("zoom_conditions"))
          ),
          shiny::column(
            width = 8,
            ##-----------------------------------------
            ##  Ridgeline plots                      --
            ##-----------------------------------------
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
      
      grv$zoom <- shiny::reactiveValues()
      
      munge_module_data <- function(data_input, color_input, palette_input) {
        if (is.null(data_input)) {
          NULL
        } else {
          data_input |>
            dplyr::mutate(
              Buffer = dplyr::if_else(
                stringr::str_detect(
                  Buffer,
                  "(Neutral Buffer)|(NB)"
                ),
                "Neutral Buffer",
                Buffer
              )
            ) |> 
            df_char_int64() |> 
            cbind_colors(color_input, palette_input)
        }
      }
      
      module_data <- shiny::reactive({
        grv$data_filtered() |> 
          munge_module_data(
            color_input = grv$scatter_opts$color_global,
            palette_input = grv$scatter_opts$palette_global
          )
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
          alpha_marker = grv$scatter_opts$alpha_points(),
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
          alpha_marker = grv$scatter_opts$alpha_points(),
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
            plotly::ggplotly(source = "S", tooltip = "text") |> 
            plotly::event_register("plotly_selected") |>
            plotly::event_register("plotly_click") |>
            plotly::event_register("plotly_hover"),
          plot_SLS_DSF() |>
            plotly::ggplotly(source = "S", tooltip = "text") |>
            plotly::event_register("plotly_selected") |>
            plotly::event_register("plotly_click") |>
            plotly::event_register("plotly_hover"),
          nrows = 1,
          titleX = TRUE,
          titleY = TRUE,
          margin = 0.04
        ) |>
          plotly::layout(
            legend = legendList,
            dragmode = "select"
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
          plotly::config(displaylogo = FALSE) |> 
          plotly::event_register("plotly_selected") |>
          plotly::event_register("plotly_click") |>
          plotly::event_register("plotly_hover")
      })
      
      
      ##------------------------
      ##  Selection callback  --
      ##------------------------
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter selected plotly event        <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        # similar to a debounce to delay repeat invalidation/evaluation
        # shiny::invalidateLater(1000, session)
        
        # plotly callback
        event <- plotly::event_data(
          event = "plotly_selected",
          source = "S"
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
            summary_ids = event[["key"]],
            well_ids = event[["customdata"]]
          )
        }
      }, label = "scatter_selected")
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter selected data                <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        shiny::req(module_data(), grv$scatter$selected)
        grv$scatter$selected$data <- cbind_colors(
          df = df_char_int64(
            dplyr::filter(
              module_data(),
              uncle_summary_id %in% grv$scatter$selected[["summary_ids"]]
            )
          ),
          color_var = grv$scatter_opts$color_zoom,
          palette_name = grv$scatter_opts$palette_global
        )
      }, label = "scatter_selected_data")
      
      
      ##----------------------
      ##  Click callback    --
      ##----------------------
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter clicked plotly event         <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        # similar to a debounce to delay repeat invalidation/evaluation
        # shiny::invalidateLater(1000, session)
        
        # plotly callback
        event <- plotly::event_data(
          event = "plotly_click",
          source = "S"
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
            # these should be converted to int64 as the spark module uses
            # top-level data that has not been factored for plotting aesthetics
            summary_ids = bit64::as.integer64.character(event[["key"]]),
            well_ids = bit64::as.integer64.character(event[["customdata"]])
          )
        }
      }, label = "scatter_clicked")
      
      
      ##----------------------
      ##  Hover callback    --
      ##----------------------
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Scatter hovered plotly event         <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        # similar to a debounce to delay repeat invalidation/evaluation
        
        # plotly callback
        event <- plotly::event_data(
          event = "plotly_hover",
          source = "S"
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
            summary_ids = event[["key"]],
            well_ids = event[["customdata"]]
          )
        }
        # if (rlang::is_empty(grv$scatter$hovered[["summary_ids"]])) {
        #   cat("Notice, that empty vector error happened again on hover.\n")
        # }
      }, label = "scatter_hovered")
      
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
      
      
      ##-----------------------------------------
      ##  Zoom Plot                            --
      ##-----------------------------------------
      # Reactive object of SLS & DSF plot
      plot_zoom <- shiny::reactive({
        shiny::req(grv$scatter$selected$data)
        ggscatter(
          data = grv$scatter$selected$data,
          x_var = grv$scatter_opts$xvar3,
          y_var = grv$scatter_opts$yvar3,
          label = "well",
          color_var = grv$scatter_opts$color_zoom,
          color_encoded = FALSE,
          palette_name = grv$scatter_opts$palette_global,
          size = grv$scatter_opts$size_points(),
          alpha_marker = grv$scatter_opts$alpha_points(),
          alpha_label = 0.5,
          # show_vert_guides = grv$scatter_opts$show_guides_v2,
          # vert_guides = grv$scatter_opts$guides_v2(),
          # show_horiz_guides = grv$scatter_opts$show_guides_h2,
          # horiz_guides = grv$scatter_opts$guides_h2(),
          x_is_log = grv$scatter_opts$xvar3_is_log,
          custom_data = "uncle_summary_id",
          show_legend = TRUE
        )
      })
      

      ##----------------------------------------
      ##  Zoom Plotly Output                  --
      ##----------------------------------------
      # Plotly subplot output rendering
      output$plot_zoom <- plotly::renderPlotly({
        plot_zoom() |> 
        plotly::ggplotly(source = "Z", tooltip = "text") |>
          plotly::layout(
            legend = legendList,
            dragmode = "select"
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
      
      
      ##-----------------------
      ##  Select callback    --
      ##-----------------------
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Zoom selected plotly event          <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        # similar to a debounce to delay repeat invalidation/evaluation
        # shiny::invalidateLater(1000, session)

        # plotly callback
        event <- plotly::event_data(
          event = "plotly_selected",
          source = "Z"
        )

        # sometimes selection returns a list if points overlap which is
        # incompatible with filtering and must be repaired
        if (rlang::is_list(event[["customdata"]])) {
          event[["customdata"]] <- as.character(unlist(event[["customdata"]]))
        }

        # output of a list containing key and customdata values for selection
        if (is.null(event)) {
          grv$zoom$selected <- NULL
        } else {
          grv$zoom$selected <- list(
            summary_ids = event[["customdata"]]
          )
        }
      }, label = "zoom_selected")
      
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Zoom selected data                  <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # This should be a local reactive instead of a global reactive inside of
      # an observer. The logic above is negated by this logic to check for the
      # presence of a selection event, thus it never plots if there is no event.
      # If needed elsewhere, make a separate reactiveValues object instead.
      ridgeline_data <- shiny::reactive({
        shiny::req(grv$scatter$selected$data)
        coded_data <- grv$scatter$selected$data
        if (is.null(grv$zoom$selected)) {
          coded_data
        } else {
          dplyr::filter(
            coded_data,
            uncle_summary_id %in% grv$zoom$selected[["summary_ids"]]
          )
        }
      })
      
      
      ##----------------------
      ##  Hover callback    --
      ##----------------------
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Zoom hovered plotly event            <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        # similar to a debounce to delay repeat invalidation/evaluation
        
        # plotly callback
        event <- plotly::event_data(
          event = "plotly_hover",
          source = "Z"
        )
        
        # sometimes selection returns a list if points overlap which is
        # incompatible with filtering and must be repaired
        if (rlang::is_list(event[["customdata"]])) {
          event[["customdata"]] <- as.character(unlist(event[["customdata"]]))
        }
        
        # output of a list containing key and customdata values for selection
        if (is.null(event)) {
          grv$zoom$hovered <- NULL
        } else {
          grv$zoom$hovered <- list(
            summary_ids = event[["customdata"]]
          )
        }
        # if (rlang::is_empty(grv$zoom$hovered[["summary_ids"]])) {
        #   cat("Notice, that empty vector error happened again on hover.\n")
        # }
      }, label = "zoom_hovered")
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Zoom hovered data                    <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        shiny::req(module_data(), grv$zoom$hovered)
        if (rlang::is_empty(grv$zoom$hovered[["summary_ids"]])) {
          grv$zoom$hovered$data <- NULL
        } else {
          grv$zoom$hovered$data <- dplyr::filter(
            module_data(),
            uncle_summary_id %in% grv$zoom$hovered[["summary_ids"]]
          )
        }
      }, label = "zoom_hovered_data")
      
      
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
        ridgeline_data,
        robj_palette_name = shiny::reactive({grv$scatter_opts$palette_global})
      )
      
      
      ##////////////////////////////////////////
      ##  Conditions viewer modules           //
      ##////////////////////////////////////////
      conditionsViewerServer(
        "scatter_conditions",
        shiny::reactive({grv$scatter$hovered$data})
      )
      
      conditionsViewerServer(
        "zoom_conditions",
        shiny::reactive({grv$zoom$hovered$data})
      )
      
      ##////////////////////////////////////////
      ##  Wordcloud module                    //
      ##////////////////////////////////////////
      wordcloudServer(
        "scatter_wordcloud",
        shiny::reactive({grv$scatter$selected$data}),
        grv$condition_groups
      )
    }
  )

}
