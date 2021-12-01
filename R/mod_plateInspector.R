
##-------------------------------------------------------------------------
##  mod_plateInspector - plate layout sample picker                      --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
plateInspectorUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      sidebarPanel = shiny::sidebarPanel(
        width = 4,
        shiny::uiOutput(ns("plate_layouts"))#,
        # shiny::verbatimTextOutput(ns("inspector_selected"))
      ),
      mainPanel = shiny::mainPanel(
        width = 8,
        spectraViewerUI(ns("inspector_ridgeline"))
      )
    )
  )
}

##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
plateInspectorServer <- function(id, grv) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ##----------------------------------------
      ##  Data instance for module            --
      ##----------------------------------------
      if (use_testing_mode) {
        module_data <- shiny::reactive({
          test_data |>
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
            dplyr::mutate(dplyr::across(
              c("well_id"),
              .fns = bit64::as.character.integer64
            )) |> 
            format_plate_overlay()
            # cbind_colors(opts_obj$color_global, opts_obj$palette_global)
        })
      } else {
        module_data <- shiny::reactive({
          grv$robj_collected_data() |> 
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
            dplyr::mutate(dplyr::across(
              c("well_id"),
              .fns = bit64::as.character.integer64
            )) |> 
            format_plate_overlay()
            # cbind_colors(opts_obj$color_global, opts_obj$palette_global)
        })
      }
      
      ##-----------------------------------------
      ##  Dynamic outputs                      --
      ##-----------------------------------------
      shiny::observe({
        purrr::iwalk(
          module_data(),
          function(df, nm) {
            output[[paste0(nm, "_plot")]] <- plotly::renderPlotly({
              build_plate_layout(
                format = 96, overlay_data = df,
                source = paste0(nm, "_source"), customdata = "well_id"
              )
            })
            output[[paste0(nm, "_selection")]] <- shiny::renderPrint({
              event <- plotly::event_data(
                event = "plotly_selected",
                source = paste0(nm, "_source")
              )
              if (isTruthy(event)) {
                dplyr::filter(event, !is.na(customdata))
              } else {
                "Nothing is selected."
              }
            })
            output[[paste0(nm, "_df")]] <- shiny::renderPrint({
              df
            })
          }
        )
      })
      
      ##-----------------------------------------
      ##  Dynamic UI generation                --
      ##-----------------------------------------
      # output$plate_tabs <- shiny::renderUI({
      #   ns <- session$ns
      #   tab_list <- purrr::imap(
      #     module_data(),
      #     function(df, nm) {
      #       shiny::tabPanel(
      #         title = nm,
      #         plotly::plotlyOutput(ns(paste0(nm, "_plot"))),
      #         shiny::verbatimTextOutput(ns(paste0(nm, "_selection"))),
      #         shiny::verbatimTextOutput(ns(paste0(nm, "_df")))
      #       )
      #     }
      #   )
      #   rlang::inject(shiny::tabsetPanel(!!!unname(tab_list), type = "pills"))
      # })
      
      output$plate_layouts <- shiny::renderUI({
        ns <- session$ns
        layout_list <- purrr::imap(
          module_data(),
          function(df, nm) {
            shiny::fluidRow(
              shiny::h4(nm),
              plotly::plotlyOutput(ns(paste0(nm, "_plot")), height = "400px")#,
              # shiny::verbatimTextOutput(ns(paste0(nm, "_selection")))
            )
          }
        )
        rlang::inject(shiny::tagList(!!!unname(layout_list)))
      })
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Inspector selected plotly event      <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      grv$inspector_selected_event <- shiny::debounce(shiny::reactive({
        sources <- purrr::map_chr(names(module_data()), paste0, "_source")
        purrr::map(
          sources,
          function(plot_source) {
            event <- plotly::event_data(
              event = "plotly_selected",
              source = plot_source
            )
            if (is.null(event)) {
              return(NULL)
            } else {
              event |> 
                dplyr::filter(!is.na(customdata)) |> 
                dplyr::pull(customdata)
            }
          }
        ) |> purrr::flatten_chr()
      }), 1000)
      
      ##----------------------------------------
      ##  Raw selection output                --
      ##----------------------------------------
      output$inspector_selected <- shiny::renderPrint({
        shiny::req(grv$inspector_selected_event())
        event <- grv$inspector_selected_event()
        if (isTruthy(event)) {
          print(bit64::as.integer64.character(event))
        } else {
          "Nothing is selected."
        }
      })
      
      ##----------------------------------------
      ##  Reactive selection                  --
      ##----------------------------------------
      robj_inspector_selected <- shiny::reactive({
        bit64::as.integer64.character(
          grv$inspector_selected_event()
        )
      })
      
      ##/////////////////////////////////////////
      ##  Spectra viewer module                //
      ##/////////////////////////////////////////
      spectraViewerServer(
        "inspector_ridgeline",
        grv,
        "well_id",
        robj_inspector_selected
      )
    }
  )
}
