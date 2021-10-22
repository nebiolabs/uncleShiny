
##-------------------------------------------------------------------------
##  mod_plateInspector - plate layout sample picker                      --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
plateInspectorUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("plate_tabs"))
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
              Buffer = dplyr::if_else(
                stringr::str_detect(Buffer, "Neutral Buffer"),
                "Neutral Buffer",
                Buffer
              )
            ) |> 
            formatPlateOverlay()
            # cbindColors(opts_obj$color_global, opts_obj$palette_global)
        })
      } else {
        module_data <- shiny::reactive({
          grv$robj_collected_data() |> 
            dplyr::mutate(
              Buffer = dplyr::if_else(
                stringr::str_detect(Buffer, "Neutral Buffer"),
                "Neutral Buffer",
                Buffer
              )
            ) |> 
            formatPlateOverlay()
            # cbindColors(opts_obj$color_global, opts_obj$palette_global)
        })
      }
      
      shiny::observe({
        purrr::iwalk(
          module_data(),
          function(df, nm) {
            output[[paste0(nm, "_plot")]] <- plotly::renderPlotly({
              buildPlateLayout(
                format = 96, overlay_data = df,
                source = paste0(nm, "_plot"), customdata = "well_id"
              )
            })
            output[[paste0(nm, "_selection")]] <- shiny::renderPrint({
              event <- plotly::event_data(
                event = "plotly_selected",
                source = paste0(nm, "_plot")
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
      
      output$plate_tabs <- shiny::renderUI({
        ns <- session$ns
        tab_list <- purrr::imap(
          module_data(),
          function(df, nm) {
            shiny::tabPanel(
              title = nm,
              plotly::plotlyOutput(ns(paste0(nm, "_plot"))),
              shiny::verbatimTextOutput(ns(paste0(nm, "_selection"))),
              shiny::verbatimTextOutput(ns(paste0(nm, "_df")))
            )
          }
        )
        rlang::inject(shiny::tabsetPanel(!!!unname(tab_list), type = "pills"))
      })
      
      
    }
  )
}