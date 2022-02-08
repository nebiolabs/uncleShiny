
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
        shiny::fluidRow(
          shiny::selectInput(
            ns("color_inspector_wells"),
            "Color wells by:",
            choices = colorvarChoices,
            selected = "Buffer_condition_name",
            width = "25%"
          ),
          shiny::selectInput(
            ns("color_inspector_spectra"),
            "Color spectra by:",
            choices = colorvarChoices,
            selected = "Buffer_condition_name",
            width = "25%"
          ),
          shiny::selectInput(
            ns("palette_inspector"),
            shiny::HTML(
              "<a target='_blank' href='https://colorbrewer2.org'>ColorBrewer</a> Palette:"
            ),
            choices = palChoices,
            selected = "Set2",
            width = "25%"
          )
        ),
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
      
      grv$inspector <- shiny::reactiveValues(selected = list())
      
      munge_module_data <- function(data_input) {
        if (is.null(data_input)) {
          munged_data <- NULL
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
            df_char_int64() |> 
            cbind_colors(
              input$color_inspector_spectra,
              input$palette_inspector
            ) |>
            {\(df) 
              dplyr::mutate(
                df,
                tooltip = rlang::eval_tidy(
                  short_tootip_glue_string,
                  data = df
                )
              )
            }() |> 
            format_plate_overlay()
        }
      }
      
      ##----------------------------------------
      ##  Data instance for module            --
      ##----------------------------------------
      module_data <- shiny::reactive({
        grv$data() |>
          munge_module_data()
      })
      
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
                source = paste0(nm, "_source"), customdata = "well_id",
                color_var = input$color_inspector_wells,
                palette_name = input$palette_inspector,
                tooltip = "tooltip"
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
      ## Outputs a tab for each plate.. I don't like this layout as much.
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
      # This outputs just a set of plate plots and provides a better overview.
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
      shiny::observe({
        sources <- purrr::map_chr(names(module_data()), paste0, "_source")
        grv$inspector$selected$event <- purrr::map(
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
      }, label = "inspector_selected")
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Inspector selected data              <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        shiny::req(module_data())
        if (rlang::is_empty(grv$inspector$selected$event)) {
          grv$inspector$selected$data <- NULL
        } else {
          grv$inspector$selected$data <- purrr::map_dfr(
            module_data(),
            function(df) {
              dplyr::filter(
                df,
                well_id %in% grv$inspector$selected$event
              )
            }
          )
        }
      }, label = "inspector_selected_data")
      
      
      ##----------------------------------------
      ##  Raw selection output                --
      ##----------------------------------------
      # output$inspector_selected <- shiny::renderPrint({
      #   # shiny::req(grv$inspector$selected$event)
      #   event <- grv$inspector$selected$event
      #   # if (isTruthy(event)) {
      #   #   print(bit64::as.integer64.character(event))
      #   # } else {
      #   #   "Nothing is selected."
      #   # }
      #   if (rlang::is_empty(event)) {
      #     print("Nothing is selected.")
      #   } else {
      #     print(bit64::as.integer64.character(event))
      #   }
      # })
      
      
      ##/////////////////////////////////////////
      ##  Spectra viewer module                //
      ##/////////////////////////////////////////
      spectraViewerServer(
        "inspector_ridgeline",
        shiny::reactive({grv$inspector$selected$data}),
        robj_palette_name = shiny::reactive({input$palette_inspector})
      )
    }
  )
}
