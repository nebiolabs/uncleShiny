
##--------------------------------------------------------------------------
##  mod_dataFilters - live filtering                                      --
##--------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
dataFiltersUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Data Filters"),
    shiny::helpText(
      "These filters alter the underlying data used for creating plots
      and can be used to remove unwanted data points for a less cluttered
      and more informative analysis."
    ),
    shiny::br(),
    shiny::br(),
    shiny::actionButton(
      ns("bttn_update_filters"),
      "Update",
      width = "30%"
    ),
    shiny::actionButton(
      ns("bttn_apply_filters"),
      "Apply",
      width = "30%"
    ),
    shiny::actionButton(
      ns("bttn_reset_filters"),
      "Reset",
      width = "30%"
    ),
    shiny::br(),
    shiny::br(),
    shiny::helpText(
      "Select your filters from the panels below and use the buttons above
      to apply or remove them."
    ),
    shiny::br(),
    shiny::br(),
    shiny::tabsetPanel(
      type = "pills",
      ##-----------------------------------------
      ##  Condition Filters                    --
      ##-----------------------------------------
      shiny::tabPanel(
        title = NULL,#"Conditions",
        value = "filters_conditions",
        icon = shiny::icon("flask"),
        shiny::h4("Condition Filters"),
        shiny::helpText("Anything selected will be excluded."),
        shiny::br(),
        shiny::br(),
        shiny::uiOutput(ns("condition_filters_UI"))
      ),
      ##-----------------------------------------
      ##  Numeric Filters                      --
      ##-----------------------------------------
      shiny::tabPanel(
        title = NULL,#"Numeric",
        value = "filters_numeric",
        icon = shiny::icon("columns"),
        shiny::h4("Numeric Filters"),
        shiny::helpText("Only values in the selected range are included."),
        shiny::br(),
        shiny::br(),
        shiny::numericInput(
          ns("filter_Z_D"),
          "Avg. Z Dia.",
          value = 50,
          min = 0.01,
          max = 1000,
          step = 10
        ),
        shiny::uiOutput(ns("numeric_filters_UI"))
      )
    )
  )
}

dataFiltersServer <- function(id, grv) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ##-----------------------------------------
      ##  Button and filter logic              --
      ##-----------------------------------------
      
      ##----------------------
      ##  Button counter    --
      ##----------------------
      counter <- shiny::reactiveVal(value = 0, label = "bttn_counter")
      
      ##-----------------------
      ##  Initial state      --
      ##-----------------------
      shiny::observe({
        if (counter() == 0) {
          grv$data_filtered <- grv$data
        }
      })
      
      ##-----------------------
      ##  Apply filters      --
      ##-----------------------
      shiny::observeEvent(input$bttn_apply_filters, {
        counter(counter() + 1)
        grv$data_filtered <- shiny::reactive({
          grv$data() |>
            dplyr::filter(Z_D <= shiny::isolate(input$filter_Z_D))
        })
      })
      
      ##-----------------------
      ##  Reset filters      --
      ##-----------------------
      shiny::observeEvent(input$bttn_reset_filters, {
        counter(0)
      })
      
      
      ##----------------------------------------
      ##  Variables to filter on              --
      ##----------------------------------------
      
      ##----------------------
      ##  Conditions        --
      ##----------------------
      condition_filters_list <- c(
        "Buffer" = "Buffer_condition_name",
        "pH" = "pH_unit_value",
        "Buffer Salt" = "BufferSalt_condition_name",
        "ReducingAgent" = "ReducingAgent_condition_name",
        "Sugar" = "Sugar_condition_name",
        "Plate Type" = "plate",
        "Exp. Set ID" = "exp_set_id",
        "Notes" = "notes",
        "Exp. ID" = "exp_id",
        "Well" = "well",
        "Instrument" = "instrument"
      )
      
      ##-----------------------
      ##  Numeric            --
      ##-----------------------
      numeric_filters_list <- c(
        "Tm" = "Tm1",
        "Tagg @ 266nm" = "Tagg266",
        "Tagg @ 473nm" = "Tagg473",
        "Polydispersity Index" = "PdI",
        "Z Diameter (nm)" = "Z_D",
        "Peak 1 Diameter (nm)" = "peak1_D",
        "pH" = "pH_unit_value"
      )
      
      
      ##-----------------------------------------
      ##  Dynamic UI generation                --
      ##-----------------------------------------
      
      ##----------------------
      ##  Conditions        --
      ##----------------------
      output$condition_filters_UI <- shiny::renderUI({
        ns <- session$ns
        UI_list <- purrr::imap(
          condition_filters_list,
          function(var, nm) {
            shiny::selectInput(
              ns(paste("filter", var, sep = "_")),
              nm,
              choices = NULL,
              multiple = TRUE,
              width = "100%"
            )
          }
        )
        rlang::inject(shiny::tagList(!!!unname(UI_list)))
      })
      
      ##-----------------------
      ##  Numeric            --
      ##-----------------------
      # output$numeric_filters_UI <- shiny::renderUI({
      #   ns <- session$ns
      #   UI_list <- purrr::imap(
      #     numeric_filters_list,
      #     function(var, nm) {
      #       shiny::sliderInput(
      #         ns(paste("filter", var, sep = "_")),
      #         nm,
      #         min = 0.0001,
      #         max = 1000,
      #         value = c(0.0001, 1000),
      #         ticks = TRUE,
      #         width = "100%"
      #       )
      #     }
      #   )
      #   rlang::inject(shiny::tagList(!!!unname(UI_list)))
      # })
      output$numeric_filters_UI <- shiny::renderUI({
        ns <- session$ns
        UI_list <- purrr::imap(
          numeric_filters_list,
          function(var, nm) {
            shiny::tagList(
              shiny::h5(nm),
              shiny::splitLayout(
                shiny::numericInput(
                  ns(paste("filter", var, "min", sep = "_")),
                  label = "min",
                  value = 0.0001
                ),
                shiny::numericInput(
                  ns(paste("filter", var, "max", sep = "_")),
                  label = "max",
                  value = 1000
                )
              )
            )
          }
        )
        rlang::inject(shiny::tagList(!!!unname(UI_list)))
      })
      
      ##-----------------------------------------
      ##  Dynamic UI update                    --
      ##-----------------------------------------
      
      ##----------------------
      ##  Conditions        --
      ##----------------------
      shiny::observeEvent(input$bttn_update_filters_conditions, {
        purrr::iwalk(
          condition_filters_list,
          function(var, nm) {
            shiny::updateSelectInput(
              session = session,
              inputId = paste("filter", var, sep = "_"),
              choices = unique(grv$data()[[var]]),
              selected = unique(grv$data()[[var]])
            )
          }
        )
      })
      
      output$conditions <- shiny::renderPrint({
        purrr::map(
          condition_filters_list,
          function(var) input[[paste("filter", var, sep = "_")]]
        )
      })
      
      ##-----------------------
      ##  Numeric            --
      ##-----------------------
      # shiny::observeEvent(input$bttn_update_filters_numeric, {
      #   purrr::iwalk(
      #     numeric_filters_list,
      #     function(var, nm) {
      #       shiny::updateSliderInput(
      #         session = session,
      #         inputId = paste("filter", var, sep = "_"),
      #         min = round(min(as.numeric(grv$data()[[var]]), na.rm = TRUE), 1),
      #         max = round(max(as.numeric(grv$data()[[var]]), na.rm = TRUE), 1),
      #         value = round(range(as.numeric(grv$data()[[var]]), na.rm = TRUE), 1)
      #       )
      #     }
      #   )
      # })
      shiny::observeEvent(input$bttn_update_filters_numeric, {
        purrr::iwalk(
          numeric_filters_list,
          function(var, nm) {
            shiny::updateNumericInput(
              session = session,
              inputId = paste("filter", var, "min", sep = "_"),
              value = round(min(as.numeric(grv$data()[[var]]), na.rm = TRUE), 1)
            )
            shiny::updateNumericInput(
              session = session,
              inputId = paste("filter", var, "max", sep = "_"),
              value = round(max(as.numeric(grv$data()[[var]]), na.rm = TRUE), 1)
            )
          }
        )
      })
      
      output$numeric <- shiny::renderPrint({
        purrr::map(
          numeric_filters_list,
          function(var) 
            c(
              input[[paste("filter", var, "min", sep = "_")]],
              input[[paste("filter", var, "max", sep = "_")]]
            )
        )
      })
      
    }
  )
}


##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------


