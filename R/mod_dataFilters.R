
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
      ns("bttn_apply_filters"),
      label = "Apply Filters",
      icon = shiny::icon("check"),
      width = "45%"
    ),
    shiny::actionButton(
      ns("bttn_reset_filters"),
      label = "Remove Filters",
      icon = shiny::icon("minus-circle"),
      width = "45%"
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
        title = "Conditions",
        value = "filters_conditions",
        icon = shiny::icon("flask"),
        shiny::h4("Condition Filters"),
        shiny::actionButton(
          ns("bttn_update_filters_conditions"),
          label = "Refresh Options",
          icon = shiny::icon("sync-alt"),
          width = "100%"
        ),
        shiny::br(),
        shiny::br(),
        shiny::helpText("Anything selected will be included"),
        shiny::br(),
        shiny::br(),
        shiny::uiOutput(ns("condition_filters_UI"))
      ),
      ##-----------------------------------------
      ##  Numeric Filters                      --
      ##-----------------------------------------
      shiny::tabPanel(
        title = "Numeric",
        value = "filters_numeric",
        icon = shiny::icon("columns"),
        shiny::h4("Numeric Filters"),
        shiny::actionButton(
          ns("bttn_update_filters_numeric"),
          label = "Refresh Options",
          icon = shiny::icon("sync-alt"),
          width = "100%"
        ),
        shiny::br(),
        shiny::br(),
        shiny::helpText("Only values within the selected range are included."),
        shiny::br(),
        shiny::br(),
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
      counter <- shiny::reactiveVal(value = 0)
      counter_conditions <- shiny::reactiveVal(value = 0)
      counter_numeric <- shiny::reactiveVal(value = 0)
      
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
      # Update filter counters
      shiny::observeEvent(input$bttn_apply_filters, {
        counter(counter() + 1)
      })
      
      shiny::observeEvent(input$bttn_update_filters_conditions, {
        counter_conditions(counter_conditions() + 1)
      })

      shiny::observeEvent(input$bttn_update_filters_numeric, {
        counter_numeric(counter_numeric() + 1)
      })
      
      # Apply filters if they have been updated to match current data
      shiny::observeEvent(input$bttn_apply_filters,
        {
          if (counter_conditions() == 0) {
            filtered_conditions <- shiny::isolate(grv$data())
          } else {
            filtered_conditions <- purrr::map(
              condition_filters_list,
              function(var) {
                # cat(input[[paste("filter", var, sep = "_")]])
                dplyr::filter(
                  shiny::isolate(grv$data()),
                  is.na(.data[[var]]) |
                    .data[[var]] %in%
                    shiny::isolate(input[[paste("filter", var, sep = "_")]])
                )
              }
            ) |>
              purrr::reduce(.f = dplyr::inner_join)
          }
          
          if (counter_numeric() == 0) {
            numeric_conditions <- shiny::isolate(grv$data())
          } else {
            numeric_conditions <- purrr::map(
              numeric_filters_list,
              function(var) {
                cat(input[[paste("filter", var, sep = "_")]])
                dplyr::filter(
                  shiny::isolate(grv$data()),
                  dplyr::between(
                    .data[[var]],
                    shiny::isolate(input[[paste("filter", var, "min", sep = "_")]]),
                    shiny::isolate(input[[paste("filter", var, "max", sep = "_")]])
                  )
                )
              }
            ) |>
              purrr::reduce(.f = dplyr::inner_join)
          }
          
          grv$data_filtered <- shiny::eventReactive(input$bttn_apply_filters, {
            dplyr::inner_join(filtered_conditions, numeric_conditions)
          })
        }
      )
      
      ##-----------------------
      ##  Reset filters      --
      ##-----------------------
      shiny::observeEvent(grv$data(), {
        counter(0)
      }, ignoreInit = TRUE)
      
      shiny::observeEvent(input$bttn_reset_filters, {
        counter(0)
      })
      
      
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
      
      ##-----------------------
      ##  Numeric            --
      ##-----------------------
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
      
    }
  )
}


##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------


