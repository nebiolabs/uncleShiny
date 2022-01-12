
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
    shiny::actionButton(
      ns("bttn_apply_filters"),
      "Apply Filters"
    ),
    shiny::actionButton(
      ns("bttn_reset_filters"),
      "Reset Filters"
    ),
    shiny::tabsetPanel(
      type = "pills",
      ##-----------------------------------------
      ##  Condition Filters                    --
      ##-----------------------------------------
      shiny::tabPanel(
        title = NULL,#"Conditions",
        value = "filters_conditions",
        icon = shiny::icon("flask")
      ),
      ##-----------------------------------------
      ##  Numeric Filters                      --
      ##-----------------------------------------
      shiny::tabPanel(
        title = NULL,#"Numeric",
        value = "filters_numeric",
        icon = shiny::icon("columns"),
        ##-----------------------
        ##  DLS                --
        ##-----------------------
        shiny::numericInput(
          ns("filter_Z_D"),
          "Avg. Z Dia.",
          value = 50,
          min = 0.01,
          max = 1000,
          step = 10
        )
      )
    )
  )
}

dataFiltersServer <- function(id, grv) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      counter <- shiny::reactiveVal(value = 0, label = "bttn_counter")
      
      shiny::observeEvent(input$bttn_apply_filters, {
        counter(counter() + 1)
      })
      
      shiny::observeEvent(input$bttn_reset_filters, {
        counter(0)
      })
      
      shiny::observe({
        if (counter() == 0) {
          grv$data_filtered <- grv$data
        } else {
          grv$data_filtered <- shiny::reactive({
            grv$data() |> 
              dplyr::filter(Z_D <= input$filter_Z_D)
          })
        }
      })
    }
  )
}


##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------


