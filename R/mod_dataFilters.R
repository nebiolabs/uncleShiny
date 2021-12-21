
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
        icon = shiny::icon("columns")
      )
    )
  )
}

dataFiltersServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}


##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------