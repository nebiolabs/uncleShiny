
##-------------------------------------------------------------------------
##  mod_dbQuery.R - db query module                                      --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI components                                      --
##-------------------------------------------------------

dbQueryUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(style = "display: inline-block", icon("database")),
    shiny::div(style = "display: inline-block", h4("Database")),
    shiny::helpText("Click refresh to re-establish connection to ebase
                   and display available protein datasets for analysis."),
    shiny::verbatimTextOutput(ns("raw_product_selection")),
    shiny::selectInput(
      ns("product_selection"),
      NULL,
      choices = NULL
    ),
    shiny::actionButton(
      ns("bttn_refresh"),
      "Connect/Refresh",
      icon = shiny::icon("sync-alt")
    ),
    shiny::br(),
    shiny::br(),
    shiny::br(),
    shiny::verbatimTextOutput(ns("raw_experiment_set_selection")),
    shiny::selectInput(
      ns("experiment_set_selection"),
      NULL,
      choices = NULL
    )
  )
}

##-------------------------------------------------------
##  Server function                                    --
##-------------------------------------------------------

dbQueryServer <- function(id, grv, dbobj) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # Reactive object of available products
      grv$robj_products <- shiny::eventReactive(input$bttn_refresh, {
        shiny::req(dbobj)
        getQuery(dbobj, sql_queries$products)
      })
      
      # Update choices for product selection
      shiny::observeEvent(grv$robj_products(), {
        shiny::req(grv$robj_products())
        
        updated_choices <- grv$robj_products() |>
          dplyr::select(product_name, product_id) |>
          tibble::deframe()
        
        shiny::updateSelectInput(
          session,
          "product_selection",
          choices = c(" " = 0, updated_choices),
          selected = 0
        )
      })
      
      # Raw output of product_id for debugging
      output$raw_product_selection <- shiny::renderPrint({
        input$product_selection
      })
      
    }
  )
}
