
##-------------------------------------------------------------------------
##  mod_dbView.R - db query viewer module                                --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
dbViewUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textOutput(ns("raw_bttn_collect")),
    shiny::tabsetPanel(
      id = ns("db_tbls"),
      type = "pills",
      shiny::tabPanel(
        title = "db Table Inspection",
        icon = shiny::icon("object-group"),
        value = "inspection",
        shiny::fluidRow(
          column(
            width = 4,
            shiny::h4("Products available on server:"),
            DT::DTOutput(
              ns("table_products_available"),
              width = "100%",
              height = "375px"
            )
          ),
          shiny::column(
            width = 8,
            shiny::h4("Experiments sets available for selected product:"),
            DT::DTOutput(
              ns("table_experiment_sets_available"),
              width = "100%",
              height = "375px"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h4("Experiments in experiment sets:"),
            DT::DTOutput(
              ns("table_experiments_available"),
              width = "100%",
              height = "275px"
            )
          )
        )
      ),
      shiny::tabPanel(
        title = "db Data Collection",
        icon = shiny::icon("cloud-download-alt"),
        value = "collection",
        DT::DTOutput(ns("table_collected_data"), width = "100%")
      )
    )
  )
}


##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
dbViewServer <- function(id, grv) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # Render products table for db inspection
      output$table_products_available <- DT::renderDT({
        shiny::req(grv$dbquery$products)
        DT::datatable(
          data = grv$dbquery$products,
          selection = "none",
          # extensions = c("FixedColumns"),
          options = list(
            dom = "itp",
            # i - information
            # f - filter
            # searchHighlight = TRUE,
            # p - pagination
            scrollX = FALSE,
            scrollY = "300px",
            paging = FALSE,
            pageLength = 10,
            scrollCollapse = TRUE,
            # t - table
            # fixedColumns = list(leftColumns = 5),
            # order = list(list(3, "asc")),
            columnDefs = list(list(visible = FALSE, targets = c(0)))
          )
        )
      })
      
      # Render experiment sets table for db inspection
      output$table_experiment_sets_available <- DT::renderDT({
        shiny::req(grv$dbquery$exp_sets)
        
        data_with_urls <- grv$dbquery$exp_sets |> 
          dplyr::mutate(benchling_url = glue::glue(
            "<a href='{benchling_url}' target='_blank'>click to open</a>"
          ))
        
        DT::datatable(
          data = data_with_urls,
          selection = "none",
          # extensions = c("FixedColumns"),
          options = list(
            dom = "itp",
            # i - information
            # f - filter
            # searchHighlight = TRUE,
            # p - pagination
            scrollX = TRUE,
            scrollY = "300px",
            paging = FALSE,
            pageLength = 20,
            scrollCollapse = TRUE,
            # t - table
            # fixedColumns = list(leftColumns = 5),
            # order = list(list(3, "asc")),
            columnDefs = list(list(visible = FALSE, targets = c(0)))
          ),
          escape = FALSE
        )
      })
      
      # Render experiments table for db inspection
      output$table_experiments_available <- DT::renderDT({
        shiny::req(grv$dbquery$exps)
        DT::datatable(
          data = grv$dbquery$exps,
          selection = "none",
          # extensions = c("FixedColumns"),
          options = list(
            dom = "itp",
            # i - information
            # f - filter
            # searchHighlight = TRUE,
            # p - pagination
            scrollX = TRUE,
            scrollY = "200px",
            paging = FALSE,
            pageLength = 80,
            scrollCollapse = TRUE,
            # t - table
            # fixedColumns = list(leftColumns = 5),
            # order = list(list(3, "asc")),
            columnDefs = list(list(visible = FALSE, targets = c(0)))
          )
        )
      })
      
      # On data collection, switch to the collection tab
      shiny::observeEvent(grv$dbquery$state_bttn_collect, {
        shiny::updateTabsetPanel(
          inputId = "db_tbls",
          selected = "collection"
        )
      })
      
      shiny::observe({
        if (use_testing_mode) {
          grv$dbquery$state_bttn_collect <- 
            grv$dbquery$state_bttn_collect + 1
        }
      })
      
      # Render summary data table for collected selection
      output$table_collected_data <- DT::renderDT({
        shiny::req(grv$data())
        DT::datatable(
          data = dplyr::select(
            grv$data(),
            !tidyselect::matches("^spec.*|^residuals$")
          ),
          selection = "none",
          # extensions = c("FixedColumns"),
          extensions = c("Buttons"),
          options = list(
            dom = "iBft",
            # i - information
            # B - buttons
            buttons = c("copy", "excel", "csv"),
            # f - filter
            searchHighlight = TRUE,
            # p - pagination
            scrollX = TRUE,
            scrollY = "600px",
            paging = FALSE,
            pageLength = 80,
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
