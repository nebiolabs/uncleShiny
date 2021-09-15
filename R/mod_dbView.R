
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
            width = 5,
            shiny::h4("Products available on server:"),
            DT::DTOutput(
              ns("table_products_available"),
              width = "100%",
              height = "375px"
            )
          ),
          shiny::column(
            width = 7,
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
        shiny::req(grv$robj_products())
        DT::datatable(
          data = grv$robj_products(),
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
            scrollCollapse = TRUE#,
            # t - table
            # fixedColumns = list(leftColumns = 5),
            # order = list(list(3, "asc")),
            # columnDefs = list(list(visible = FALSE, targets = c(1, 2)))
          )
        )
      })
      
      # Render experiment sets table for db inspection
      output$table_experiment_sets_available <- DT::renderDT({
        data_with_urls <- grv$robj_experiment_sets() |> 
          dplyr::mutate(benchling_url = glue::glue(
            "<a href='{benchling_url}' target='_blank'>click to open</a>"
          ))
        
        shiny::req(grv$robj_experiment_sets())
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
            scrollCollapse = TRUE#,
            # t - table
            # fixedColumns = list(leftColumns = 5),
            # order = list(list(3, "asc")),
            # columnDefs = list(list(visible = FALSE, targets = c(1, 2))),
          ),
          escape = FALSE
        )
      })
      
      # Render experiments table for db inspection
      output$table_experiments_available <- DT::renderDT({
        shiny::req(grv$robj_experiments())
        DT::datatable(
          data = grv$robj_experiments(),
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
            scrollCollapse = TRUE#,
            # t - table
            # fixedColumns = list(leftColumns = 5),
            # order = list(list(3, "asc")),
            # columnDefs = list(list(visible = FALSE, targets = c(1, 2)))
          )
        )
      })
      
      # On data collection, switch to the collection tab
      shiny::observeEvent(grv$state_bttn_collect, {
        shiny::updateTabsetPanel(
          inputId = "db_tbls",
          selected = "collection"
        )
      })
      
      # Render summary data table for collected selection
      output$table_collected_data <- DT::renderDT({
        shiny::req(grv$robj_collected_data())
        DT::datatable(
          data = dplyr::select(
            grv$robj_collected_data(),
            -tidyselect::contains("residuals"), -tidyselect::contains("spec")
          ),
          selection = "none",
          # extensions = c("FixedColumns"),
          options = list(
            dom = "ift",
            # i - information
            # f - filter
            searchHighlight = TRUE,
            # p - pagination
            scrollX = TRUE,
            scrollY = "800px",
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
