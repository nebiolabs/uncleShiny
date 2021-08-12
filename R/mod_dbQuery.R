
##-------------------------------------------------------------------------
##  mod_dbQuery.R - db query module                                      --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI components                                      --
##-------------------------------------------------------
dbQueryUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(style = "display: inline-block", shiny::icon("database")),
    shiny::div(style = "display: inline-block", shiny::h4("Database")),
    shiny::helpText("Click 'Connect/Refresh' to establish or refresh
                    the connection to ebase and explore available 
                    Uncle datasets for analysis."),
    shiny::br(),
    shiny::actionButton(
      ns("bttn_refresh"),
      "Connect/Refresh",
      icon = shiny::icon("sync-alt")
    ),
    shiny::br(),
    shiny::br(),
    shiny::helpText("Available products with correspoding Uncle datasets
                    are listed in the dropdown menu below. Select one to view
                    experiments associated with that product."),
    shiny::br(),
    shiny::selectInput(
      ns("product_selection"),
      "Select product:",
      choices = NULL
    ),
    shiny::verbatimTextOutput(ns("raw_product_selection")),
    shiny::br(),
    shiny::helpText("Once a product is selected, associated Uncle experiments
                    will be listed in the dropdown below. Select one, or
                    multiple experiments for visualization and analysis."),
    shiny::br(),
    shiny::selectInput(
      ns("experiment_set_selection"),
      "Select experiment(s)",
      choices = NULL,
      multiple = TRUE
    ),
    shiny::verbatimTextOutput(ns("raw_experiment_set_selection")),
    shiny::br(),
    shiny::helpText("Once a selection of experiments has been made,
                    click 'Collect Data' to gather the data from ebase."),
    shiny::br(),
    shiny::actionButton(
      ns("bttn_collect"),
      "Collect Data",
      icon = shiny::icon("sync-alt")
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
      ##----------------------------------------
      ##  Available products                  --
      ##----------------------------------------
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
      
      ##-----------------------------------------
      ##  Available experiments                --
      ##-----------------------------------------
      # Reactive object of available experiment sets for user-selected product
      grv$robj_experiment_sets <- shiny::eventReactive(input$product_selection, {
        shiny::req(grv$robj_products())
        getQuery(dbobj, sql_queries$experiment_sets, input$product_selection)
      })
      
      # Reactive object of available experiments within the sets above
      grv$robj_experiments <- shiny::eventReactive(grv$robj_experiment_sets(), {
        shiny::req(grv$robj_products(), grv$robj_experiment_sets())
        sets <- grv$robj_experiment_sets() |> 
          dplyr::pull(exp_set_id) |> 
          unique()
        getQuery(dbobj, sql_queries$experiments, sets)
      })
      
      # Update choices for experiment set selection
      shiny::observeEvent(grv$robj_experiment_sets(), {
        shiny::req(grv$robj_experiment_sets())
        
        updated_choices <- grv$robj_experiment_sets() |>
          tidyr::unite(
            col = "experiment",
            exp_type, gen, exp_set_id, well_set_id,
            sep = "_",
            remove = FALSE
          ) |> 
          dplyr::select(experiment, exp_set_id) |> 
          dplyr::mutate(across(c(exp_set_id), .fns = bit64::as.integer64)) |> 
          tibble::deframe()
        
        shiny::updateSelectInput(
          session,
          "experiment_set_selection",
          choices = bit64::c.integer64(" " = 0, updated_choices)
        )
      })
      
      # Raw output of experiment_set_id for debugging
      output$raw_experiment_set_selection <- shiny::renderPrint({
        input$experiment_set_selection
      })
      
      ##-------------------------------------------------------
      ##  Data collection                                    --
      ##-------------------------------------------------------
      grv$robj_collected_data <- shiny::eventReactive(input$bttn_collect, {
        summary_data <- getQuery(
          dbobj, sql_queries$summary_data, input$experiment_set_selection
        ) |> 
          # dplyr::mutate(sharedKey = id) |> 
          dplyr::rename(
            Tagg266 = t_agg_266,
            Tagg473 = t_agg_473,
            Tm1 = t_m_1,
            Z_D = z_avg_diameter,
            peak1_D = pk_1_mode_diameter,
            PdI = pdi
          ) |> 
          dplyr::mutate(residuals = purrr::map(residuals, parse_float8)) |>
          dplyr::rename(uncle_summary_id = id) |> 
          tibble::as_tibble()
        
        summary_ids <- summary_data |> dplyr::pull(uncle_summary_id)
        
        # spec_tbls <- get_spec_tbls(ebase_dev, spec_tbl_list, summary_ids)
        
        # return(nest_spectra(summary_data, spec_tbls))
        return(summary_data)
      })
    }
  )
}
