
##-------------------------------------------------------------------------
##  mod_dbQuery.R - db query module                                      --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
dbQueryUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(style = "display: inline-block", shiny::icon("database")),
    shiny::div(style = "display: inline-block", shiny::h4("Database")),
    shiny::br(),
    shiny::actionButton(
      ns("bttn_refresh"),
      "Connect/Refresh",
      icon = shiny::icon("sync-alt")
    ),
    shiny::br(),
    shiny::helpText("Click to establish/refresh
                    the connection to ebase and explore
                    datasets available for analysis."),
    shiny::br(),
    shiny::br(),
    shiny::selectInput(
      ns("product_selection"),
      "Select product:",
      choices = NULL
    ),
    # shiny::br(),
    shiny::helpText("Products with correspoding Uncle datasets
                    are listed in the dropdown menu above. Select one to view
                    experiments associated with that product."),
    # shiny::textOutput(ns("raw_product_selection")),
    shiny::br(),
    shiny::br(),
    shiny::selectInput(
      ns("experiment_set_selection"),
      "Select experiment(s)",
      choices = NULL,
      multiple = TRUE
    ),
    # shiny::br(),
    shiny::helpText("Once a product is selected, associated Uncle experiments
                    will be listed in the dropdown above. Select one (or
                    multiple) experiment(s) for visualization and analysis."),
    # shiny::textOutput(ns("raw_experiment_set_selection")),
    shiny::br(),
    shiny::br(),
    shiny::actionButton(
      ns("bttn_collect"),
      "Collect Data",
      icon = shiny::icon("sync-alt")
    ),
    shiny::br(),
    shiny::helpText("Once a selection of experiments has been made,
                    click to request/load the data from ebase.")#,
    # shiny::textOutput(ns("raw_bttn_collect"))
  )
}

##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
dbQueryServer <- function(id, grv, dbobj) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Available prods                     <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
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
      # output$raw_product_selection <- shiny::renderPrint({
      #   print(input$product_selection)
      # })
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Available exp_sets                  <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # Reactive object of available experiment sets for user-selected product
      grv$robj_experiment_sets <- shiny::eventReactive(
        input$product_selection, {
          shiny::req(grv$robj_products())
          getQuery(
            dbobj,
            sql_queries$experiment_sets,
            input = input$product_selection
          )
        }
      )
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Available exps                      <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # Reactive object of available experiments within the sets above
      grv$robj_experiments <- shiny::eventReactive(grv$robj_experiment_sets(), {
        shiny::req(grv$robj_products(), grv$robj_experiment_sets())
        sets <- grv$robj_experiment_sets() |> 
          dplyr::pull(exp_set_id) |> 
          unique()
        getQuery(dbobj, sql_queries$experiments, input = sets)
      })
      
      # Update choices for experiment set selection
      shiny::observeEvent(grv$robj_experiment_sets(), {
        shiny::req(grv$robj_experiment_sets())
        
        updated_choices <- grv$robj_experiment_sets() |>
          tidyr::unite(
            col = "experiment",
            plate, exp_set_id, well_set_id,
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
      # output$raw_experiment_set_selection <- shiny::renderPrint({
      #   print(input$experiment_set_selection)
      # })
      
      ##-------------------------------------------------------
      ##  Data collection                                    --
      ##-------------------------------------------------------
      # Reactive object for bttn_collect value
      # grv$state_bttn_collect <- shiny::eventReactive(input$bttn_collect, {
      #   input$bttn_collect[1]
      # })
      shiny::observeEvent(input$bttn_collect, {
        grv$state_bttn_collect <- input$bttn_collect[1]
      })
      
      # Raw output of bttn_collect state for debugging
      # output$raw_bttn_collect <- shiny::renderPrint({
      #   print(grv$state_bttn_collect)
      # })
      
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Collected data                      <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # Reactive object of data collected from server matching selection
      grv$robj_collected_data <- shiny::eventReactive(input$bttn_collect, {
        summary_data <- getQuery(
          dbobj,
          sql_queries$summary_data,
          input = input$experiment_set_selection
        ) |> 
          # dplyr::mutate(sharedKey = id) |> 
          dplyr::rename(
            dls_temperature = temperature,
            Tagg266 = t_agg_266,
            Tagg473 = t_agg_473,
            Tm1 = t_m_1,
            Z_D = z_avg_diameter,
            peak1_D = pk_1_mode_diameter,
            PdI = pdi
          ) |> 
          dplyr::mutate(residuals = purrr::map(residuals, parse_float8)) |>
          dplyr::rename(uncle_summary_id = id) |> 
          dplyr::select(-tidyselect::any_of(c(
            "experiment_condition_id",
            "condition_id",
            "unit_id"
          )))
        
        # uncle_summary_id keys for spectra table query
        summary_ids <- summary_data |> dplyr::pull(uncle_summary_id)
        
        # vector of distinct groups for tooltip variable check
        condition_groups <- dplyr::pull(
          getQuery(dbobj, sql_queries$condition_groups),
          name
        )
        
        # Base table for condition and unit join manipulations; join on well_id
        conditions_units <- getQuery(
          dbobj,
          sql_queries$conditions_units,
          input = input$experiment_set_selection
        ) |> 
          pivot_conditions()
        
        # Join summary data with conditions and units
        summary_cond_unit_join <- purrr::reduce(
          list(summary_data, conditions_units),
          dplyr::left_join,
          by = c("well_id")
        )
        
        # Retrieve nested spectra tables for selected summary data
        spec_tbls <- get_spec_tbls(dbobj, spec_tbl_list, summary_ids)
        
        # Join nested spectra tables to summary data and return
        return(
          nest_spectra(summary_cond_unit_join, spec_tbls) |> 
            add_missing_tooltip_vars(condition_groups) |> 
            normalizeSpectra()
        )
      })
      
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  SharedData obj                      <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # Reactive object of collected data `crosstalk` SharedData instance
      grv$robj_collected_SharedData <- shiny::eventReactive(
        input$bttn_collect, {
          shiny::req(grv$robj_collected_data())
          crosstalk::SharedData$new(
            grv$robj_collected_data,
            key = ~bit64::as.character.integer64(uncle_summary_id)
          )
        }
      )
    }
  )
}
