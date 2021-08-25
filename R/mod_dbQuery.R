
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
    shiny::textOutput(ns("raw_product_selection")),
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
    shiny::textOutput(ns("raw_experiment_set_selection")),
    shiny::br(),
    shiny::helpText("Once a selection of experiments has been made,
                    click 'Collect Data' to gather the data from ebase."),
    shiny::br(),
    shiny::actionButton(
      ns("bttn_collect"),
      "Collect Data",
      icon = shiny::icon("sync-alt")
    ),
    shiny::textOutput(ns("raw_bttn_collect"))
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
      output$raw_product_selection <- shiny::renderPrint({
        print(input$product_selection)
      })
      
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
        print(input$experiment_set_selection)
      })
      
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
      output$raw_bttn_collect <- shiny::renderPrint({
        print(grv$state_bttn_collect)
      })
      
      
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
            Tagg266 = t_agg_266,
            Tagg473 = t_agg_473,
            Tm1 = t_m_1,
            Z_D = z_avg_diameter,
            peak1_D = pk_1_mode_diameter,
            PdI = pdi
          ) |> 
          dplyr::mutate(residuals = purrr::map(residuals, parse_float8)) |>
          dplyr::rename(uncle_summary_id = id) |> 
          tibble::as_tibble() |> 
          dplyr::select(-tidyselect::any_of(c(
            "experiment_condition_id",
            "condition_id",
            "unit_id"
          )))
        
        # uncle_summary_id keys for spectra table query
        summary_ids <- summary_data |> dplyr::pull(uncle_summary_id)
        
        # Recode values as temporary fix until condition_type added to ebase
        cond_recode_vals <- readr::read_csv("data/condition_types.csv") |> 
          dplyr::select(name, type) |> 
          tibble::deframe()
        
        # Base table for condition and unit join manipulations; join on well_id
        conditions_units <- getQuery(
          dbobj,
          sql_queries$conditions_units,
          input = input$experiment_set_selection
        ) |> 
          dplyr::mutate(
            condition_type = purrr::map_chr(
              condition_name,
              \(nm) rlang::exec(dplyr::recode, nm, !!!cond_recode_vals)
            )
          )
        
        # Pivoted join table for conditions
        cond_join <- conditions_units |> 
          dplyr::select(-c(unit_value, unit_name)) |> 
          tidyr::pivot_wider(
            names_from = condition_type,
            values_from = condition_name
          )
        
        # Pivoted join table for units
        unit_join <- conditions_units |> 
          dplyr::select(-c(condition_name)) |> 
          tidyr::pivot_wider(
            names_from = c(condition_type, unit_name),
            values_from = unit_value,
            names_sep = "_"
          )
        
        # Join summary data with conditions and units
        summary_cond_unit_join <- purrr::reduce(
          list(summary_data, cond_join, unit_join),
          dplyr::left_join,
          on = c("well_id")
        )
        # return(summary_cond_unit_join)
        
        # Retrieve nested spectra tables for selected summary data
        spec_tbls <- get_spec_tbls(dbobj, spec_tbl_list, summary_ids)
        
        # Join nested spectra tables to summary data and return
        return(nest_spectra(summary_cond_unit_join, spec_tbls))
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
