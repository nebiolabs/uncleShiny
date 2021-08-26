
##--------------------------------------------------------------------------
##  mod_dbDiag - diagnostic querying                                      --
##--------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
dbDiagUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      ##----------------------------------------
      ##  Side panel                          --
      ##----------------------------------------
      shiny::sidebarPanel(
        width = 2,
        shiny::actionButton(
          ns("bttn_check_db"),
          "Check/Refresh Connection",
          icon = shiny::icon("sync-alt")
        ),
        shiny::br(),
        shiny::br(),
        shiny::div(
          style = "display: inline-block",
          "Connection is valid?"
        ),
        shiny::div(
          style = "display: inline-block",
          shiny::textOutput(
            ns("raw_db_is_valid")
          )
        ),
        shiny::br(),
        shiny::br(),
        shiny::textOutput(
          ns("dbobj_print")
        ),
        shiny::br(),
        shiny::br(),
        shiny::selectInput(
          ns("palette1"),
          "Palette 1:",
          choices = palChoices,
          selected = "Spectral"
        ),
        shiny::br(),
        shiny::selectInput(
          ns("palette2"),
          "Palette 2:",
          choices = palChoices,
          selected = "Set2"
        )
      ),
      ##----------------------------------------
      ##  Main panel                          --
      ##----------------------------------------
      shiny::mainPanel(
        width = 10,
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::h4("Products"),
            shiny::actionButton(
              ns("bttn_get_products"),
              "Retrieve Products"
            )
          ),
          shiny::column(
            width = 4,
            shiny::h4("Experiment Sets"),
            shiny::actionButton(
              ns("bttn_get_exp_sets"),
              "Retrieve All Sets"
            )
          ),
          shiny::column(
            width = 5,
            shiny::h4("Experiments"),
            shiny::actionButton(
              ns("bttn_get_exps"),
              "Retrieve All Exps"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 3,
            DT::DTOutput(ns("products"))
          ),
          shiny::column(
            width = 4,
            DT::DTOutput(ns("exp_sets"))
          ),
          shiny::column(
            width = 5,
            DT::DTOutput(ns("exps"))
          )
        )
      )
    )
  )
}

##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
dbDiagServer <- function(id, dbobj) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ##-----------------------------------------
      ##  Check db connection                  --
      ##-----------------------------------------
      db_is_valid <- reactiveVal(value = FALSE)
      
      shiny::observeEvent(input$bttn_check_db, {
        if (exists(rlang::as_name(rlang::enquo(dbobj)))) {
          db_is_valid(DBI::dbIsValid(dbobj))
          output$raw_db_is_valid <- shiny::renderText({print(db_is_valid())})
          output$dbobj_print <- shiny::renderPrint({DBI::dbGetInfo(dbobj)})
        } else {
          output$raw_db_is_valid <- shiny::renderText({
            "NULL"
          })
        }
      })
      
      
      ##-----------------------------------------
      ##  Products with exp_set n              --
      ##-----------------------------------------
      # Query
      diag_products <- shiny::eventReactive(input$bttn_get_products, {
        if (db_is_valid()) {
          DBI::dbGetQuery(
            dbobj,
            glue::glue_sql(
              "SELECT p.name, COUNT(exp_sets.id) AS n_exp_sets,
                p.id AS product_id
              FROM products p
              INNER JOIN uncle_experiment_sets exp_sets
                ON exp_sets.product_id = p.id
              GROUP BY p.id
              ORDER BY name",
              .con = dbobj  
            )
          )
        }
      })
      # Output
      output$products <- DT::renderDT({
        DT::datatable(
          data = diag_products(),
          selection = "none",
          rownames = FALSE,
          options = list(
            dom = "it",
            scrollX = FALSE,
            scrollY = "1000px",
            paging = FALSE,
            scrollCollapse = TRUE
          )
        ) |> 
          DT::formatStyle(
            "product_id",
            backgroundColor = DT::styleEqual(
              levels = sort(unique(diag_products()$product_id)),
              values = purrr::map_chr(
                mycolors(input$palette1, length(unique(diag_products()$product_id))),
                paste0,
                "90"
              )
            )
          )
      })
      
      
      ##----------------------------------------
      ##  Exp_sets with exps n                --
      ##----------------------------------------
      # Query
      diag_exp_sets <- shiny::eventReactive(input$bttn_get_exp_sets, {
        if (db_is_valid()) {
          DBI::dbGetQuery(
            dbobj,
            glue::glue_sql(
              "SELECT p.id AS product_id,
                CONCAT(exp_sets.exp_type, '_', exp_sets.plate_generation) AS plate, 
                COUNT(exps.id) AS n_exps, exp_sets.id AS exp_set_id
              FROM products AS p
              INNER JOIN uncle_experiment_sets AS exp_sets
                ON exp_sets.product_id = p.id
              INNER JOIN uncle_experiments AS exps
                ON exps.uncle_experiment_set_id = exp_sets.id
              GROUP BY p.id, exp_sets.id
              ORDER BY exp_set_id",
              .con = dbobj  
            )
          )
        }
      })
      # Output
      output$exp_sets <- DT::renderDT({
        DT::datatable(
          data = diag_exp_sets(),
          selection = "none",
          rownames = FALSE,
          options = list(
            dom = "it",
            scrollX = FALSE,
            scrollY = "1000px",
            paging = FALSE,
            scrollCollapse = TRUE
          )
        ) |> 
          DT::formatStyle(
            columns = "product_id",
            backgroundColor = DT::styleEqual(
              levels = sort(unique(diag_exp_sets()$product_id)),
              values = purrr::map_chr(
                mycolors(input$palette1, length(unique(diag_exp_sets()$product_id))),
                paste0,
                "90"
              )
            )
          ) |>  
          DT::formatStyle(
            columns = "exp_set_id",
            backgroundColor = DT::styleEqual(
              levels = sort(
                unique(
                  bit64::as.character.integer64(diag_exp_sets()$exp_set_id)
                )
              ),
              values = purrr::map_chr(
                mycolors(
                  input$palette2,
                  length(
                    unique(
                      bit64::as.character.integer64(diag_exp_sets()$exp_set_id)
                    )
                  )
                ),
                paste0,
                "90"
              )
            )
          )
      })
      
      
      ##-----------------------------------------
      ##  Exps with summary n                  --
      ##-----------------------------------------
      # Query
      diag_exps <- shiny::eventReactive(input$bttn_get_exps, {
        if (db_is_valid()) {
          DBI::dbGetQuery(
            dbobj,
            glue::glue_sql(
              "SELECT exp_sets.id AS exp_set_id,
                exps.plate_side AS side, exps.uncle_instrument_id AS inst_id,
              exps.date, COUNT(sums.id) AS n_sums, exps.id AS exp_id
              FROM uncle_experiment_sets AS exp_sets
              INNER JOIN uncle_experiments AS exps
                ON exps.uncle_experiment_set_id = exp_sets.id
              INNER JOIN uncle_summaries AS sums
                ON sums.uncle_experiment_id = exps.id
              GROUP BY exp_sets.id, exps.id
              ORDER BY exp_set_id",
              .con = dbobj  
            )
          )
        }
      })
      # Output
      output$exps <- DT::renderDT({
        DT::datatable(
          data = diag_exps(),
          selection = "none",
          rownames = FALSE,
          options = list(
            dom = "it",
            scrollX = FALSE,
            scrollY = "1000px",
            paging = FALSE,
            scrollCollapse = TRUE
          )
        ) |>  
          DT::formatStyle(
            columns = "exp_set_id",
            backgroundColor = DT::styleEqual(
              levels = sort(
                unique(
                  bit64::as.character.integer64(diag_exps()$exp_set_id)
                )
              ),
              values = purrr::map_chr(
                mycolors(
                  input$palette2,
                  length(
                    unique(
                      bit64::as.character.integer64(diag_exps()$exp_set_id)
                    )
                  )
                ),
                paste0,
                "90"
              )
            )
          ) |> 
          DT::formatStyle(
            columns = "n_sums",
            backgroundColor = DT::styleInterval(
              c(47,49), c("#67a9cf90", NA, "#ef8a6290")
            )
          )
      })
    }
  )
}