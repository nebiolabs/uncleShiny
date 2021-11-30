
## --------------------------------------------------------------------------
##  mod_conditionsViewer.R - full condition printing on plotly event      --
## --------------------------------------------------------------------------

## -------------------------------------------------------
##  UI COMPONENTS                                      --
## -------------------------------------------------------
conditionsViewerUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Full Conditions"),
    shiny::helpText("Activated on hover."),
    shiny::br(),
    shiny::br(),
    shiny::htmlOutput(ns("conditions_html"))
  )
}

## -------------------------------------------------------
##  SERVER FUNCTION                                    --
## -------------------------------------------------------
conditionsViewerServer <- function(id, grv, select_var, select_vals) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      munge_module_data <- function(data_input, select_var, select_vals) {
        if (is.null(select_vals)) {
          munged_data <- NULL
        } else {
          munged_data <- data_input |>
            dplyr::mutate(
              Buffer_condition_name = dplyr::if_else(
                stringr::str_detect(
                  Buffer_condition_name,
                  "(Neutral Buffer)|(NB)"
                ),
                "Neutral Buffer",
                Buffer_condition_name
              )
            ) |>
            dplyr::filter(
              .data[[select_var]] %in% select_vals()
            )
        }

        return(munged_data)
      }

      if (use_testing_mode) {
        module_data <- shiny::reactive({
          test_data |>
            munge_module_data(
              select_var = select_var,
              select_vals = select_vals()
            )
        })
      } else {
        module_data <- shiny::reactive({
          grv$robj_collected_data() |>
            munge_module_data(
              select_var = select_var,
              select_vals = select_vals()
            )
        })
      }

      output$conditions_html <- shiny::renderText({
        rlang::eval_tidy(
          long_tootip_glue_string,
          data = module_data()
        )
      })
    }
  )
}
