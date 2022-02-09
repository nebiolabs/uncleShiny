
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
    shiny::htmlOutput(ns("conditions_html"))
  )
}

## -------------------------------------------------------
##  SERVER FUNCTION                                    --
## -------------------------------------------------------
conditionsViewerServer <- function(id, robj_data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      output$conditions_html <- shiny::renderText({
        shiny::req(robj_data())
        rlang::eval_tidy(
          long_tooltip_glue_string,
          data = robj_data()
        )
      })
    }
  )
}
