
## --------------------------------------------------------------------------
##  mod_wordcloud.R - condition wordcloud for plot selection              --
## --------------------------------------------------------------------------

## -------------------------------------------------------
##  UI COMPONENTS                                      --
## -------------------------------------------------------
wordcloudUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Selection Wordcloud"),
    shiny::helpText("Activated on selection."),
    shiny::numericInput(
      ns("min_freq"),
      "Min. freq. of condition occurance:",
      value = 5,
      min = 1,
      step = 1
    ),
    shiny::plotOutput(
      ns("wordcloud"),
      height = "300px"
    )
  )
}

## -------------------------------------------------------
##  SERVER FUNCTION                                    --
## -------------------------------------------------------
wordcloudServer <- function(id, robj_data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ## ----------------------------------------
      ##  Data instance for module            --
      ## ----------------------------------------
      output$wordcloud <- shiny::renderPlot({
        robj_data() |>
          make_wordcloud(min_freq = input$min_freq)
      })
    }
  )
}
