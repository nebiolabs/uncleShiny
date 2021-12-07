
##--------------------------------------------------------------------------
##  mod_wordcloud.R - condition wordcloud for plot selection              --
##--------------------------------------------------------------------------

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
      value = 1,
      min = 1,
      step = 1
    ),
    wordcloud2::wordcloud2Output(
      ns("wordcloud"),
      height = "200px"
    )
  )
}

## -------------------------------------------------------
##  SERVER FUNCTION                                    --
## -------------------------------------------------------
wordcloudServer <- function(id, grv, select_var, select_vals) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      munge_module_data <- function(data_input, select_var, select_vals) {
        if (!is.null(select_vals())) {
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
      
      output$wordcloud <- wordcloud2::renderWordcloud2({
        module_data() |>
          make_wordcloud(min_freq = input$min_freq)
      })
    }
  )
}