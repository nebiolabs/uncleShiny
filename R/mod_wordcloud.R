
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
    shiny::fluidRow(
      shiny::splitLayout(
        shiny::numericInput(
          ns("min_freq"),
          "Min. occurance:",
          value = 5,
          min = 1,
          step = 1
        ),
        shiny::selectInput(
          ns("word_exclusion"),
          "Exclude conditions:",
          choices = NULL,
          multiple = TRUE,
          selectize = TRUE,
          width = "100%"
        )
      ) 
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
      
      ##----------------------------------------
      ##  Wordcloud data                      --
      ##----------------------------------------
      wordcloud_data <- shiny::reactive({
        make_wordcloud_data(robj_data())
      })
      
      wordcloud_data_filtered <- shiny::debounce(shiny::reactive({
        dplyr::filter(
          wordcloud_data(),
          freq > input$min_freq,
          freq < dplyr::if_else(is.null(robj_data()), 20L, nrow(robj_data())),
          !(word %in% input$word_exclusion)
        )
      }), millis = 200)
      
      
      ##----------------------------------------
      ##  Filter options                      --
      ##----------------------------------------
      shiny::observe({
        shiny::req(wordcloud_data())
        shiny::updateSelectInput(
          session = session,
          inputId = "word_exclusion",
          choices = unique(wordcloud_data()[["word"]]),
          selected = c("NaCl")
        )
      })
      
      
      ##-----------------------------------------
      ##  Wordcloud                            --
      ##-----------------------------------------
      output$wordcloud <- shiny::renderPlot({
        make_wordcloud(
          wordcloud_data_filtered(),
          max_area = 12,
          pal = "Set2"
        )
      })
    }
  )
}
