
##------------------------------------------------------------------------------
##  Printer Module                                                            --
##------------------------------------------------------------------------------


library(shiny)
library(tidyverse)
library(DT)

# UI
printerUI <- function(id) {
  ns <- NS(id)

  # column(
  # width = 12,
  uiOutput(ns("DTUI"))
  # )
}


# SERVER
printerServer <- function(id, dataList) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      req(dataList)
      # the calling of session$ns is very imnportant here for using `renderUI` within a module
      output$DTUI <- renderUI({
        ns <- session$ns
        tableList <- purrr::map(
          names(dataList),
          function(name) {
            dfname <- paste0(name, "df")
            tagList(
              hr(),
              h4(paste0(name)),
              DT::DTOutput(ns(paste0(dfname)), width = "100%"),
              # hr()
            )
          }
        )
        # adds some explanation text before the tables
        tableList <- purrr::prepend(
          tableList,
          c(
            quote(h5("The loaded dataset contains the following components:"))
          )
        )
        do.call(tagList, tableList)
      })
      
      observeEvent(dataList, {
        purrr::iwalk(
          dataList,
          function(df, name) {
            dfname <- paste0(name, "df")
            output[[dfname]] <- DT::renderDT(
              DT::datatable(
                df %>% select(!(contains("spec"))),
                extensions = "FixedColumns",
                options = list(
                  scrollX = TRUE,
                  scrollY = "300px",
                  paging = FALSE,
                  scrollCollapse = TRUE,
                  fixedColumns = list(leftColumns = 4),
                  order = list(list(1, "desc"))
                )
              )
            )
          }
        )
      })
    }
  )
}
