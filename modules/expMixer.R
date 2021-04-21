library(shiny)
library(tidyverse)
library(glue)

source("R/vars.R")
source("R/funs.R")
source("R/plotly.R")

# UI
expMixerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 11,
        offset = 1,
        h4("Experiment Comparison:"),
        helpText("This is for joining two sets of experimental data",
                 "for the same protein and comparatively plotting both."),
        fluidRow(
          column(
            width = 5,
            h5("Select First Experiment:"),
            selectInput(
              ns("expSelection1"),
              NULL,
              choices = NULL
            ),
            verbatimTextOutput(ns("data1"))
          ),
          column(
            width = 5,
            h5("Select Second Experiment:"),
            selectInput(
              ns("expSelection2"),
              NULL,
              choices = NULL
            ),
            verbatimTextOutput(ns("data2"))
          )
        )
      )
    ) # End of experiment selection segment
  ) # taglist
  
  
}



# SERVER
expMixer <- function(input, output, session, dataList) {
  observeEvent(dataList(), {
    updateSelectInput(
      session,
      "expSelection1",
      choices = names(dataList()),
      selected = names(dataList())[1]
    )
    updateSelectInput(
      session,
      "expSelection2",
      choices = names(dataList()),
      selected = names(dataList())[2]
    )
  })
  
  output$data1 <- renderPrint({
    dataList() %>% pluck(input$expSelection1)
  })
  output$data2 <- renderPrint({
    dataList() %>% pluck(input$expSelection2)
  })
  
}