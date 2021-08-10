library(shiny)
library(shinyWidgets)
library(tidyverse)

source("R/vars.R")
source("R/funs.R")

# UI
plotOptsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    ##================================================================
    ##                        Global options                        ==
    ##================================================================
    br(),
    br(),
    br(),
    br(),
    h5("Global Settings"),
    # the dropdown buttons were breaking the UI
    # so I switched to this more basic dropdown
    dropdown(
      selectInput(
        ns("color"),
        "Color Points By:",
        choices = colorvarChoices,
        selected = "buffer"
      ),
      selectInput(
        ns("palette"),
        HTML("<a target='_blank' href='https://colorbrewer2.org'>ColorBrewer</a> Palette:"),
        choices = palChoices,
        selected = "Spectral"
      ),
      # actionButton(
      #   inputId = ns("updateGlobal"),
      #   label = "Update",
      #   icon = icon("sync-alt")
      # ),
      right = TRUE,
      icon = icon("sliders"),
      size = "sm",
      width = "250px"
    ),
    br(),
    ##================================================================
    ##                        Plot 1 options                        ==
    ##================================================================
    h5("Plot1 Settings"),
    dropdown(
      selectInput(
        ns("yvar1"),
        "Y Variable:",
        choices = yvarChoices,
        selected = "PdI"
      ),
      selectInput(
        ns("xvar1"),
        "X Variable:",
        choices = xvarChoices,
        selected = "Z_D"
      ),
      # actionButton(
      #   inputId = ns("updatePlot1"),
      #   label = "Update",
      #   icon = icon("sync-alt")
      # ),
      right = TRUE,
      icon = icon("sliders"),
      size = "sm",
      width = "250px"
    ),
    br(),
    ##================================================================
    ##                        Plot 2 options                        ==
    ##================================================================
    h5("Plot2 Settings"),
    dropdown(
      selectInput(
        ns("yvar2"),
        "Y Variable:",
        choices = yvarChoices,
        selected = "Tagg266"
      ),
      selectInput(
        ns("xvar2"),
        "X Variable:",
        choices = xvarChoices,
        selected = "Tm1"
      ),
      # actionButton(
      #   inputId = ns("updatePlot2"),
      #   label = "Update",
      #   icon = icon("sync-alt")
      # ),
      right = TRUE,
      icon = icon("sliders"),
      size = "sm",
      width = "250px"
    ),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    ##===============================================================
    ##                     Zoomed-Plot options                     ==
    ##===============================================================
    h5("Zoom Settings"),
    dropdown(
      selectInput(
        ns("zoomycolor"),
        "Color Points By:",
        choices = colorvarChoices,
        selected = "buffer"
      ),
      selectInput(
        ns("yvar3"),
        "Y Variable:",
        choices = yvarChoices,
        selected = "Tagg266"
      ),
      selectInput(
        ns("xvar3"),
        "X Variable:",
        choices = xvarChoices,
        selected = "Z_D"
      ),
      # actionButton(
      #   inputId = ns("updatePlot2"),
      #   label = "Update",
      #   icon = icon("sync-alt")
      # ),
      up = TRUE,
      right = TRUE,
      icon = icon("sliders"),
      size = "sm",
      width = "250px"
    )
  )
}



# SERVER
plotOptsServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # observe({
      #   # xvar choices
      #   updateSelectInput(
      #     inputId = "xvar1",
      #     choices = x_choices
      #   )
      #   updateSelectInput(
      #     inputId = "xvar2",
      #     choices = x_choices
      #   )
      #   updateSelectInput(
      #     inputId = "xvar3",
      #     choices = x_choices
      #   )
      #   # yvar choices
      #   updateSelectInput(
      #     inputId = "yvar1",
      #     choices = y_choices
      #   )
      #   updateSelectInput(
      #     inputId = "yvar2",
      #     choices = y_choices
      #   )
      #   updateSelectInput(
      #     inputId = "yvar3",
      #     choices = y_choices
      #   )
      # })
      
      # # dynamically updated filters for plots
      # observeEvent(dataList(), {
      #   req(dataList())
      #   # update filter options
      #   updateSelectInput(
      #     session, "bufferFilter",
      #     choices = purrr::map(dataList(), ~ dplyr::pull(dplyr::distinct(.x, buffer))) %>% purrr::flatten_chr()
      #   )
      #   updateSelectInput(
      #     session, "saltFilter",
      #     choices = purrr::map(dataList(), ~ dplyr::pull(dplyr::distinct(.x, salt))) %>% purrr::flatten_chr()
      #   )
      # })
      # 
      # output$dataName <- renderText({
      #   stringr::str_extract(dataName, "(?<=./data/).*(?=_data.rds)")
      # })
      
      return(
        list(
          color = reactive({
            input$color
          }),
          palette = reactive({
            input$palette
          }),
          updateGlobal = reactive({
            input$updateGlobal
          }),
          xvar1 = reactive({
            input$xvar1
          }),
          yvar1 = reactive({
            input$yvar1
          }),
          xvar2 = reactive({
            input$xvar2
          }),
          yvar2 = reactive({
            input$yvar2
          }),
          zoomycolor = reactive({
            input$zoomycolor
          }),
          xvar3 = reactive({
            input$xvar3
          }),
          yvar3 = reactive({
            input$yvar3
          })
        )
      )
    }
  )
}