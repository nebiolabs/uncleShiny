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
    dropMenu(
      actionBttn(
        inputId = "openGlobal",
        label = NULL, 
        style = "bordered",
        color = "danger",
        size = "sm",
        icon = icon("sliders")
      ),
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
      padding = "10px",
      placement = "bottom",
      trigger = "click",
      theme = "translucent",
      hideOnClick = TRUE,
      maxWidth = "250px"
    ),
    br(),
    ##================================================================
    ##                        Plot 1 options                        ==
    ##================================================================
    h5("Plot1 Settings"),
    dropMenu(
      actionBttn(
        inputId = "openPlot1",
        label = NULL, 
        style = "bordered",
        color = "warning",
        size = "sm",
        icon = icon("sliders")
      ),
      selectInput(
        ns("xvar1"),
        "X Variable:",
        choices = xvarChoices,
        selected = "Z_D"
      ),
      selectInput(
        ns("yvar1"),
        "Y Variable:",
        choices = yvarChoices,
        selected = "PdI"
      ),
      # actionButton(
      #   inputId = ns("updatePlot1"),
      #   label = "Update",
      #   icon = icon("sync-alt")
      # ),
      padding = "10px",
      placement = "bottom",
      trigger = "click",
      theme = "translucent",
      hideOnClick = TRUE,
      maxWidth = "250px"
    ),
    br(),
    ##================================================================
    ##                        Plot 2 options                        ==
    ##================================================================
    h5("Plot2 Settings"),
    dropMenu(
      actionBttn(
        inputId = "openPlot2",
        label = NULL, 
        style = "bordered",
        color = "warning",
        size = "sm",
        icon = icon("sliders")
      ),
      selectInput(
        ns("xvar2"),
        "X Variable:",
        choices = xvarChoices,
        selected = "Tm1"
      ),
      selectInput(
        ns("yvar2"),
        "Y Variable:",
        choices = yvarChoices,
        selected = "Tagg266"
      ),
      # actionButton(
      #   inputId = ns("updatePlot2"),
      #   label = "Update",
      #   icon = icon("sync-alt")
      # ),
      padding = "10px",
      placement = "bottom",
      trigger = "click",
      theme = "translucent",
      hideOnClick = TRUE,
      maxWidth = "250px"
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
    dropMenu(
      actionBttn(
        inputId = "openZoom",
        label = NULL, 
        style = "bordered",
        color = "warning",
        size = "sm",
        icon = icon("sliders")
      ),
      selectInput(
        ns("zoomycolor"),
        "Color Points By:",
        choices = colorvarChoices,
        selected = "buffer"
      ),
      selectInput(
        ns("xvar3"),
        "X Variable:",
        choices = xvarChoices,
        selected = "Z_D"
      ),
      selectInput(
        ns("yvar3"),
        "Y Variable:",
        choices = yvarChoices,
        selected = "Tagg266"
      ),
      # actionButton(
      #   inputId = ns("updatePlot2"),
      #   label = "Update",
      #   icon = icon("sync-alt")
      # ),
      padding = "10px",
      placement = "bottom",
      trigger = "click",
      theme = "translucent",
      hideOnClick = TRUE,
      maxWidth = "250px"
    )
  )
}



# SERVER
plotOpts <- function(input, output, session) {
  
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