
##-------------------------------------------------------------------------------
##  Spectra Viewer Module                                                      --
##-------------------------------------------------------------------------------


library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(ggridges)
library(rlang)

source("R/vars.R")
source("R/funs.R")

spectraTheme <- function() {
  list(
    theme(
      axis.line = element_line(),
      axis.title = element_text(),
      axis.text.y = element_text(face = "bold"),
      axis.ticks.y = element_line(),
      legend.justification = "top"
    )
  )
}



# UI
spectraViewerUI <- function(id) {
  ns <- NS(id)
  optcol1 <- 4
  optcol2 <- (12-optcol1)
  optcoladj <- 4
  
  tagList(
    tabsetPanel(
      type = "pills",
      tabPanel(
        "DLS",
        value = "DLSpanel",
        fluidRow(
          plotOutput(ns("plotDLSspec"), height = "350px")
        )
      ),
      tabPanel(
        "SLS",
        value = "SLSpanel",
        fluidRow(
          plotOutput(ns("plotSLSspec"), height = "350px")
        )
      ),
      tabPanel(
        "NanoDSF",
        value = "nanoDSFpanel",
        fluidRow(
          plotOutput(ns("plotNanoDSF"), height = "350px")
        )
      ),
      tabPanel(
        "Data",
        value = "specData",
        fluidRow(
          DTOutput(ns("selectRef"), width = "100%")
        )
      ),
      tabPanel(
        "Options",
        value = "specOpts",
        br(),
        fluidRow(
          column(
            width = optcol1,
            h5("DLS Type:")
          ),
          column(
            width = optcol2,
            radioGroupButtons(
              inputId = ns("DLStype"),
              label = NULL,
              choices = c(
                "Intensity" = "specDLS_I",
                "Mass" = "specDLS_M"
              ),
              justified = FALSE
            )
          )
        ),
        fluidRow(
          column(
            width = optcol1,
            h5("SLS Type:")
          ),
          column(
            width = optcol2,
            radioGroupButtons(
              inputId = ns("SLStype"),
              label = NULL,
              choices = c(
                "266nm" = "specSLS266",
                "473nm" = "specSLS473"
              ),
              justified = FALSE
            )
          )
        ),
        fluidRow(
          column(
            width = optcol1,
            h5("Color Traces By:")
          ),
          column(
            width = optcol2 - optcoladj,
            pickerInput(
              inputId = ns("fillColor"),
              label = NULL,
              choices = c("Well" = "well", colorvarChoices),
              selected = "well"
            )
          ),
          column(
            width = optcoladj,
            materialSwitch(
              inputId = ns("showLegend"),
              label = "Legend?",
              right = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = optcol1,
            h5("Add Label:")
          ),
          column(
            width = optcol2 - optcoladj,
            pickerInput(
              inputId = ns("label"),
              label = NULL,
              choices = c(colorvarChoices),
              selected = "buffer"
            )
          )
        ),
        fluidRow(
          column(
            width = optcol1,
            h5("Ridgeline Scale:")
          ),
          column(
            width = optcol2,
            sliderInput(
              ns("ridgeScale"),
              label = NULL,
              min = 0.5,
              max = 3,
              value = 1.2,
              step = 0.1
            )
          )
        ),
        fluidRow(
          column(
            width = optcol1,
            h5("Well Exclusion:")
          ),
          column(
            width = optcol2,
            selectInput(
              ns("wellExclusion"),
              label = NULL,
              choices = c(" ", wellOrder),
              selected = NULL,
              multiple = TRUE,
              selectize = TRUE,
              width = NULL,
              size = NULL
            )
          )
        )
      )
    )
  )
}



# SERVER
spectraViewerServer <- function(id, data, pal) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      selectedData <- reactive({
        data
      })
      
      observeEvent(selectedData(), {
        updateSelectInput(
          session = session,
          inputId = "wellExclusion",
          choices = c(" ", wellOrder[wellOrder %in% unique(data[["well"]])]),
          selected = c(" ")
        )
      }, priority = 200)
      
      # I made the observe above higher priority and the separate selected data reactive
      # in order to avoid the updating of the select input trigger a plot redraw due to
      # invalidating the plot data itself..
      
      plotData <- debounce(reactive({
        req(selectedData)
        selectedData() %>% 
          filter(!(well %in% input$wellExclusion))
      }), 200)
      
      
      # SELECTION PRINTS
      output$selectRef <- DT::renderDT({
        req(selectedData())
        datatable(
          selectedData() %>%
            select(sharedKey, well, buffer:date) %>%
            select(-date),
          selection = "none",
          rownames = FALSE,
          extensions = c("FixedColumns"),
          options = list(
            dom = "fti",
            # f - filter
            searchHighlight = TRUE,
            # p - pagination
            scrollX = TRUE,
            scrollY = "250px",
            paging = FALSE,
            # pageLength = 16,
            scrollCollapse = TRUE,
            # t - table
            fixedColumns = list(leftColumns = 3),
            order = list(list(2, "desc")),
            # columnDefs = list(list(visible = FALSE, targets = c(1)))
            columns = list(list(visible = FALSE))
          )
        )
      })
      
      specColors <- reactive({
        source("R/funs.R", local = TRUE)
        if (is.na(pal) | pal == "Default") {
          pal <- "Spectral"
        }
        mycolors(pal, length(unique(plotData()[[input$fillColor]])))
      })
      
      # fillColor <- reactive({
      #   rlang::sym(input$fillColor)
      # })
      
      # PLOTS
      output$plotDLSspec <- renderPlot({
        opts <- c("specDLS_M" = "Mass", "specDLS_I" = "Intensity")
        type <- opts[input$DLStype]
        req(plotData(), cancelOutput = TRUE)
        plotData() %>%
          unnest(input$DLStype) %>%
          ggplot(aes(x = hydroDia_x)) +
          geom_vline(xintercept = 5, linetype = "dashed", alpha = 0.5) +
          geom_vline(xintercept = 20, linetype = "dashed", alpha = 0.5) +
          ggridges::geom_density_ridges(
            # aes_q(y = ~well, height = ~amp_y, group = ~sharedKey, fill = as.symbol(input$fillColor)),
            aes(y = well, height = amp_y, group = sharedKey, fill = !!sym(input$fillColor)),
            stat = "identity",
            scale = input$ridgeScale,
            show.legend = input$showLegend,
            alpha = 0.4
          ) +
          geom_text(
            data = plotData() %>% select(well, buffer:albumin_mgml),
            aes(
              label = !!sym(input$label),
              x = Inf,
              y = well
            ),
            vjust = 0,
            hjust = 1,
            family = "Roboto Condensed"
          ) +
          scale_fill_manual(values = specColors()) +
          scale_x_log10(limits = c(1, 100000), breaks = c(1, 5, 10, 100, 1000), labels = scales::label_comma(accuracy = 1)) +
          annotation_logticks(sides = "b") +
          spectraTheme() + 
          labs(
            title = glue("{type} DLS Spectra"),
            x = "Hydrodynamic Diameter (nm)",
            y = "Amplitude"
          )
      })
      
      output$plotSLSspec <- renderPlot({
        opts <- c("specSLS266" = "266nm", "specSLS473" = "473nm")
        lambda <- opts[input$SLStype]
        req(plotData())
        plotData() %>%
          unnest(input$SLStype) %>%
          ggplot(aes(x = temp_x)) +
          ggridges::geom_density_ridges(
            aes(y = well, height = intensity_y, group = sharedKey, fill = !!sym(input$fillColor)),
            stat = "identity",
            scale = input$ridgeScale,
            show.legend = input$showLegend,
            alpha = 0.5
          ) +
          geom_text(
            data = plotData() %>% select(well, buffer:albumin_mgml),
            aes(
              label = !!sym(input$label),
              x = Inf,
              y = well
            ),
            vjust = 0,
            hjust = 1,
            family = "Roboto Condensed"
          ) +
          scale_fill_manual(values = specColors()) +
          spectraTheme() + 
          labs(
            title = glue("SLS Spectra @ {lambda}"),
            x = "Temp (°C)",
            y = "Intensity"
          )
      })
      
      output$plotNanoDSF <- renderPlot({
        req(plotData())
        plotData() %>%
          unnest(specTm) %>%
          # group_by(well) %>%
          mutate(BCM_y = (BCM_y - min(BCM_y)) / (max(BCM_y) - min(BCM_y))) %>%
          ggplot(aes(x = temp_x)) +
          ggridges::geom_density_ridges(
            aes(y = well, height = BCM_y, group = sharedKey, fill = !!sym(input$fillColor)),
            stat = "identity",
            scale = input$ridgeScale,
            show.legend = input$showLegend,
            alpha = 0.5,
            position = position_nudge(y = -0.5)
          ) +
          geom_text(
            data = plotData() %>% select(well, buffer:albumin_mgml),
            aes(
              label = !!sym(input$label),
              x = Inf,
              y = well
            ),
            vjust = 0,
            hjust = 1,
            family = "Roboto Condensed"
          ) +
          scale_fill_manual(values = specColors()) +
          spectraTheme() + 
          labs(
            title = "Tm Fluorescence Spectra",
            x = "Temp (°C)",
            y = "First Derivative of BCM"
          )
      })
    }
  )
}
