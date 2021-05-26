library(shiny)
library(shinyWidgets)
library(tidyverse)
library(extrafont)
library(DT)
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
activityAssayUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    br(),
    tabsetPanel(
      id = "activityPanel",
      type = "pills",
      tabPanel(
        "Plate Layout",
        value = "plateLayout",
        fluidRow(
          plotlyOutput(ns("plotPlateLayout"), height = "800px")
          # verbatimTextOutput(ns("testysquid"))
        )
      )
    )
  )
}



# SERVER
activityAssay <- function(input, output, session, data, pal) {
  
  activityData <- reactive({
    if ("assay_act" %in% names(data)) {
      data %>% 
        relocate(assay_act) %>% 
        unnest(assay_act)
    } else {
      return(NULL)
    }
  })
  
  specColors <- reactive({
    source("R/funs.R", local = TRUE)
    if (is.na(pal) | pal == "Default") {
      pal <- "Spectral"
    }
    mycolors(pal, length(unique(plotData()[[input$fillColor]])))
  })
  
  
  output$testysquid <- renderPrint({
    activityData() %>% 
      separate(well, into = c("letter", "number"), sep = 1, remove = FALSE) %>%
      # arrange(time_hr, match(well, metaList$Titration002[["well"]])) %>%
      modify_at(.at = vars(letter, number), parse_factor, ordered = TRUE)
  })
  
  # PLOTS
  output$plotPlateLayout <- renderPlotly({
    req(activityData(), cancelOutput = TRUE)
    
    plotData <- activityData() %>% 
      # group_by(well) %>%
      # filter(act < act[1]*1.4) %>%
      # filter(act < act[time_hr == 0] * 1.4) %>%
      # mutate(across(.cols = c(act), .fns = ~ .x / .x[[1]], .names = "{.col}_norm")) %>%
      # filter(act_norm < 1.5) %>%
      separate(well, into = c("letter", "number"), sep = 1, remove = FALSE) %>%
      # arrange(time_hr, match(well, metaList$Titration002[["well"]])) %>%
      modify_at(.at = vars(letter, number), parse_factor, ordered = TRUE)
      
    p <- ggplot(data = plotData, aes_string(x = colnames(plotData)[1], y = colnames(plotData)[2])) +
          # geom_hline(yintercept = 10000, linetype = "dotted", color = "green") +
          # geom_hline(yintercept = 5000, linetype = "dotted", color = "red") +
          geom_line(size = 1, alpha = 0.2) +
          geom_point(aes(
            text = glue::glue(
              "<b>Time:</b> {time_hr}hrs
            <b>Buffer:</b> {buffer} {buffer_mM}mM, pH {pH}; {salt_mM}mM {salt}
            <b>Additive 1:</b> {additive1} {additive1_conc}{additive1_unit}
            <b>Additive 2:</b> {additive2} {additive2_conc}{additive2_unit}
            {DTT_mM}mM <em>DTT</em>  /  {TCEP_mM}mM <em>TCEP</em>
            <em>{glycerol}% Glycerol</em>
            <b>Yield:</b> {round(plotData[[colnames(plotData)[2]]], 1)}ng
            <em>User Comment:</em> {comment}"
            ),
            color = plotData[[2]]
          )) +
        # geom_text(aes(
        #   label = glue::glue(
        #     "{additive1} {additive1_conc}{additive1_unit}
        #     {additive2} {additive2_conc}{additive2_unit}
        #     {DTT_mM}mM DTT
        #     {TCEP_mM}mM TCEP
        #     {glycerol}% Glycerol"
        #   ),
        #   x = 0,
        #   y = Inf
        # ), size = 2, family = "Roboto Condensed", hjust = 0, vjust = 1, nudge_y = 1) +
          facet_grid(
            rows = vars(letter),
            cols = vars(number),
          #   # labeller = labeller(
          #   #   time_hr = c(
          #   #     "0" = "control",
          #   #     "48" = "48 hrs",
          #   #     "96" = "96 hrs",
          #   #     "168" = "7 days",
          #   #     "336" = "14 days",
          #   #     "504" = "21 days",
          #   #     "672" = "28 days"
          #   #   )
          #   # ),
            switch = "y"
          ) +
          scale_color_viridis_c(option = "D") +
          # scale_fill_viridis_c(option = "D") +
          labs(x = NULL, y = NULL, fill = "Activity\nAvg.", size = "Activity\nStdev.", color = "Activity") +
          theme(
            legend.position = "right",
            legend.justification = "top",
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            strip.background = element_blank(),
            panel.border = element_rect(fill = NA)
            # panel.grid.major.y = element_line(linetype = "dotted")
          )
    
    # source("R/plotly.R", local = TRUE)
    
    ggplotly(p, tooltip = "text")
    # }
  })
}
