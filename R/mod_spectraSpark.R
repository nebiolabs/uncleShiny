
##------------------------------------------------------------------------------
##  Spectra Sparkline Module                                                  --
##------------------------------------------------------------------------------


library(shiny)
library(tidyverse)
library(glue)


# source("R/vars.R")


# Theme for spectra sparklines.. 
sparklineTheme <- function() {
  list(
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(),
      axis.line.y = element_blank()
    )
  )
}










# UI
specSparkUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("sparky")),
    htmlOutput(ns("sparkyID"))
    # verbatimTextOutput(ns("sparkyPrint"))
  )
  
}










# SERVER
specSparkServer <- function(id, data, hover, click, pal) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      hoverData <- reactive({
        if (is.null(hover)) {
          return(NULL)
        } else {
          # return(data[data$sharedKey %in% hover, ][1, ])
          return(data %>% filter(sharedKey %in% hover))
        }
      })
      
      clickData <- reactive({
        if (is.null(click)) {
          return(NULL)
        } else {
          # return(data[data$sharedKey %in% click, ][1, ])
          return(data %>% filter(sharedKey %in% click))
        }
      })
      
      output$sparkyID <- renderText({
        # paste0(hover,"<br>",click)
        if (is.null(click)) {
          # glue("Showing spectra for:<br><em>Well</em> {hoverData()[['well']]}, <em>Uni</em> {hoverData()[['uni']]}")
          glue("Showing spectra for:<br><em>Well</em> {hoverData()[['well']]}, <em>Uni</em> {hoverData()[['uni']]}")
        } else {
          glue("Showing spectra for:<br><em>Well</em> {hoverData()[['well']]}, <em>Uni</em> {hoverData()[['uni']]}<br>
         Compared to (in grey): <br><em>Well</em> {clickData()[['well']]}, <em>Uni</em> {clickData()[['uni']]}")
        }
      })
      
      output$sparky <- renderPlot({
        req(hover)
        
        # spectra columns available to plot
        hoverList <- grep(pattern = "spec", colnames(hoverData()), value = TRUE)
        hoverToPlot <- specList[specList %in% hoverList]
        namesToPlot <- names(hoverToPlot)
        
        # sparkline and color creation function, important: called locally within this renderPlot evironment
        source("R/funs.R", local = TRUE)
        
        # color setup
        if (is.na(pal) | pal == "Default") {
          pal <- "Spectral"
        }
        colors <- mycolors(pal, length(hoverList))
        
        # generate the plots
        plist <- purrr::pmap(
          list(
            hoverToPlot,
            namesToPlot,
            seq_along(hoverToPlot)
          ),
          ~ ggspark(hoverData(), clickData(), ..1, ..2, ..3, specDerived)
        )
        
        cowplot::plot_grid(plotlist = plist, ncol = 1)
      })
    }
  )
}
