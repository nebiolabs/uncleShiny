
##-------------------------------------------------------------------------------
##  Scatter Plot Module                                                        --
##-------------------------------------------------------------------------------


library(shiny)
library(tidyverse)
library(glue)
library(rlang)

# Modules..
# source("R/vars.R")
# source("R/funs.R")
# source("R/plotly.R")

# Helpers..
# source("modules/plotOpts.R")
# source("modules/specSpark.R")
# source("modules/spectraViewer.R")


# UI
scatterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 1,
        plotOptsUI(ns("plotOpts"))
      ),
      column(
        width = 11,
        fluidRow( # HELP TEXT
          column(
            width = 9,
            h4("Summary Data:"),
            helpText("These data points are called by the Uncle.",
                     "Anomalous raw spectra can lead to unreliable values.",
                     "Thus, please note that not all conditions tested may appear here,",
                     "and it is good practice to inspect the raw spectra after narrowing conditions of interest.")
          ),
          column(
            width = 3,
            h4("Spectra QuickView:"),
            helpText("Hover over a point to view sparklines of raw spectra.",
                     "Click a point to freeze those spectra for comparison.",
                     "Double-click to un-freeze.")
          )
        ),
        fluidRow( # MAIN PLOTS AND SPARKLINES
          column(
            width = 9,
            plotlyOutput(ns("summydots"))
          ),
          column(
            width = 3,
            specSparkUI(ns("specSparkHover"))
            # plotOutput(ns("ggspecspark"))
          )
        ),
        fluidRow( # HELP TEXT
          column(
            width = 6,
            h4("Take a Closer Look:"),
            helpText("This plot shows a zoomed-in version of the points selected above,",
                     "which you can further dissect with differnt axis variables using the options to the left.",
                     "The table below shows the details for the points selected here.",
                     "This plot and the table are live and interconnected;",
                     "selecting points here will filter the table.")
          ),
          column(
            width = 6,
            h4("Spectra Overlay"),
            helpText("Lorem ipsum forever.")
          )
        ),
        fluidRow( # ZOOM PLOT AND DATATABLE
          column(
            width = 6,
            plotlyOutput(ns("zoomydots"))
          ),
          column(
            width = 6,
            div(h1(icon("grav")))
          )
        ),
        fluidRow(
          column(
            width = 6,
            h4("Interactive Table:"),
            helpText("This is an interactive table.",
                     "The rows can be filtered using the search bar, or sorted using the header row arrow buttons.",
                     "It shows details for what appears in the 'Zoom' plot above.",
                     "Selecting points there will filter what is shown here.")
          )
        ),
        fluidRow(
          column(
            width = 12,
            DTOutput(ns("sharedselect"), width = "100%")
          )
        )
      )
    )
  ) # taglist
  
}



# SERVER
scatterServer <- function(id, data, loadBtn, expSel) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # Loading in the plot options module as reactive values..
      plotOpts <- plotOptsServer("plotOpts")
      
      
      
      # The spectra columns are removed for plotting to make the size smaller
      plotData <- debounce(reactive({
        req(data)
        req(plotOpts$color())
        fctr <- rlang::sym(c(plotOpts$color()))
        data %>%  
          select(!(contains("spec"))) %>% 
          mutate(!! fctr := forcats::fct_infreq(as.character(!! fctr)))
      }), 100, priority = 100)
      
      
      
      ### MAIN PLOT ###
      
      # Creating the `crosstalk` shared data entity for plotting..
      observe({
        plotData()
        plotShared <<- highlight_key(plotData, key = ~sharedKey, group = paste0(loadBtn, expSel))
      })
      
      
      
      # Plot 1 reactive..
      p1 <- reactive({
        source("R/func_plotly.R", local = TRUE)
        buildplotly(
          data = plotData(),
          x = plotOpts$xvar1(),
          y = plotOpts$yvar1(),
          source = "summydots",
          color = plotOpts$color(),
          palette = plotOpts$palette(),
          customdata = "well"
        ) %>% 
          event_register(event = "plotly_selected") %>% 
          event_register(event = "plotly_click") %>% 
          event_register(event = "plotly_hover")
      })
      
      # Plot 2 reactive..
      p2 <- reactive({
        source("R/func_plotly.R", local = TRUE)
        buildplotly(
          data = plotData(),
          x = plotOpts$xvar2(),
          y = plotOpts$yvar2(),
          source = "summydots",
          color = plotOpts$color(),
          palette = plotOpts$palette(),
          # showlegend = FALSE,
          # colorbar = FALSE,
          customdata = "well"
        ) %>% 
          event_register(event = "plotly_selected") %>% 
          event_register(event = "plotly_click") %>% 
          event_register(event = "plotly_hover")
      })
      
      # The plotly subplot of both connected plots..
      output$summydots <- renderPlotly({
        subplot(
          p1(), p2(),
          nrows = 1,
          titleX = TRUE,
          titleY = TRUE,
          margin = 0.04
        ) %>%
          layout(
            annotations = list(
              list(x = 0, xref = "paper", y = 1.09, yref = "paper", text = "Plot1", showarrow = F, font = list(size = 18, family = "Roboto Condensed"), xanchor = "right"),
              list(x = 0.54, xref = "paper", y = 1.09, yref = "paper", text = "Plot2", showarrow = F, font = list(size = 18, family = "Roboto Condensed"), xanchor = "right")
            ),
            legend = legendList
          ) %>%
          # highlight(on = "plotly_selected", off = "plotly_deselect", opacityDim = 0.15, selected = attrs_selected(showlegend = FALSE)) %>%
          config(displaylogo = FALSE) %>% 
          toWebGL()
      })
      
      ###
      
      
      
      
      
      
      
      
      
      
      ### SPECTRA QUICKVIEW SPARKLINES ###
      
      
      
      # The hover data for the sparkline quick-view spectra plot..
      hoverData_summary <- debounce(reactive({
        cd <- event_data(event = "plotly_hover", source = "summydots")[["key"]]
        if (is.null(cd)) {
          return(NULL)
        } else {
          return(data[data$sharedKey %in% cd, ][1, ])
        }
      }), 1000)
      
      
      
      # The click data for the sparkline quick-view spectra plot..
      clickData_summary <- reactive({
        cd <- event_data(event = "plotly_click", source = "summydots")[["key"]]
        if (is.null(cd)) {
          return(NULL)
        } else {
          return(data[data$sharedKey %in% cd, ][1, ])
        }
      })
      
      # The sparkline quick-view spectra plot..
      observe({
        req(hoverData_summary())
        specSparkServer("specSparkHover", hoverData_summary(), clickData_summary(), plotOpts$palette())
      })
      
      
      
      
      ###
      
      
      
      
      
      
      
      
      
      
      ### SELECTED DATA ###
      
      
      
      # The select event from the main plots is used to create a reactive data, still without spectra columns..
      selectedData <- reactive({
        cd <- event_data(event = "plotly_selected", source = "summydots")[["key"]]
        plotData()[plotData()$sharedKey %in% cd, ]
      })
      
      # Creating the `crosstalk` shared data entity for plotting the zoomed data..
      selectedShared <<- selectedData %>% highlight_key(key = ~sharedKey)
      
      output$sharedselect <- renderDT(
        {
          datatable(
            data = selectedShared,
            selection = "none",
            # extensions = c("Buttons", "FixedColumns"),
            # extensions = c("Select", "Buttons"),
            extensions = c("Buttons"),
            options = list(
              # select = list(style = "multi", items = "row"),
              dom = "Bftip",
              # f - filter
              searchHighlight = TRUE,
              # p - pagination
              scrollX = TRUE,
              # scrollY = "250px",
              paging = TRUE,
              pageLength = 20,
              scrollCollapse = TRUE,
              # t - table
              # fixedColumns = list(leftColumns = 4),
              order = list(list(1, "desc")),
              columnDefs = list(list(visible = FALSE, targets = c(1, 2))),
              # B - Buttons
              buttons = 
                list('copy', list(
                  extend = 'collection',
                  buttons = c('csv', 'excel', 'pdf'),
                  text = 'Download'
                ))
            )
          )
        },
        server = FALSE
      )
      
      # The zoomed-in plot..
      zoomy <- reactive({
        source("R/func_plotly.R", local = TRUE)
        if (is.null(selectedData())) {
          p <- NULL
        } else {
          p <- buildplotly(
            data = selectedShared,
            x = plotOpts$xvar3(),
            y = plotOpts$yvar3(),
            source = "zoomydots",
            color = plotOpts$color(),
            palette = plotOpts$palette(),
            showlegend = FALSE,
            customdata = "well"
          ) %>% 
            layout(
              annotations = list(
                list(x = 0, xref = "paper", y = 1.09, yref = "paper", text = "Zoom", showarrow = F, font = list(size = 18, family = "Roboto Condensed"), xanchor = "right")
              )
            ) %>% 
            highlight(on = "plotly_select", off = "plotly_deselect", opacityDim = 0.15, selected = attrs_selected(showlegend = FALSE)) %>%
            config(displaylogo = FALSE) %>% 
            event_register(event = "plotly_selected") %>% 
            event_register(event = "plotly_click") %>% 
            event_register(event = "plotly_hover") %>% 
            toWebGL()
        }
      })
      
      output$zoomydots <- renderPlotly({
        zoomy()
      })
      
      
      spectraData <- reactive({
        cd <- event_data(event = "plotly_selected", source = "zoomydots")[["key"]]
        if (is.null(cd)) {
          data[data$sharedKey %in% selectedData()$sharedKey, ]
        } else {
          data[data$sharedKey %in% cd, ]
        }
      })
      
      spectraViewerServer("spectraViewer", spectraData)
      
      
      
      ###
      
      
      
      
      ### EVENT DATA RAW RETURNS ###
      
      # For the summary plot..
      summyHov <- reactive({
        df <- event_data(event = "plotly_hover", source = "summydots")
        if (is.null(df)) {
          return(NULL)
        }
        return(df[["customdata"]])
      })
      
      summyClk <- reactive({
        df <- event_data(event = "plotly_click", source = "summydots")
        if (is.null(df)) {
          return(NULL)
        }
        return(df[["customdata"]])
      })
      
      summySel <- reactive({
        df <- event_data(event = "plotly_selected", source = "summydots")
        if (is.null(df)) {
          return(NULL)
        }
        return(df[["key"]])
      })
      
      # For the zoomed plot..
      zoomyHov <- reactive({
        df <- event_data(event = "plotly_hover", source = "zoomydots")
        if (is.null(df)) {
          return(NULL)
        }
        return(df[["customdata"]])
      })
      
      zoomyClk <- reactive({
        df <- event_data(event = "plotly_click", source = "zoomydots")
        if (is.null(df)) {
          return(NULL)
        }
        return(df[["customdata"]])
      })
      
      zoomySel <- reactive({
        df <- event_data(event = "plotly_selected", source = "zoomydots")
        if (is.null(df)) {
          return(NULL)
        }
        return(df[["key"]])
      })
      
      # Return reactive values from the module..
      return(
        list(
          summyHov = summyHov,
          summyClk = summyClk,
          summySel = summySel,
          zoomyHov = zoomyHov,
          zoomyClk = zoomyClk,
          zoomySel = zoomySel
        )
      )
      
      ###
    }
  )
}