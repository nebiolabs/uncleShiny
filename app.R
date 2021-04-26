
###########################################################################
###########################################################################
###########################################################################
###########################################################################
####                                                                   ####
####                                                                   ####
####                                                                   ####
####                         Author: Eric Hunt                         ####
####                          Date: 2021-02-05                         ####
####                                                                   ####
####                                                                   ####
####                                                                   ####
###########################################################################
###########################################################################
###########################################################################
###########################################################################


##================================================================
##                           Packages                           ==
##================================================================

library(shiny)
library(shinythemes)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(extrafont)
library(RColorBrewer)
library(plotly)
library(DT)
# library(profvis)


##================================================================
##                           Defaults                           ==
##================================================================

# ggplot theme defaults
theme_set(
  theme_bw(base_family = "Roboto Condensed") +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      axis.text = element_text(face = "bold")
    )
)


##===============================================================
##                           Modules                           ==
##===============================================================

source("modules/printer.R")
source("modules/plotOpts.R")
source("modules/specSpark.R")
source("modules/spectraViewer.R")


##===============================================================
##                           Helpers                           ==
##===============================================================

source("R/vars.R")


############################################################################
############################################################################
############################################################################
############################################################################
####                                                                    ####
####                                                                    ####
####                                                                    ####
####                                 UI                                 ####
####                         The in your face..                         ####
####                                                                    ####
####                                                                    ####
####                                                                    ####
############################################################################
############################################################################
############################################################################
############################################################################


ui <- tagList(
  tags$head(tags$script(type = "text/javascript", src = "code.js")),
  ##===============================================================
  ##                         Navbar page                         ==
  ##===============================================================
  navbarPage(
    title = "Uncle Dashboard",
    # theme = shinytheme("yeti"),
    theme = bslib::bs_theme(version = 4, bootswatch = "yeti"),
    # theme = bslib::bs_theme(version = 4),
    ##===============================================================
    ##                    Navbar: Data Analysis                    ==
    ##===============================================================
    tabPanel(
      "Data Analysis",
      icon = icon("chart-bar"),
      value = "analysis",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          # h3("Selections:"),
          # profvis::profvis_ui("profiler"),
          div(style = "display: inline-block", icon("database")),
          div(style = "display: inline-block", h4("Dataset")),
          helpText("Select a protein and then click the 'Load Dataset' button."),
          selectInput(
            "dataSelection",
            NULL,
            choices = NULL
          ),
          actionButton("loadData", "Load Dataset", icon = icon("file-import")),
          br(),
          br(),
          div(style = "display: inline-block", icon("vial")),
          div(style = "display: inline-block", h4("Experiment")),
          helpText("Select a formulation experiment for the protein selected above to view the data."),
          selectInput(
            "expSelection",
            NULL,
            choices = NULL
          ),
          br(),
          div(style = "display: inline-block", icon("vials")),
          div(style = "display: inline-block", h4("Data Mode")),
          radioGroupButtons(
            inputId = "modeSelection",
            # label = "Choose a mode:", 
            choices = c(
              `<i class='fa fa-vial'></i>` = "normal",
              `<i class='fa fa-blender'></i>` = "blend"
            ),
            justified = TRUE
          ),
          helpText("Select a mode for analyzing expeiments.",
                   "The default is to view a single experiment,",
                   "which can be selected in the dropdown above.",
                   "Alternatively, multiple experiments can be joined",
                   "using the 'blender' tab before plotting."),
          br(),
          ##::::::::::
          ##  Info  ::
          ##::::::::::
          div(icon("info-circle")),
          helpText("Axes and other attributes can be changed for the plots using the dropdown settings to the right."),
          helpText("Selections made on Plot1 and Plot2 are live and interconnected.",
                   "Points can be filtered by clicking or double-clicking a group in the legend."),
          helpText("Hovering over a point will show a metadata tooltip and generate a quick preview of the raw spectra for that point."),
          helpText("The Zoom plot is populated with selected data points from above and is used to further interrogate the data.
            Selection made here will populate the Spectra tab."),
          br(),
          ##:::::::::::::::::
          ##  User inputs  ::
          ##:::::::::::::::::
          div(style = "display: inline-block", icon("code")),
          div(style = "display: inline-block", h4("User Inputs")),
          h5("Summary Plots:"),
          helpText("Hover:"),
          verbatimTextOutput("summyHov"),
          helpText("Click:"),
          verbatimTextOutput("summyClk"),
          # helpText("Select:"),
          # verbatimTextOutput("summySel"),
          br(),
          h5("Zoom Plot:"),
          helpText("Hover:"),
          verbatimTextOutput("zoomyHov"),
          helpText("Click:"),
          verbatimTextOutput("zoomyClk"),
          # helpText("Select:"),
          # verbatimTextOutput("zoomySel")
        ),
        ##================================================================
        ##                          Main panel                          ==
        ##================================================================
        mainPanel(
          width = 10,
          tabsetPanel(
            id = "dataTabs",
            # tabPanel(
            #   "Test Module",
            #   value = "testTab",
            #   icon = icon("hard-hat")
            # ),
            ##================================================================
            ##                           Data tab                           ==
            ##================================================================
            tabPanel(
              "Dataset Preview",
              value = "dataTab",
              icon = icon("table"),
              fluidRow(
                column(
                  width = 8,
                  h4("Dataset Preview:"),
                  helpText("When you load-in a dataset for a protein on the left",
                           "the data for each experiment performed for that protein",
                           "can be viewed here.")
                )
              ),
              fluidRow(
                column(
                  width = 10,
                  # tagList(
                  #   div(style = "display: inline-block; vertical-align:middle;", h1(icon("kiwi-bird"))),
                  #   div(style = "display: inline-block; vertical-align:middle;", h4("Such nothings."))
                  # ),
                  # verbatimTextOutput("tableOutput")
                  printerUI("dataListPrinter")
                )
              )
            ),
            ##===============================================================
            ##                          Blender tab                        ==
            ##===============================================================
            tabPanel(
              "Blender",
              value = "blenderTab",
              icon = icon("blender"),
              fluidRow(
                column(
                  width = 6,
                  h4("Experiment Blender:"),
                  helpText("This is for joining two sets of experimental data",
                           "for the same protein and comparatively plotting both."),
                  helpText("Select two expeiments to blend from the dropdowns below",
                           "and then click the 'blend my data' button."),
                  br(),
                  actionButton("blendData", "Blend my data!", icon = icon("blender"), width = "50%")
                )
              ),
              fluidRow(
                column(
                  width = 11,
                  # offset = 1,
                  fluidRow(
                    column(
                      width = 6,
                      h5("Select First Experiment:"),
                      selectInput(
                        "blendSelectionL",
                        NULL,
                        choices = NULL
                      )
                    ),
                    column(
                      width = 6,
                      h5("Select Second Experiment:"),
                      selectInput(
                        "blendSelectionR",
                        NULL,
                        choices = NULL
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      DTOutput("blendL", width = "100%")
                    ),
                    column(
                      width = 6,
                      DTOutput("blendR", width = "100%")
                    )
                  )
                )
              )
            ),
            ##===============================================================
            ##                         Summary tab                         ==
            ##===============================================================
            tabPanel(
              "Summary",
              value = "summaryTab",
              icon = icon("braille"),
              fluidRow(
                column(
                  width = 1,
                  plotOptsUI("summyOpts")
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
                      plotlyOutput("summydots")
                    ),
                    column(
                      width = 3,
                      specSparkUI("specSparkHover")
                      # plotOutput("ggspecspark")
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
                      helpText("This module creates ridgeline overlays of",
                               "the raw spectra corresponding to the selection",
                               "made on the zommed-in plot to the left."),
                      helpText("Note: A selection must be made before spectra will appear.")
                    )
                  ),
                  fluidRow( # ZOOM PLOT AND DATATABLE
                    column(
                      width = 6,
                      plotlyOutput("zoomydots", height = "400px")
                    ),
                    column(
                      width = 6,
                      # div(h1(icon("grav")))
                      spectraViewerUI("spectraViewer")
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
                      DTOutput("zoomyDT", width = "100%"),
                      # DTOutput("zoomytable")
                      # hr(),
                      # verbatimTextOutput("testysquid")
                    )
                  )
                )
              )
            ),
            ##================================================================
            ##                         Activity tab                         ==
            ##================================================================
            # tabPanel(
            #   "Activity Data",
            #   value = "activityTab",
            #   icon = icon("dna")
            # )#,
            ##===============================================================
            ##                         Spectra tab                         ==
            ##===============================================================
            # tabPanel(
            #   "Spectra",
            #   value = "spectraTab",
            #   icon = icon("chart-area"),
            #   # fluidRow(
            #   #   column(
            #   #     width = 8,
            #   #     h4("Data Table:"),
            #   #     helpText("There's nothing here yet, but the plan is",
            #   #              "to create a space where spectra can still be viewed,",
            #   #              "even if the Uncle didn't call any summary data."),
            #   #     helpText("True, this probably means we don't care about it",
            #   #              "because it's messed up beyond recognition by the instrument,",
            #   #              "but just in case.")
            #   #   )
            #   # ),
            #   fluidRow(
            #     column(
            #       width = 8,
            #       h4("Raw Spectra:"),
            #       helpText("Here you will find the raw spectra for the points selected on the zoom plot (located on the previous tab)."),
            #       helpText("SLS is recorded at both 266nm and 473nm. Scattering and molecule size are proportional",
            #                "and therefore the shorter, 266nm wavelength is more sensitive to early-onset aggregation."),
            #       helpText("Please note, for DLS the 'intensity' option is the only truely 'raw' DLS spectra.",
            #                "Because of the relationship between molecule size and amount of scattering",
            #                "larger molecules will scatter MUCH more strongly and give a higher signal on the 'intensity' spectrum.",
            #                "The 'mass' option spectra is a calculated/processed spectrum which tries to adjust amplitude",
            #                "in order to give a better idea of the proportionality between different sized species in the sample.")
            #     )
            #   ),
            #   # fluidRow(
            #   #   column(
            #   #     width = 12,
            #   #     spectraViewerUI("spectraViewer")
            #   #   )
            #   # ),
            #   # fluidRow(
            #   #     column(
            #   #         width = 12,
            #   #         verbatimTextOutput("spectraDataPrint")
            #   #     )
            #   # )
            # )
          )
        )
      )
    ),
    ##===============================================================
    ##                      Navbar: Data Prep                      ==
    ##===============================================================
    tabPanel(
      "Data Preparation",
      icon = icon("soap"),
      value = "preparation",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tagList(
            div(style = "display: inline-block; vertical-align:middle;", h1(icon("grav"))),
            div(style = "display: inline-block; vertical-align:middle;", h4("Such nothings."))
          )
        ),
        mainPanel(
          width = 9,
          h1("The first rule of data preparation is..", noWS = TRUE),
          h1("..you don't talk about data preparation.", noWS = TRUE)
        )
      )
    ),
    ##================================================================
    ##                       Navbar: One-Offs                       ==
    ##================================================================
    tabPanel(
      "One-Off Experiments",
      icon = icon("hat-cowboy-side"),
      value = "oneoff",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tagList(
            div(style = "display: inline-block; vertical-align:middle;", h1(icon("grav"))),
            div(style = "display: inline-block; vertical-align:middle;", h4("Such nothings."))
          )
        ),
        mainPanel(
          width = 9,
          h1("What are you looking at?..", noWS = FALSE),
          br(),
          div(style = "display: inline-block; vertical-align:middle;", HTML("&emsp;")),
          div(style = "display: inline-block; vertical-align:middle;", HTML("&emsp;")),
          div(style = "display: inline-block; vertical-align:middle;", HTML("&emsp;")),
          div(style = "display: inline-block; vertical-align:middle;", HTML("&emsp;")),
          div(style = "display: inline-block; vertical-align:middle;", h1("..I know what I'm doing.", noWS = FALSE)),
          br(),
          div(style = "display: inline-block; vertical-align:middle;", HTML("&emsp;")),
          div(style = "display: inline-block; vertical-align:middle;", HTML("&emsp;")),
          div(style = "display: inline-block; vertical-align:middle;", HTML("&emsp;")),
          div(style = "display: inline-block; vertical-align:middle;", HTML("&emsp;")),
          div(style = "display: inline-block; vertical-align:middle;", HTML("&emsp;")),
          div(style = "display: inline-block; vertical-align:middle;", HTML("&emsp;")),
          div(style = "display: inline-block; vertical-align:middle;", HTML("&emsp;")),
          div(style = "display: inline-block; vertical-align:middle;", HTML("&emsp;")),
          div(style = "display: inline-block; vertical-align:middle;", h2("—Han Solo"))
        )
      )
    )
  )
)
















###########################################################################
###########################################################################
###########################################################################
###########################################################################
####                                                                   ####
####                                                                   ####
####                                                                   ####
####                              SERVER                               ####
####                   The behind the scenes magic..                   ####
####                                                                   ####
####                                                                   ####
####                                                                   ####
###########################################################################
###########################################################################
###########################################################################
###########################################################################


server <- function(input, output, session) {
  # bslib::bs_themer()
  
  # callModule(profvis::profvis_server, "profiler")
  
  ##================================================================
  ##                           Database                           ==
  ##================================================================
  
  
  # watches the data directory for any changes and auto-updates a named path list every ten seconds..
  dataPaths <- reactiveFileReader(
    intervalMillis = 10000,
    session,
    filePath = "data/",
    readFunc = function(x) {
      purrr::set_names(
        list.files(x, full.names = TRUE),
        nm = stringr::str_extract(list.files(x, full.names = FALSE), ".*(?=_data.rds)")
      )
    }
  )
  # this observer updates the data input dropdown whenever there is a change detected in the data directory..
  observeEvent(dataPaths(), {
    updateSelectInput(
      session,
      "dataSelection",
      choices = c(dataPaths())
    )
  })
  
  # this updates the experiment list when the data selection changes..
  observeEvent(input$loadData, {
    req(dataList())
    updateSelectInput(
      session,
      "expSelection",
      choices = names(dataList()),
      selected = names(dataList())[1]
    )
  })
  
  
  ##===============================================================
  ##                           Load-in                           ==
  ##===============================================================
  
  
  # creates the reactive list of experiment data tables from selected value in sidebar on button click..
  dataList <- eventReactive(input$loadData, {
    if (input$dataSelection == "") {
      data <- NULL
    } else {
      data <- readRDS(input$dataSelection) %>%
        map(
          function(df) {
            drops <- c("map_color")
            # dummy variables are added for tooltip metadata information if missing
            if (!(identical(setdiff(tooltipVars, names(df)), character(0)))) {
              missing <- setdiff(tooltipVars, names(df))
              df[missing] <- NA
            }
            newdf <- df %>%
              select(-any_of(drops)) %>% 
              modify_at(vars(well), ~ parse_factor(as.character(.x), levels = wellOrder, ordered = TRUE)) %>%
              # a shared key is added for crosstalk compatibility
              unite("sharedKey", c(plate, well, buffer, pH, salt, additive1, additive2), remove = FALSE)
            return(newdf)
          }
        )
    }
    return(data)
  })
  
  
  ##================================================================
  ##                         Print Module                         ==
  ##================================================================
  
  
  observeEvent(input$loadData, {
    req(dataList())
    callModule(printer, "dataListPrinter", dataList())
  }, ignoreInit = TRUE)
  
  
  ##===============================================================
  ##                       Experiment Data                       ==
  ##===============================================================
  
  
  observeEvent(input$loadData, {
    updateSelectInput(
      session,
      "blendSelectionL",
      choices = names(dataList()),
      selected = names(dataList())[1]
    )
    updateSelectInput(
      session,
      "blendSelectionR",
      choices = names(dataList()),
      selected = names(dataList())[2]
    )
  })
  
  output$blendL <- renderDT({
    req(dataList(), input$blendSelectionL)
    datatable(
      data = dataList()[[input$blendSelectionL]][!(grepl("spec", names(dataList()[[input$blendSelectionL]])))],
      # data = dataList()[[input$blendSelectionL]] %>% select(!(contains("spec"))),
      selection = "none",
      extensions = c("FixedColumns"),
      options = list(
        dom = "ftip",
        # f - filter
        searchHighlight = TRUE,
        # p - pagination
        scrollX = TRUE,
        # scrollY = "250px",
        paging = TRUE,
        pageLength = 16,
        scrollCollapse = TRUE,
        # t - table
        fixedColumns = list(leftColumns = 5),
        order = list(list(3, "asc")),
        columnDefs = list(list(visible = FALSE, targets = c(1, 2)))
      )
    )
  })
  
  output$blendR <- renderDT({
    req(dataList(), input$blendSelectionR)
    datatable(
      data = dataList()[[input$blendSelectionR]][!(grepl("spec", names(dataList()[[input$blendSelectionR]])))],
      # data = dataList()[[input$blendSelectionR]] %>% select(!(contains("spec"))),
      selection = "none",
      extensions = c("FixedColumns"),
      options = list(
        dom = "ftip",
        # f - filter
        searchHighlight = TRUE,
        # p - pagination
        scrollX = TRUE,
        # scrollY = "250px",
        paging = TRUE,
        pageLength = 16,
        scrollCollapse = TRUE,
        # t - table
        fixedColumns = list(leftColumns = 5),
        order = list(list(3, "asc")),
        columnDefs = list(list(visible = FALSE, targets = c(1, 2)))
      )
    )
  })
  
  data <- debounce(
    eventReactive(c(input$loadData, input$expSelection, input$modeSelection, input$blendData), {
      req(dataList(), input$expSelection, input$blendSelectionL, input$blendSelectionR)
      if(input$modeSelection == "blend") {
        expData <- purrr::map_dfr(
          dataList()[c(input$blendSelectionL, input$blendSelectionR)],
          bind_rows#,
          # .id = "exp"
        )
      } else {
        expData <- dataList() %>% pluck(input$expSelection)
      }
      return(expData)
    }),
    millis = 100
  )
  
  output$tableOutput <- renderPrint({
    data()
  })
  
  
  ##===============================================================
  ##                     Plot options module                     ==
  ##===============================================================
  
  
  # Loading in the plot options module as reactive values..
  plotOpts <- callModule(plotOpts, "summyOpts")
  
  
  ##===============================================================
  ##                        Summary plots                        ==
  ##===============================================================
  
  
  ##:::::::::::::::::::::::
  ##  Summary plot data  ::
  ##:::::::::::::::::::::::
  
  
  summyData <- eventReactive(c(data(), plotOpts$color()), {
    req(data(), plotOpts$color())
    fctr <- rlang::sym(c(plotOpts$color()))
    data() %>%  
      # The spectra columns are removed for plotting and displaying event_data..
      select(!(contains("spec"))) %>% 
      # The data is factored by the color variable (coerced to character) for matching between plots..
      mutate(!! fctr := replace_na(!! fctr, "none")) %>% 
      mutate(!! fctr := fct_infreq(factor(!! fctr)))
      # mutate(!! fctr := forcats::fct_infreq(as.character(!! fctr)))
  })
  
  # Creating the `crosstalk` shared data entity for plotting..
  summyShared <- eventReactive(c(summyData(), plotOpts$color()), {
    req(summyData())
    highlight_key(
      summyData,
      key = ~sharedKey,
      group = paste0("summy", input$modeSelection, plotOpts$color())
    )
  })
  
  
  ##::::::::::::::::::
  ##  Plotly plots  ::
  ##::::::::::::::::::
  
  
  # Plot 1 as a reactive..
  p1 <- reactive({
    source("R/plotly.R", local = TRUE)
    buildplotly(
      data = summyShared(),
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
  
  # Plot 2 as a reactive..
  p2 <- reactive({
    source("R/plotly.R", local = TRUE)
    buildplotly(
      data = summyShared(),
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
          list(
            x = 0, xref = "paper", xanchor = "right",
            y = 1.09, yref = "paper", 
            text = "Plot1", font = list(size = 18, family = "Roboto Condensed"),
            showarrow = F
          ),
          list(
            x = 0.54, xref = "paper", xanchor = "right",
            y = 1.09, yref = "paper",
            text = "Plot2", font = list(size = 18, family = "Roboto Condensed"),
            showarrow = F
          )
        ),
        legend = legendList
      ) %>%
      highlight(
        on = "plotly_selected", off = "plotly_deselect",
        opacityDim = 0.15,
        selected = attrs_selected(showlegend = FALSE)
      ) %>%
      config(displaylogo = FALSE) %>% 
      toWebGL()
  })
  
  
  ##:::::::::::::::::
  ##  Eevent data  ::
  ##:::::::::::::::::
  
  
  # The hover data for the sparkline quick-view spectra, debounced for user input timing..
  summyHov <- debounce(reactive({
    df <- event_data(event = "plotly_hover", source = "summydots")
    if (is.null(df)) {
      return(NULL)
    }
    return(df[["key"]])
  }), 500)
  
  # summyHovData <- debounce(reactive({
  #   cd <- event_data(event = "plotly_hover", source = "summydots")[["key"]]
  #   if (is.null(cd)) {
  #     return(NULL)
  #   } else {
  #     return(data()[data()$sharedKey %in% cd, ][1, ])
  #   }
  # }), 500)
  
  # The click data for the sparkline quick-view spectra plot..
  summyClk <- reactive({
    df <- event_data(event = "plotly_click", source = "summydots")
    if (is.null(df)) {
      return(NULL)
    }
    return(df[["key"]])
  })
  
  # summyClkData <- reactive({
  #   cd <- event_data(event = "plotly_click", source = "summydots")[["key"]]
  #   if (is.null(cd)) {
  #     return(NULL)
  #   } else {
  #     return(data()[data()$sharedKey %in% cd, ][1, ])
  #   }
  # })
  
  # The selected data from the summary plot..
  summySelData <- reactive({
    req(summyData())
    cd <- event_data(event = "plotly_selected", source = "summydots")[["key"]]
    summyData()[summyData()$sharedKey %in% cd, ]
  })
  
  
  ##================================================================
  ##                      Spectra sparklines                      ==
  ##================================================================
  
  
  # The sparkline quick-view spectra plot..
  observeEvent(summyHov(), {
    # req(summyHov())
    # callModule(specSpark, "specSparkHover", summyHovData(), summyClkData(), plotOpts$palette())
    callModule(specSpark, "specSparkHover", data(), summyHov(), summyClk(), plotOpts$palette())
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  
  ##================================================================
  ##                        Zoomed-in plot                        ==
  ##================================================================
  
  
  ##:::::::::::::::::::::::::
  ##  Zoomed-in plot data  ::
  ##:::::::::::::::::::::::::
  
  
  # Creating the `crosstalk` shared data entity for plotting the zoomed data..
  zoomyShared <- eventReactive(c(summySelData(), plotOpts$zoomycolor()), {
    req(summySelData())
    # highlight_key(summySelData, key = ~sharedKey, group = paste0("zoomy", plotOpts$zoomycolor()))
    highlight_key(summySelData, key = ~sharedKey, group = "zoomy")
  })
  
  
  ##:::::::::::::::::
  ##  Plotly plot  ::
  ##:::::::::::::::::
  
  
  # The zoomed-in plot as a reactive..
  zoomy <- reactive({
    req(zoomyShared())
    source("R/plotly.R", local = TRUE)
    if (is.null(summySelData())) {
      p <- NULL
    } else {
      p <- buildplotly(
        data = zoomyShared(),
        x = plotOpts$xvar3(),
        y = plotOpts$yvar3(),
        source = "zoomydots",
        # color = plotOpts$color(),
        color = plotOpts$zoomycolor(),
        palette = plotOpts$palette(),
        # showlegend = FALSE,
        customdata = "well"
      ) %>%
        layout(
          annotations = list(
            list(x = 0, xref = "paper", y = 1.09, yref = "paper", text = "Zoom", showarrow = F, font = list(size = 18, family = "Roboto Condensed"), xanchor = "right")
          ),
          legend = legendList
        )
    }
  })
  
  output$zoomydots <- renderPlotly({
    zoomy() %>%
      highlight(
        on = "plotly_select", off = "plotly_deselect",
        opacityDim = 0.15,
        selected = attrs_selected(showlegend = FALSE)
      ) %>%
      config(displaylogo = FALSE) %>%
      event_register(event = "plotly_selected") %>%
      event_register(event = "plotly_click") %>%
      event_register(event = "plotly_hover") %>%
      toWebGL()
  })
  
  
  ##:::::::::::::::::
  ##  Eevent data  ::
  ##:::::::::::::::::
  
  
  # The selected data from the zoomed-in plot..
  zoomySelData <- reactive({
    req(data(), summySelData())
    cd <- event_data(event = "plotly_selected", source = "zoomydots")[["key"]]
    if (is.null(summySelData())) {
      return(NULL)
    } else if (is.null(cd)) {
      return(data()[data()$sharedKey %in% summySelData()$sharedKey, ])
    } else {
      data()[data()$sharedKey %in% cd, ]
    }
  })
  
  
  zoomyDTdata <- reactive({
    req(summySelData())
    cd <- event_data(event = "plotly_selected", source = "zoomydots")[["key"]]
    if (is.null(cd)) {
      summySelData()
    } else {
      summySelData()[summySelData()$sharedKey %in% cd, ]
    }
  })
  
  
  ##:::::::::::::::::::::::::::
  ##  DT of selected values  ::
  ##:::::::::::::::::::::::::::
  
  # output$zoomytable <- renderDT({
  #   zoomyShared()$data(withSelection = TRUE, withFilter = TRUE, withKey = FALSE)
  # })
  
  zoomyDT <- reactive({
    req(summySelData(), zoomyShared())
    datatable(
      data = zoomyShared(),
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
  })
  
  output$zoomyDT <- renderDT({
    req(zoomyDT())
    zoomyDT()
  }, server = FALSE)
  
  
  output$testysquid <- renderPrint({
    # zoomyDTdata()
    zoomyShared()$data()
  })
  
  
  ##===============================================================
  ##                    Spectra viewer module                    ==
  ##===============================================================
  
  observeEvent(c(zoomySelData(), summySelData()), {
    req(zoomySelData())
    callModule(spectraViewer, "spectraViewer", zoomySelData(), plotOpts$palette())
  }, ignoreNULL = FALSE)
  # observe({
  #   req(zoomySelData())
  #   callModule(spectraViewer, "spectraViewer", zoomySelData(), plotOpts$palette())
  # })

  
  ##================================================================
  ##                        Raw input data                        ==
  ##================================================================
  
  
  # For the summary plot..
  output$summyHov <- renderPrint({
    summyHov()
  })
  
  output$summyClk <- renderPrint({
    summyClk()
  })
  
  output$summySel <- renderPrint({
    df <- event_data(event = "plotly_selected", source = "summydots")
    if (is.null(df)) {
      return(NULL)
    }
    return(df[["key"]])
  })
  
  # For the zoomed plot..
  output$zoomyHov <- renderPrint({
    df <- event_data(event = "plotly_hover", source = "zoomydots")
    if (is.null(df)) {
      return(NULL)
    }
    return(df[["customdata"]])
  })
  
  output$zoomyClk <- renderPrint({
    df <- event_data(event = "plotly_click", source = "zoomydots")
    if (is.null(df)) {
      return(NULL)
    }
    return(df[["customdata"]])
  })
  
  output$zoomySel <- renderPrint({
    df <- event_data(event = "plotly_selected", source = "zoomydots")
    if (is.null(df)) {
      return(NULL)
    }
    return(df[["key"]])
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
