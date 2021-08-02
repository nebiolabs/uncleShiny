
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
library(DBI)
library(pool)
library(RPostgres)
library(bit64)
# library(shinythemes) # using bootswatch library bslib for theming
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(glue)
library(rlang)
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
source("modules/activityAssay.R")


##===============================================================
##                           Helpers                           ==
##===============================================================

source("R/vars.R")
source("R/funs.R")


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
    ##================================================================
    ##                       Navbar: Database                       ==
    ##================================================================
    tabPanel(
      "Database Connection",
      icon = icon("database"),
      value = "database",
      sidebarLayout(
        ##================================================================
        ##                          Side panel                          ==
        ##================================================================
        sidebarPanel(
          width = 2,
          div(style = "display: inline-block", icon("database")),
          div(style = "display: inline-block", h4("Database")),
          helpText("Click refresh to re-establish connection to ebase
                   and display available protein datasets for analysis."),
          verbatimTextOutput("db_prod_selected"),
          selectInput(
            "db_prod_sel",
            NULL,
            choices = NULL
          ),
          actionButton(
            "db_refresh",
            "Refresh",
            icon = icon("sync-alt")
          ),
          br(),
          br(),
          br(),
          verbatimTextOutput("db_exp_selected"),
          selectInput(
            "db_exp_sel",
            NULL,
            choices = NULL
          ),
          actionButton(
            "db_load",
            "Load Experiment",
            icon = icon("sync-alt")
          ),
          br()
        ),
        ##================================================================
        ##                          Main panel                          ==
        ##================================================================
        mainPanel(
          width = 9,
          tabsetPanel(
            selected = "db_visualization",
            ##===============================================================
            ##                        Selection tab                        ==
            ##===============================================================
            tabPanel(
              "Selection",
              value = "db_selection",
              icon = icon("object-group"),
              h4("Products available on server:"),
              DTOutput("db_product_table", width = "100%"),
              h4("Experiments available for selected product:"),
              DTOutput("db_exp_available", width = "100%")
            ),
            ##===============================================================
            ##                      Visualization tab                      ==
            ##===============================================================
            # tabPanel(
            #   "Simple Visualization",
            #   value = "db_visualize",
            #   icon = icon("chart-area"),
            #   DTOutput("db_data_print"),
            #   plotlyOutput("db_summydots")
            # ),
            tabPanel(
              "Visualization",
              value = "db_visualization",
              icon = icon("braille"),
              fluidRow(
                column(
                  width = 1,
                  plotOptsUI("db_summyOpts")
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
                      plotlyOutput("db_summydots")
                    ),
                    column(
                      width = 3,
                      div(h1(icon("grav")))
                      # specSparkUI("db_specSparkHover")
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
                      div(h1(icon("grav")))
                      # plotlyOutput("db_zoomydots", height = "400px")
                    ),
                    column(
                      width = 6,
                      div(h1(icon("grav")))
                      # div(h1(icon("grav")))
                      # spectraViewerUI("db_spectraViewer")
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
                      DTOutput("db_data_print")
                      # DTOutput("db_zoomyDT", width = "100%"),
                      # DTOutput("db_zoomytable")
                      # hr(),
                      # verbatimTextOutput("db_testysquid")
                    )
                  )
                )
              )
            ),
            ##===============================================================
            ##                        Visual QC tab                        ==
            ##===============================================================
            tabPanel(
              "Parser QC",
              value = "parser_qc",
              icon = icon("eye"),
              fluidRow(
                column(
                  width = 6,
                  plotOutput("db_dls"),
                  plotOutput("db_sls")
                ),
                column(
                  width = 6,
                  plotOutput("local_dls"),
                  plotOutput("local_sls")
                )
              )
            )
          )
        )
      )
    ),
    ##===============================================================
    ##                    Navbar: Data Analysis                    ==
    ##===============================================================
    tabPanel(
      "Data Analysis",
      icon = icon("chart-bar"),
      value = "analysis",
      sidebarLayout(
        ##================================================================
        ##                          Side panel                          ==
        ##================================================================
        sidebarPanel(
          width = 2,
          # h3("Selections:"),
          # profvis::profvis_ui("profiler"),
          div(style = "display: inline-block", icon("table")),
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
            tabPanel(
              "Activity Data",
              value = "activityTab",
              icon = icon("dna"),
              fluidRow(
                column(
                  width = 6,
                  h4("Activity Assay Data:"),
                  helpText("This module is currently under construction."),
                  br(),
                  helpText("The goal is to provide a convenient location",
                           "to view activity assay data which corresponds",
                           "the the formulations analyzed on the Uncle."),
                  br(),
                  helpText("This is more flexible than activity data",
                           "that is bundled with the Uncle data,",
                           "and it can also include timecourse studies",
                           "and user defined controls that do not directly",
                           "relate back to a screen formulation ",
                           "(e.g. a previous lot or development formulation).")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  activityAssayUI("activityAssay")
                )
              )
            )#,
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
  ##                       Local 'Database'                       ==
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
  
  
  ##===============================================================
  ##                     PostgreSQL Database                     ==
  ##===============================================================
  
  ##::::::::::::::::::::::::
  ##  Available Products  ::
  ##::::::::::::::::::::::::
  
  db_product_table <- eventReactive(input$db_refresh, {
    req(ebase_dev)
    # inner_join method.. might create replicate protein entries
    # if there are multiple matches in the experiment_sets table
    # DBI::dbGetQuery(
    #   ebase_dev,
    #   "SELECT p.name product_name, p.id product_id, p.catalog_number
    #    FROM products p
    #    INNER JOIN uncle_experiment_sets exp_sets
    #    on p.id = exp_sets.product_id"
    # )
    # semi_join method.. could prevent the above issue, maybe fastest
    DBI::dbGetQuery(
      ebase_dev,
      "SELECT p.name AS product_name, p.id AS product_id, p.catalog_number
       FROM products p
       WHERE EXISTS (SELECT * FROM uncle_experiment_sets exp_sets
                     WHERE exp_sets.product_id = p.id)"
    )
    # # collapse with distinct? SELECT DISTINCT(p.name...) FROM...
    # DBI::dbGetQuery(
    #   ebase_dev,
    #   "SELECT DISTINCT p.name AS product_name, p.id AS product_id, p.catalog_number
    #    FROM products p INNER JOIN uncle_experiment_sets exp_sets
    #     ON p.id = exp_sets.product_id)"
    # )
  })

  output$db_product_table <- renderDT({
    req(db_product_table())
    datatable(
      data = db_product_table(),
      selection = "none",
      # extensions = c("FixedColumns"),
      options = list(
        dom = "ftip",
        # f - filter
        searchHighlight = TRUE,
        # p - pagination
        scrollX = TRUE,
        # scrollY = "250px",
        paging = TRUE,
        pageLength = 16,
        scrollCollapse = TRUE#,
        # t - table
        # fixedColumns = list(leftColumns = 5),
        # order = list(list(3, "asc")),
        # columnDefs = list(list(visible = FALSE, targets = c(1, 2)))
      )
    )
  })
  
  db_prod_available <- eventReactive(input$db_refresh, {
    req(db_product_table())
    db_product_table() |>
      dplyr::select(product_name, product_id) |>
      tibble::deframe()
  })

  observeEvent(db_prod_available(), {
    req(db_prod_available())
    updateSelectInput(
      session,
      "db_prod_sel",
      choices = c(" " = 0, db_prod_available()),
      selected = 0
    )
  })
  
  output$db_prod_selected <- renderPrint({
    input$db_prod_sel
  })
  
  
  ##:::::::::::::::::::::::::::
  ##  Available Experiments  ::
  ##:::::::::::::::::::::::::::
  
  db_exp_table <- eventReactive(input$db_prod_sel, {
    req(db_product_table())
    DBI::dbGetQuery(
      ebase_dev,
      glue::glue_sql(
        "SELECT id AS experiment_set_id, product_id,
                exp_type, plate_generation, well_set_id
          FROM uncle_experiment_sets
          WHERE product_id = {input}",
        input = input$db_prod_sel,
        .con = ebase_dev
      )
    )
  })
  
  output$db_exp_available <- renderDT({
    req(db_exp_table())
    datatable(
      data = db_exp_table(),
      selection = "none",
      # extensions = c("FixedColumns"),
      options = list(
        dom = "ftip",
        # f - filter
        searchHighlight = TRUE,
        # p - pagination
        scrollX = TRUE,
        # scrollY = "250px",
        paging = TRUE,
        pageLength = 16,
        scrollCollapse = TRUE#,
        # t - table
        # fixedColumns = list(leftColumns = 5),
        # order = list(list(3, "asc")),
        # columnDefs = list(list(visible = FALSE, targets = c(1, 2)))
      )
    )
  })
  
  db_exp_available <- eventReactive(input$db_prod_sel, {
    req(db_exp_table())
    db_exp_table() |>
      tidyr::unite(
        col = "experiment",
        exp_type, plate_generation, experiment_set_id, well_set_id,
        sep = "_",
        remove = FALSE
      ) |> 
      dplyr::select(experiment, experiment_set_id) |> 
      dplyr::mutate(across(c(experiment_set_id), .fns = bit64::as.integer64)) |> 
      tibble::deframe()
  })
  
  observeEvent(db_exp_available(), {
    req(db_exp_available())
    updateSelectInput(
      session,
      "db_exp_sel",
      choices = bit64::c.integer64(" " = 0, db_exp_available())
    )
  })
  
  output$db_exp_selected <- renderPrint({
    input$db_exp_sel
  })
  
  
  ##::::::::::::::::::
  ##  PSQL Load-in  ::
  ##::::::::::::::::::
  
  db_data <- eventReactive(input$db_load, {
    summary_data <- DBI::dbGetQuery(
      ebase_dev,
      # glue::glue_sql(
      #   "SELECT sum.*, wells.layout_address AS well
      #   FROM uncle_summaries sum INNER JOIN wells
      #     ON sum.well_id = wells.id
      #         WHERE EXISTS (SELECT *
      #                       FROM uncle_experiments exps
      #                       WHERE exps.uncle_experiment_set_id IN ({input*})
      #                         AND sum.uncle_experiment_id = exps.id)",
      glue::glue_sql(
        "WITH cte_sum AS
        (SELECT wells.layout_address AS well, sum.* 
        FROM uncle_summaries sum
        INNER JOIN wells
          ON sum.well_id = wells.id
        WHERE EXISTS (SELECT *
                      FROM uncle_experiments exps
                      WHERE exps.uncle_experiment_set_id IN ({input*})
                        AND sum.uncle_experiment_id = exps.id)
        )
        SELECT cte_sum.*, exp_conds.well_id, exp_conds.id AS experiment_condition_id, exp_conds.condition_id,
          exp_conds.unit_id, exp_conds.raw_value AS unit_value,
          units.name AS unit_name, conds.name AS cond_name 
        FROM cte_sum
        INNER JOIN experimental_conditions AS exp_conds
          ON cte_sum.well_id = exp_conds.well_id
        INNER JOIN units
          ON exp_conds.unit_id = units.id
        INNER JOIN conditions AS conds
         ON exp_conds.condition_id = conds.id",
        input = input$db_exp_sel,
        .con = ebase_dev
      )
    ) |> 
      dplyr::mutate(sharedKey = id) |> 
      dplyr::rename(
        Tagg266 = t_agg_266,
        Tagg473 = t_agg_473,
        Tm1 = t_m_1,
        Z_D = z_avg_diameter,
        peak1_D = pk_1_mode_diameter,
        PdI = pdi
      ) |> 
      dplyr::mutate(residuals = purrr::map(residuals, parse_float8)) |>
      dplyr::rename(uncle_summary_id = id) |> 
      tibble::as_tibble()
    
    summary_ids <- summary_data |> dplyr::pull(uncle_summary_id)
    
    spec_tbls <- get_spec_tbls(ebase_dev, spec_tbl_list, summary_ids)
    
    # return(nest_spectra(summary_data, spec_tbls))
    return(summary_data)
  })
  
  
  output$db_data_print <- renderDT({
    db_data() |> 
      dplyr::select(-contains("spec"), -contains("residuals"))
  })
  
  
  ##::::::::::::::::::::
  ##  PSQL Plot Data  ::
  ##::::::::::::::::::::
  
  db_summyShared <- eventReactive(c(db_data()), {
    req(db_data())
    highlight_key(
      db_data,
      key = ~sharedKey,
      group = paste("summy", input$db_prod_sel, input$db_exp_sel, sep = "_")
    )
  })
  
  
  ##::::::::::::::::
  ##  PSQL Plots  ::
  ##::::::::::::::::
  
  db_plotOpts <- callModule(plotOpts, "db_summyOpts")
  
  # Plot 1 as a reactive..
  db_p1 <- reactive({
    source("R/plotly.R", local = TRUE)
    db_buildplotly(
      data = db_summyShared(),
      x = db_plotOpts$xvar1(),
      y = db_plotOpts$yvar1(),
      source = "db_summydots",
      color = "well",
      palette = db_plotOpts$palette(),
      customdata = "well"
    )
  })
  
  # Plot 2 as a reactive..
  db_p2 <- reactive({
    source("R/plotly.R", local = TRUE)
    db_buildplotly(
      data = db_summyShared(),
      x = db_plotOpts$xvar2(),
      y = db_plotOpts$yvar2(),
      source = "db_summydots",
      color = "well",
      palette = db_plotOpts$palette(),
      # showlegend = FALSE,
      # colorbar = FALSE,
      customdata = "well"
    )
  })
  
  # The plotly subplot of both connected plots..
  output$db_summydots <- renderPlotly({
    subplot(
      db_p1(), db_p2(),
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
  
  
  ##================================================================
  ##                      db/Local Visual QC                      ==
  ##================================================================
  
  
  ##:::::::::
  ##  DLS  ::
  ##:::::::::
  
  output$db_dls <- renderPlot({
  req(db_data())
  fun_data <- db_data() |>
    dplyr::select(-c(created_at:cond_name)) |> 
    dplyr::distinct() |> 
    dplyr::filter(between(Z_D, 0, 999)) |> 
    dplyr::mutate(PdI = round(PdI, digits = 2))
  plot <- ggplot(
    data = fun_data,
    aes_string(x = "Z_D", y = "PdI")
  ) +
    geom_point(aes_string(fill = "well"),
               shape = 21,
               color = "black",
               size = 2,
               alpha = 0.5,
               show.legend = FALSE) +
    ggrepel::geom_text_repel(aes_string(label = "well"), alpha = 0.5) +
    scale_color_manual(values = mycolors("Spectral", 96)) +
    theme(legend.position = "none") +
    labs(
      subtitle = "(db) DLS Summary"
    ) +
    geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.2) +
    geom_vline(xintercept = 10, linetype = "dashed", alpha = 0.2) +
    scale_x_log10(limits = c(0.1, max(fun_data[["Z_D"]]))) +
    scale_y_continuous(limits = c(min(fun_data[["PdI"]]), max(fun_data[["PdI"]]))) +
    # scale_x_log10(limits = c(1, 1000)) +
    # scale_y_continuous(limits = c(0,1)) +
    annotation_logticks(sides = "b")
  return(plot)
})
  
  output$local_dls <- renderPlot({
    req(summyData())
    fun_data <- summyData() |> 
      tidyr::drop_na(Z_D)
    plot <- ggplot(
      data = fun_data,
      aes_string(x = "Z_D", y = "PdI")
    ) +
      geom_point(aes_string(fill = "well"),
                 shape = 21,
                 color = "black",
                 size = 2,
                 alpha = 0.5,
                 show.legend = FALSE) +
      ggrepel::geom_text_repel(aes_string(label = "well"), alpha = 0.5) +
      scale_color_manual(values = mycolors("Spectral", 96)) +
      theme(legend.position = "none") +
      labs(
        subtitle = "(local) DLS Summary"
      ) +
      geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.2) +
      geom_vline(xintercept = 10, linetype = "dashed", alpha = 0.2) +
      scale_x_log10(limits = c(0.1, max(fun_data[["Z_D"]]))) +
      scale_y_continuous(limits = c(min(fun_data[["PdI"]]), max(fun_data[["PdI"]]))) +
      # scale_x_log10(limits = c(1, 1000)) +
      # scale_y_continuous(limits = c(0,1)) +
      annotation_logticks(sides = "b")
    return(plot)
  })
  
  
  ##:::::::::
  ##  SLS  ::
  ##:::::::::
  
  output$db_sls <- renderPlot({
    req(db_data())
    fun_data <- db_data() |> 
      dplyr::select(-c(created_at:cond_name)) |> 
      dplyr::distinct()
    plot <- ggplot(
      data = fun_data,
      aes_string(x = "Tm1", y = "Tagg266")
    ) +
      geom_point(aes_string(fill = "well"),
                 shape = 21,
                 color = "black",
                 size = 2,
                 alpha = 0.5,
                 show.legend = FALSE) +
      ggrepel::geom_text_repel(aes_string(label = "well"), alpha = 0.5) +
      # geom_text(aes_string(label = "well"), alpha = 0.5) +
      scale_color_manual(values = mycolors("Spectral", 96)) +
      theme(legend.position = "none") +
      labs(
        subtitle = "(db) SLS Summary"
      ) +
      scale_x_continuous(limits = c(min(fun_data[["Tm1"]]), max(fun_data[["Tm1"]]))) +
      scale_y_continuous(limits = c(min(fun_data[["Tagg266"]]), max(fun_data[["Tagg266"]])))
      # scale_x_continuous(limits = c(20, 80)) +
      # scale_y_continuous(limits = c(20, 80))
    return(plot)
  })
  
  output$local_sls <- renderPlot({
    req(summyData())
    fun_data <- summyData()
    plot <- ggplot(
      data = fun_data,
      aes_string(x = "Tm1", y = "Tagg266")
    ) +
      geom_point(aes_string(fill = "well"),
                 shape = 21,
                 color = "black",
                 size = 2,
                 alpha = 0.5,
                 show.legend = FALSE) +
      ggrepel::geom_text_repel(aes_string(label = "well"), alpha = 0.5) +
      # geom_text(aes_string(label = "well"), alpha = 0.5) +
      scale_color_manual(values = mycolors("Spectral", 96)) +
      theme(legend.position = "none") +
      labs(
        subtitle = "(local) SLS Summary"
      ) +
      scale_x_continuous(limits = c(min(fun_data[["Tm1"]]), max(fun_data[["Tm1"]]))) +
      scale_y_continuous(limits = c(min(fun_data[["Tagg266"]]), max(fun_data[["Tagg266"]])))
      # scale_x_continuous(limits = c(20, 80)) +
      # scale_y_continuous(limits = c(20, 80))
    return(plot)
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
  
  # this updates the experiment lists when the data selection changes..
  observeEvent(input$loadData, {
    req(dataList())
    updateSelectInput(
      session,
      "expSelection",
      choices = names(dataList()),
      selected = names(dataList())[1]
    )
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
      req(dataList(), input$expSelection, input$blendSelectionL)
      if (input$modeSelection == "blend") {
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
    millis = 500, priority = -100
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
            list(x = 0,
                 xref = "paper",
                 y = 1.09,
                 yref = "paper",
                 text = "Zoom",
                 showarrow = F,
                 font = list(size = 18,
                             family = "Roboto Condensed"),
                 xanchor = "right")
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
  
  
  ##===============================================================
  ##                    Activity assay module                    ==
  ##===============================================================
  
  observeEvent(data(), {
    req(data())
    callModule(activityAssay, "activityAssay", data(), plotOpts$palette())
  }, ignoreNULL = FALSE)
  
  
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

##===============================================================
##                         Connections                         ==
##===============================================================

global <- function() {
  ebase_dev <<- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = "ebase_dev",
    host = "ebase-db-c.neb.com",
    port = 5432,
    user = Sys.getenv("ebase_uid"),
    password = Sys.getenv("ebase_pwd")
  )
  message("Connection established? : ", DBI::dbIsValid(ebase_dev))
  print(ebase_dev)
  onStop(function() {
    pool::poolClose(ebase_dev)
    message("Connection closed? : ", !(DBI::dbIsValid(ebase_dev)))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server, onStart = global)
# 
# opens the app in a local, pre-sized RStudio window
# shiny::runGadget(
#   ui, server,
#   viewer = dialogViewer("UncleDashboard", width = 1200, height = 800)
#   # viewer = browserViewer()
# )
