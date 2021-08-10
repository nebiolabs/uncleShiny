
####----------------------------------------------------------------------------
####----------------------------------------------------------------------------
####                                                                        ----
####  ui.R                                                                  ----
####  User Interface                                                        ----
####                                                                        ----
####----------------------------------------------------------------------------
####----------------------------------------------------------------------------




tagList(
  tags$head(tags$script(type = "text/javascript", src = "code.js")),
  ##===============================================================
  ##                         Navbar page                         ==
  ##===============================================================
  navbarPage(
    title = "Uncle Dashboard",
    # theme = shinytheme("yeti"),
    theme = theme_dark,
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
          verbatimTextOutput("db_exp_set_selected"),
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
            ##===============================================================
            ##                        Selection tab                        ==
            ##===============================================================
            tabPanel(
              "Selection",
              value = "db_selection",
              icon = icon("object-group"),
              fluidRow(
                h5("Products available on server:"),
                DTOutput("db_products_available", width = "100%"),
              ),
              fluidRow(
                column(
                  width = 6,
                  h5("Experiments sets available for selected product:"),
                  DTOutput("db_exp_sets_available", width = "100%")
                ),
                column(
                  width = 6,
                  h5("Experiments in experiment sets:"),
                  DTOutput("db_exps_available", width = "100%")
                )
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Data Visualization",
      icon = icon("braille"),
      value = "visualization",
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
              DTOutput("db_data_print"),
              DT::DTOutput("db_summySel")
              # DTOutput("db_zoomyDT", width = "100%"),
              # DTOutput("db_zoomytable")
              # hr(),
              # verbatimTextOutput("db_testysquid")
            )
          )
        )
      )
    ),
    shinybusy::add_busy_gif(src = "https://jeroen.github.io/images/banana.gif",
                            height = 70,
                            width = 70,
                            position = "bottom-left")
  )
)