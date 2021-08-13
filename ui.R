
## --------------------------------------------------------------------------
##  ui.R                                                                  --
## --------------------------------------------------------------------------

tagList(
  # Adds the NEB logo to the navbar
  tags$head(tags$script(type = "text/javascript", src = "code.js")),
  ##--------------------------------------------------------
  ##  Navbar tab: database                                --
  ##--------------------------------------------------------
  navbarPage(
    title = "Uncle Dashboard",
    # theme = shinytheme("yeti"),
    theme = theme_light,
    ## ----------------------------------------
    ##  Navbar tab: database                --
    ## ----------------------------------------
    tabPanel(
      "Database Connection",
      icon = icon("database"),
      value = "database",
      sidebarLayout(
        ## ----------------------------------------
        ##  Side panel                          --
        ## ----------------------------------------
        sidebarPanel(
          width = 2,
          dbQueryUI("ebase_query")
        ),
        ## ----------------------------------------
        ##  Main panel                          --
        ## ----------------------------------------
        mainPanel(
          width = 9,
          dbViewUI("ebase_view")
        )
      )
    ),
    ##-------------------------------------------------------
    ##  Navbar tab: visualization                          --
    ##-------------------------------------------------------
    tabPanel(
      "Data Visualization",
      icon = icon("braille"),
      value = "visualization",
      ## ----------------------------------------
      ##  Main panel                          --
      ## ----------------------------------------
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
              helpText(
                "These data points are called by the Uncle.",
                "Anomalous raw spectra can lead to unreliable values.",
                "Thus, please note that not all conditions tested may appear here,",
                "and it is good practice to inspect the raw spectra after narrowing conditions of interest."
              )
            ),
            column(
              width = 3,
              h4("Spectra QuickView:"),
              helpText(
                "Hover over a point to view sparklines of raw spectra.",
                "Click a point to freeze those spectra for comparison.",
                "Double-click to un-freeze."
              )
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
              helpText(
                "This plot shows a zoomed-in version of the points selected above,",
                "which you can further dissect with differnt axis variables using the options to the left.",
                "The table below shows the details for the points selected here.",
                "This plot and the table are live and interconnected;",
                "selecting points here will filter the table."
              )
            ),
            column(
              width = 6,
              h4("Spectra Overlay"),
              helpText(
                "This module creates ridgeline overlays of",
                "the raw spectra corresponding to the selection",
                "made on the zommed-in plot to the left."
              ),
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
              helpText(
                "This is an interactive table.",
                "The rows can be filtered using the search bar, or sorted using the header row arrow buttons.",
                "It shows details for what appears in the 'Zoom' plot above.",
                "Selecting points there will filter what is shown here."
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
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
    shiny::br(),
    shiny::icon("adjust"),
    shinyWidgets::prettySwitch(
      inputId = "dark_mode",
      label = NULL,
      value = TRUE,
      slim = TRUE,
      bigger = TRUE,
      inline = TRUE,
      width = "42px"
    ),
    ##----------------------------------------
    ##  Busy indicator                      --
    ##----------------------------------------
    shinybusy::add_busy_gif(
      src = "https://jeroen.github.io/images/banana.gif",
      height = 70,
      width = 70,
      position = "bottom-right"
    )
  )
)