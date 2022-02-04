
## --------------------------------------------------------------------------
##  ui.R                                                                  --
## --------------------------------------------------------------------------
  shiny::tagList(
    # Adds the NEB logo to the navbar
    tags$head(tags$script(type = "text/javascript", src = "code.js")),
    tags$style(type = "text/css", "body {padding-top: 60px;}"),
    ##-------------------------------------------------------
    ##  NAVBAR PAGE                                        --
    ##-------------------------------------------------------
    bslib::page_navbar(
      title = "Uncle Dashboard",
      id = "dashboard_navbar",
      selected = "tab_database",
      position = "fixed-top",
      ##----------------------------------------
      ##  Dark mode controls                  --
      ##----------------------------------------
      # Theme default on app load
      theme = theme_light,
      bg = "#073642",
      header = shiny::tagList(
        # shiny::div(style = "display: inline-block", shiny::icon("adjust")),
        # shiny::div(style = "display: inline-block", " "),
        shiny::div(
          style = "display: inline-block",
          shinyWidgets::prettySwitch(
            inputId = "dark_mode",
            label = "Dark mode.",
            value = FALSE # set to match the default theme on app load
          )
        ),
      shiny::div(
        style = "display: inline-block",
        shiny::em("DATA WARNING: The app is currently pointed at ebase dev!")
      )
      ),
      ##--------------------------------------------------------
      ##  NAVBAR TAB: DATABASE                                --
      ##--------------------------------------------------------
      shiny::tabPanel(
        "Database Connection",
        icon = shiny::icon("database"),
        value = "tab_database",
        shiny::sidebarLayout(
          ##----------------------------------------
          ##  Side panel                          --
          ##----------------------------------------
          shiny::sidebarPanel(
            width = 2,
            dbQueryUI("db_query")
          ),
          ##----------------------------------------
          ##  Main panel                          --
          ##----------------------------------------
          shiny::mainPanel(
            width = 10,
            dbViewUI("db_view")
          )
        )
      ),
      ##-------------------------------------------------------
      ##  NAVBAR TAB: SUMMARY SCATTER PLOTS                  --
      ##-------------------------------------------------------
      shiny::tabPanel(
        "Summary Data Visualization",
        icon = shiny::icon("braille"),
        value = "tab_scatter",
        shiny::sidebarLayout(
          ##----------------------------------------
          ##  Side panel                          --
          ##----------------------------------------
          shiny::sidebarPanel(
            width = 2,
            shiny::tabsetPanel(
              # id = "sidebarPanel_scatter",
              type = "tabs",
              selected = "tab_opts_scatter",
              shiny::tabPanel(
                title = "Filters",
                icon = shiny::icon("filter"),
                value = "tab_data_filters",
                dataFiltersUI("filters")
              ),
              shiny::tabPanel(
                title = "Options",
                icon = shiny::icon("palette"),
                value = "tab_opts_scatter",
                plotOptsUI("opts_scatter")
              )
            )
          ),
          ##----------------------------------------
          ##  Main panel                          --
          ##----------------------------------------
          shiny::mainPanel(
            width = 10,
            scatterPlotsUI("scatter")
          )
        )
      ),
      ##-------------------------------------------------------
      ##  NAVBAR TAB: PLATE LAYOUT SAMPLE INSPECTOR          --
      ##-------------------------------------------------------
      shiny::tabPanel(
        "Plate Inspector",
        icon = shiny::icon("search"),
        value = "tab_inspector",
        plateInspectorUI("inspector")
      ),
      ##--------------------------------------------------------
      ##  NAVBAR DROPDOWN: TOOLS                              --
      ##--------------------------------------------------------
      shiny::navbarMenu(
        "Tools",
        icon = shiny::icon("tools"),
        shiny::tabPanel(
          "Database Troubleshooting",
          icon = shiny::icon("database"),
          value = "tab_diag",
          dbDiagUI("db_diag")
        )
      ),
      ##-------------------------------------------------------
      ##  NAVBAR TAB: ABOUT THE APP                          --
      ##-------------------------------------------------------
      shiny::tabPanel(
        "About this App",
        icon = shiny::icon("info-circle"),
        value = "tab_about",
        aboutUI("about")
      )
    )
  )
##--------------------------------------------------------------------------
##  end ui.R                                                              --
##--------------------------------------------------------------------------
