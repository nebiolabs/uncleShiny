
## --------------------------------------------------------------------------
##  ui.R                                                                  --
## --------------------------------------------------------------------------

shiny::tagList(
  # Adds the NEB logo to the navbar
  tags$head(tags$script(type = "text/javascript", src = "code.js")),
  ##-------------------------------------------------------
  ##  NAVBAR PAGE                                        --
  ##-------------------------------------------------------
  shiny::navbarPage(
    title = "Uncle Dashboard",
    id = "dashboard_navbar",
    selected = "tab_database",
    ##----------------------------------------
    ##  Dark mode controls                  --
    ##----------------------------------------
    # Theme default on app load
    theme = theme_dark,
    shiny::div(style = "display: inline-block", shiny::icon("adjust")),
    shiny::div(style = "display: inline-block", " "),
    shiny::div(
      style = "display: inline-block",
      shinyWidgets::prettySwitch(
        inputId = "dark_mode",
        label = "Dark mode.",
        value = TRUE # set to match the default theme on app load
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
          dbQueryUI("ebase_query")
        ),
        ##----------------------------------------
        ##  Main panel                          --
        ##----------------------------------------
        shiny::mainPanel(
          width = 10,
          dbViewUI("ebase_view")
        )
      )
    ),
    ##-------------------------------------------------------
    ##  NAVBAR TAB: SUMMARY SCATTER PLOTS                  --
    ##-------------------------------------------------------
    shiny::tabPanel(
      "Summary Scatter Plots",
      icon = shiny::icon("braille"),
      value = "tab_scatter",
      shiny::sidebarLayout(
        ##----------------------------------------
        ##  Side panel                          --
        ##----------------------------------------
        shiny::sidebarPanel(
          width = 2,
          plotOptsUI("opts_scatter")
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
    ##  NAVBAR TAB: SPECTRA RIDGELINE PLOTS                --
    ##-------------------------------------------------------
    shiny::tabPanel(
      "Spectra Ridgeline Plots",
      icon = shiny::icon("chart-area"),
      value = "tab_ridgeline",
      shiny::sidebarLayout(
        ##----------------------------------------
        ##  Side panel                          --
        ##----------------------------------------
        shiny::sidebarPanel(
          width = 2,
          "Nothing to see here.."
        ),
        ##----------------------------------------
        ##  Main panel                          --
        ##----------------------------------------
        shiny::mainPanel(
          width = 10,
          spectraViewerUI("ridgeline")
        )
      )
    ),
    shiny::navbarMenu(
      "Tools",
      icon = shiny::icon("tools"),
      shiny::tabPanel(
        "Database Troubleshooting",
        icon = shiny::icon("database")
      )
    )
  )
)
##--------------------------------------------------------------------------
##  end ui.R                                                              --
##--------------------------------------------------------------------------
