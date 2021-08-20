
## --------------------------------------------------------------------------
##  ui.R                                                                  --
## --------------------------------------------------------------------------

shiny::tagList(
  # Adds the NEB logo to the navbar
  tags$head(tags$script(type = "text/javascript", src = "code.js")),
  ##--------------------------------------------------------
  ##  Navbar tab: database                                --
  ##--------------------------------------------------------
  shiny::navbarPage(
    title = "Uncle Dashboard",
    id = "dashboard_navbar",
    theme = theme_dark,
    ## ----------------------------------------
    ##  Navbar tab: database                --
    ## ----------------------------------------
    shiny::tabPanel(
      "Database Connection",
      icon = shiny::icon("database"),
      value = "tab_database",
      shiny::sidebarLayout(
        ## ----------------------------------------
        ##  Side panel                          --
        ## ----------------------------------------
        shiny::sidebarPanel(
          width = 2,
          dbQueryUI("ebase_query")
        ),
        ## ----------------------------------------
        ##  Main panel                          --
        ## ----------------------------------------
        shiny::mainPanel(
          width = 10,
          dbViewUI("ebase_view")
        )
      )
    ),
    ##-------------------------------------------------------
    ##  Navbar tab: scatter plots                          --
    ##-------------------------------------------------------
    shiny::tabPanel(
      "Summary Scatter Plots",
      icon = shiny::icon("braille"),
      value = "tab_scatter",
      shiny::sidebarLayout(
        ## ----------------------------------------
        ##  Side panel                          --
        ## ----------------------------------------
        shiny::sidebarPanel(
          width = 2,
          plotOptsUI("opts_scatter")
        ),
        ## ----------------------------------------
        ##  Main panel                          --
        ## ----------------------------------------
        shiny::mainPanel(
          width = 10,
          scatterPlotsUI("scatter")
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
##--------------------------------------------------------------------------
##  end ui.R                                                              --
##--------------------------------------------------------------------------
