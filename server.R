
##--------------------------------------------------------------------------
##  server.R                                                              --
##--------------------------------------------------------------------------

function(input, output, session) {
  
  ##-------------------------------------------------------
  ##  Interactive theme selection                        --
  ##-------------------------------------------------------
  # Instantiates an overlay UI for previewing bootswatch themes
  # bslib::bs_themer()
  
  # Switching between dark and light theme based on user-input
  shiny::observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) {theme_dark} else {theme_light}
  ))
  
  
  
  ##-------------------------------------------------------
  ##  Performance profiling                              --
  ##-------------------------------------------------------
  
  # callModule(profvis::profvis_server, "profiler")
  
  
  
  ##--------------------------------------------------------
  ##  Global reactive values                              --
  ##--------------------------------------------------------
  
  # Instantiates a global reactive values object to share amongst modules;
  # is is passed as an argument to the moduleServer functions
  grv <- shiny::reactiveValues()
  
  
  ##--------------------------------------------------------
  ##  Postgres database access                            --
  ##--------------------------------------------------------
  
  ##----------------------------------------
  ##  Database querying module            --
  ##----------------------------------------
  dbQueryServer("ebase_query", grv, ebase_dev)
  
  
  ##-----------------------------------------
  ##  Result viewing module                --
  ##-----------------------------------------
  dbViewServer("ebase_view", grv)
  
  
  ##-------------------------------------------------------
  ##  Summary scatter plot module                        --
  ##-------------------------------------------------------
  
  ##----------------------------------------
  ##  Plot options reactive values        --
  ##----------------------------------------
  scatter_opts <- shiny::reactiveValues()
  
  ##-----------------------------------------
  ##  Plot options module                  --
  ##-----------------------------------------
  plotOptsServer("opts_scatter", scatter_opts, grv)
  
  ##----------------------------------------
  ##  Scatter module                      --
  ##----------------------------------------
  scatterPlotsServer(
    "scatter",
    scatter_opts,
    grv,
    grv$robj_collected_SharedData()
    # test_shared # for testing without database connection
  )
}
