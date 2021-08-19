
##--------------------------------------------------------------------------
##  server.R                                                              --
##--------------------------------------------------------------------------

function(input, output, session) {
  ##-------------------------------------------------------
  ##  THEME SELECTION                                    --
  ##-------------------------------------------------------
  # Instantiates an overlay UI for previewing bootswatch themes
  # bslib::bs_themer()
  
  # Switching between dark and light theme based on user-input
  shiny::observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) {theme_dark} else {theme_light}
  ))
  
  
  
  ##-------------------------------------------------------
  ##  PERFORMANCE PROFILING                              --
  ##-------------------------------------------------------
  # callModule(profvis::profvis_server, "profiler")
  
  
  
  
  ##-------------------------------------------------------
  ##  SHARED VALUES                                      --
  ##-------------------------------------------------------

  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ##  Global                              >>
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # Instantiates a global reactive values object to share amongst modules;
  # is is passed as an argument to the moduleServer functions
  grv <- shiny::reactiveValues()
  # grv$testy_data <- reactive({test_data})
  
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ##  Scatter plot options                >>
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  scatter_opts <- shiny::reactiveValues()
  
  
  
  ##-------------------------------------------------------
  ##  POSTGRES DATABASE                                  --
  ##-------------------------------------------------------
  
  ##////////////////////////////////////////
  ##  Query module                        //
  ##////////////////////////////////////////
  dbQueryServer("ebase_query", grv, ebase_dev)
  
  ##////////////////////////////////////////
  ##  Print module                        //
  ##////////////////////////////////////////
  dbViewServer("ebase_view", grv)
  
  
  
  ##-------------------------------------------------------
  ##  VISUALIZATION                                      --
  ##-------------------------------------------------------
  
  ##////////////////////////////////////////
  ##  Scatter options module              //
  ##////////////////////////////////////////
  plotOptsServer("opts_scatter", scatter_opts, grv)
  
  ##////////////////////////////////////////
  ##  Scatter plots module                //
  ##////////////////////////////////////////
  scatterPlotsServer(
    "scatter",
    scatter_opts,
    grv,
    grv$robj_collected_SharedData()
    # test_shared # for testing without database connection
  )
  
}
##--------------------------------------------------------------------------
##  end server.R                                                          --
##--------------------------------------------------------------------------
