
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
  ##  SHARED VALUES                                      --
  ##-------------------------------------------------------
  
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ##  Global                              >>
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # Instantiates a global reactive values object to share amongst modules;
  # is is passed as an argument to the moduleServer functions
  grv <- shiny::reactiveValues()
  
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ##  Scatter plot options                >>
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  grv$scatter_opts <- shiny::reactiveValues()
  
  
  ##-------------------------------------------------------
  ##  TESTING                                            --
  ##-------------------------------------------------------
  # Performance profiler
  # callModule(profvis::profvis_server, "profiler")
  
  # Navigate directly to plots when in testing mode
  if (use_testing_mode) {
    grv$data <- shiny::reactive({test_data})
    updateNavbarPage(inputId = "dashboard_navbar", selected = "tab_scatter")
  }
  
  
  ##-------------------------------------------------------
  ##  POSTGRES DATABASE                                  --
  ##-------------------------------------------------------
  
  ##////////////////////////////////////////
  ##  Query module                        //
  ##////////////////////////////////////////
  dbQueryServer("db_query", grv, db_pool_obj)
  
  ##////////////////////////////////////////
  ##  Print module                        //
  ##////////////////////////////////////////
  dbViewServer("db_view", grv)
  
  ##////////////////////////////////////////
  ##  Diagnostics module                  //
  ##////////////////////////////////////////
  dbDiagServer("db_diag", db_pool_obj)
  
  
  
  ##-------------------------------------------------------
  ##  VISUALIZATION                                      --
  ##-------------------------------------------------------
  
  ##////////////////////////////////////////
  ##  Scatter options module              //
  ##////////////////////////////////////////
  plotOptsServer("opts_scatter", grv$scatter_opts)
  
  ##////////////////////////////////////////
  ##  Scatter plots module                //
  ##////////////////////////////////////////
  scatterPlotsServer("scatter", grv)
  
  ##////////////////////////////////////////
  ##  Plate inspector module              //
  ##////////////////////////////////////////
  plateInspectorServer(
    "inspector",
    grv
  )
  
  
  
  ##-------------------------------------------------------
  ##  ABOUT                                              --
  ##-------------------------------------------------------
  
  ##////////////////////////////////////////
  ##  About module                        //
  ##////////////////////////////////////////
  aboutServer("about")
  
  
  
}
##--------------------------------------------------------------------------
##  end server.R                                                          --
##--------------------------------------------------------------------------
