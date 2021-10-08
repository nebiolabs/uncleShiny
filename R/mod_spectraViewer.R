
##-------------------------------------------------------------------------
##  mod_spectraViewer - spectra ridgeline plots                          --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
spectraViewerUI <- function(id) {
  ns <- NS(id)
  
  shiny::tagList(
    shiny::sidebarLayout(
      ##----------------------------------------
      ##  Side panel                          --
      ##----------------------------------------
      shiny::sidebarPanel(
        width = 2,
        shiny::h5("Well Exclusion:"),
        shiny::selectInput(
          ns("filter_wells"),
          label = NULL,
          choices = c(" ", wellOrder),
          selected = NULL,
          multiple = TRUE,
          selectize = TRUE
        )
      ),
      ##----------------------------------------
      ##  Main panel                          --
      ##----------------------------------------
      shiny::mainPanel(
        width = 10,
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::h4("Dynamic Method Spectra"),
            shiny::radioButtons(
              ns("type_dynamic"),
              label = NULL,
              choices = list(
                "Intensity" = "intensity",
                "Mass" = "mass"
              ),
              selected = "intensity",
              inline = TRUE
            )
          ),
          shiny::column(
            width = 6,
            shiny::h4("Static Method Spectra")
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::plotOutput(ns("dls"), height = "800px")
          ),
          shiny::column(
            width = 2,
            shiny::plotOutput(ns("corr"), height = "800px")
          ),
          shiny::column(
            width = 3,
            shiny::plotOutput(ns("sls"), height = "800px")
          ),
          shiny::column(
            width = 3,
            shiny::plotOutput(ns("dsf"), height = "800px")
          )
        ) 
      )
    )
  )
}


##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
spectraViewerServer <- function(id, grv) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ##----------------------------------------
      ##  Data instance for module            --
      ##----------------------------------------
      unnest_conflicts <- c("created_at", "updated_at")
      
      if (use_testing_mode) {
        module_data <- shiny::reactive({
          # shiny::req(grv$scatter_selected_summary_ids())
          test_data |> 
            dplyr::select(-tidyselect::any_of(unnest_conflicts)) |> 
            dplyr::filter(
              uncle_summary_id %in% 
                grv$scatter_selected_summary_ids()[["summary_ids"]]
            )
        })
      } else {
        module_data <- shiny::reactive({
          # shiny::req(grv$scatter_selected_summary_ids())
          grv$robj_collected_data() |> 
            dplyr::select(-tidyselect::any_of(unnest_conflicts)) |> 
            dplyr::filter(
              uncle_summary_id %in% 
                grv$scatter_selected_summary_ids()[["summary_ids"]]
            )
        })
      }
      
      
      ##----------------------------------------
      ##  Dynamic spectra plot                --
      ##----------------------------------------
      output$dls <- shiny::renderPlot({
        ggridgeline(
          data = module_data(),
          spec_type = "dls",
          dls_type = input$type_dynamic,
          facet_var = "well",
          color_var = "well",
          palette_name = "Set2",
          show_legend = TRUE
        )
      })
      
      
      ##-----------------------------------------
      ##  Correlation function plot            --
      ##-----------------------------------------
      output$corr <- shiny::renderPlot({
        ggridgeline(
          data = module_data(),
          spec_type = "corr",
          facet_var = "well",
          color_var = "well",
          palette_name = "Set2",
          show_legend = FALSE
        )
      })
      
      
      ##-----------------------------------------
      ##  Static spectra plot                  --
      ##-----------------------------------------
      output$sls <- renderPlot({
        ggridgeline(
          data = module_data(),
          spec_type = "sls",
          facet_var = "well",
          color_var = "well",
          palette_name = "Set2",
          show_legend = TRUE
        )
      })
      output$dsf <- renderPlot({
        ggridgeline(
          data = module_data(),
          spec_type = "dsf",
          facet_var = "well",
          color_var = "well",
          palette_name = "Set2",
          show_legend = FALSE
        )
      })
    }
  )
}
