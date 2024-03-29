
##-------------------------------------------------------------------------
##  mod_spectraViewer - spectra ridgeline plots                          --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
spectraViewerUI <- function(id) {
  ns <- NS(id)
  
  shiny::tagList(
    shiny::tabsetPanel(
      type = "pills",
      shiny::tabPanel(
        title = "Dynamic Method Spectra",
        shiny::radioButtons(
          ns("type_dynamic"),
          label = NULL,
          choices = list(
            "Intensity" = "intensity",
            "Mass" = "mass"
          ),
          selected = "intensity",
          inline = TRUE
        ),
        shiny::fluidRow(
          shiny::column(
            width = 7,
            shiny::plotOutput(ns("dls"), height = "700px")
          ),
          shiny::column(
            width = 5,
            shiny::plotOutput(ns("corr"), height = "700px")
          )
        )
      ),
      shiny::tabPanel(
        title = "Static Method Spectra",
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::plotOutput(ns("sls"), height = "700px")
          ),
          shiny::column(
            width = 6,
            shiny::plotOutput(ns("dsf"), height = "700px")
          )
        )
      )
    )
  )
}


##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
spectraViewerServer <- function(id, robj_data, robj_color_var = NULL,
                                robj_palette_name = NULL,
                                force_encoding = FALSE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      if (force_encoding) {
        show_legend <- TRUE
      } else {
        show_legend <- FALSE
      }
      
      munge_module_data <- function(data_input) {
        unnest_conflicts <- c("created_at", "updated_at")
        
        if (is.null(data_input)) {
          NULL
        } else {
          data_input |> 
            dplyr::select(-tidyselect::any_of(unnest_conflicts))
        }
      }
      
      if (is.null(robj_color_var)) {
        robj_color_var <- shiny::reactive({NA_character_})
      }
      
      if (is.null(robj_palette_name)) {
        robj_palette_name <- shiny::reactive({"Set2"})
      }
      
      ##----------------------------------------
      ##  Data instance for module            --
      ##----------------------------------------
      module_data <- shiny::reactive({
        robj_data() |> 
          munge_module_data()
      })
      
      
      ##----------------------------------------
      ##  Dynamic spectra plot                --
      ##----------------------------------------
      output$dls <- shiny::renderPlot({
        ggridgeline(
          data = module_data(),
          spec_type = "dls",
          dls_type = input$type_dynamic,
          sort_var = "Z_D",
          color_var = robj_color_var(),
          palette_name = robj_palette_name(),
          force_encoding = force_encoding,
          show_legend = show_legend
        )
      })
      
      
      ##-----------------------------------------
      ##  Correlation function plot            --
      ##-----------------------------------------
      output$corr <- shiny::renderPlot({
        ggridgeline(
          data = module_data(),
          spec_type = "corr",
          sort_var = "Z_D",
          color_var = robj_color_var(),
          palette_name = robj_palette_name(),
          force_encoding = force_encoding,
          show_legend = show_legend
        )
      })
      
      
      ##-----------------------------------------
      ##  Static spectra plot                  --
      ##-----------------------------------------
      output$sls <- renderPlot({
        ggridgeline(
          data = module_data(),
          spec_type = "sls",
          sort_var = "Z_D",
          color_var = robj_color_var(),
          palette_name = robj_palette_name(),
          force_encoding = force_encoding,
          show_legend = show_legend
        )
      })
      output$dsf <- renderPlot({
        ggridgeline(
          data = module_data(),
          spec_type = "dsf",
          sort_var = "Z_D",
          color_var = robj_color_var(),
          palette_name = robj_palette_name(),
          force_encoding = force_encoding,
          show_legend = show_legend
        )
      })
    }
  )
}
