
##-------------------------------------------------------------------------
##  mod_plotOpts - plot options                                          --
##-------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
plotOptsUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3("Plot Options"),
    shiny::tabsetPanel(
      type = "pills",
      ##----------------------------------------
      ##  Global Options                      --
      ##----------------------------------------
      shiny::tabPanel(
        title = NULL,
        value = "globalOpts",
        icon = shiny::icon("globe"),
        shiny::h5("Global Plot Settings"),
        ##-----------------------
        ##  Color              --
        ##-----------------------
        shiny::selectInput(
          ns("color_global"),
          "Color points by:",
          choices = colorvarChoices,
          selected = "Buffer"
        ),
        ##-----------------------
        ##  Palette            --
        ##-----------------------
        shiny::selectInput(
          ns("palette_global"),
          shiny::HTML(
            "<a target='_blank' href='https://colorbrewer2.org'>ColorBrewer</a> Palette:"
          ),
          choices = palChoices,
          selected = "Set2"
        ),
        ##----------------------
        ##  Point size        --
        ##----------------------
        shiny::sliderInput(
          ns("size_points"),
          label = "Size of points:",
          min = 0.4,
          max = 3,
          value = 1.4,
          step = 0.1,
          ticks = TRUE
        ),
        ##-----------------------
        ##  Point alpha        --
        ##-----------------------
        shiny::sliderInput(
          ns("alpha_points"),
          label = "Opacity of points:",
          min = 0.05,
          max = 1,
          value = 0.6,
          step = 0.05,
          ticks = TRUE
        ),
        ##----------------------
        ##  Highlight mode    --
        ##----------------------
        shiny::radioButtons(
          ns("mode_highlight"),
          label = "Point highlighting mode:",
          choices = c(
            "Click+Drag" = "plotly_selected",
            "Click" = "plotly_click",
            "Hover" = "plotly_hover"
          ),
          selected = "plotly_selected",
          inline = TRUE
        ),
        ##-----------------------
        ##  Highlight color    --
        ##-----------------------
        shiny::radioButtons(
          ns("color_highlight"),
          label = "Point highlighting color:",
          choices = c(
            "Palette" = "none",
            "Red" = "red",
            "Green" = "green"
          ),
          selected = "none",
          inline = TRUE
        )
      ),
      ##----------------------------------------
      ##  Plot 1/L Options                    --
      ##----------------------------------------
      shiny::tabPanel(
        title = NULL,
        value = "plot1Opts",
        icon = shiny::icon("arrow-alt-circle-left"),
        shiny::h5("Left Plot Settings"),
        ##-----------------------
        ##  Y-var              --
        ##-----------------------
        shiny::selectInput(
          ns("yvar1"),
          "Y variable:",
          choices = yvarChoices,
          selected = "PdI"
        ),
        ##----------------------
        ##  H-guides          --
        ##----------------------
        shiny::checkboxInput(
          ns("show_guides_h1"),
          "Show hoiz. guides?",
          value = FALSE
        ),
        shiny::sliderInput(
          ns("guides_h1"),
          label = "Guide locations:",
          min = 0,
          max = 2,
          value = c(0,0.8),
          step = 1,
          ticks = FALSE
        ),
        ##-----------------------
        ##  X-var              --
        ##-----------------------
        shiny::selectInput(
          ns("xvar1"),
          "X variable:",
          choices = xvarChoices,
          selected = "Z_D"
        ),
        ##-----------------------
        ##  Log transform      --
        ##-----------------------
        shiny::checkboxInput(
          ns("xvar1_is_log"),
          "Transform axis with log()?",
          value = TRUE
        ),
        ##----------------------
        ##  V-guides          --
        ##----------------------
        shiny::checkboxInput(
          ns("show_guides_v1"),
          "Show vertical guides?",
          value = TRUE
        ),
        shiny::sliderInput(
          ns("guides_v1"),
          label = "Guide locations:",
          min = 2,
          max = 50,
          value = c(5,20),
          step = 1,
          ticks = FALSE
        )
      ),
      ##----------------------------------------
      ##  Plot 2/R Options                    --
      ##----------------------------------------
      shiny::tabPanel(
        title = NULL,
        value = "plot2Opts",
        icon = shiny::icon("arrow-alt-circle-right"),
        shiny::h5("Right Plot Settings"),
        ##-----------------------
        ##  Y-var              --
        ##-----------------------
        shiny::selectInput(
          ns("yvar2"),
          "Y variable:",
          choices = yvarChoices,
          selected = "Tagg266"
        ),
        ##----------------------
        ##  H-guides          --
        ##----------------------
        shiny::checkboxInput(
          ns("show_guides_h2"),
          "Show hoiz. guides?",
          value = FALSE
        ),
        shiny::sliderInput(
          ns("guides_h2"),
          label = "Guide locations:",
          min = 1,
          max = 100,
          value = c(10,70),
          step = 1,
          ticks = FALSE
        ),
        ##-----------------------
        ##  X-var              --
        ##-----------------------
        shiny::selectInput(
          ns("xvar2"),
          "X variable:",
          choices = xvarChoices,
          selected = "Tm1"
        ),
        ##-----------------------
        ##  Log transform      --
        ##-----------------------
        shiny::checkboxInput(
          ns("xvar2_is_log"),
          "Transform axis with log()?",
          value = FALSE
        ),
        ##----------------------
        ##  V-guides          --
        ##----------------------
        shiny::checkboxInput(
          ns("show_guides_v2"),
          "Show vertical guides?",
          value = FALSE
        ),
        shiny::sliderInput(
          ns("guides_v2"),
          label = "Guide locations:",
          min = 1,
          max = 100,
          value = c(10,70),
          step = 1,
          ticks = FALSE
        )
      ),
      ##----------------------------------------
      ##  Plot 3/Z Options                    --
      ##----------------------------------------
      shiny::tabPanel(
        title = NULL,
        value = "plot3Opts",
        icon = shiny::icon("search-plus"),
        shiny::h5("Zoomed Plot Settings"),
        ##-----------------------
        ##  Color              --
        ##-----------------------
        shiny::selectInput(
          ns("color_zoom"),
          "Color points by:",
          choices = colorvarChoices,
          selected = "Buffer"
        ),
        ##-----------------------
        ##  Y-var              --
        ##-----------------------
        shiny::selectInput(
          ns("yvar3"),
          "Y variable:",
          choices = yvarChoices,
          selected = "Tagg266"
        ),
        ##-----------------------
        ##  X-var              --
        ##-----------------------
        shiny::selectInput(
          ns("xvar3"),
          "X variable:",
          choices = xvarChoices,
          selected = "Z_D"
        ),
        ##-----------------------
        ##  Log transform      --
        ##-----------------------
        shiny::checkboxInput(
          ns("xvar3_is_log"),
          "Transform axis with log()?",
          value = TRUE
        )
      )
    )
  )
}


##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
plotOptsServer <- function(id, opts_obj) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Theme selections                    <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        opts_obj$palette_global <- input$palette_global
      })
      shiny::observe({
        opts_obj$color_global <- input$color_global
      })
      shiny::observe({
        opts_obj$color_zoom <- input$color_zoom
      })
      shiny::observe({
        opts_obj$size_points <- shiny::debounce(
          function(){input$size_points},
          2000
        )
      })
      shiny::observe({
        opts_obj$alpha_points <- shiny::debounce(
          function(){input$alpha_points},
          2000
        )
      })
      
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Functionality selections            <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        opts_obj$mode_highlight_on <- input$mode_highlight
        
        opts_obj$mode_highlight_off <- switch(
          input$mode_highlight,
          "plotly_selected" = "plotly_deselect",
          "plotly_click" = "plotly_doubleclick",
          "plotly_hover" = "plotly_doubleclick"
        )
      })
      
      shiny::observe({
        opts_obj$color_highlight <- switch(
          input$color_highlight,
          "none" = NULL,
          "red" = "red",
          "green" = "green"
        )
      })
      
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  X var selections                    <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # xvar1
      shiny::observe({
        opts_obj$xvar1 <- input$xvar1
      })
      shiny::observe({
        opts_obj$xvar1_is_log <- input$xvar1_is_log
      })
      shiny::observe({
        opts_obj$show_guides_v1 <- input$show_guides_v1
      })
      shiny::observe({
        opts_obj$guides_v1 <- shiny::debounce(
          function(){input$guides_v1},
          2000
        )
      })
      # xvar2
      shiny::observe({
        opts_obj$xvar2 <- input$xvar2
      })
      shiny::observe({
        opts_obj$xvar2_is_log <- input$xvar2_is_log
      })
      shiny::observe({
        opts_obj$show_guides_v2 <- input$show_guides_v2
      })
      shiny::observe({
        opts_obj$guides_v2 <- shiny::debounce(
          function(){input$guides_v2},
          2000
        )
      })
      # xvar3
      shiny::observe({
        opts_obj$xvar3 <- input$xvar3
      })
      shiny::observe({
        opts_obj$xvar3_is_log <- input$xvar3_is_log
      })
      
      
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##  Y var selections                    <<
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      shiny::observe({
        opts_obj$yvar1 <- input$yvar1
      })
      shiny::observe({
        opts_obj$show_guides_h1 <- input$show_guides_h1
      })
      shiny::observe({
        opts_obj$guides_h1 <- shiny::debounce(
          function(){input$guides_h1},
          2000
        )
      })
      shiny::observe({
        opts_obj$yvar2 <- input$yvar2
      })
      shiny::observe({
        opts_obj$show_guides_h2 <- input$show_guides_h2
      })
      shiny::observe({
        opts_obj$guides_h2 <- shiny::debounce(
          function(){input$guides_h2},
          2000
        )
      })
      shiny::observe({
        opts_obj$yvar3 <- input$yvar3
      })
    }
  )
}