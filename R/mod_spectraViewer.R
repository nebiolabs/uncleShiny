
##-------------------------------------------------------------------------
##  mod_spectraViewer - spectra ridgeline plots                          --
##-------------------------------------------------------------------------

ridgelineTheme <- function() {
  list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(hjust = 0.95, size = 15),
      axis.text = ggplot2::element_text(size = 11),
      axis.text.y = ggplot2::element_text(face = "bold"),
      axis.line.x.bottom = ggplot2::element_line(),
      plot.margin = ggplot2::margin(0.5,0.5,0.5,0.5, "cm"),
      legend.position = "left",
      legend.justification = "top"
    )
  )
}

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
            width = 5,
            shiny::h4("Dynamic Method Spectra"),
            shinyWidgets::radioGroupButtons(
              ns("type_dynamic"),
              label = NULL,
              choices = list(
                "Intensity" = "specDLS_I",
                "Mass" = "specDLS_M"
              ),
              selected = "specDLS_I"
            )
          ),
          shiny::column(
            width = 5,
            offset = 2,
            shiny::h4("Static Method Spectra"),
            shinyWidgets::radioGroupButtons(
              ns("type_static"),
              label = NULL,
              # choiceNames = list("SLS", "nanoDSF"),
              # choiceValues = list(
              #   rlang::expr(c("specSLS266", "specSLS473")),
              #   rlang::expr("specTm")
              # )
              # choices = list(
              #   "SLS" = c("specSLS266", "specSLS473"),
              #   "nanoDSF" = "specTm"
              # )
              choices = list(
                "SLS",
                "nanoDSF"
              )
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 5,
            shiny::plotOutput(ns("dls"), height = "700px")
          ),
          shiny::column(
            width = 2,
            shiny::plotOutput(ns("corr"), height = "700px")
          ),
          shiny::column(
            width = 5,
            shiny::plotOutput(ns("sls"), height = "700px")
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
        # shiny::req(module_data())
        xvar <- do.call(switch, as.list(c(input$type_dynamic, spec_x_switch)))
        yvar <- do.call(switch, as.list(c(input$type_dynamic, spec_y_switch)))
        module_data() |>
          tidyr::unnest(input$type_dynamic) |>
          ggplot2::ggplot(ggplot2::aes(x = .data[[xvar]])) +
          ggridges::geom_ridgeline(
            ggplot2::aes(
              y = .data[["well"]],
              height = .data[[yvar]],
              fill = .data[["well"]]
            ),
            scale = 0.9,
            alpha = 0.7
          ) +
          ggplot2::annotate(
            "rect",
            xmin = 1,
            xmax = 2,
            ymin = 0,
            ymax = Inf,
            fill = "yellow",
            alpha = 0.1
          ) +
          ggplot2::annotate(
            "rect",
            xmin = 20,
            xmax = Inf,
            ymin = 0,
            ymax = Inf,
            fill = "red",
            alpha = 0.1
          ) +
          ggplot2::geom_vline(xintercept = 2, linetype = "dotted", alpha = 0.5) +
          ggplot2::geom_vline(xintercept = 20, linetype = "dotted", alpha = 0.5) +
          ggplot2::scale_x_log10(
            limits = c(1, 1000),
            breaks = c(1, 2, 5, 20, 100, 1000), 
            labels = scales::label_comma(accuracy = 1)
          ) +
          ggplot2::annotation_logticks(sides = "b", alpha = 0.5) + 
          ridgelineTheme()
      })
      
      
      ##-----------------------------------------
      ##  Correlation function plot            --
      ##-----------------------------------------
      output$corr <- shiny::renderPlot({
        xvar <- do.call(switch, as.list(c("specDLS_C", spec_x_switch)))
        yvar <- do.call(switch, as.list(c("specDLS_C", spec_y_switch)))
        module_data() |>
          tidyr::unnest("specDLS_C") |>
          ggplot2::ggplot(ggplot2::aes(x = .data[[xvar]])) +
          ggridges::geom_ridgeline(
            ggplot2::aes(
              y = .data[["well"]],
              height = .data[[yvar]],
              fill = .data[["well"]]
            ),
            scale = 0.9,
            alpha = 0.7,
            show.legend = FALSE
          ) +
          ggplot2::scale_x_log10() +
          ggplot2::annotation_logticks(sides = "b") + 
          ridgelineTheme()
      })
      
      
      ##-----------------------------------------
      ##  Static spectra plot                  --
      ##-----------------------------------------
      output$sls <- renderPlot({
        # shiny::req(module_data())
        
        if (input$type_static == "SLS") {
          module_data() |>
            tidyr::unnest(
              cols = c("specSLS266", "specSLS473"),
              names_sep = "_"
            ) |> 
            dplyr::mutate(temperature = map2_dbl(
              specSLS266_temperature,
              specSLS473_temperature,
              \(x, y) median(c(x,y))
            )) |> 
            ggplot2::ggplot(ggplot2::aes(
              x = .data[["temperature"]],
              y = .data[["well"]],
              color = .data[["well"]]
            )) +
            ggridges::geom_ridgeline(
              ggplot2::aes(height = .data[["specSLS266_sls_266"]]),
              fill = NA,
              stat = "identity",
              linetype = "solid",
              show.legend = TRUE
            ) +
            ggridges::geom_ridgeline(
              ggplot2::aes(height = .data[["specSLS473_sls_473"]]),
              fill = NA,
              stat = "identity",
              linetype = "dashed",
              show.legend = TRUE
            ) +
            ridgelineTheme()
        } else if (input$type_static == "nanoDSF") {
          module_data() |>
            tidyr::unnest(
              cols = c("specTm"),
              names_sep = "_"
            ) |> 
            dplyr::rename(temperature = specTm_temperature) |> 
            ggplot2::ggplot(ggplot2::aes(
              x = .data[["temperature"]],
              y = .data[["well"]],
              color = .data[["well"]]
            )) +
            ggridges::geom_ridgeline(
              ggplot2::aes(height = .data[["specTm_bcm"]]),
              fill = NA,
              stat = "identity",
              linetype = "solid",
              show.legend = TRUE
            ) +
            ridgelineTheme()
        } else {
          return(NULL)
        }
      })
    }
  )
}
