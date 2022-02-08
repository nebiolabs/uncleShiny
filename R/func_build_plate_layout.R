
##--------------------------------------------------------------------------
##  Construct plate layout with plotly                                    --
##--------------------------------------------------------------------------

build_plate_layout <- function(format = NULL, overlay_data = NULL, 
                              source = NULL, customdata = "well",
                              color = "color_hex", palette_name = "Set2",
                              tooltip = "well") {
  base_plate <- make_base_plate(format = format)
  
  if (is.null(overlay_data)) {
    overlay_data <- dplyr::slice_sample(base_plate, n = 24)
  }
  
  p <- plotly::plot_ly(
    data = base_plate,
    type = "scatter",
    mode = "markers",
    x = ~well_number,
    y = ~well_letter,
    marker = list(
      symbol = "circle",
      size = 25,
      color = NA,
      opacity = 0.1,
      line = list(
        color = "black",
        width = 2
      )
    ),
    text = ~well,
    hoverinfo = "text",
    source = source
  )
  
  if (shiny::isTruthy(overlay_data)) {
    palette_colors <- make_palette(
      palette_name,
      length(unique(overlay_data[[color]]))
    )
    overlay_split <- dplyr::mutate(
      overlay_data,
      !!color := forcats::fct_rev(forcats::fct_infreq(.data[[color]]))
    ) |> 
      dplyr::group_split(.data[[color]])
    for (i in seq_along(palette_colors)) {
      p <- plotly::add_trace(
        p,
        data = overlay_split[[i]],
        inherit = FALSE,
        type = "scatter",
        mode = "markers",
        x = ~well_number,
        y = ~well_letter,
        showlegend = FALSE,
        marker = list(
          symbol = "circle",
          size = 20,
          color = palette_colors[i],
          opacity = 0.9
        ),
        text = rlang::new_formula(NULL, rlang::sym(tooltip)),
        hoverinfo = "text",
        customdata = rlang::new_formula(NULL, rlang::sym(customdata)),
        selected = list(
          marker = list(
            color = "red",
            alpha = 0.5
          )
        )
      )
    }
  }
  
  # if (shiny::isTruthy(overlay_data)) {
  #   p <- p |>
  #     plotly::add_trace(
  #       data = overlay_data,
  #       inherit = FALSE,
  #       type = "scatter",
  #       mode = "markers",
  #       x = ~well_number,
  #       y = ~well_letter,
  #       color = rlang::new_formula(NULL, rlang::sym(color)),
  #       showlegend = FALSE,
  #       marker = list(
  #         symbol = "circle",
  #         size = 20,
  #         opacity = 1
  #       ),
  #       text = as.formula(paste0("~", tooltip)),
  #       hoverinfo = "text",
  #       customdata = rlang::new_formula(NULL, rlang::sym(customdata)),
  #       selected = list(
  #         marker = list(
  #           color = "red"
  #         )
  #       )
  #     )
  # }
  
  p |> 
    plotly::layout(
      modebar = list(
        orientation = "v"
      ),
      dragmode = "select",
      yaxis = list(
        title = NA,
        showgrid = TRUE,
        tickson = "boundaries",
        zeroline = FALSE,
        showline = FALSE,
        autorange = "reversed",
        categoryorder = "array",
        categoryarray = LETTERS[1:8]
      ),
      xaxis = list(
        type = "category",
        side = "top",
        title = NA,
        showgrid = TRUE,
        tickson = "boundaries",
        zeroline = FALSE,
        showline = FALSE,
        tickmode = "array",
        # tickvals = c(1:12),
        ticktext = as.character(c(1:12)),
        categoryorder = "array",
        categoryarray = as.character(c(1:12))
      )
    ) |>
    plotly::config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("zoom", "pan", "zoomIn", "zoomOut", "toImage",
                                 "resetScale")
    ) |> 
    plotly::event_register("plotly_selected")
}
