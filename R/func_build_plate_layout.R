
##--------------------------------------------------------------------------
##  Construct plate layout with plotly                                    --
##--------------------------------------------------------------------------

build_plate_layout <- function(format = NULL, overlay_data = NULL, 
                              source = NULL, customdata = "well",
                              color_var = "color_hex", palette_name = "Set2",
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
    overlay_split <- dplyr::group_split(overlay_data, .data[[color_var]])
    
    for (i in seq_along(overlay_split)) {
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
          color = unique(overlay_split[[i]][["color_hex"]]),
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
