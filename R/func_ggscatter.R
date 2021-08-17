
##--------------------------------------------------------------------------
##  Summary scatter plot building function                                --
##--------------------------------------------------------------------------

ggscatter <- function(data_shared, x_var, y_var, color_var, palette_name,
                      size = NULL, alpha = NULL,
                      show_vert_guides = FALSE, vert_guides = c(5,20),
                      show_horiz_guides = FALSE, horiz_guides = c(0,0),
                      x_is_log = FALSE, custom_data = "well_id",
                      show_legend = TRUE) {
  if (!is.logical(x_is_log)) {
    stop("Argument x_is_log must be TRUE or FALSE.")
  }
  
  # Expression for generating hover tooltip (see text aesthetic below)
  tootip_glue_string <- quote(
    glue::glue(
      "<em>Plate Well: {well} </em><br>",
      "<b>Buffer: </b> {Buffer_mM}mM {Buffer}<br>",
      "<b>Salt: </b> {`Buffer Salt`} {`Buffer Salt_mM`}mM<br>"
    )
  )

  p <- ggplot2::ggplot(
    # A shared data object passed to the function
    data = data_shared,
    # Aesthetic mapping, using aes and the .data pronoun,
    # see https://dplyr.tidyverse.org/articles/programming.html
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      # customdata is a special plotly field to be passed on in event_data calls
      customdata = .data[[custom_data]],
      # aesthetic used as the tooltip; assigned in plotly::ggplotly(tooltip = .)
      text = eval(tootip_glue_string)
    )
  ) +
    # Scatter plot
    ggplot2::geom_point(
      ggplot2::aes(color = .data[[color_var]]),
      size = size,
      alpha = alpha
    ) +
    # Color palette; see R/func_paletteGenerator.R
    ggplot2::scale_color_manual(
      values = mycolors(
        palette_name,
        length(unique(data_shared$origData()[[color_var]]))
      )
    ) +
    # Calling this fixes strange behavior with plotly::ggplotly() axis scaling
    ggplot2::scale_y_continuous()
  
  # Vertical guides show/hide
  if (show_vert_guides) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = vert_guides[1],
        linetype = "dashed",
        alpha = 0.5
      ) +
      ggplot2::geom_vline(
        xintercept = vert_guides[2],
        linetype = "dashed",
        alpha = 0.5
      )
  }
  
  # Hoizontal guides show/hide
  if (show_horiz_guides) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = horiz_guides[1],
        linetype = "dashed",
        alpha = 0.5
      ) +
      ggplot2::geom_hline(
        yintercept = horiz_guides[2],
        linetype = "dashed",
        alpha = 0.5
      )
  }
  
  # Legend show/hide
  if (!(show_legend)) {
    p <- p +
      ggplot2::theme(legend.position = "none")
  }
  
  # Log transform for DLS data
  if (x_is_log) {
    p <- p +
      ggplot2::scale_x_log10()
  } else {
    p <- p +
      ggplot2::scale_x_continuous()
  }
}