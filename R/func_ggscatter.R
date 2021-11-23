
##--------------------------------------------------------------------------
##  Summary scatter plot building function                                --
##--------------------------------------------------------------------------
ggscatter <- function(data, x_var, y_var, label = NULL,
                      color_var, color_encoded = FALSE, palette_name,
                      size = NULL, alpha = NULL,
                      show_vert_guides = FALSE, vert_guides = c(5,20),
                      show_horiz_guides = FALSE, horiz_guides = c(0,0),
                      x_is_log = FALSE, custom_data = "well_id",
                      show_legend = TRUE) {
  if (!is.logical(color_encoded)) {
    stop("Argument color_encoded must be TRUE or FALSE.")
  }
  if (!is.logical(show_vert_guides)) {
    stop("Argument show_vert_guides must be TRUE or FALSE.")
  }
  if (!is.logical(show_horiz_guides)) {
    stop("Argument show_horiz_guides must be TRUE or FALSE.")
  }
  if (!is.logical(x_is_log)) {
    stop("Argument x_is_log must be TRUE or FALSE.")
  }
  if (!is.logical(show_legend)) {
    stop("Argument show_legend must be TRUE or FALSE.")
  }
  
  if (color_encoded) {
    color_var <- "color_hex"
  }
  
  if (R6::is.R6(data)) {
    data_static <- data$data()
  } else {
    data_static <- data
  }
  
  p <- ggplot2::ggplot(
    # A shared data object passed to the function
    data = data,
    # Aesthetic mapping, using aes and the .data pronoun,
    # see https://dplyr.tidyverse.org/articles/programming.html
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      # customdata is a special plotly field to be passed on in event_data calls
      customdata = bit64::as.character.integer64(.data[[custom_data]]),
      # aesthetic used as the tooltip; assigned in plotly::ggplotly(tooltip = .)
      text = eval(tootip_glue_string)
    )
  ) +
    # Scatter plot
    ggplot2::geom_point(
      ggplot2::aes(color = .data[[color_var]]),
      size = size,
      alpha = alpha
    )# +
    # Calling this fixes strange behavior with plotly::ggplotly() axis scaling
    # ggplot2::scale_y_continuous()
  
  # Apply color
  if (color_encoded) {
    p <- p +
      # Hardcoded color palette; see R/func_paletteGenerator.R
      ggplot2::scale_color_identity(
        name = color_var,
        breaks = mycolors(
          palette_name,
          length(unique(data_static[[color_var]]))
        ),
        labels = levels(data_static[[color_var]]),
        guide = "legend"
      )
  } else {
    p <- p +
      # Color palette; see R/func_paletteGenerator.R
      ggplot2::scale_color_manual(
        values = mycolors(
          palette_name,
          length(unique(data_static[[color_var]]))
        )
      )
  }
  
  # Add labels
  if (shiny::isTruthy(label)) {
    p <- p +
      ggplot2::geom_text(
        aes(label = .data[[label]]),
        hjust = 0,
        vjust = 1
      )
  }
  
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
  
  return(p)
}