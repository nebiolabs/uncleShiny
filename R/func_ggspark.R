
##-------------------------------------------------------------------------
##  Plot spectra sparklines                                              --
##-------------------------------------------------------------------------

ggspark <- function(data, spec_var, spec_name, x_var, y_var, summary_var,
                    palette_name = "Spectral", color_n = 1, alpha = 0.6) {
  ##----------------------------------------
  ##  Sparkline ggplot theme              --
  ##----------------------------------------
  # ggplot theme
  sparklineTheme <- function() {
    list(
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(face = "bold"),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(),
        axis.line.y = ggplot2::element_blank()
      )
    )
  }
  
  # color palette to use for geom_area fills
  sparklineColors <- mycolors(palette_name, 6)
  
  
  ##-----------------------------------------
  ##  Data-derived values                  --
  ##-----------------------------------------
  # derived values from the data
  if (spec_var %in% colnames(data)) {
    spec_df <- data[[spec_var]][[1]]
  } else {
    spec_df <- NULL
  }

  # x intercept
  if (is.na(summary_var)) {
    summary_val <- NA_real_
  } else {
    summary_val <- data[[summary_var]][[1]]
  }

  # y on spectra curve at x intercept
  if (shiny::isTruthy(spec_df) & shiny::isTruthy(summary_val)) {
    nearest_y <- spec_df[which(
      abs(spec_df[[x_var]] - summary_val) == 
        min(abs(spec_df[[x_var]] - summary_val))
    ), ][[y_var]]
  } else {
    nearest_y <- NULL
  }
  
  # limiting data range to quell ggplot errors
  if (grepl("specDLS_I|specDLS_M", spec_var, perl = TRUE)) {
  spec_df <- dplyr::filter(spec_df, between(.data[[x_var]], 1, 1000))
  }
  
  ##----------------------------------------
  ##  Sparkline plot                      --
  ##----------------------------------------
  # Base plot of spectrum
  if (shiny::isTruthy(spec_df)) {
    p <- ggplot2::ggplot(
      data = spec_df,
      ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
    ) +
      ggplot2::geom_area(fill = sparklineColors[color_n], alpha = alpha) +
      ggplot2::geom_line(color = "black", alpha = alpha)
  } else {
    stop("No spectra data available.")
  }
  
  # Overlay of summary value if it is present
  if (shiny::isTruthy(nearest_y)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = summary_val,
        linetype = "dashed",
        alpha = 0.8
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = summary_val, y = nearest_y),
        color = "red",
        alpha = 0.8
      ) +
      ggplot2::geom_label(
        ggplot2::aes(
          x = summary_val,
          y = 0.9 * max(.data[[y_var]]),
          label = summary_var
        ),
        nudge_x = -0.02 * summary_val,
        hjust = 1,
        vjust = 1
      )
  }
  
  # X-axis transform and plot annotation for DLS I/M
  if (grepl("DLS_I|DLS_M", spec_var, perl = TRUE)) {
    p <- p +
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
      ggplot2::annotation_logticks(sides = "b", alpha = 0.5)
  }
  
  # X-axis transform for DLS C
  if (grepl("DLS_C", spec_var, perl = TRUE)) {
    p <- p +
      ggplot2::scale_x_log10() +
      ggplot2::annotation_logticks(sides = "b")
  }
  
  # Apply theme, add title, and return plot obj
  return(p + sparklineTheme() + ggplot2::ggtitle(spec_name))
}
