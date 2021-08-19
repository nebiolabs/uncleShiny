
##-------------------------------------------------------------------------
##  Plot spectra sparklines                                              --
##-------------------------------------------------------------------------

ggspark <- function(data, spec_var, x_var, y_var, summary_var,
                    palette_name = "Spectral", color_n = 1, alpha = 0.6) {
  ##----------------------------------------
  ##  Sparkline ggplot theme              --
  ##----------------------------------------
  # ggplot theme
  sparklineTheme <- function() {
    list(
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_blank()
      )
    )
  }
  
  # color palette to use for geom_area fills
  sparklineColors <- mycolors(palette_name, 6)
  
  
  ##-----------------------------------------
  ##  Data-derived values                  --
  ##-----------------------------------------
  # derived values from the data
  spec_df <- shiny::reactive({
    if (spec_var %in% colnames(data())) {
      data()[[spec_var]][[1]] |> 
        mutate(!!y_var := (.data[[y_var]] - abs(min(.data[[y_var]]))) / 
                 (max(.data[[y_var]]) - abs(min(.data[[y_var]]))))
    } else {
      NULL
    }
  })
  
  # x intercept
  summary_val <- shiny::reactive({
    if (is.null(summary_var)) {
      NA_real_
    } else {
      data()[[summary_var]][[1]]
    }
  })
  
  # y on spectra curve at x intercept
  nearest_y <- shiny::reactive({
    if (shiny::isTruthy(spec_df()) & shiny::isTruthy(summary_val())) {
      spec_df()[which(abs(spec_df()[[x_var]] - 
                            summary_val()) == 
                        min(abs(spec_df()[[x_var]] - 
                                  summary_val()))), ][[y_var]]
    } else {
      NULL
    }
  })

  
  
  ##----------------------------------------
  ##  Sparkline plot                      --
  ##----------------------------------------
  if (shiny::isTruthy(spec_df())) {
    p <- ggplot2::ggplot(
      data = spec_df(),
      ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
      ) +
      ggplot2::geom_area(fill = sparklineColors[color_n])
  } else {return(NULL)}
  
  if (shiny::isTruthy(nearest_y())) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = summary_val(),
        linetype = "dashed",
        alpha = 0.8
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = summary_val(), y = nearest_y()),
        color = "red",
        alpha = 0.8
      ) +
      ggplot2::geom_label(
        ggplot2::aes(x = summary_val(), y = Inf, label = summary_var),
        hjust = 1,
        vjust = 1
      )
  }
  
  if (grepl("DLS_I|DLS_M", spec_var, perl = TRUE)) {
    p <- p +
      annotate(
        "rect",
        xmin = 1,
        xmax = 5,
        ymin = 0,
        ymax = Inf,
        fill = "orange",
        alpha = 0.3
      ) +
      annotate(
        "rect",
        xmin = 20,
        xmax = Inf,
        ymin = 0,
        ymax = Inf,
        fill = "red",
        alpha = 0.3
      ) +
      geom_vline(xintercept = 5, linetype = "dotted", alpha = 0.5) +
      geom_vline(xintercept = 20, linetype = "dotted", alpha = 0.5) +
      scale_x_log10(
        limits = c(1, 100000),
        breaks = c(1, 5, 10, 100, 1000), 
        labels = scales::label_comma(accuracy = 1)
      ) +
      annotation_logticks(sides = "b", alpha = 0.5)
  }
  
  if (grepl("DLS_C", spec_var, perl = TRUE)) {
    p <- p +
      ggplot2::scale_x_log10() +
      ggplot2::annotation_logticks(sides = "b")
  }
  
  return(p + sparklineTheme())
  
  # # this grabs the summary value associated with the plotted spectra (see util_vars.R)
  # targetHover <- derivedList[hover]
  # 
  # 
  # if (targetHover == "none") {
  #   ## the "none" is for the correlation plot, which has no derived summary value
  #   valHover <- NA
  #   xval <- 0
  #   yval <- 0
  # } else {
  #   valHover <- dfHover[[targetHover]][[1]]# in case two points are somehow selected by hover, take the first
  #   if (is.na(valHover)) {
  #     xval <- 0
  #     yval <- 0
  #   } else {
  #     xval <- valHover
  #     ## this is to compensate for the called value not being in the spectra plot vectors, it will match the nearest value
  #     # xval <- dfHover[[hover]][[1]][which(abs(dfHover[[hover]][[1]][[1]]-valHover) == min(abs(dfHover[[hover]][[1]][[1]]-valHover))), ][[1]]
  #     yval <- dfHover[[hover]][[1]][which(abs(dfHover[[hover]][[1]][[1]]-valHover) == min(abs(dfHover[[hover]][[1]][[1]]-valHover))), ][[2]]
  #   }
  # }
  # 
  # base <- ggplot(
  #   data = dfHover[[hover]][[1]],
  #   aes(
  #     x = dfHover[[hover]][[1]][[1]],
  #     y = dfHover[[hover]][[1]][[2]]
  #   )
  # )
  # 
  # if (!is.null(dfClick) & !is.null(dfClick[[hover]][[1]])) {
  #   base <- base +
  #     geom_area(
  #       data = dfClick[[hover]][[1]],
  #       aes(
  #         x = dfClick[[hover]][[1]][[1]],
  #         y = dfClick[[hover]][[1]][[2]]
  #       ),
  #       fill = "black", alpha = 0.3
  #     ) +
  #     geom_line(
  #       data = dfClick[[hover]][[1]],
  #       aes(
  #         x = dfClick[[hover]][[1]][[1]],
  #         y = dfClick[[hover]][[1]][[2]]
  #       ),
  #       color = "black", alpha = 0.4
  #     )
  # }
  # 
  # p <- base +
  #   geom_area(fill = colors[n], alpha = alpha) +
  #   geom_line(color = "black", alpha = alpha) +
  #   geom_vline(xintercept = xval, color = "red", linetype = "dashed") +
  #   geom_point(aes(x = xval, y = yval), color = "red") +
  #   annotate("text", x = xval, y = Inf, hjust = 1, vjust = 1, label = targetHover) +
  #   ggtitle(title) +
  #   sparklineTheme()
  # 
  # if (grepl("DLS_I|DLS_M", hover, perl = TRUE) & !is.null(dfHover[[hover]][[1]]) & (is.null(dfClick) | !is.null(dfClick[[hover]][[1]]))) {
  #   p <- p +
  #     annotate("rect", xmin = 1, xmax = 5, ymin = 0, ymax = Inf, fill = "orange", alpha = 0.15) +
  #     annotate("rect", xmin = 20, xmax = Inf, ymin = 0, ymax = Inf, fill = "red", alpha = 0.15) +
  #     geom_vline(xintercept = 5, linetype = "dotted", alpha = 0.5) +
  #     geom_vline(xintercept = 20, linetype = "dotted", alpha = 0.5) +
  #     scale_x_log10(limits = c(1, 100000), breaks = c(1, 5, 10, 100, 1000), labels = scales::label_comma(accuracy = 1)) +
  #     annotation_logticks(sides = "b", alpha = 0.5)
  # }
  # 
  # if (grepl("DLS_C", hover, perl = TRUE) & !is.null(dfHover[[hover]][[1]]) & (is.null(dfClick) | !is.null(dfClick[[hover]][[1]]))) {
  #   p <- p +
  #     scale_x_log10() +
  #     annotation_logticks(sides = "b", alpha = 0.5)
  # }
  # 
  # 
  # 
}
