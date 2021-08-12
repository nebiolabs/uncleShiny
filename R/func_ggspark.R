
##-------------------------------------------------------------------------
##  Plot spectra sparklines                                              --
##-------------------------------------------------------------------------

ggspark <- function(dfHover, dfClick, hover, title, n, derivedList, alpha = 0.6) {
  # this grabs the summary value associated with the plotted spectra (see util_vars.R)
  targetHover <- derivedList[hover]
  
  
  if (targetHover == "none") {
    ## the "none" is for the correlation plot, which has no derived summary value
    valHover <- NA
    xval <- 0
    yval <- 0
  } else {
    valHover <- dfHover[[targetHover]][[1]]# in case two points are somehow selected by hover, take the first
    if (is.na(valHover)) {
      xval <- 0
      yval <- 0
    } else {
      xval <- valHover
      ## this is to compensate for the called value not being in the spectra plot vectors, it will match the nearest value
      # xval <- dfHover[[hover]][[1]][which(abs(dfHover[[hover]][[1]][[1]]-valHover) == min(abs(dfHover[[hover]][[1]][[1]]-valHover))), ][[1]]
      yval <- dfHover[[hover]][[1]][which(abs(dfHover[[hover]][[1]][[1]]-valHover) == min(abs(dfHover[[hover]][[1]][[1]]-valHover))), ][[2]]
    }
  }
  
  base <- ggplot(
    data = dfHover[[hover]][[1]],
    aes(
      x = dfHover[[hover]][[1]][[1]],
      y = dfHover[[hover]][[1]][[2]]
    )
  )
  
  if (!is.null(dfClick) & !is.null(dfClick[[hover]][[1]])) {
    base <- base +
      geom_area(
        data = dfClick[[hover]][[1]],
        aes(
          x = dfClick[[hover]][[1]][[1]],
          y = dfClick[[hover]][[1]][[2]]
        ),
        fill = "black", alpha = 0.3
      ) +
      geom_line(
        data = dfClick[[hover]][[1]],
        aes(
          x = dfClick[[hover]][[1]][[1]],
          y = dfClick[[hover]][[1]][[2]]
        ),
        color = "black", alpha = 0.4
      )
  }
  
  p <- base +
    geom_area(fill = colors[n], alpha = alpha) +
    geom_line(color = "black", alpha = alpha) +
    geom_vline(xintercept = xval, color = "red", linetype = "dashed") +
    geom_point(aes(x = xval, y = yval), color = "red") +
    annotate("text", x = xval, y = Inf, hjust = 1, vjust = 1, label = targetHover) +
    ggtitle(title) +
    sparklineTheme()
  
  if (grepl("DLS_I|DLS_M", hover, perl = TRUE) & !is.null(dfHover[[hover]][[1]]) & (is.null(dfClick) | !is.null(dfClick[[hover]][[1]]))) {
    p <- p +
      annotate("rect", xmin = 1, xmax = 5, ymin = 0, ymax = Inf, fill = "orange", alpha = 0.15) +
      annotate("rect", xmin = 20, xmax = Inf, ymin = 0, ymax = Inf, fill = "red", alpha = 0.15) +
      geom_vline(xintercept = 5, linetype = "dotted", alpha = 0.5) +
      geom_vline(xintercept = 20, linetype = "dotted", alpha = 0.5) +
      scale_x_log10(limits = c(1, 100000), breaks = c(1, 5, 10, 100, 1000), labels = scales::label_comma(accuracy = 1)) +
      annotation_logticks(sides = "b", alpha = 0.5)
  }
  
  if (grepl("DLS_C", hover, perl = TRUE) & !is.null(dfHover[[hover]][[1]]) & (is.null(dfClick) | !is.null(dfClick[[hover]][[1]]))) {
    p <- p +
      scale_x_log10() +
      annotation_logticks(sides = "b", alpha = 0.5)
  }
  
  
  
  return(p)
}
