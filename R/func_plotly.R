
##--------------------------------------------------------------------------
##  Plotly plot-building functions                                        --
##--------------------------------------------------------------------------

# Plotly scatter plot function..
buildplotly <- function(data, x, y, source, color, palette, showlegend = TRUE, legendgroup = NULL, colorbar = TRUE, customdata = NA) {
  if (is.null(legendgroup)) {
    legendgroup <- color
  }
  
  p <- plot_ly(
    data = data,
    source = source,
    color = as.formula(paste0("~", color)),
    colors = mycolors(palette, length( unique( data$origData()[[color]] ) ) ),
    showlegend = showlegend,
    legendgroup = as.formula(paste0("~", legendgroup)),
    customdata = as.formula(paste0("~", customdata)),
    hoverinfo = "text",
    text = ~paste0(
      "<em>Well: ", well, ", Uni: ", uni, "</em><br>",
      "<b>Buffer: </b>", buffer, ", pH ", pH, "<br>",
      "<b>Salt: </b>", salt, " ", salt_mM, "mM", "<br>",
      "<b>Add.1: </b>", additive1, " ", additive1_conc, additive1_unit, "<br>",
      "<b>Add.2: </b>", additive2, " ", additive2_conc, additive2_unit, "<br>",
      "<b>Comment: </b>", comment
    )
  ) %>% 
    add_markers(
      x = as.formula(paste0("~", x)),
      y = as.formula(paste0("~", y)),
      marker = markerList,
      showlegend = showlegend
    )
  
  if (x %in% c("Z_D", "peak1_D")) {
    p <- p %>%
      layout(
        xaxis = axisList,
        yaxis = axisList,
        shapes = list(vline(5), vline(20), vrect(5, 20, 0.2))#,
        # colorbar = colorbarList,
        # legend = legendList
      ) %>%
      layout(xaxis = list(type = "log"))
  } else {
    p <- p %>%
      layout(
        xaxis = axisList,
        yaxis = axisList#,
        # colorbar = colorbarList,
        # legend = legendList
      )
  }
  
  if (colorbar == TRUE) {
    return(p)
  } else {
    return(p %>% hide_colorbar())
  }
  
}

# Plotly scatter plot function for db-derived data
db_buildplotly <- function(data, x, y, source, color, palette, showlegend = TRUE, legendgroup = NULL, colorbar = TRUE, customdata = NA) {
  if (is.null(legendgroup)) {
    legendgroup <- color
  }
  
  p <- plot_ly(
    data = data,
    source = source,
    color = as.formula(paste0("~", color)),
    colors = mycolors(palette, length( unique( data$origData()[[color]] ) ) ),
    showlegend = showlegend,
    legendgroup = as.formula(paste0("~", legendgroup)),
    customdata = as.formula(paste0("~", customdata)),
    hoverinfo = "text",
    text = ~paste0(
      "<em>Well: ", well, "</em><br>"
    )
  ) %>% 
    add_markers(
      x = as.formula(paste0("~", x)),
      y = as.formula(paste0("~", y)),
      marker = markerList,
      showlegend = showlegend
    )
  
  if (x %in% c("Z_D", "peak1_D")) {
    p <- p %>%
      layout(
        xaxis = axisList,
        yaxis = axisList,
        shapes = list(vline(5), vline(20), vrect(5, 20, 0.2))#,
        # colorbar = colorbarList,
        # legend = legendList
      ) %>%
      layout(xaxis = list(type = "log"))
  } else {
    p <- p %>%
      layout(
        xaxis = axisList,
        yaxis = axisList#,
        # colorbar = colorbarList,
        # legend = legendList
      )
  }
  
  if (colorbar == TRUE) {
    return(p)
  } else {
    return(p %>% hide_colorbar())
  }
  
}
