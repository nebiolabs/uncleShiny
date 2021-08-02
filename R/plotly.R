source("R/vars.R")
source("R/funs.R")

# These are different parameter sets for formatting and building plotly figures.

# How the axes and gridlines look..
axisList <- list(
  zeroline = TRUE,
  showline = TRUE,
  showgrid = FALSE
)

# How the legends appear..
legendList <- list( # vertical right
  orientation = "v",
  y = 1,
  yref = "paper",
  yanchor = "top", # y in reference to the bottom of the legend
  x = 1,
  xref = "paper",
  xanchor = "left" # x in reference to the left of the legend
)

# legendList <- list( # bottom left
#   orientation = "h",
#   y = -0.2,
#   yref = "paper",
#   yanchor = "top", # y in reference to the bottom of the legend
#   x = -0.1,
#   xref = "paper",
#   xanchor = "left" # x in reference to the left of the legend
# )

colorbarList <- list(
  orientation = "h",
  y = 1,
  yanchor = "bottom", # y in reference to the bottom of the legend
  x = 0,
  xanchor = "left" # x in reference to the left of the legend
)

# How the marker points appear in plotly plots
markerList <- list(
  size = 6,
  opacity = 0.6,
  line = list(color = "black", width = 1)
)

# Shape parameters to add a vertical line with `layout`..
vline <- function(x = 0, color = "black") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper", # extend beyond the limits of the plot view, "paper" instead of "y"
    x0 = x, 
    x1 = x, 
    line = list(color = color, dash = "dot"),
    opacity = 0.2
  )
}

# Shape parameters to add a horizontal line with `layout`..
hline <- function(y = 0, color = "black") {
  list(
    type = "line",
    x0 = 0, 
    x1 = 1, 
    xref = "paper", # extend beyond the limits of the plot view, "paper" instead of "x"
    y0 = y, 
    y1 = y, 
    line = list(color = color, dash = "dot"),
    opacity = 0.2
  )
}

# Shape parameters to add a rectangle with `layout`..
vrect <- function(x0 = 0, x1 = 1, opacity = 0.5, color = "lightgrey") {
  list(
    type = "rect",
    fillcolor = color,
    line = list(color = "rgba(0,0,0,0)"),
    x0 = x0, 
    x1 = x1, 
    yref = "paper", # extend beyond the limits of the plot view
    y0 = 0, 
    y1 = 1,
    opacity = 0.2
  )
}

prect <- function(x0 = 0, x1 = 1, y0 = 0, y1 = 1, opacity = 0.5, color = "lightgrey") {
  list(
    type = "rect",
    fillcolor = "rgba(0,0,0,0)",
    line = list(color = "black"),
    x0 = x0, 
    x1 = x1, 
    xref = "paper",
    y0 = y0, 
    y1 = y1,
    # yref = "paper", # extend beyond the limits of the plot view
    opacity = 0.2
  )
}

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


