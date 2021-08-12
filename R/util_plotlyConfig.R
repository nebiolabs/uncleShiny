
##--------------------------------------------------------------------------
##  Plotly configuration and annotations                                  --
##--------------------------------------------------------------------------

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