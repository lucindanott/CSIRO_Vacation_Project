###############################################################################
############################ SCATTER PLOT FUNCTION ############################ 

# This function is used across both Groups 
# It is the standard 3D Scatter Plot produced 

# Inputs: 
#       d       - data structure/vector 
#       x       - x value
#       y       - y value
#       z       - z value 
#       cols    - group classification 
#       leg     - legend colouring (relating to R shiny app k or legend 2)
#       xax     - x label for scene in plotly 
#       yax     - y label for scene in plotly 
#       zax     - z label for scene in plotly 

# Outputs:
#       Structure for Plotly output 


ScatterPlotFunction <- function(d,xdat,ydat,zdat,cols,leg,xax,yax,zax){
  produced.plotly <- plot_ly(
    data = d, 
    x = ~xdat, 
    y = ~ydat,
    z = ~zdat, 
    type = "scatter3d", 
    mode = "markers",
    sizemode = "Diameter", 
    color = ~cols,
    colors = c("firebrick","darkorange", "gold", "forestgreen")
  ) %>% 
    layout(
      legend = leg, 
      scene = list(xaxis = xax,
                   yaxis = yax,
                   zaxis = zax)
    )
  produced.plotly
}