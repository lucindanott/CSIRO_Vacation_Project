###############################################################################
############################# SCATTER WITH PLANES ############################# 
###############################################################################

# This function is used across both Groups 
# This function adds the animation for the plots in 
# It is the standard 3D Scatter Plot produced 

# Inputs: 
#       dat       - data structure/vector 
#       x       - x value
#       y       - y value
#       z       - z value 
#       cols    - group classification 
#       leg     - legend colouring (relating to R shiny app k or legend 2)
#       xax     - x label for scene in plotly 
#       yax     - y label for scene in plotly 
#       zax     - z label for scene in plotly 
#       XCUT    - X cut off
#       YCUT    - Y cut off
#       ZCUT    - Z cut off
#       XNAME   - name for x cut off
#       YNAME   - name for y cut off
#       ZNAME   - name for z cut off

# Outputs:
#       Structure for Plotly output with Mesh


PlanesFunction <- function(dat,xinput,yinput,zinput,cols,leg,xax,yax,zax,XCUT,YCUT,ZCUT,XNAME,YNAME,ZNAME){
  
  produced.plotly <- plot_ly(dat) %>%
    add_markers(x = ~xinput, 
                y = ~yinput, 
                z = ~zinput,
                color = ~cols, colors = c("firebrick", "darkorange", "gold", "forestgreen")) %>%
    add_trace(type = 'mesh3d',
              x = c(XCUT, XCUT, XCUT, XCUT),
              y = c(max(yinput, na.rm = T), max(yinput, na.rm = T), min(yinput, na.rm = T), min(yinput, na.rm = T)),
              z = c(max(zinput, na.rm = T), min(zinput, na.rm = T), max(zinput, na.rm = T), min(zinput, na.rm = T)),
              # Next define all triples (i,j,k) of vertices that form a 2-cell face.
              # 1 face
              i = c(0,1),
              j = c(1,2),
              k = c(2,3),
              name = XNAME,
              showlegend = T,
              
              # Define the appearance of the 4 faces (2-cells)
              opacity = 0.2) %>%
    add_trace(type = "mesh3d",
              # this is the cut off for ptau - changing it to 59.23
              x = c(min(xinput, na.rm = T),min(xinput, na.rm = T),max(xinput, na.rm = T),max(xinput, na.rm = T)),
              y = c(YCUT,  YCUT,  YCUT , YCUT),
              z = c(max(zinput, na.rm = T), min(zinput, na.rm = T), max(zinput, na.rm = T), min(zinput, na.rm = T)),
              i = c(0,3),
              j = c(1,2),
              k = c(3,0),
              opacity = 0.2,
              name = YNAME,
              showlegend = T) %>%
    add_trace(type = 'mesh3d',
              x = c(min(xinput, na.rm = T),min(xinput, na.rm = T),max(xinput, na.rm = T),max(xinput, na.rm = T)),
              y = c(max(yinput, na.rm = T),min(yinput, na.rm = T),max(yinput, na.rm = T),min(yinput, na.rm = T)),
              z = c(ZCUT, ZCUT, ZCUT, ZCUT),
              i = c(0,3),
              j = c(1,2),
              k = c(3,0),
              name = ZNAME,
              showlegend = T,
              opacity = 0.2)%>%
    layout(legend = leg,
           scene = list(xaxis = xax, yaxis = yax, zaxis = zax))
  produced.plotly
}