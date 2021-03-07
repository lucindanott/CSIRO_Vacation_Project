###############################################################################
######################## REgression Surfaces Function ######################### 
###############################################################################

# This function is used across both Groups 
# It is to produce the surface regressions for each demographic characeristic 







RegressionSurfaces <- function(dat, xinput,yinput,zinput,demographic_characteristic, 
                               surface_title_1, surface_title_2,cols,leg,xax,yax,zax, 
                               ){
  # Filter for demographic characteristics
  new_data_frame <- filter(dat, demographic_characteristic == 1)
  
  # Set up linear regression
  my_lm <- lm(zinput ~xinput+yinput, new_data_frame)
  
  # Set up axis 
  axis_x1 <- seq(min(xinput),max(xinput))
  axis_z1 <- seq(min(yinput),max(yinput))
  
  # Plot the points 
  my_lm_surface <- expand.grid(xinput = axis_x1, yinput = axis_z1, KEEP.OUT.ATTRS = F)
  my_lm_surface$zinput <- predict.lm(my_lm, newdata = my_lm_surface)
  my_lm_surface <- acast(my_lm_surface, yinput~xinput, value.var = "zinput")
  
  # Set up Plotly 
  my_plot <- plot_ly()
  my_plot <- add_markers(
    p = my_plot,
    data = dat, 
    x = ~xinput,
    y = ~yinput, 
    z = ~zinput, 
    text = ~Diagnosis, 
    type = "scatter3d", 
    color = ~cols, 
    colors = c("firebrick", "darkorange", "gold", "forestgreen"), 
    mode = "markers"
  )
  my_plot <- add_trace(
    p = my_plot, 
    z = my_lm_surface, 
    x = axis_x1, 
    z = axis_z1, 
    type = "surface", 
    colorscale = list(c(0,1),c("green","red")), 
    opacity = 0.6, 
    showscale = F, 
    name = surface_title_1, 
    showlegend = T
  )
  
  # Filter for second part 
  new_data_frame_2 <- filter(dat, demographic_characteristic == 0)
  
  # Set up linear regression 
  
  my_lm_2 <- lm(zinput ~xinput+yinput, new_data_frame_2)
  
  # Set up Axis 
  axis_x2 <- seq(min(xinput), max(xinput))
  axis_z2 <- seq(min(yinput), max(yinput))
  
  # Extend the surface 
  my_lm_surface_2 <- expand.grid(xinput = axis_x2,yinput = axis_z2, KEEP.OUT.ATTRS = F)
  my_lm_surface_2$zinput <- predict.lm(my_lm_2, newdata = my_lm_surface_2)
  my_lm_surface_2 <- acast(my_lm_surface_2, yinput~xinput, value.var = 'zinput')
  
  my_plot <- add_trace(
    p = my_plot, 
    z = my_lm_surface_2,
    x = axis_x2, 
    y = axis_z2, 
    type = "surface", 
    colorscale = list(c(0,1),c("tan","blue")), 
    opacity = 0.6,
    showscale = F, 
    name = surface_title_2, 
    showlegend = T
  )
  my_plot <- my_plot %>%
    layout(legend = leg, 
           scene = list(xaxis = xax, yaxis = yax, zaxis = zax))
  my_plot
}











x <- RegressionSurfaces(dat = simulated.data,
                        xinput = simulated.data$CSF.AB42.INNO,
                        yinput = simulated.data$CSF.pTau.INNO,
                        zinput = simulated.data$CSF.tTau.INNO,
                        demographic_characteristic = simulated.data$apoe4, 
                        surface_title_1 = "Regression surface for carrier",
                        surface_title_2 = "Regression surface for non-carrier",
                        cols = simulated.data$Burnham_class,
                        leg = LEGEND_1,
                        xax = axx,
                        yax = axy,
                        zax = axz, 
                        something = "CSF.tTau.INNO")
x





