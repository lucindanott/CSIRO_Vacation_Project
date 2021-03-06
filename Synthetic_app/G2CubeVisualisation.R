###############################################################################
################################ Cube Function ################################ 
###############################################################################

# This function is for GROUP 2
# It is the standard 3D Scatter Plot produced with cube visualisation

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
#       XCUT    - X cut off threshold 
#       YCUT    - Y cut off threshold 
#       ZCUT    - Z Cut off threshold

# Outputs:
#       Structure for Plotly output 



G2CubeVisualisation <- function(dat, xinput,yinput,zinput,leg,xax,yax,zax,
                                XCUT,YCUT,ZCUT){
  
  df7 <- mutate(dat, scat_col = ifelse(yinput >=YCUT & zinput >=ZCUT & xinput<=XCUT & Clifford_class == "Stage 2, clinically asymptomatic",
                                       "selected - Stage 2 clinically asymptomatic",
                                       ifelse(yinput >=YCUT & zinput >=ZCUT & xinput <=XCUT & Clifford_class == "Stage 1, preclinical AD stage",
                                              "selected - Stage 1 preclinical AD stage",
                                              ifelse(yinput >=YCUT & zinput >=ZCUT & xinput <=XCUT & Clifford_class == "SNAP",
                                                     "selected - SNAP",
                                                     ifelse(yinput >=YCUT & zinput >= ZCUT & xinput <= XCUT & Clifford_class == "MCI unlikely due to AD",
                                                            "selected - MCI unlikely due to AD","unselected")))))
  
  
  df7$scat_col <- factor(df7$scat_col, levels = c("unselected", "selected - Stage 2 clinically asymptomatic",
                                                  "selected - Stage 1 preclinical AD stage", "selected - SNAP",
                                                  "selected - MCI unlikely due to AD"))
  
  
  df_mesh_1 <- data.frame(X_VAL = c(XCUT,  XCUT,  min(xinput, na.rm = T),    min(xinput, na.rm = T),    
                                    XCUT,   XCUT,   min(xinput, na.rm = T),   min(xinput, na.rm = T)),
                          Y_VAL = c(YCUT,max(yinput, na.rm = T),    YCUT,  max(yinput, na.rm = T),    
                                    YCUT, max(yinput, na.rm = T),   YCUT, max(yinput, na.rm = T)),
                          Z_VAL = c(ZCUT, ZCUT, ZCUT, ZCUT, max(zinput, na.rm = T),  
                                    max(zinput, na.rm = T),  
                                    max(zinput, na.rm = T),  max(zinput, na.rm = T)),
                          MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))
  
  plot_produced <- plot_ly()%>%
    add_markers(type = "scatter3d",
                mode = "markers",
                data = df7,
                x = ~xinput,
                y = ~yinput,
                z = ~zinput,
                color = ~scat_col,
                colors = c('gray', "firebrick", "darkorange", "gold", "forestgreen")) %>%
    add_trace(type = 'mesh3d',
              data = df_mesh_1,
              x = ~X_VAL,
              y = ~Y_VAL,
              z = ~Z_VAL,
              i = c(7, 1,  6, 0, 4, 0, 3, 6, 0, 3, 4,7),
              j = c(3, 5,  4, 2, 0, 1, 6, 3, 1, 2, 5,6),
              k = c(1, 7, 0, 6, 5, 5, 7, 2, 3, 0, 7, 4),
              facecolor = rep("blue", 12),
              opacity = 0.1,
              name = "A+/T+/N+ Positive Area",
              showlegend = T) %>%
    layout(legend = leg,
           scene = list(xaxis = xax, yaxis = yax, zaxis = zax))
  
  plot_produced
}


