###############################################################################
############################ Add in your own data ############################# 
###############################################################################


# This is the function to be used for Add in your own Data Points 


AddinDataG1 <- function(dat, xinput,yinput,zinput,cols,leg,xax,yax,zax, size_guide_1){
  
  Processed.plot <- plot_ly(dat,
                     x = ~xinput,
                     y = ~yinput,
                     z = ~zinput,
                     type = "scatter3d",
                     color = ~cols,
                     colors = c("firebrick", "darkorange", "gold", "forestgreen","hotpink" ),
                     mode = "markers",
                     marker = list(size = ~size_guide_1))
  # size = ~size_guide,
  # sizes = c(700,2000))
  Processed.plot <- Processed.plot %>%
    layout(legend = leg, scene = list(xaxis = xax, yaxis = yax, zaxis = zax))
  Processed.plot
}




ATNVisualisation <- function(dat, xinput, yinput, zinput, XCUT,YCUT,ZCUT,leg,xax,yax,zax,size_guide){

    df7 <- mutate(dat, colours_new = ifelse(yinput >YCUT & zinput > ZCUT & xinput <XCUT & Burnham_class == "AD",
                                                 "selected - AD",
                                                 ifelse(yinput >YCUT & zinput >ZCUT & xinput < XCUT & Burnham_class == "Pathological Change",
                                                        "selected - Pathological Change",
                                                        ifelse(yinput >YCUT & zinput > ZCUT & xinput < XCUT & Burnham_class == "Non-AD pathological Change",
                                                               "selected - Non-AD pathological Change",
                                                               ifelse(yinput >YCUT & zinput > ZCUT & xinput < XCUT & Burnham_class == "Normal AD Biomarkers",
                                                                      "selected - Normal AD Biomarker",
                                                                      ifelse(Burnham_class == "My own markers",
                                                                             "My own markers", "unselected"))))))
  
  
  df7$colours_new <- factor(df7$colours_new, levels = c("unselected", "selected - AD",
                                                        "selected - Pathological Change",
                                                        "selected - Non-AD pathological Change",
                                                        "selected - Normal AD Biomarker",
                                                        "My own markers"))
  
  
  df_mesh_1 <- data.frame(X_VAL = c(XCUT,  XCUT,    min(xinput),    min(xinput),    XCUT,   XCUT,   min(xinput),   min(xinput)),
                          Y_VAL = c(YCUT,  max(yinput),    YCUT,  max(yinput),    YCUT, max(yinput),   YCUT, max(yinput)),
                          Z_VAL = c(ZCUT, ZCUT, ZCUT, ZCUT, max(zinput),  max(zinput),  max(zinput),  max(zinput)),
                          MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))
  cube3 <- plot_ly()%>%
    add_markers(type = "scatter3d",
                mode = "markers",
                marker = list(size = ~size_guide),
                data = df7,
                x = ~xinput,
                y = ~yinput,
                z = ~zinput,
                color = ~colours_new,
                colors = c('gray', "firebrick", "orange", "gold", "forestgreen", "hotpink")) %>%
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
              showlegend = T)
  cube3 <- cube3 %>%
    layout(legend = leg,
           scene = list(xaxis = xax, yaxis = yax, zaxis = zax))
  cube3
}


G2ATNVisualisation <- function(dat, xinput, yinput, zinput, XCUT,YCUT,ZCUT,leg,xax,yax,zax,size_guide){
  new_group2 <- mutate(dat, scat_col = ifelse(yinput >YCUT  & xinput < XCUT & zinput >ZCUT & Clifford_class == "Stage 2, clinically asymptomatic",
                                                 "selected - Stage 2 clinically asymptomatic",
                                                 ifelse(yinput >YCUT  & xinput <XCUT & zinput > ZCUT & Clifford_class == "Stage 1, preclinical AD stage",
                                                        "selected - Stage 1 preclinical AD stage",
                                                        ifelse(yinput >YCUT  & xinput < XCUT & zinput > ZCUT & Clifford_class == "SNAP",
                                                               "selected - SNAP",
                                                               ifelse(yinput >YCUT  & xinput < XCUT & zinput > ZCUT & Clifford_class == "MCI unlikely due to AD",
                                                                      "selected - MCI unlikely due to AD",
                                                                      ifelse(Clifford_class == "My own markers", "My own markers", "unselected"))))))
  
  
  new_group2$scat_col <- factor(new_group2$scat_col, levels = c("unselected", "selected - Stage 2 clinically asymptomatic",
                                                                "selected - Stage 1 preclinical AD stage", "selected - SNAP",
                                                                "selected - MCI unlikely due to AD",
                                                                "My own markers"))
  
  
  df_mesh_1 <- data.frame(Y_VAL = c(max(yinput), max(yinput), YCUT,     YCUT ,   max(yinput),  max(yinput),  YCUT,     YCUT),
                          X_VAL = c(min(xinput, na.rm = T),  XCUT, min(xinput, na.rm = T), XCUT, min(xinput, na.rm = T),   XCUT, min(xinput, na.rm = T), XCUT),
                          Z_VAL = c(ZCUT,      ZCUT,    ZCUT,      ZCUT,  max(zinput, na.rm = T), max(zinput, na.rm = T),   max(zinput, na.rm = T), max(zinput, na.rm = T)),
                          MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))
  
  # Make apoe4 a factor
  
  cube_data_vis <- plot_ly()%>%
    add_markers(type = "scatter3d",
                mode = "markers",
                marker = list(size = ~size_guide),
                data = new_group2,
                x = ~xinput,
                y = ~yinput,
                z = ~zinput,
                color = ~scat_col,
                colors = c('gray', 'red', "yellow", "green", "hotpink")) %>%
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
              showlegend = T)
  
  cube_data_vis <- cube_data_vis %>%
    layout(legend = leg, scene = list(xaxis = xax, yaxis = yax, zaxis = zax))
  cube_data_vis
  
}
