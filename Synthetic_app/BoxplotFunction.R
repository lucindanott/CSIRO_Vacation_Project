###############################################################################
############################ Boxplot FUNCTION ############################ 

# This function is used across both Groups 
# It is the standard 3D Scatter Plot produced 

# Inputs: 
#       d       - data structure/vector 
#       x       - value 
# Outputs:
#       Structure for graph output 


BoxplotFunction <- function(dat, xinput, input_title){
  dat$apoe4 <- factor(dat$apoe4, levels = c("0","1"))
  dat$Age_binary <- factor(dat$Age_binary, levels = c("0","1"))
  dat$Sex <- factor(dat$Sex, levels = c("Female", "Male"))
  dat$Education_binary <- factor(dat$Education_binary, levels = c("0","1"))
  
  plot2 <- ggplot(dat, aes(x = Sex, y = xinput, na.rm = T))+
    geom_boxplot(color = "red", fill = "orange", alpha = 0.2)+
    theme_bw()+
    ylab(input_title)+
    ggtitle(paste0("Sex and ", input_title, sep = " "))
  
  
  plot3 <- ggplot(dat, aes(x = apoe4, y = xinput, na.rm = T))+
    geom_boxplot(color = "red", fill = "orange", alpha = 0.2)+
    theme_bw()+
    ylab(input_title)+
    xlab("apoe4")+
    ggtitle(paste0("APOE4 and ", input_title, sep = " "))
  
  
  setDT(dat)[Age < 65.5, AgeGroup := "<65"]
  dat[Age >65.5 & Age < 70.5, AgeGroup := "66-70"]
  dat[Age > 70.5 & Age < 75.5, AgeGroup := "71-75"]
  dat[Age > 75.5 & Age < 80.5, AgeGroup := "76-80"]
  dat[Age > 80.5 & Age < 85.5, AgeGroup := "81-85"]
  dat[Age > 85.5, AgeGroup := "86+"]
  
  plot5 <- ggplot(dat, aes(x = AgeGroup, y = xinput, na.rm = T))+
    geom_boxplot(color = "red", fill = "orange", alpha = 0.2)+
    theme_bw()+
    ylab(input_title)+
    xlab("Age Group")+
    ggtitle(paste0("AgeGroup and ",input_title, sep = " "))
  

  hhh <- ggplot(dat, aes(x = Education_binary, y = xinput, na.rm = T))+
    geom_boxplot(color = "red", fill = "orange", alpha = 0.2)+
    theme_bw()+
    ylab(input_title)+
    xlab("Education Binary")+
    ggtitle(paste0("Education_binary and ",input_title, sep = " "))
  
  ggg <- ggplot(dat, aes(x = Age_binary, y = xinput, na.rm = T))+
    geom_boxplot(color = "red", fill = "orange", alpha = 0.2)+
    theme_bw()+
    ylab(input_title)+
    xlab("Age Binary")+
    ggtitle(paste0("Age Binary and ",input_title, sep = " "))
  
  grid.arrange(plot2, plot3, plot5, hhh, ggg, ncol = 3)

  
}

