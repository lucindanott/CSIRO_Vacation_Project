###############################################################################
############################ Process Data Function  ########################### 
###############################################################################


# This function is for when the user inputs file1_inputa and processing needs to occur to 
# get it into the correct 

# Inputs: The file1_inputa file needed for processing 
# Outputs: The file1_inputa file


ProcessingData <- function(file1_input){
  
  file1_input$Diagnosis <- factor(file1_input$Diagnosis, levels = c("AD", "MCI", "HC"))
  
  # # See above for alternative way to use mutate - feed it multiple variables to make in the one go
  file1_input <- mutate(file1_input,
                        A_status = ifelse(AB.status == "positive", "A+", "A-"),
                        T_status = ifelse(pTau.status == "positive", "T+", "T-"),
                        N_status = ifelse(tTau.status == "positive", "N+", "N-")
  )
  file1_input <- tidyr::unite(file1_input, Overall_status, A_status:N_status, sep = '/')
  
  file1_input$Overall_status <- factor(file1_input$Overall_status, levels = c("A+/T+/N+", "A+/T+/N-", "A+/T-/N+",
                                                                              "A-/T+/N+", "A+/T-/N-","A-/T-/N+",
                                                                              "A-/T+/N-", "A-/T-/N-"))
  
  
  file1_input <- mutate(
    file1_input, Burnham_class = ifelse(Overall_status == "A-/T-/N-", "Normal AD Biomarkers",
                                        ifelse(Overall_status == "A-/T+/N-", "Non-AD pathological Change",
                                               ifelse(Overall_status == "A-/T+/N+", "Non-AD pathological Change",
                                                      ifelse(Overall_status == "A-/T-/N+", "Non-AD pathological Change",
                                                             ifelse(Overall_status == "A+/T-/N-", "Pathological Change",
                                                                    ifelse(Overall_status == "A+/T-/N+", "Pathological Change",
                                                                           ifelse(Overall_status == "A+/T+/N+", "AD",
                                                                                  ifelse(Overall_status == "A+/T+/N-", "AD", NA)))))))))
  file1_input <- mutate(file1_input, Clifford_class = ifelse(Burnham_class == "Normal AD Biomarkers", "MCI unlikely due to AD",
                                                             ifelse(Burnham_class == "AD", "Stage 2, clinically asymptomatic",
                                                                    ifelse(Burnham_class == "Pathological Change", "Stage 1, preclinical AD stage",
                                                                           ifelse(Burnham_class == "Non-AD pathological Change", "SNAP", NA)))))
  
  
  
  
  # # Surface for APOE4
  # 
  # file1_input$Apoe4 <- factor(simulated.file1_inputa$apoe4, levels = c("1","0"))
  # 
  file1_input$Clifford_class <- factor(file1_input$Clifford_class, levels = c("Stage 2, clinically asymptomatic",
                                                                              "Stage 1, preclinical AD stage",
                                                                              "SNAP",
                                                                              "MCI unlikely due to AD"))
  
  
  
  
  file1_input$Burnham_class <- factor(file1_input$Burnham_class, levels = c("AD",
                                                                            "Pathological Change",
                                                                            "Non-AD pathological Change",
                                                                            "Normal AD Biomarkers"))
  
  file1_input <- mutate(file1_input, Age_binary = ifelse(Age < 72.5,1,0))
  

  # file1_input <- factor(file1_input$Education_binary, levels = c("1","0"))
  
  ops <- list(file1_input = file1_input)
  return(ops)
  
}










