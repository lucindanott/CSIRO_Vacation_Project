###############################################################################
########################### Table Output Functions ############################ 
###############################################################################

# This function is used across both Groups 
# It is to produce the table outputs for both groups. 


# Input: Dat
# Output: Two lots of tables 



TableOutputs <- function(dat){
  group_1 <- dat[, c("CSF.AB42.INNO", "CSF.pTau.INNO", "CSF.tTau.INNO", "Age", "Burnham_class")]
  group_1_2 <- dat[, c("CSF.AB42.INNO", "CSF.pTau.INNO", "CSF.tTau.INNO", "Age",  "Diagnosis", "Sex", 
                           "apoe4","AB.status", "pTau.status", "tTau.status", "Education_binary", "Burnham_class")]
  
  
  data_melted <- melt(group_1, id.vars = "Burnham_class")
  
  group_1_summary <- data_melted %>%
    group_by(Burnham_class, variable) %>%
    summarise(mean = round(mean(value, na.rm = T),2),
              sd =round(sd(value, na.rm = T),2),
              #median = round(median(value, na.rm = T),2),
              Missing = sum(is.na(value))) %>%
    data.frame()
  
  # Creating a Table like Marcela's 
  
  # Group left most column by Diagnosis 
  number_group <- group_1 %>%
    group_by(Burnham_class) %>%
    summarise(n = n()) %>%
    data.frame()
  
  # SEX 
  sex.number <- group_1_2 %>% 
    group_by(Burnham_class, Sex) %>% 
    summarise(n = n()) %>%
    data.frame
  
  
  # APOE4 POSITIVE/NEGATIVE 
  
  apoe.no <- group_1_2 %>%
    group_by(Burnham_class, apoe4) %>%
    summarise(n=n()) %>%
    data.frame  
  
  # AMYLOID STATUS
  
  AB_status <- group_1_2 %>%
    group_by(Burnham_class, AB.status) %>%
    summarise(n=n()) %>%
    data.frame
  
  # TAU STATUS
  
  tau_status <- group_1_2 %>%
    group_by(Burnham_class, pTau.status) %>%
    summarise(n=n()) %>%
    data.frame
  
  # NEURODEGENERATION STATUS 
  
  CSF.ptau181_status <- group_1_2 %>%
    group_by(Burnham_class, tTau.status) %>%
    summarise(n=n()) %>%
    data.frame
  
  # Create Table 
  
  # 20 by 6 table 
  
  # Set up headers 
  group_1_table <- matrix(NA, ncol=7, nrow=10)
  group_1_table[1,1] <- "Biomarker"
  group_1_table[1,2] <- "Summary"
  group_1_table[1,3] <- "All"
  group_1_table[1,4] <- "Normal AD Biomarkers"
  group_1_table[1,5] <- "Non-AD Pathological Change"
  group_1_table[1,6] <- "Pathological Change"
  group_1_table[1,7] <- "AD"
  
  
  
  group_1_table[2,1] <- "N"
  group_1_table[2,2] <- ""
  group_1_table[2,3] <- number_group[4,2]+number_group[3,2]+number_group[2,2]+number_group[1,2]
  group_1_table[2,4] <- number_group[4,2]
  group_1_table[2,5] <- number_group[3,2]
  group_1_table[2,6] <- number_group[2,2]
  group_1_table[2,7] <- number_group[1,2]
  
  # ADD in SEX
  
  group_1_table[3,1] <- "Sex"
  group_1_table[3,2] <- "Male"
  group_1_table[3,3] <- sex.number[8,3]+sex.number[6,3]+sex.number[4,3]+sex.number[2,3]
  group_1_table[3,4] <- sex.number[8,3]
  group_1_table[3,5] <- sex.number[6,3]
  group_1_table[3,6] <- sex.number[4,3]
  group_1_table[3,7] <- sex.number[2,3]
  
  group_1_table[4,1] <- ""
  group_1_table[4,2] <- "Female"
  group_1_table[4,3] <- sex.number[7,3]+sex.number[5,3]+sex.number[3,3]+sex.number[1,3]
  group_1_table[4,4] <- sex.number[7,3]
  group_1_table[4,5] <- sex.number[5,3]
  group_1_table[4,6] <- sex.number[3,3]
  group_1_table[4,7] <- sex.number[1,3]
  
  # AGE 
  group_1_table[5,1] <- "Age"
  group_1_table[5,2] <- "Mean  (SD)"
  group_1_table[5,3] <- paste(round(mean(group_1$Age),2), "    (", round(sd(group_1$Age),2), ")  ", sep = "")
  group_1_table[5,4] <- paste(group_1_summary[16,3], "   (",group_1_summary[16,4],") ", sep = ""  )
  group_1_table[5,5] <- paste(group_1_summary[12,3], "   (",group_1_summary[12,4],") ", sep = ""  )
  group_1_table[5,6] <- paste(group_1_summary[8,3], "   (",group_1_summary[8,4],") ", sep = ""  )
  group_1_table[5,7] <- paste(group_1_summary[4,3], "   (",group_1_summary[4,4],") ", sep = ""  )
  
  
  # ADD in APOE4 STATUS 
  
  group_1_table[6,1] <- "Number of APOE4 Status"
  group_1_table[6,2] <- "Carriers"
  group_1_table[6,3] <- apoe.no[8,3]+apoe.no[6,3]+apoe.no[4,3]+apoe.no[2,3] 
  group_1_table[6,4] <- apoe.no[8,3]
  group_1_table[6,5] <- apoe.no[6,3]
  group_1_table[6,6] <- apoe.no[4,3]
  group_1_table[6,7] <- apoe.no[2,3]
  
  
  group_1_table[7,2] <- "Non-Carriers"
  group_1_table[7,3] <- apoe.no[7,3]+apoe.no[5,3]+apoe.no[3,3]+apoe.no[1,3]
  group_1_table[7,4] <- apoe.no[7,3]
  group_1_table[7,5] <- apoe.no[5,3]
  group_1_table[7,6] <- apoe.no[3,3]
  group_1_table[7,7] <- apoe.no[1,3]
  
  # Add in mean, sd and Missing for AB, pTau and tTau 
  
  # ADD in CSF.AB42.INNO
  
  group_1_table[8,1] <- "CSF AB1-42 pg/mL"
  group_1_table[8,2] <- "Mean (SD)"
  group_1_table[8,3] <- paste(round(mean(group_1$CSF.AB42.INNO),2), "   (",round(sd(group_1$CSF.AB42.INNO), 2),") ", sep = ""  ) 
  group_1_table[8,4] <- paste(group_1_summary[13,3], "   (",group_1_summary[13,4],") ", sep = ""  )
  group_1_table[8,5] <- paste(group_1_summary[9,3], "   (",group_1_summary[9,4],") ", sep = ""  )
  group_1_table[8,6] <- paste(group_1_summary[5,3], "   (",group_1_summary[5,4],") ", sep = ""  )
  group_1_table[8,7] <- paste(group_1_summary[1,3], "   (",group_1_summary[1,4],") ", sep = ""  )
  
  
  group_1_table[9,1] <- "CSF pTau pg/mL"
  group_1_table[9,2] <- "Mean (SD)"
  group_1_table[9,3] <- paste(round(mean(group_1$CSF.pTau.INNO),2), " (",round(sd(group_1$CSF.pTau.INNO),2),") ", sep = ""  ) 
  group_1_table[9,4] <- paste(group_1_summary[14,3], "   (",group_1_summary[14,4],")", sep = ""  )
  group_1_table[9,5] <- paste(group_1_summary[10,3], "   (",group_1_summary[10,4],")", sep = ""  )
  group_1_table[9,6] <- paste(group_1_summary[6,3], "   (",group_1_summary[6,4],")", sep = ""  )
  group_1_table[9,7] <- paste(group_1_summary[2,3], "   (",group_1_summary[2,4],")", sep = ""  )
  
  
  
  group_1_table[10,1] <- "CSF tTau pg/mL"
  group_1_table[10,2] <- "Mean (SD)"
  group_1_table[10,3] <- paste(round(mean(group_1$CSF.tTau.INNO),2), " (",round(sd(group_1$CSF.tTau.INNO),2),") ", sep = ""  ) 
  group_1_table[10,4] <- paste(group_1_summary[15,3], "   (",group_1_summary[15,4],")", sep = ""  )
  group_1_table[10,5] <- paste(group_1_summary[11,3], "   (",group_1_summary[11,4],")", sep = ""  )
  group_1_table[10,6] <- paste(group_1_summary[7,3], "   (",group_1_summary[7,4],")", sep = ""  )
  group_1_table[10,7] <- paste(group_1_summary[3,3], "   (",group_1_summary[3,4],")", sep = ""  )
  
  
  group_2 <- dat[,c("Sum.hippo", "Centiloid", "Age", "CSF.pTau.INNO", 
                        "CSF.tTau.INNO", "Clifford_class")]
  
  group_2_2 <- dat[,c("Sum.hippo", "Centiloid", "Age", "Clifford_class", "CSF.pTau.INNO", "CSF.tTau.INNO",  "Sex", 
                          "apoe4")]
  
  group_2_continuous <- melt(group_2, id.vars = "Clifford_class")
  
  group_2_summary <- group_2_continuous %>%
    group_by(Clifford_class, variable) %>%
    summarise(mean = round(mean(value, na.rm = T),2),
              sd =round(sd(value, na.rm = T),2),
              #median = round(median(value, na.rm = T),2),
              Missing = sum(is.na(value))) %>%
    data.frame()
  
  
  
  # AGE 
  age_group_2 <- group_2 %>%
    group_by(Clifford_class, Age) %>%
    summarise(n = n()) %>%
    data.frame()
  
  
  
  # Group left most column by Diagnosis 
  number_group_2 <- group_2 %>%
    group_by(Clifford_class) %>%
    summarise(n = n()) %>%
    data.frame()
  
  # SEX 
  sex.number_2 <- group_2_2 %>% 
    group_by(Clifford_class, Sex) %>% 
    summarise(n = n()) %>%
    data.frame
  
  
  # APOE4 POSITIVE/NEGATIVE 
  
  apoe.no_2 <- group_2_2 %>%
    group_by(Clifford_class, apoe4) %>%
    summarise(n=n()) %>%
    data.frame  
  
  group_2_table <- matrix(NA, ncol=7, nrow=10)
  group_2_table[1,1] <- "Biomarker"
  group_2_table[1,2] <- "Summary"
  group_2_table[1,3] <- "All"
  group_2_table[1,4] <- "MCI unlikely due to AD"
  group_2_table[1,5] <- "SNAP"
  group_2_table[1,6] <- "Stage 1, Preclinical AD Stage"
  group_2_table[1,7] <- "Stage 2, Clinically asymptomatic"
  
  # ADD IN NUMBER
  
  group_2_table[2,1] <- "N"
  group_2_table[2,3] <- number_group_2[4,2]+number_group_2[3,2]+number_group_2[2,2]+number_group_2[1,2]
  group_2_table[2,4] <- number_group_2[4,2]
  group_2_table[2,5] <- number_group_2[3,2]
  group_2_table[2,6] <- number_group_2[2,2]
  group_2_table[2,7] <- number_group_2[1,2]
  
  # ADD IN AGE
  
  group_2_table[3,1] <- "Age"
  group_2_table[3,2] <- "Mean  (SD)"
  group_2_table[3,3] <- paste(round(mean(group_2$Age),2), "   (", round(sd(group_2$Age),2),")", sep = " ")
  group_2_table[3,4] <- paste(group_2_summary[18,3], "   (",group_2_summary[18,4],") ", sep = "" )
  group_2_table[3,5] <- paste(group_2_summary[13,3], "   (",group_2_summary[13,4],") ", sep = "" )
  group_2_table[3,6] <- paste(group_2_summary[8,3], "   (",group_2_summary[8,4],") ", sep = "" )
  group_2_table[3,7] <- paste(group_2_summary[3,3], "   (",group_2_summary[3,4],") ", sep = "" )
  
  # Add in Sex
  group_2_table[4,1] <- "Sex"
  group_2_table[4,2] <- "Male"
  group_2_table[4,3] <- sex.number_2[8,3]+sex.number_2[6,3]+sex.number_2[4,3]+sex.number_2[2,3]
  group_2_table[4,4] <- sex.number_2[8,3]
  group_2_table[4,5] <- sex.number_2[6,3]
  group_2_table[4,6] <- sex.number_2[4,3]
  group_2_table[4,7] <- sex.number_2[2,3]
  
  group_2_table[5,2] <- "Female"
  group_2_table[5,3] <- sex.number_2[7,3]+sex.number_2[5,3]+sex.number_2[3,3]+sex.number_2[1,3]
  group_2_table[5, 4] <- sex.number_2[7,3]
  group_2_table[5,5] <- sex.number_2[5,3]
  group_2_table[5,6] <- sex.number_2[3,3]
  group_2_table[5,7] <- sex.number_2[1,3]
  
  # ADD in APOE4 STATUS 
  
  group_2_table[6,1] <- "APOE4 Status"
  group_2_table[6,2] <- "Carrier"
  group_2_table[6,3] <- apoe.no_2[8,3]+apoe.no_2[6,3]+apoe.no_2[4,3]+apoe.no_2[2,3]
  group_2_table[6,4] <- apoe.no_2[8,3]
  group_2_table[6,5] <- apoe.no_2[6,3]
  group_2_table[6,6] <- apoe.no_2[4,3]
  group_2_table[6,7] <- apoe.no_2[2,3]
  
  group_2_table[7,2] <- "Non-Carrier"
  group_2_table[7,3] <- apoe.no_2[7,3]+apoe.no_2[5,3]+apoe.no_2[3,3]+apoe.no_2[1,3]
  group_2_table[7,4] <- apoe.no_2[7,3]
  group_2_table[7,5] <- apoe.no_2[5,3]
  group_2_table[7,6] <- apoe.no_2[3,3]
  group_2_table[7,7] <- apoe.no_2[1,3]
  
  # ADD IN CENTILOID 
  group_2_table[8,1] <- "Centiloid"
  group_2_table[8,2] <- "Mean (SD)"
  # paste(group_1_summary[20,3], "   (",group_1_summary[20,4],") " )
  group_2_table[8,3] <- paste(round(mean(group_2$Centiloid, na.rm = T),2), "   (",round(sd(group_2$Centiloid, na.rm = T),2),") ", sep = "" )
  group_2_table[8,4] <- paste(group_2_summary[17,3], "   (",group_2_summary[17,4],") ", sep = "" )
  group_2_table[8,5] <- paste(group_2_summary[12,3], "   (",group_2_summary[12,4],") ", sep = "" )
  group_2_table[8,6] <- paste(group_2_summary[7,3], "   (",group_2_summary[7,4],") ", sep = "" )
  group_2_table[8,7] <- paste(group_2_summary[2,3], "   (",group_2_summary[2,4],") ", sep = "" )
  
  # SUM.HIPPO 
  group_2_table[9,1] <- "Hippocampus volume mL^3"
  group_2_table[9,2] <- "Mean (SD)"
  group_2_table[9,3] <- paste(round(mean(group_2$Sum.hippo, na.rm = T),2), "   (",round(sd(group_2$Sum.hippo, na.rm = T),2),") ", sep = "" )
  group_2_table[9,4] <- paste(group_2_summary[16,3], "   (",group_2_summary[16,4],") ", sep = "" )
  group_2_table[9,5] <- paste(group_2_summary[11,3], "   (",group_2_summary[11,4],") ", sep = "" )
  group_2_table[9,6] <- paste(group_2_summary[6,3], "   (",group_2_summary[6,4],") ", sep = "" )
  group_2_table[9,7] <- paste(group_2_summary[1,3], "   (",group_2_summary[1,4],") ", sep = "" )
  
  
  # pTAU 
  group_2_table[10,1] <- "CSF pTau pg/mL"
  group_2_table[10,2] <- "Mean (SD)"
  group_2_table[10,3] <- paste(round(mean(group_2$CSF.pTau.INNO),2), "   (",round(sd(group_2$CSF.pTau.INNO),2),") ", sep = "" )
  group_2_table[10,4] <- paste(group_2_summary[19,3], "   (",group_2_summary[19,4],") ", sep = "" )
  group_2_table[10,5] <- paste(group_2_summary[14,3], "   (",group_2_summary[14,4],") ", sep = "" )
  group_2_table[10,6] <- paste(group_2_summary[9,3], "   (",group_2_summary[9,4],") ", sep = "" )
  group_2_table[10,7] <- paste(group_2_summary[4,3], "   (",group_2_summary[4,4],") ", sep = "" )
  
  ops <- list(
    group_1_table = group_1_table, 
    group_2_table = group_2_table)
  return(ops)
  
}