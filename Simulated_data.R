############################################
# Synthetic ATN data
# Lucy Nott
# Thurs 28th Jan 
# 17 Variables - 300 Observations 


# Load the Necessary Libraries
library(ggplot2)
library(reshape2)
library(dplyr)

# Remove base
rm(list = ls())

# Set random seed 
set.seed(10258884)

# Variables: ID, ABeta, pTau, tTau, Sum.Hippo, Centiloid, Age, Apoe4, Education,
# Sex, Ab.status, Tau181.status, Tau.status, Diagnosis, Burnham, Clifford

N = 45

# Setting the proportions according to the real data 

no.AD = round(N*0.15)
no.MCI = round(N*0.20)
no.HC = round(N*0.65)

########################
##### Demographic ######


simulated.data <- data.frame(
  # The ID number (the point in dataframe)
  ID = 1:N, 
  Age = rnorm(N, mean = 72, sd = 6), 
  apoe4 = sample(c(0,1), size = N, 
                 replace = T, prob = c(0.6, 0.4)), 
  Sex = sample(c("Male", "Female"), size = N, 
               replace = T, prob = c(0.5,0.5)), 
  Education_binary = sample(c("0","1"), size = N, 
                     replace = T, prob = c(0.4,0.6)), 
  Diagnosis = c(rep("HC", no.HC), rep("MCI", no.MCI), rep("AD", no.AD))
)

########################
# Add in CSF Variables #

####### AB ############


simulated.data$CSF.AB42.INNO <- c(rnorm(no.HC, mean = 900, sd =100),
                           rnorm(no.MCI, mean = 700, sd = 100),
                           rnorm(no.AD, mean = 580, sd = 95))

####### pTau ############


simulated.data$CSF.pTau.INNO <- c(rnorm(no.HC, mean = 50, sd =9),
                           rnorm(no.MCI, mean = 60, sd = 12),
                           rnorm(no.AD, mean = 80, sd = 12))


####### tTau ############


simulated.data$CSF.tTau.INNO <- c(rnorm(no.HC, mean = 200, sd =55),
                             rnorm(no.MCI, mean = 300, sd = 95),
                             rnorm(no.AD, mean = 500, sd = 95))

########################
# Add in Other Variables 

####### HIPPOCAMPUS ############


simulated.data$Sum.hippo <- c(rnorm(no.HC, mean = 6, sd =0.2),
                             rnorm(no.MCI, mean = 5.5, sd = 0.35),
                             rnorm(no.AD, mean = 4.5, sd = 0.5))

####### CENTILOID ############


simulated.data$Centiloid <- c(rnorm(no.HC, mean = 17, sd =15),
                             rnorm(no.MCI, mean = 43, sd = 25),
                             rnorm(no.AD, mean = 90, sd = 20))


# Check summaries of variables 

summary(simulated.data$CSF.AB42.INNO)      # Looks all good 
summary(simulated.data$CSF.pTau.INNO)    # Need to correct for negative values - no negs
summary(simulated.data$CSF.tTau.INNO)    # Need to correct for negative values
summary(simulated.data$Sum.hippo) # Looks all good
summary(simulated.data$Centiloid)   # Correct for negative values - no less than -27



# Correct for Negative pTau Values 

for(i in 1:N){
  
  # CORRECT CSF.pTau Values 
  while(simulated.data$CSF.pTau.INNO[i] < 20){
    if(simulated.data$Diagnosis[i] == "HC"){
      simulated.data$CSF.pTau[i] <- rnorm(1,mean = 50, sd =9)
    }
    if(simulated.data$Diagnosis[i] == "MCI"){
      simulated.data$CSF.pTau.INNO[i] <- rnorm(1, mean = 60, sd = 12)
    }
    if(simulated.data$Diagnosis[i] == "AD"){
      simulated.data$CSF.pTau.INNO[i] <- rnorm(1, mean = 80, sd = 12)
    }
  }
  
  # Correct CSF.tTau.INNO Values 
  while(simulated.data$CSF.tTau.INNO[i] < 60){
    if(simulated.data$Diagnosis[i] == "HC"){
      simulated.data$CSF.tTau.INNO[i] <- rnorm(1,mean = 200, sd =55)
    }
    if(simulated.data$Diagnosis[i] == "MCI"){
      simulated.data$CSF.tTau.INNO[i] <- rnorm(1,mean = 300, sd = 95)
    }
    if(simulated.data$Diagnosis[i] == "AD"){
      simulated.data$CSF.tTau.INNO[i] <- rnorm(1, mean = 500, sd = 95)
    }
  }
  
  # Correct Centiloid Values 
  while(simulated.data$Centiloid[i] < -27){
    if(simulated.data$Diagnosis[i] == "HC"){
      simulated.data$Centiloid[i] <- rnorm(1,mean = 17, sd =15)
    }
    if(simulated.data$Diagnosis[i] == "MCI"){
      simulated.data$Centiloid[i] <- rnorm(1,mean = 43, sd = 25)
    }
    if(simulated.data$Diagnosis[i] == "AD"){
      simulated.data$Centiloid[i] <- rnorm(1, mean = 90, sd = 20)
    }
  }
  
}

# Check the summaries

summary(simulated.data$CSF.pTau.INNO)  # Much better
summary(simulated.data$CSF.tTau.INNO)    
summary(simulated.data$Centiloid)  
# 
simulated.data$Diagnosis <- factor(simulated.data$Diagnosis, levels = c("AD", "MCI", "HC"))

# Add in AB positive, pTau Positive and tTau Positive.
# To see the cut points please view: https://pubmed.ncbi.nlm.nih.gov/26401938/

simulated.data <- mutate(simulated.data, AB.status = ifelse(CSF.AB42.INNO < 656,
                                                            "positive", "negative"))
simulated.data <- mutate(simulated.data, pTau.status = ifelse(CSF.pTau.INNO > 73,
                                                            "positive", "negative"))
simulated.data <- mutate(simulated.data, tTau.status = ifelse(CSF.tTau.INNO > 303,
                                                              "positive", "negative"))
# # Add in Burnham classification 
# 
# # See above for alternative way to use mutate - feed it multiple variables to make in the one go
# simulated.data <- mutate(simulated.data, A_status = ifelse(AB.status == "positive", "A+", "A-"),
#                   T_status = ifelse(pTau.status == "positive", "T+", "T-"),
#                   N_status = ifelse(tTau.status == "positive", "N+", "N-")
#                                     )
# simulated.data <- tidyr::unite(simulated.data, Overall_status, A_status:N_status, sep = '/')
# 
# simulated.data$Overall_status <- factor(simulated.data$Overall_status, levels = c("A+/T+/N+", "A+/T+/N-", "A+/T-/N+", 
#                                                             "A-/T+/N+", "A+/T-/N-","A-/T-/N+", 
#                                                             "A-/T+/N-", "A-/T-/N-"))
# 
# 
# simulated.data <- mutate(
#   simulated.data, Burnham_class = ifelse(Overall_status == "A-/T-/N-", "Normal AD Biomarkers", 
#                         ifelse(Overall_status == "A-/T+/N-", "Non-AD pathological Change",
#                                ifelse(Overall_status == "A-/T+/N+", "Non-AD pathological Change", 
#                                       ifelse(Overall_status == "A-/T-/N+", "Non-AD pathological Change", 
#                                              ifelse(Overall_status == "A+/T-/N-", "Pathological Change",
#                                                     ifelse(Overall_status == "A+/T-/N+", "Pathological Change", 
#                                                            ifelse(Overall_status == "A+/T+/N+", "AD", 
#                                                                   ifelse(Overall_status == "A+/T+/N-", "AD", NA)))))))))
# simulated.data <- mutate(simulated.data, Clifford_class = ifelse(Burnham_class == "Normal AD Biomarkers", "MCI unlikely due to AD", 
#                                            ifelse(Burnham_class == "AD", "Stage 2, clinically asymptomatic", 
#                                                   ifelse(Burnham_class == "Pathological Change", "Stage 1, preclinical AD stage", 
#                                                          ifelse(Burnham_class == "Non-AD pathological Change", "SNAP", NA)))))
# 
# 
# 
# 
# # Surface for APOE4 
# 
# simulated.data$Apoe4 <- factor(simulated.data$apoe4, levels = c("1","0"))
# 
# simulated.data$Clifford_class <- factor(simulated.data$Clifford_class, levels = c("Stage 2, clinically asymptomatic",
#                                                                                   "Stage 1, preclinical AD stage", 
#                                                                                   "SNAP", 
#                                                                                   "MCI unlikely due to AD"))
# 
# 
# 
# 
# simulated.data$Burnham_class <- factor(simulated.data$Burnham_class, levels = c("AD", 
#                                                                                 "Pathological Change", 
#                                                                                 "Non-AD pathological Change", 
#                                                                                 "Normal AD Biomarkers"))
# 
# simulated.data <- mutate(simulated.data, Age_binary = ifelse(Age < 72.5,1,0))



# Save data into RData Format 

# save(simulated.data, file = "simulated_data.RData")


# write.csv(simulated.data, 'C:/Users/NOT031/Dropbox/VacationWork2020&2021/SIMULATED_DATA_APP\\tester_LN2.csv', row.names = T)

