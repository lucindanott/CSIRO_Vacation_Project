syntheticProccess <- function (){
  
  load("simulated_data.RData")
  
  # load("processData2.R")
  ############################ GROUP 1 AXIS TITLES #########################
  
  
  
  # # ADD in AB status 
  # group_1_table[11,1] <- "Number of CSF AB1-42 pg/mL Status"
  # group_1_table[11,2] <- "Positive"
  # 
  # group_1_table[11,3] <- AB_status[2,3]+AB_status[1,3]
  # group_1_table[11,4] <- 0
  # group_1_table[11,5] <- 0
  # group_1_table[11,6] <- AB_status[2,3]
  # group_1_table[11,7] <- AB_status[1,3]
  # 
  # 
  # group_1_table[12,2] <- "Negative"
  # 
  # group_1_table[12,3] <- 167
  # group_1_table[12,4] <- AB_status[4,3]
  # group_1_table[12,5] <- AB_status[3,3]
  # group_1_table[12,6] <- 0
  # group_1_table[12,7] <- 0
  # 
  # # ADD in Tau Status 
  # group_1_table[13,1] <- "Number of CSF pTau pg/mL Status"
  # group_1_table[13,2] <- "Positive"
  # group_1_table[13,3] <- (27+31+13)
  # group_1_table[13,4] <- 0
  # group_1_table[13,5] <- tau_status[4,3]
  # group_1_table[13,6] <- 0
  # group_1_table[13,7] <- tau_status[1,3]
  # 
  # group_1_table[14,2] <- "Negative"
  # group_1_table[14,3] <- (9+159+24)
  # group_1_table[14,4] <- tau_status[5,3]
  # group_1_table[14,5] <- tau_status[3,3]
  # group_1_table[14,6] <- tau_status[2,3]
  # group_1_table[14,7] <- 0
  # 
  # 
  # # ADD in NEURODEGENERATION 
  # 
  # group_1_table[15,1] <- "Number of CSF tTau pg/mL Status"
  # group_1_table[15,2] <- "Positive"
  # group_1_table[15,3] <- (16+35+13)
  # group_1_table[15,4] <- 0
  # group_1_table[15,5] <- CSF.ptau181_status[6,3]
  # group_1_table[15,6] <- CSF.ptau181_status[4,3]
  # group_1_table[15,7] <- CSF.ptau181_status[2,3]
  # 
  # group_1_table[16,2] <- "Negative"
  # group_1_table[16,3] <- (20+155+24)
  # group_1_table[16,4] <- CSF.ptau181_status[7,3]
  # group_1_table[16,5] <- CSF.ptau181_status[5,3]
  # group_1_table[16,6] <- CSF.ptau181_status[3,3]
  # group_1_table[16,7] <- CSF.ptau181_status[1,3]
  
  ################## GROUP 1 TABLE COMPLETE ##################################
  
  ##################    GROUP 2 TABLE  ##################################
  

  ops <- list(
    group_1_table = group_1_table, 
    group_2_table  = group_2_table
  )
  
  return(ops)
  
}
