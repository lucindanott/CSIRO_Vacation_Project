#
##
###
#############################################################################
######################## SIMULATED DATA SERVER CODE #########################
#############################################################################
###
##
#
############################################
# Synthetic ATN data
# Lucinda Nott
# Supervised by Marcela Cespedes 


server <- function(input, output){
  
  credentials <- data.frame(
    user = "ATN_synthetic_app",
    password = "live_laugh_biomarkers", 
    stringsAsFactors = F
  )
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  # LOAD ALL FUNCTIONS AND SOURCES 
  load("simulated_data.RData")
  load("ExampleCSV.RData")
  source("ScatterPlotFunction.R")
  source("AnimatedScatterPlotFunction.R")
  source("PlanesFunction.R")
  source("AnimatedPlanesFunction.R")
  source("HippoFunction.R")
  # source("G1CubeVisualisation.R")
  source("AnimatedG1CubeVisualisation.R")
  source("G2CubeVisualisation.R")
  source("AnimatedG2CubeVisualisation.R")
  source("BoxplotFunction.R")
  source("new_table_function.R")
  
  ########################## DOWNLOAD DATA SET ####################################
  
  #### Our dataset 
  output$downloadData <- downloadHandler(
    # new.dat <- req(data_internal$raw)
    filename = function() {
      paste("Synthetic ATN Data - ",Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(simulated.data, file)
    }
  )
  
  ###################### DOWNLOAD EXAMPLE DATA SET ###########################
  # Example_csv <- read.csv("Example_csv.csv", header = T)
  output$DOWNLOADEXAMPLE <- downloadHandler(
    # new.dat <- req(data_internal$raw)
    filename = function() {
      paste("Example CSV File - ",Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(Example_csv, file)
    }
  )
  
  output$DOWNLOADEXAMPLE2 <- downloadHandler(
    # new.dat <- req(data_internal$raw)
    filename = function() {
      paste("Example CSV File - ",Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(Example_csv, file)
    }
  )
  
  
  ############################# ADD IN DATA OPTION ##############################
  
  #### THis is for the first tab data selection and gives a header of the table
  output$contents <- renderTable({
    
    # Input file1 will be NULL initially after the user selects 
    # and uploads a file, head of that data file will be shown 
    
    req(input$file1)
    
    new_data_in <- read.csv(input$file1$datapath, 
                            header = T)
    head(new_data_in)
    
  })
  
  output$contents2 <- renderTable({
    # Input INPUTfile will be NULL initiailly after the user selects 
    # and uploads a file, head of that data file will be shown 
    
    req(input$INPUTfile)
    
    new_data_in2 <- read.csv(input$INPUTfile$datapath, 
                            header = T)
    head(new_data_in2)
    
    
  })
  
  
  data_internal <- reactiveValues(raw = NULL)
  observeEvent(input$newDataSUBMIT, {
    file1_input<- read.csv(
      file = input$file1$datapath, 
      header = T, 
      stringsAsFactors = F
    )
    
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
    # file1_input$Apoe4 <- factor(simulated.data$apoe4, levels = c("1","0"))
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
    data_internal$raw <- file1_input
    shinyjs::runjs("window.scrollTo(0, 100)")

  })
  observeEvent(input$SUMBITNEW, {
    file1_input<- read.csv(
      file = input$INPUTfile$datapath, 
      header = T, 
      stringsAsFactors = F
    )
    
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
    # file1_input$Apoe4 <- factor(simulated.data$apoe4, levels = c("1","0"))
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
    data_internal$raw <- file1_input
    shinyjs::runjs("window.scrollTo(0, 100)")
    
    
  })
  observeEvent(input$sample_or_real, {
    if(input$sample_or_real == "sample"){
      data_internal$raw <- simulated.data
    }else 
      data_internal$raw <- NULL
  })
  
  # do reactives for CSF cut offs 
  
  AB_cutoff <- reactiveValues(raw = NULL)
  ptau_cutoff_lower <- reactiveValues(raw = NULL)
  ptau_cutoff_upper <- reactiveValues(raw = NULL)
  ttau_cutoff_lower <- reactiveValues(raw = NULL)
  ttau_cutoff_upper <- reactiveValues(raw = NULL)
  AB_cutoff_singular <- reactiveValues(raw = NULL)
  ptau_cutoff_singular <- reactiveValues(raw = NULL)
  ttau_cutoff_singular <- reactiveValues(raw = NULL)
  centiloid_cutoff <- reactiveValues(raw = NULL)
  
  # For sample Data 
  observeEvent(input$sample_or_real, {
    if(input$sample_or_real == "sample"){
      AB_cutoff$raw <- 656 
      ptau_cutoff_lower$raw <- 59.23 
      ptau_cutoff_upper$raw <- 73.83 
      ttau_cutoff_lower$raw <- 303.54 
      ttau_cutoff_upper$raw <- 378.65 
      centiloid_cutoff$raw <- 20
    }
    
  })
  
  
  observeEvent(input$newDataSUBMIT, {
    AB_cutoff$raw <- 656
    ptau_cutoff_lower$raw <- 59.23 
    ptau_cutoff_upper$raw <- 73.83 
    ttau_cutoff_lower$raw <- 303.54 
    ttau_cutoff_upper$raw <- 378.65
    centiloid_cutoff$raw <- 20
  })
  
  # For SINGULAR INPUT 
  
  observeEvent(input$THRESHOLDSUB, {
    AB_cutoff_singular$raw <- input$ABthreshold
    ptau_cutoff_singular$raw <- input$pTauThreshold
    ttau_cutoff_singular$raw <- input$tTautheshold
  })
  
  ################# CHANGE THE DATA FILE USED IF SERVE IS CLICKED ##############
  

  LEGEND_1 <- list(
    title = list(text = "<b>ATN Burnham Classification</b>"),
    font = list(
      family = "sans-serif",
      size = 12,
      color = "#000"),
    bgcolor = "#E2E2E2",
    bordercolor = "black",
    borderwidth = 1.25, 
    x = 1,
    y = 0.5)
  
  ############################ GROUP 2 AXIS TITLES #########################
  
  LEGEND_2 <- list(
    title = list(text = "<b>ATN Clifford Classification</b>"),
    font = list(
      family = "sans-serif",
      size = 12,
      color = "#000"),
    bgcolor = "#E2E2E2",
    bordercolor = "black",
    borderwidth = 1.25, 
    x = 1, 
    y = 0.5)
  
  ################# GROUP 1 LABELS ###############
  axx <- list(
    title = "CSF AB1-42 pg/mL"
  )
  
  axy <- list(
    title = "CSF p-Tau pg/mL"
  )
  
  axz <- list(
    title = "CSF t-Tau pg/mL"
  )
  
  ################# GROUP 2 LABELS ###############
  axx2 <- list(
    title = "Hippocampus mL<sup>3</sup>"
  )
  axy2 <- list(
    title = "CSF p-Tau pg/mL"
  )
  axz2 <- list(
    title = "Centiloid"
  )
  
  ################# GROUP 1 CUTOFF LABELS ###############
  AB_cutoff_label <- list(
    title = "CSF AB 1-42 pg/mL Cutoff"
  )
  ptau_cutoff_label <- list(
    title = "CSF pTau pg/mL Cutoff"
  )
  ttau_cutoff_label <- list(
    title = "CSF tTau pg/mL Cutoff"
  )
  
  HIPPO_cutoff_label <- list(
    title = "Hippocampus mL<sup>3</sup> Cutoff"
  )
  Centloid_cutoff_label <- list(
    title = "Centiloid Cutoff"
  )
  hippo.boxplot <- list(
    title = "Hippocampus"
  )
  
  ############### Warning Message ########################
  shinyalert(title = "Important Note", 
             text = "This application was developed by CSIRO student intern. \n
             This app is used as an interactive demostration tool for Alzheimer's Disease to enable clear, 
             effective and engaging communication of project results for future researchers. The data included in this app is 
             obtained from the AIBL study and contains sensitive medical, demographic and cognitive assessments participant 
             information.\n
             For more information on the AIBL study, please refer to https://aibl.csiro.au/", 
             type = "info"
  )
  
  ############################# SANKEY DIAGRAM ########################
  
  # Create a NETWORK for CSF DATA STUFF 
  output$SankeyDiagram <- renderSankeyNetwork({
    DATALINKS <- data.frame(
      source = c("AB-Amyloid", "AB-Amyloid", "CSF AB1-42 pg/mL", "Centiloid", 
                 "Tau", "Tau","Tau", "PET SCAN", "CSF p-Tau pg/mL", "CSF p-Tau pg/mL", 
                 "Neurodegeneration", "Neurodegeneration", "CSF t-Tau pg/mL","Hippocampus"), 
      target = c("CSF AB1-42 pg/mL",  "Centiloid", "Group 1", "Group 2","PET SCAN",
                 "CSF p-Tau pg/mL","CSF p-Tau pg/mL","Expression of Interest Required",
                 "Group 1", "Group 2", "CSF t-Tau pg/mL","Hippocampus","Group 1","Group 2"), 
      value = c(2,2,2,2,2,2,2,2,2,2,2,2,2,2)
    )
    DATALINKS
    
    nodes <- data.frame(
      name=c(as.character(DATALINKS$source), 
             as.character(DATALINKS$target)) %>% unique()
    )
    
    # ADD COLOUR TO IT 
    DATALINKS$group <- as.factor(c("TYPE_1", "TYPE_2", "TYPE_1", 
                                   "TYPE_2", "TYPE_3","TYPE_1", "TYPE_2", "TYPE_3", 
                                   "TYPE_1", "TYPE_2", "TYPE_1", "TYPE_2", "TYPE_1", "TYPE_2"))
    nodes$group <- as.factor(c("my_unique_group"))
    
    # Give a color for each group:
    my_color <- 'd3.scaleOrdinal() .domain(["TYPE_1", "TYPE_2", "TYPE_3", "my_unique_group"]) .range(["#69b3a2", "steelblue", "thistle", "grey"])'
    
    
    
    # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
    DATALINKS$IDsource <- match(DATALINKS$source, nodes$name)-1 
    DATALINKS$IDtarget <- match(DATALINKS$target, nodes$name)-1
    
    # Make the Network
    p <- sankeyNetwork(Links = DATALINKS, Nodes = nodes,
                       Source = "IDsource", Target = "IDtarget",
                       Value = "value", NodeID = "name", 
                       colourScale=my_color, LinkGroup="group", NodeGroup="group",
                       sinksRight=FALSE,
                       fontFamily = 'serif',
                       fontSize = 19, width = 1000, height = 100)
    p
  })
  
  
  #
  ##
  ###
  #############################################################################
  ################################## GROUP 1 ##################################
  #############################################################################
  ###
  ##
  #
  
  
  
  ################################ 2D Visualisation ###########################
  ################################ 2D RADIO GRAPH #############################
  d <- reactive({
    new.dat <- req(data_internal$raw)
    X_input <- switch(input$X_input,
                      AB = new.dat$CSF.AB42.INNO,
                      pTau = new.dat$CSF.pTau.INNO,
                      tTau = new.dat$CSF.tTau.INNO)
    
  })
  q <- reactive({
    new.dat <- req(data_internal$raw)
    Y_input <- switch(input$Y_input,
                      AB = new.dat$CSF.AB42.INNO,
                      pTau = new.dat$CSF.pTau.INNO,
                      tTau = new.dat$CSF.tTau.INNO)
  })
  output$plot <- renderPlotly({
    new.dat <- req(data_internal$raw)
    X_input <- input$X_input
    Y_input <- input$Y_input
    X_LAB <- "default"
    if(X_input == 'AB'){
      X_LAB = "CSF AB1-42 pg/mL"
      # INTER = 656
    }
    if(X_input == 'pTau'){
      X_LAB = "CSF p-Tau pg/mL"
    }
    if(X_input == 'tTau'){
      X_LAB = "CSF t-Tau pg/mL"
    }
    Y_LAB <- "default"
    if(Y_input == 'AB'){
      Y_LAB = "CSF AB1-42 pg/mL"
      # INTERy = 656
    }
    if(Y_input == 'pTau'){
      Y_LAB = "CSF p-Tau pg/mL"
    }
    if(Y_input == 'tTau'){
      Y_LAB = "CSF t-Tau pg/mL"
    }
    
    
    gg <- ggplot(new.dat, aes_string(x = d(), y=q()))+
      geom_point(data = new.dat, aes(color = Burnham_class, group = ID))+
      theme_bw()+
      geom_smooth(method = "loess")+
      scale_color_manual(values = c("firebrick", "darkorange", "gold", "forestgreen"))+
      xlab(X_LAB)+
      ylab(paste(Y_LAB))+
      # geom_vline(xintercept = INTER)+
      # geom_hline(yintercept = INTERy)+
      labs(color = "ATN Burnham et. al \n Classification")+
      ggtitle(paste(X_LAB,"vs",Y_LAB, sep = "  "))
    
    
    ggplotly(gg)
  })
  
  ############################# 3D Visualisation ############################## 
  ############################ SCATTER AND PLANES #############################
  
  ######################### STATIONARY SCATTER PLOT ###########################

  v <- reactiveValues(
    plot = NULL
  )
  
  observeEvent(input$Static, {
    new.dat <- req(data_internal$raw)
    v$plot <- ScatterPlotFunction(
      d = new.dat, 
      xdat = new.dat$CSF.AB42.INNO, 
      ydat = new.dat$CSF.pTau.INNO, 
      zdat = new.dat$CSF.tTau.INNO, 
      cols = new.dat$Burnham_class, 
      leg = LEGEND_1, 
      xax = axx, 
      yax = axy, 
      zax = axz
    )
  }
  )
  
  
  ########### ANIMATED SCATTER PLOT #############
  
  observeEvent(input$rotating, {
    new.dat <- req(data_internal$raw)
    v$plot <-  AnimatedScatterPlotFunction(
      d = new.dat, 
      xdat = new.dat$CSF.AB42.INNO, 
      ydat = new.dat$CSF.pTau.INNO, 
      zdat = new.dat$CSF.tTau.INNO, 
      cols = new.dat$Burnham_class, 
      leg = LEGEND_1, 
      xax = axx, 
      yax = axy, 
      zax = axz
    )
    
  })
  
  
  ################ CONDITIONAL ################
  # Conditional that the user uses supplied data 
  # or data with same cut points!
  
  ############## AGE GROUP: 60-70 #############

 observeEvent(input$staticP2, {
   new.dat <- req(data_internal$raw)
   AB_threshold <- req(AB_cutoff$raw)
   ptau_threshold <- req(ptau_cutoff_lower$raw)
   ttau_treshold <- req(ttau_cutoff_lower$raw)
   df_young <- filter(new.dat, Age < 70)
   v$plot <- PlanesFunction(
     dat = df_young, 
     xinput = df_young$CSF.AB42.INNO, 
     yinput = df_young$CSF.pTau.INNO, 
     zinput = df_young$CSF.tTau.INNO, 
     cols = df_young$Burnham_class, 
     leg = LEGEND_1,
     xax = axx,
     yax = axy,
     zax = axz,
     XCUT = AB_threshold, 
     YCUT = ptau_threshold, 
     ZCUT = ttau_treshold, 
     XNAME = AB_cutoff_label, 
     YNAME = ptau_cutoff_label, 
     ZNAME = ttau_cutoff_label
   )
 })


 observeEvent(input$rotateP2, {
   new.dat <- req(data_internal$raw)
   AB_threshold <- req(AB_cutoff$raw)
   ptau_threshold <- req(ptau_cutoff_lower$raw)
   ttau_treshold <- req(ttau_cutoff_lower$raw)
   df_young <- filter(new.dat, Age < 70)
   v$plot <- AnimatedPlanesFunction(
     dat = df_young, 
     xinput = df_young$CSF.AB42.INNO, 
     yinput = df_young$CSF.pTau.INNO, 
     zinput = df_young$CSF.tTau.INNO, 
     cols = df_young$Burnham_class, 
     leg = LEGEND_1,
     xax = axx,
     yax = axy,
     zax = axz,
     XCUT = AB_threshold, 
     YCUT = ptau_threshold, 
     ZCUT = ttau_treshold, 
     XNAME = AB_cutoff_label, 
     YNAME = ptau_cutoff_label, 
     ZNAME = ttau_cutoff_label
   ) 
 })

 observeEvent(input$staticP3, {
   new.dat <- req(data_internal$raw)
   AB_threshold <- req(AB_cutoff$raw)
   ptau_threshold <- req(ptau_cutoff_upper$raw)
   ttau_treshold <- req(ttau_cutoff_upper$raw)
   df_old <- filter(new.dat, Age > 70)
   v$plot <- PlanesFunction(
     dat = df_old, 
     xinput = df_old$CSF.AB42.INNO, 
     yinput = df_old$CSF.pTau.INNO, 
     zinput = df_old$CSF.tTau.INNO, 
     cols = df_old$Burnham_class, 
     leg = LEGEND_1,
     xax = axx,
     yax = axy,
     zax = axz,
     XCUT = AB_threshold, 
     YCUT = ptau_threshold, 
     ZCUT = ttau_treshold, 
     XNAME = AB_cutoff_label, 
     YNAME = ptau_cutoff_label, 
     ZNAME = ttau_cutoff_label
   )

 })

 observeEvent(input$rotateP3, {
   new.dat <- req(data_internal$raw)
   AB_threshold <- req(AB_cutoff$raw)
   ptau_threshold <- req(ptau_cutoff_upper$raw)
   ttau_treshold <- req(ttau_cutoff_upper$raw)
   df_old <- filter(new.dat, Age > 70)
   v$plot <- AnimatedPlanesFunction(
     dat = df_old, 
     xinput = df_old$CSF.AB42.INNO, 
     yinput = df_old$CSF.pTau.INNO, 
     zinput = df_old$CSF.tTau.INNO, 
     cols = df_old$Burnham_class, 
     leg = LEGEND_1,
     xax = axx,
     yax = axy,
     zax = axz,
     XCUT = AB_threshold, 
     YCUT = ptau_threshold, 
     ZCUT = ttau_treshold, 
     XNAME = AB_cutoff_label, 
     YNAME = ptau_cutoff_label, 
     ZNAME = ttau_cutoff_label
   )


 })

  output$PLOTLYOUTPUT <- renderPlotly({
    if (is.null(v$plot)) return()
    v$plot
  
  })

  ########################### A+/T+/N+ Visualisation ##########################
  ################################# ALL CUBES #################################




  cube_animate <- reactiveValues(plot = NULL)

  observeEvent(input$ATNStatic, {
    new.dat <- req(data_internal$raw)
    AB_threshold <- req(AB_cutoff$raw)
    ptau_threshold <- req(ptau_cutoff_lower$raw)
    ttau_treshold <- req(ttau_cutoff_lower$raw)
    df_young <- filter(new.dat, Age < 70)
    cube_animate$plot <- G1CubeVisualisation(dat = df_young, 
                                             xinput = df_young$CSF.AB42.INNO,
                                             yinput = df_young$CSF.pTau.INNO, 
                                             zinput = df_young$CSF.tTau.INNO, 
                                             leg = LEGEND_1,
                                             xax=axx,
                                             yax = axy,
                                             zax = axz, 
                                             XCUT = AB_threshold, 
                                             YCUT = ptau_threshold, 
                                             ZCUT = ttau_treshold)
  })

  observeEvent(input$ATNrotating, {
    new.dat <- req(data_internal$raw)
    AB_threshold <- req(AB_cutoff$raw)
    ptau_threshold <- req(ptau_cutoff_lower$raw)
    ttau_treshold <- req(ttau_cutoff_lower$raw)
    df_young <- filter(new.dat, Age < 70)
    cube_animate$plot <- AnimatedG1CubeVisualisation(dat = df_young, 
                                             xinput = df_young$CSF.AB42.INNO,
                                             yinput = df_young$CSF.pTau.INNO, 
                                             zinput = df_young$CSF.tTau.INNO, 
                                             leg = LEGEND_1,
                                             xax=axx,
                                             yax = axy,
                                             zax = axz, 
                                             XCUT = AB_threshold, 
                                             YCUT = ptau_threshold, 
                                             ZCUT = ttau_treshold)
  })

  observeEvent(input$ATNStaticP2, {
    new.dat <- req(data_internal$raw)
    AB_threshold <- req(AB_cutoff$raw)
    ptau_threshold <- req(ptau_cutoff_upper$raw)
    ttau_treshold <- req(ttau_cutoff_upper$raw)
    df_old <- filter(new.dat, Age > 70)
    cube_animate$plot <- G1CubeVisualisation(dat = df_old, 
                                             xinput = df_old$CSF.AB42.INNO,
                                             yinput = df_old$CSF.pTau.INNO, 
                                             zinput = df_old$CSF.tTau.INNO, 
                                             leg = LEGEND_1,
                                             xax=axx,
                                             yax = axy,
                                             zax = axz, 
                                             XCUT = AB_threshold, 
                                             YCUT = ptau_threshold, 
                                             ZCUT = ttau_treshold)
  
    })
  
  observeEvent(input$ATNrotatingP2, {
    new.dat <- req(data_internal$raw)
    AB_threshold <- req(AB_cutoff$raw)
    ptau_threshold <- req(ptau_cutoff_upper$raw)
    ttau_treshold <- req(ttau_cutoff_upper$raw)
    df_old <- filter(new.dat, Age > 70)
    cube_animate$plot <- AnimatedG1CubeVisualisation(dat = df_old, 
                                             xinput = df_old$CSF.AB42.INNO,
                                             yinput = df_old$CSF.pTau.INNO, 
                                             zinput = df_old$CSF.tTau.INNO, 
                                             leg = LEGEND_1,
                                             xax=axx,
                                             yax = axy,
                                             zax = axz, 
                                             XCUT = AB_threshold, 
                                             YCUT = ptau_threshold, 
                                             ZCUT = ttau_treshold)
  })

  output$plot5.P3 <- renderPlotly({
    if (is.null(cube_animate$plot)) return()
    cube_animate$plot
  })


  ############################# SCATTER AND CUBE ##############################

  output$OWNBIOMARKERS <- renderPlotly({
    new.dat <- req(data_internal$raw)

    df6 <- new.dat[,c("CSF.AB42.INNO", "CSF.pTau.INNO", "CSF.tTau.INNO", "Burnham_class")]
    New.Person <- data.frame(input$obs, input$ptau, input$ttau, "My own markers")
    names(New.Person) <- c("CSF.AB42.INNO", "CSF.pTau.INNO", "CSF.tTau.INNO", "Burnham_class")
    newDF <- rbind(df6, New.Person)
    newDF$Burnham_class <- factor(newDF$Burnham_class, levels = c("AD", "Pathological Change", "Non-AD pathological Change",
                                                      "Normal AD Biomarkers", "My own markers"))
    ADD_DATA <- mutate(newDF, size_guide = ifelse(Burnham_class == "My own markers",18,12))
    ADD_DATA$size_guide <- as.numeric(ADD_DATA$size_guide)

    if (input$DATAVIS =="3D Plot"){

      own_dat <- plot_ly(ADD_DATA,
                         x = ~CSF.AB42.INNO,
                         y = ~CSF.pTau.INNO,
                         z = ~CSF.tTau.INNO,
                         type = "scatter3d",
                         color = ~Burnham_class,
                         colors = c("firebrick", "darkorange", "gold", "forestgreen","hotpink" ),
                         mode = "markers",
                         marker = list(size = ~size_guide))
      # size = ~size_guide,
      # sizes = c(700,2000))
      own_dat <- own_dat %>%
        layout(legend = LEGEND_1, scene = list(xaxis = axx, yaxis = axy, zaxis = axz))
      own_dat


    } else if(input$DATAVIS == "A+/T+/N+ Visualisation"){

      Ageinput <- input$age1

      if (Ageinput > 70){
        old_df <- filter(new.dat, Age >70)
        df6 <- old_df[,c("CSF.AB42.INNO", "CSF.pTau.INNO", "CSF.tTau.INNO", "Burnham_class")]
        New.Person <- data.frame(input$obs, input$ptau, input$ttau, "My own markers")
        names(New.Person) <- c("CSF.AB42.INNO", "CSF.pTau.INNO", "CSF.tTau.INNO", "Burnham_class")
        newDF <- rbind(df6, New.Person)
        newDF$Burnham_class <- factor(newDF$Burnham_class, levels = c("AD", "Pathological Change", "Non-AD pathological Change",
                                                          "Normal AD Biomarkers", "My own markers"))
        ADD_DATA <- mutate(newDF, size_guide = ifelse(Burnham_class == "My own markers",18,12))
        ADD_DATA$size_guide <- as.numeric(ADD_DATA$size_guide)
        df7 <- mutate(ADD_DATA, colours_new = ifelse(CSF.pTau.INNO >73.83 & CSF.tTau.INNO > 378.65 & CSF.AB42.INNO <656 & Burnham_class == "AD",
                                                     "selected - AD",
                                                     ifelse(CSF.pTau.INNO >73.83 & CSF.tTau.INNO >378.65 & CSF.AB42.INNO < 656 & Burnham_class == "Pathological Change",
                                                            "selected - Pathological Change",
                                                            ifelse(CSF.pTau.INNO >73.83 & CSF.tTau.INNO > 378.65 & CSF.AB42.INNO < 656 & Burnham_class == "Non-AD pathological Change",
                                                                   "selected - Non-AD pathological Change",
                                                                   ifelse(CSF.pTau.INNO >73.83 & CSF.tTau.INNO > 378.65 & CSF.AB42.INNO < 656 & Burnham_class == "Normal AD Biomarkers",
                                                                          "selected - Normal AD Biomarker",
                                                                          ifelse(Burnham_class == "My own markers",
                                                                                 "My own markers", "unselected"))))))


        df7$colours_new <- factor(df7$colours_new, levels = c("unselected", "selected - AD",
                                                              "selected - Pathological Change",
                                                              "selected - Non-AD pathological Change",
                                                              "selected - Normal AD Biomarker",
                                                              "My own markers"))


        df_mesh_1 <- data.frame(X_VAL = c(656,  656,    min(df7$CSF.AB42.INNO),    min(df7$CSF.AB42.INNO),    656,   656,   min(df7$CSF.AB42.INNO),   min(df7$CSF.AB42.INNO)),
                                Y_VAL = c(73.83,  max(df7$CSF.pTau.INNO),    73.83,  max(df7$CSF.pTau.INNO),    73.83, max(df7$CSF.pTau.INNO),   73.83, max(df7$CSF.pTau.INNO)),
                                Z_VAL = c(378.65, 378.65, 378.65, 378.65, max(df7$CSF.tTau.INNO),  max(df7$CSF.tTau.INNO),  max(df7$CSF.tTau.INNO),  max(df7$CSF.tTau.INNO)),
                                MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))
        cube3 <- plot_ly()%>%
          add_markers(type = "scatter3d",
                      mode = "markers",
                      marker = list(size = ~size_guide),
                      data = df7,
                      x = ~CSF.AB42.INNO,
                      y = ~CSF.pTau.INNO,
                      z = ~CSF.tTau.INNO,
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
          layout(legend = LEGEND_1,
                 scene = list(xaxis = axx, yaxis = axy, zaxis = axz))
        cube3
      }else{
        df_young <- filter(new.dat, Age <70)
        df6 <- df_young[,c("CSF.AB42.INNO", "CSF.pTau.INNO", "CSF.tTau.INNO", "Burnham_class")]
        New.Person <- data.frame(input$obs, input$ptau, input$ttau, "My own markers")
        names(New.Person) <- c("CSF.AB42.INNO", "CSF.pTau.INNO", "CSF.tTau.INNO", "Burnham_class")
        newDF <- rbind(df6, New.Person)
        newDF$Burnham_class <- factor(newDF$Burnham_class, levels = c("AD", "Pathological Change", "Non-AD pathological Change",
                                                          "Normal AD Biomarkers", "My own markers"))
        ADD_DATA <- mutate(newDF, size_guide = ifelse(Burnham_class == "My own markers",18,12))
        ADD_DATA$size_guide <- as.numeric(ADD_DATA$size_guide)
        df7 <- mutate(ADD_DATA, colours_new = ifelse(CSF.pTau.INNO >59.23  & CSF.tTau.INNO > 303.54  & CSF.AB42.INNO <656 & Burnham_class == "AD",
                                                     "selected - AD",
                                                     ifelse(CSF.pTau.INNO >59.23  & CSF.tTau.INNO >303.54  & CSF.AB42.INNO < 656 & Burnham_class == "Pathological Change",
                                                            "selected - Pathological Change",
                                                            ifelse(CSF.pTau.INNO >59.23  & CSF.tTau.INNO > 303.54  & CSF.AB42.INNO < 656 & Burnham_class == "Non-AD pathological Change",
                                                                   "selected - Non-AD pathological Change",
                                                                   ifelse(CSF.pTau.INNO >59.23 & CSF.tTau.INNO > 303.54  & CSF.AB42.INNO < 656 & Burnham_class == "Normal AD Biomarkers",
                                                                          "selected - Normal AD Biomarker",
                                                                          ifelse(Burnham_class == "My own markers",
                                                                                 "My own markers", "unselected"))))))


        df7$colours_new <- factor(df7$colours_new, levels = c("unselected", "selected - AD",
                                                              "selected - Pathological Change",
                                                              "selected - Non-AD pathological Change",
                                                              "selected - Normal AD Biomarker",
                                                              "My own markers"))


        df_mesh_1 <- data.frame(X_VAL = c(656,  656,    min(df7$CSF.AB42.INNO),    min(df7$CSF.AB42.INNO),    656,   656,   min(df7$CSF.AB42.INNO),   min(df7$CSF.AB42.INNO)),
                                Y_VAL = c(59.23 ,  max(df7$CSF.pTau.INNO),    59.23 ,  max(df7$CSF.pTau.INNO),    59.23 , max(df7$CSF.pTau.INNO),   59.23 , max(df7$CSF.pTau.INNO)),
                                Z_VAL = c(303.54 , 303.54 , 303.54 , 303.54 , max(df7$CSF.tTau.INNO),  max(df7$CSF.tTau.INNO),  max(df7$CSF.tTau.INNO),  max(df7$CSF.tTau.INNO)),
                                MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))
        cube3 <- plot_ly()%>%
          add_markers(type = "scatter3d",
                      mode = "markers",
                      marker = list(size = ~size_guide),
                      data = df7,
                      x = ~CSF.AB42.INNO,
                      y = ~CSF.pTau.INNO,
                      z = ~CSF.tTau.INNO,
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
          layout(legend = LEGEND_1,
                 scene = list(xaxis = axx, yaxis = axy, zaxis = axz))
        cube3
      }
    }
  })

  ########################### Statistical Modelling  ##########################
  ############################## VISUALISATION ################################

  ################################ BOXPLOTS ###################################
  output$G1BOXPLOTS <- renderPlot({
    new.dat <- req(data_internal$raw)
    new.dat$apoe4 <- factor(new.dat$apoe4, levels = c("0","1"))
    new.dat$Age_binary <- factor(new.dat$Age_binary, levels = c("0","1"))
    if (input$BIOMARKER == "CSF AB1-42 pg/mL"){
      
      BoxplotFunction(new.dat, new.dat$CSF.AB42.INNO, input_title = axx)
      
    }else if (input$BIOMARKER == "CSF pTau pg/mL"){
      BoxplotFunction(new.dat, new.dat$CSF.pTau.INNO, input_title = axy)

    }else if (input$BIOMARKER == "CSF tTau pg/mL"){
      
      BoxplotFunction(new.dat, new.dat$CSF.pTau.INNO, input_title = axz)
    }
  })

  ############################## 2D SCATTERS #################################

  output$SCATTERPLOT <- renderPlotly({
    new.dat <- req(data_internal$raw)
    if (input$BIOMARKER2 == "CSF AB1-42 pg/mL"){
      plot1 <- ggplot(new.dat, aes(x= Age, y = CSF.AB42.INNO, na.rm = TRUE, color = AB.status))+
        geom_point()+
        theme_bw()+
        xlab("Age")+
        ylab("CSF AB1-42 pg/mL") +
        ggtitle("Age vs CSF AB1-42 pg/mL")+
        scale_color_manual(values = c("forestgreen", "firebrick"))+
        geom_smooth(method = "loess", se = T)
      ggplotly(plot1)
    }else if (input$BIOMARKER2 == "CSF pTau pg/mL"){
      plot1 <- ggplot(new.dat, aes(x= Age, y = CSF.pTau.INNO, na.rm = TRUE, color = pTau.status))+
        geom_point()+
        theme_bw()+
        xlab("Age")+
        ylab("CSF pTau pg/mL") +
        ggtitle("Age vs CSF pTau pg/mL")+
        scale_color_manual(values = c("forestgreen", "firebrick"))+
        geom_smooth(method = "loess", se = T)
      ggplotly(plot1)
    }else if (input$BIOMARKER2 == "CSF tTau pg/mL"){
      plot1 <- ggplot(new.dat, aes(x= Age, y = CSF.tTau.INNO, na.rm = TRUE, color = tTau.status))+
        geom_point()+
        theme_bw()+
        xlab("Age")+
        ylab("CSF tTau pg/mL") +
        ggtitle("Age vs CSF tTau pg/mL")+
        scale_color_manual(values = c("forestgreen", "firebrick"))+
        geom_smooth(method = "loess", se = T)
      ggplotly(plot1)
    }
  })

  ######################## 3D PREDICTIVE SURFACES ############################

  output$SURFACEPLOT <- renderPlotly({
    new.dat <- req(data_internal$raw)

    if (input$DEMOGRAPHIC == "APOE-e4 allele"){
      new_data_frame <- filter(new.dat, apoe4 == 1)

      my_lm <- lm(CSF.tTau.INNO ~ CSF.AB42.INNO+CSF.pTau.INNO, new_data_frame)
      # Set up Axis

      axis_x1 <- seq(min(new.dat$CSF.AB42.INNO), max(new.dat$CSF.AB42.INNO))
      axis_z1 <- seq(min(new.dat$CSF.pTau.INNO), max(new.dat$CSF.pTau.INNO))

      ## PLOT THE POINTS

      my_lm_surface <- expand.grid(CSF.AB42.INNO = axis_x1,CSF.pTau.INNO = axis_z1,KEEP.OUT.ATTRS = F)
      my_lm_surface$CSF.tTau.INNO <- predict.lm(my_lm, newdata = my_lm_surface)
      my_lm_surface <- acast(my_lm_surface, CSF.pTau.INNO ~ CSF.AB42.INNO, value.var = "CSF.tTau.INNO")

      my_plot <- plot_ly()
      my_plot <- add_markers(
        p = my_plot,
        data = new.dat,
        x = ~CSF.AB42.INNO,
        y = ~ CSF.pTau.INNO,
        z = ~CSF.tTau.INNO,
        text = ~Diagnosis,
        type = "scatter3d",
        color = ~Burnham_class,
        colors = c("firebrick", "darkorange", "gold", "forestgreen"),
        mode = "markers")
      my_plot <- add_trace(
        p = my_plot,
        z = my_lm_surface,
        x = axis_x1,
        y = axis_z1,
        type = "surface",
        colorscale = list(c(0,1), c("green","red")),
        opacity = 0.6,
        showscale = F,
        name = "Regression for APOE4 Carriers",
        showlegend = T
      )

      new_data_frame_2 <- filter(new.dat, apoe4 == 0)

      my_lm_2 <- lm(CSF.tTau.INNO ~ CSF.AB42.INNO+CSF.pTau.INNO, new_data_frame_2)
      # Set up Axis

      axis_x2 <- seq(min(new.dat$CSF.AB42.INNO), max(new.dat$CSF.AB42.INNO))
      axis_z2 <- seq(min(new.dat$CSF.pTau.INNO), max(new.dat$CSF.pTau.INNO))

      ## PLOT THE POINTS

      my_lm_surface_2 <- expand.grid(CSF.AB42.INNO = axis_x2,CSF.pTau.INNO = axis_z2,KEEP.OUT.ATTRS = F)
      my_lm_surface_2$CSF.tTau.INNO <- predict.lm(my_lm_2, newdata = my_lm_surface_2)
      my_lm_surface_2 <- acast(my_lm_surface_2, CSF.pTau.INNO ~ CSF.AB42.INNO, value.var = "CSF.tTau.INNO")

      my_plot <- add_trace(p = my_plot,
                           z = my_lm_surface_2,
                           x = axis_x2,
                           y = axis_z2,
                           type = "surface",
                           colorscale = list(c(0,1), c("tan","blue")),
                           opacity = 0.6,
                           showscale = F,
                           name = "Regression for APOE4 Non-Carriers",
                           showlegend = T)
      my_plot <- my_plot %>%
        layout(legend = LEGEND_1,
               scene = list(xaxis = axx, yaxis = axy, zaxis = axz))
      my_plot
    }else if (input$DEMOGRAPHIC == "Sex"){
      female_data <- filter(new.dat, Sex == "Female")

      my_lm_female <- lm(CSF.tTau.INNO ~ CSF.AB42.INNO+CSF.pTau.INNO, female_data)
      # Set up Axis

      axis_x1_female <- seq(min(new.dat$CSF.AB42.INNO), max(new.dat$CSF.AB42.INNO))
      axis_z1_female <- seq(min(new.dat$CSF.pTau.INNO), max(new.dat$CSF.pTau.INNO))

      ## PLOT THE POINTS

      my_lm_surface_female <- expand.grid(CSF.AB42.INNO = axis_x1_female,CSF.pTau.INNO = axis_z1_female,KEEP.OUT.ATTRS = F)
      my_lm_surface_female$CSF.tTau.INNO <- predict.lm(my_lm_female, newdata = my_lm_surface_female)
      my_lm_surface_female <- acast(my_lm_surface_female, CSF.pTau.INNO ~ CSF.AB42.INNO, value.var = "CSF.tTau.INNO")

      my_plot <- plot_ly()
      my_plot <- add_markers(
        p = my_plot,
        data = new.dat,
        x = ~CSF.AB42.INNO,
        y = ~ CSF.pTau.INNO,
        z = ~CSF.tTau.INNO,
        text = ~Diagnosis,
        type = "scatter3d",
        color = ~Burnham_class,
        colors = c("firebrick", "darkorange", "gold", "forestgreen"),
        mode = "markers"
      )
      my_plot <- add_trace(
        p = my_plot,
        z = my_lm_surface_female,
        x = axis_x1_female,
        y = axis_z1_female,
        type = "surface",
        colorscale = list(c(0,1), c("green","red")),
        opacity = 0.6,
        showscale = F,
        name = "Regression for Females",
        showlegend = T)

      male_data <- filter(new.dat, Sex == "Male")

      my_lm_male <- lm(CSF.tTau.INNO ~ CSF.AB42.INNO+CSF.pTau.INNO, male_data)
      # Set up Axis

      axis_x2_male <- seq(min(new.dat$CSF.AB42.INNO), max(new.dat$CSF.AB42.INNO))
      axis_z2_male <- seq(min(new.dat$CSF.pTau.INNO), max(new.dat$CSF.pTau.INNO))

      ## PLOT THE POINTS

      my_lm_surface_male <- expand.grid(CSF.AB42.INNO = axis_x2_male,CSF.pTau.INNO = axis_z2_male,KEEP.OUT.ATTRS = F)
      my_lm_surface_male$CSF.tTau.INNO <- predict.lm(my_lm_male, newdata = my_lm_surface_male)
      my_lm_surface_male <- acast(my_lm_surface_male, CSF.pTau.INNO ~ CSF.AB42.INNO, value.var = "CSF.tTau.INNO")

      my_plot <- add_trace(p = my_plot,
                           z = my_lm_surface_male,
                           x = axis_x2_male,
                           y = axis_z2_male,
                           type = "surface",
                           colorscale = list(c(0,1), c("tan","blue")),
                           opacity = 0.6,
                           showscale = F,
                           name = "Regression for Males",
                           showlegend = T)
      my_plot <- my_plot %>%
        layout(legend = LEGEND_1,
               scene = list(xaxis = axx, yaxis = axy, zaxis = axz))
      my_plot
    }else if (input$DEMOGRAPHIC == "Age"){
      #####################################################
      ###################      AGE    #####################

      # 1 is for education over 12years
      # 0 is for education under 12 years


      Age_binary <- filter(new.dat, Age_binary == 1)

      age_lm_1 <- lm(CSF.tTau.INNO ~ CSF.AB42.INNO+CSF.pTau.INNO, Age_binary)
      # Set up Axis

      x1 <- seq(min(new.dat$CSF.AB42.INNO), max(new.dat$CSF.AB42.INNO))
      z1 <- seq(min(new.dat$CSF.pTau.INNO), max(new.dat$CSF.pTau.INNO))

      ## PLOT THE POINTS

      age_surface_1 <- expand.grid(CSF.AB42.INNO = x1,CSF.pTau.INNO = z1,KEEP.OUT.ATTRS = F)
      age_surface_1$CSF.tTau.INNO <- predict.lm(age_lm_1, newdata = age_surface_1)
      age_surface_1 <- acast(age_surface_1, CSF.pTau.INNO ~ CSF.AB42.INNO, value.var = "CSF.tTau.INNO")

      my_plot <- plot_ly()
      my_plot <- add_markers(
        p = my_plot,
        data = new.dat,
        x = ~CSF.AB42.INNO,
        y = ~ CSF.pTau.INNO,
        z = ~CSF.tTau.INNO,
        text = ~Diagnosis,
        type = "scatter3d",
        color = ~Burnham_class,
        colors = c("firebrick", "darkorange", "gold", "forestgreen"),
        mode = "markers"
      )
      my_plot <- add_trace(
        p = my_plot,
        z = age_surface_1,
        x = x1,
        y = z1,
        type = "surface",
        colorscale = list(c(0,1), c("green","red")),
        opacity = 0.6,
        showscale = F,
        name = "Regression for Individuals over the Age of 72.5",
        showlegend = T)

      age_0 <- filter(new.dat, Age_binary == 0)

      age_lm_2 <- lm(CSF.tTau.INNO ~ CSF.AB42.INNO+CSF.pTau.INNO, age_0)
      # Set up Axis

      x2 <- seq(min(new.dat$CSF.AB42.INNO), max(new.dat$CSF.AB42.INNO))
      z2 <- seq(min(new.dat$CSF.pTau.INNO), max(new.dat$CSF.pTau.INNO))

      ## PLOT THE POINTS

      age_surface_2 <- expand.grid(CSF.AB42.INNO = x2,CSF.pTau.INNO = z2,KEEP.OUT.ATTRS = F)
      age_surface_2$CSF.tTau.INNO <- predict.lm(age_lm_2, newdata = age_surface_2)
      age_surface_2 <- acast(age_surface_2, CSF.pTau.INNO ~ CSF.AB42.INNO, value.var = "CSF.tTau.INNO")

      my_plot <- add_trace(p = my_plot,
                           z = age_surface_2,
                           x = x2,
                           y = z1,
                           type = "surface",
                           colorscale = list(c(0,1), c("tan","blue")),
                           opacity = 0.6,
                           showscale = F,
                           name = "Regression for Individuals under the age of 72.5",
                           showlegend = T)
      my_plot <- my_plot %>%
        layout(legend = LEGEND_1,
               scene = list(xaxis = axx, yaxis = axy, zaxis = axz))
      my_plot
    }else if (input$DEMOGRAPHIC == "Education"){
      #####################################################
      ###################  Education  #####################


      # 1 is for education over 12years
      # 0 is for education under 12 years


      eduction_data <- filter(new.dat, Education_binary == 1)

      my_education_1 <- lm(CSF.tTau.INNO ~ CSF.AB42.INNO+CSF.pTau.INNO, eduction_data)
      # Set up Axis

      axis_x1_education <- seq(min(new.dat$CSF.AB42.INNO), max(new.dat$CSF.AB42.INNO))
      axis_z1_education <- seq(min(new.dat$CSF.pTau.INNO), max(new.dat$CSF.pTau.INNO))

      ## PLOT THE POINTS

      my_lm_surface_education <- expand.grid(CSF.AB42.INNO = axis_x1_education,CSF.pTau.INNO = axis_z1_education,KEEP.OUT.ATTRS = F)
      my_lm_surface_education$CSF.tTau.INNO <- predict.lm(my_education_1, newdata = my_lm_surface_education)
      my_lm_surface_education <- acast(my_lm_surface_education, CSF.pTau.INNO ~ CSF.AB42.INNO, value.var = "CSF.tTau.INNO")

      my_plot <- plot_ly()
      my_plot <- add_markers(
        p = my_plot,
        data = new.dat,
        x = ~CSF.AB42.INNO,
        y = ~ CSF.pTau.INNO,
        z = ~CSF.tTau.INNO,
        text = ~Diagnosis,
        type = "scatter3d",
        color = ~Burnham_class,
        colors = c("firebrick", "darkorange", "gold", "forestgreen"),
        mode = "markers")
      my_plot <- add_trace(p = my_plot,
                           z = my_lm_surface_education,
                           x = axis_x1_education,
                           y = axis_z1_education,
                           type = "surface",
                           colorscale = list(c(0,1), c("green","red")),
                           opacity = 0.6,
                           showscale = F,
                           name = "Regression for Individuals with over 12 Years of Education",
                           showlegend = T)

      education_data_0 <- filter(new.dat, Education_binary == 0)

      my_lm_ed_0 <- lm(CSF.tTau.INNO ~ CSF.AB42.INNO+CSF.pTau.INNO, education_data_0)
      # Set up Axis

      axis_x2 <- seq(min(new.dat$CSF.AB42.INNO), max(new.dat$CSF.AB42.INNO))
      axis_z2 <- seq(min(new.dat$CSF.pTau.INNO), max(new.dat$CSF.pTau.INNO))

      ## PLOT THE POINTS

      my_lm_surface_ed_0 <- expand.grid(CSF.AB42.INNO = axis_x2,CSF.pTau.INNO = axis_z2,KEEP.OUT.ATTRS = F)
      my_lm_surface_ed_0$CSF.tTau.INNO <- predict.lm(my_lm_ed_0, newdata = my_lm_surface_ed_0)
      my_lm_surface_ed_0 <- acast(my_lm_surface_ed_0, CSF.pTau.INNO ~ CSF.AB42.INNO, value.var = "CSF.tTau.INNO")

      my_plot <- add_trace(p = my_plot,
                           z = my_lm_surface_ed_0,
                           x = axis_x2,
                           y = axis_z2,
                           type = "surface",
                           colorscale = list(c(0,1), c("tan","blue")),
                           opacity = 0.6,
                           showscale = F,
                           name = "Regression for Individuals with less than 12 Years of Education",
                           showlegend = T)
      my_plot <- my_plot %>%
        layout(legend = LEGEND_1,
               scene = list(xaxis = axx, yaxis = axy, zaxis = axz))


      my_plot

    }
  })

  ############################ MODEL SUMMARIES ###############################

  ####################### 1. LINEAR FOR DEMOGRAPHICS #########################


  output$LinearREG <- renderUI({
    new.dat <- req(data_internal$raw)
    if (input$LINEAR1 == "CSF AB1-42 pg/mL"){
      lm_AB <- lm(CSF.AB42.INNO ~ Age+Sex+Education_binary+apoe4, data = new.dat)
      p_value_df <- as.data.frame(summary(lm_AB)$coefficients)
      new_pvalue <- p_value_df %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
      rownames(new_pvalue) = c("Intercept","Age","Sex (Male)","Education Binary (under 12 years education)", "apoe4 (non-carrier)")
      mytableout = htmlTable (
        new_pvalue,
        caption = "Linear summary for CSF AB1-42 ~ Age+Sex+Education_binary+apoe4",
        tfoot = paste("R-squared value=",signif(summary(lm_AB)$r.squared,2), "Adjusted R-squared value=",
                      signif(summary(lm_AB)$adj.r.squared,2),sep = "     ")
      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }else if (input$LINEAR1 == "CSF pTau pg/mL"){
      lm_pTau <- lm(CSF.pTau.INNO ~ Age+Sex+Education_binary+apoe4, data = new.dat)
      ptau_sum <- as.data.frame(summary(lm_pTau)$coefficients)
      ptau_new <- ptau_sum %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
      rownames(ptau_new) = c("Intercept","Age","Sex (Male)","Education Binary (under 12 years education)", "apoe4 (non-carrier)")
      mytableout = htmlTable (
        ptau_new,
        caption = "Linear summary for CSF pTau ~ Age+Sex+Education_binary+apoe4",
        tfoot = paste("R-squared value=",signif(summary(lm_pTau)$r.squared,2), "Adjusted R-squared value=",
                      signif(summary(lm_pTau)$adj.r.squared,2),sep = "     ")
      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }else if (input$LINEAR1 == "CSF tTau pg/mL"){
      lm_tTau <- lm(CSF.tTau.INNO ~ Age+Sex+Education_binary+apoe4, data = new.dat)
      ptau_sum <- as.data.frame(summary(lm_tTau)$coefficients)
      ptau_new <- ptau_sum %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
      rownames(ptau_new) = c("Intercept","Age","Sex (Male)","Education Binary (under 12 years education)", "apoe4 (non-carrier)")
      mytableout = htmlTable (
        ptau_new,
        caption = "Linear summary for CSF tTau ~ Age+Sex+Education_binary+apoe4",
        tfoot = paste("R-squared value=",signif(summary(lm_tTau)$r.squared,2), "Adjusted R-squared value=",
                      signif(summary(lm_tTau)$adj.r.squared,2),sep = "     ")
      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }
  })
  ####################### 2. Logistic FOR DEMOGRAPHICS ########################

  output$Logistictable <- renderUI({
    new.dat <- req(data_internal$raw)

    if (input$LOG1 == "CSF AB1-42 pg/mL"){
      new.dat <- mutate(new.dat, Binary_Abeta = ifelse(AB.status == "Positive", 1, 0))
      new.dat$Binary_Abeta <- factor(new.dat$Binary_Abeta, levels = c("1", "0"))
      new.dat$apoe4 <- factor(new.dat$apoe4, levels = c("1", "0"))

      mylogit <- glm(Binary_Abeta ~ Age+ Education_binary+Sex+apoe4, data = new.dat, family = "binomial")
      # summary(mylogit)
      sumarry_pvals <- as.data.frame(summary(mylogit)$coefficients)
      new_sum <- sumarry_pvals %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
      rownames(new_sum) = c("Intercept","Age","Sex (Male)","Education Binary (under 12 years education)", "apoe4 (non-carrier)")
      mytableout = htmlTable (
        new_sum,
        caption = "Logistic summary for CSF AB1-42 ~ Age+Sex+Education_binary+apoe4"
      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }else if (input$LOG1 == "CSF pTau pg/mL"){
      new.dat <- mutate(new.dat, Binary_pTau = ifelse(pTau.status == "Positive", 1, 0))
      new.dat$Binary_pTau <- factor(new.dat$Binary_pTau, levels = c("1", "0"))
      mylogit_2 <- glm(Binary_pTau ~ Age+ Sex+Education_binary+apoe4, data = new.dat, family = "binomial")
      # summary(mylogit)
      sumarry_pvals <- as.data.frame(summary(mylogit_2)$coefficients)
      new_sum <- sumarry_pvals %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
      rownames(new_sum) = c("Intercept","Age","Sex (Male)","Education Binary (under 12 years education)", "apoe4 (non-carrier)")
      mytableout = htmlTable (
        new_sum,
        caption = "Logistic summary for CSF pTau ~ Age+Sex+Education_binary+apoe4"
      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }else if (input$LOG1 == "CSF tTau pg/mL"){
      new.dat <- mutate(new.dat, Binary_Tau = ifelse(tTau.status == "Positive", 1, 0))
      new.dat$Binary_Tau <- factor(new.dat$Binary_Tau, levels = c("1", "0"))
      mylogit_3 <- glm(Binary_Tau ~ Age+Sex+Education_binary+apoe4, data = new.dat, family = "binomial")
      sumarry_pvals <- as.data.frame(summary(mylogit_3)$coefficients)
      new_sum <- sumarry_pvals %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
      rownames(new_sum) = c("Intercept","Age","Sex (Male)","Education Binary (under 12 years education)", "apoe4 (non-carrier)")
      mytableout = htmlTable (
        new_sum,
        caption = "Logistic summary for CSF pTau ~ Age+Sex+Education_binary+apoe4"
      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }


  })

  #################### 3. Biomarker interplay Summaries #####################

  output$BiomarkerReg <- renderUI({
    new.dat <- req(data_internal$raw)

    if (input$interplay == "CSF AB1-42 pg/mL"){
      lm_AB <- lm(CSF.AB42.INNO ~ CSF.pTau.INNO+CSF.tTau.INNO, data = new.dat)
      p_value_df <- as.data.frame(summary(lm_AB)$coefficients)
      new_pvalue <- p_value_df %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
      rownames(new_pvalue) = c("Intercept","CSF pTau pg/mL","CSF tTau pg/mL")
      mytableout = htmlTable (
        new_pvalue,
        caption = "Linear summary for CSF AB1-42 ~ CSF pTau + CSF tTau",
        tfoot = paste("R-squared value=",signif(summary(lm_AB)$r.squared,2), "Adjusted R-squared value=",
                      signif(summary(lm_AB)$adj.r.squared,2),sep = "     ")
      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable

    }else if(input$interplay == "CSF pTau pg/mL"){
      lm_AB <- lm(CSF.pTau.INNO ~ CSF.AB42.INNO+CSF.tTau.INNO, data = new.dat)
      p_value_df <- as.data.frame(summary(lm_AB)$coefficients)
      new_pvalue <- p_value_df %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
      rownames(new_pvalue) = c("Intercept","CSF AB1-42 pg/mL","CSF tTau pg/mL")
      mytableout = htmlTable (
        new_pvalue,
        caption = "Linear summary for CSF pTau ~ CSF AB1-42 + CSF tTau",
        tfoot = paste("R-squared value=",signif(summary(lm_AB)$r.squared,2), "Adjusted R-squared value=",
                      signif(summary(lm_AB)$adj.r.squared,2),sep = "     ")
      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }else if(input$interplay == "CSF tTau pg/mL"){
      lm_AB <- lm(CSF.tTau.INNO ~ CSF.AB42.INNO+CSF.pTau.INNO, data = new.dat)
      p_value_df <- as.data.frame(summary(lm_AB)$coefficients)
      new_pvalue <- p_value_df %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
      rownames(new_pvalue) = c("Intercept","CSF AB1-42 pg/mL","CSF pTau pg/mL")
      mytableout = htmlTable (
        new_pvalue,
        caption = "Linear summary for CSF tTau ~ CSF AB1-42 + CSF pTau",
        tfoot = paste("R-squared value=",signif(summary(lm_AB)$r.squared,2), "Adjusted R-squared value=",
                      signif(summary(lm_AB)$adj.r.squared,2),sep = "     ")
      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }

  })

  ########################### P-VALUE TABLE ###############################
  output$filetable <- renderUI({
    new.dat <- req(data_internal$raw)
    lm_pTau <- lm(CSF.pTau.INNO ~ Age+Sex+Education_binary+apoe4, data = new.dat)
    lm_AB <- lm(CSF.AB42.INNO ~ Age+Sex+Education_binary+apoe4, data = new.dat)
    lm_tTau <- lm(CSF.tTau.INNO ~ Age+Sex+Education_binary+apoe4, data = new.dat)
    newdf <- as.data.frame(summary(lm_AB)$coefficients[,4])
    R_Squared <- as.data.frame(summary(lm_AB)$r.squared)
    names(R_Squared) <- "summary(lm_AB)$coefficients[, 4]"
    total <- rbind(newdf, R_Squared)
    newdf1 <- as.data.frame(summary(lm_pTau)$coefficients[,4])
    R_Squared1 <- as.data.frame(summary(lm_pTau)$r.squared)
    names(R_Squared1) <- "summary(lm_pTau)$coefficients[, 4]"
    total1 <- rbind(newdf1, R_Squared1)
    newdf2 <- as.data.frame(summary(lm_tTau)$coefficients[,4])
    R_Squared2 <- as.data.frame(summary(lm_tTau)$r.squared)
    names(R_Squared2) <- "summary(lm_tTau)$coefficients[, 4]"
    total2 <- rbind(newdf2, R_Squared2)
    results <- cbind(total,total1,total2)
    results2 <- results %>%
      mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
    where <- (results2 <= 0.05)
    style <- 'background-color: yellow; color: black;'
    css.cell <- matrix('', nrow(results2), ncol(results2))
    css.cell[where] <- style
    names(results2) = c("CSF AB1-42 pg/mL","CSF pTau pg/mL","CSF tTau pg/mL")
    rownames(results2) = c("Intercept","Age","Sex (Male)","Education Binary (under 12 years education)","apoe4 (non-carrier)","R squared value")
    # print(results2)

    mytableout = htmlTable (
      results2,
      css.cell = css.cell,
      # cgroup = "P-values for Independent Variables",
      caption = "Table 2: P-Values summaries from all regression computed for Group 1"
    )

    mytableout

  })

  output$group1pvalue <- renderUI({
    new.dat <- req(data_internal$raw)
    lm_ab <- lm(CSF.AB42.INNO ~ CSF.pTau.INNO+CSF.tTau.INNO, data = new.dat)
    lm_ptau <- lm(CSF.pTau.INNO ~ CSF.AB42.INNO+CSF.tTau.INNO, data = new.dat)
    lm_ttau <- lm(CSF.tTau.INNO ~ CSF.AB42.INNO+CSF.pTau.INNO, data = new.dat)

    # Make a matrix for the data

    p_group1 <- matrix(NA, ncol=3, nrow=5)


    p_group1[1,1] = signif(summary(lm_ab)$coefficients[1,4],digits = 2)
    p_group1[1,2] = signif(summary(lm_ptau)$coefficients[1,4],digits = 2)
    p_group1[1,3] = signif(summary(lm_ttau)$coefficients[1,4],digits = 2)
    p_group1[2,1] = "NA"
    p_group1[2,2] = signif(summary(lm_ptau)$coefficients[2,4],digits = 2)
    p_group1[2,3] = signif(summary(lm_ttau)$coefficients[2,4],digits = 2)

    p_group1[3,1] = signif(summary(lm_ab)$coefficients[2,4],digits = 2)
    p_group1[3,2] = "NA"

    p_group1[3,3] = signif(summary(lm_ttau)$coefficients[3,4],digits = 2)

    p_group1[4,1] = signif(summary(lm_ab)$coefficients[3,4],digits=2)
    p_group1[4,2] = signif(summary(lm_ptau)$coefficients[3,4], digits = 2)
    p_group1[4,3] = "NA"

    p_group1[5,1] = signif(summary(lm_ab)$r.squared, digits = 2)
    p_group1[5,2] = signif(summary(lm_ptau)$r.squared, digits = 2)
    p_group1[5,3] = signif(summary(lm_ttau)$r.squared, digits =2)

    newdataframe <- as.matrix(p_group1)
    newdataframe <- as.numeric(newdataframe)
    where <- (newdataframe <= 0.05)
    style <- 'background-color: yellow; color: black;'
    css.cell <- matrix('', nrow(p_group1), ncol(p_group1))
    css.cell[where] <- style
    # p_group1 <- p_group1 %>%
    #   mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
    rownames(p_group1) = c("Intercept","CSF AB1-42 pg/mL","CSF pTau pg/mL", "CSF tTau pg/mL", "R-squared Value")
    colnames(p_group1) = c("CSF AB1-42 pg/mL   ", "CSF pTau pg/mL   ", "CSF tTau pg/mL   ")
    mytableout = htmlTable (
      p_group1,
      css.cell = css.cell,
      caption = "Table 3: p-value summaries for all Biomarker interplay"
    )
    mytableout

  })

  ####################### EXTRA A+/T+/N+ VISUALISATION ########################
  output$G1ExtraCube <- renderPlotly({
    new.dat <- req(data_internal$raw)
    if (input$cubeg1input == "60-70"){
      df_young <- filter(new.dat, Age < 70)
      df7 <- mutate(df_young, scat_col = ifelse(CSF.pTau.INNO >59.23    & CSF.AB42.INNO < 656 & CSF.tTau.INNO >303.54  & Burnham_class == "AD",
                                                "AD meeting all cut-offs",
                                                ifelse(CSF.pTau.INNO >59.23    & CSF.AB42.INNO <656 & CSF.tTau.INNO > 303.54  & Burnham_class == "Pathological Change",
                                                       "Pathological Change meeting all cut-offs",
                                                       ifelse(CSF.pTau.INNO >59.23    & CSF.AB42.INNO < 656 & CSF.tTau.INNO > 303.54  & Burnham_class == "Non-AD pathological Change",
                                                              "Non-AD Pathological Change meeting all cut-offs",
                                                              ifelse(CSF.pTau.INNO >59.23    & CSF.AB42.INNO < 656 & CSF.tTau.INNO > 303.54  & Burnham_class == "Normal AD Biomarkers",
                                                                     "Normal AD Biomarkers meeting all cut-offs",
                                                                     ifelse(CSF.pTau.INNO >59.23    & CSF.AB42.INNO < 656 & Burnham_class == "AD",
                                                                            "AD meeting 2 cut offs",
                                                                            ifelse(CSF.AB42.INNO < 656 & CSF.tTau.INNO >303.54  & Burnham_class == "AD",
                                                                                   "AD meeting 2 cut offs",
                                                                                   ifelse(CSF.pTau.INNO >59.23   & CSF.tTau.INNO >303.54  & Burnham_class == "AD",
                                                                                          "AD meeting 2 cut offs",
                                                                                          ifelse(CSF.pTau.INNO >59.23    & CSF.AB42.INNO < 656 & Burnham_class == "Pathological Change",
                                                                                                 "Pathological Change meeting 2 cut-offs",
                                                                                                 ifelse(CSF.AB42.INNO < 656 & CSF.tTau.INNO >303.54  & Burnham_class == "Pathological Change",
                                                                                                        "Pathological Change meeting 2 cut-offs",
                                                                                                        ifelse(CSF.pTau.INNO >59.23   & CSF.tTau.INNO >303.54  & Burnham_class == "Pathological Change",
                                                                                                               "Pathological Change meeting 2 cut-offs",
                                                                                                               ifelse(CSF.pTau.INNO >59.23    & CSF.AB42.INNO < 656 & Burnham_class == "Non-AD pathological Change",
                                                                                                                      "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                      ifelse(CSF.AB42.INNO < 656 & CSF.tTau.INNO >303.54  & Burnham_class == "Non-AD pathological Change",
                                                                                                                             "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                             ifelse(CSF.pTau.INNO >59.23   & CSF.tTau.INNO >303.54  & Burnham_class == "Non-AD pathological Change",
                                                                                                                                    "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                                    ifelse(CSF.pTau.INNO >59.23    & CSF.AB42.INNO < 656 & Burnham_class == "Normal AD Biomarkers",
                                                                                                                                           "Normal AD Biomarker meeting 2 cut-offs",
                                                                                                                                           ifelse(CSF.AB42.INNO < 656 & CSF.tTau.INNO >303.54  & Burnham_class == "Normal AD Biomarkers",
                                                                                                                                                  "Normal AD Biomarker meeting 2 cut-offs",
                                                                                                                                                  ifelse(CSF.pTau.INNO >59.23   & CSF.tTau.INNO >303.54  & Burnham_class == "Normal AD Biomarkers",
                                                                                                                                                         "Normal AD Biomarker meeting 2 cut-offs","unselected")))))))))))))))))



      df7$scat_col <- factor(df7$scat_col, levels = c("unselected",
                                                      "AD meeting all cut-offs",
                                                      "Pathological Change meeting all cut-offs",
                                                      "Non-AD Pathological Change meeting all cut-offs",
                                                      "Normal AD Biomarkers meeting all cut-offs",
                                                      "AD meeting 2 cut offs",
                                                      "Pathological Change meeting 2 cut-offs",
                                                      "Non-AD Pathological Change meeting 2 cut-offs",
                                                      "Normal AD Biomarker meeting 2 cut-offs"))


      df_mesh_1 <- data.frame(X_VAL = c(656,  656,  min(df7$CSF.AB42.INNO),    min(df7$CSF.AB42.INNO),    656,   656,   min(df7$CSF.AB42.INNO),   min(df7$CSF.AB42.INNO)),
                              Y_VAL = c(59.23  ,max(df7$CSF.pTau.INNO),    59.23  ,  max(df7$CSF.pTau.INNO),    59.23 , max(df7$CSF.pTau.INNO),   59.23 , max(df7$CSF.pTau.INNO)),
                              Z_VAL = c(303.54  , 303.54  , 303.54  , 303.54  , max(df7$CSF.tTau.INNO),  max(df7$CSF.tTau.INNO),  max(df7$CSF.tTau.INNO),  max(df7$CSF.tTau.INNO)),
                              MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))

      # Make apoe4 a factor
      df7$apoe4 <- factor(df7$apoe4, levels = c(1,0))

      cube <- plot_ly()%>%
        add_markers(type = "scatter3d",
                    mode = "markers",
                    data = df7,
                    x = ~CSF.AB42.INNO,
                    y = ~CSF.pTau.INNO,
                    z = ~CSF.tTau.INNO,
                    color = ~scat_col,
                    colors = c('gray', "firebrick", "darkorange", "gold", "forestgreen","hotpink", "aquamarine", "blueviolet", "darkorchid")) %>%
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
      cube <- cube %>%
        layout(legend = LEGEND_1,
               scene = list(xaxis = axx, yaxis = axy, zaxis = axz))
      cube
    }else {
      df_old <- filter(new.dat, Age > 70)
      df7 <- mutate(df_old, scat_col = ifelse(CSF.pTau.INNO >73.83   & CSF.AB42.INNO < 656 & CSF.tTau.INNO >378.65 & Burnham_class == "AD",
                                              "AD meeting all cut-offs",
                                              ifelse(CSF.pTau.INNO >73.83   & CSF.AB42.INNO <656 & CSF.tTau.INNO > 378.65 & Burnham_class == "Pathological Change",
                                                     "Pathological Change meeting all cut-offs",
                                                     ifelse(CSF.pTau.INNO >73.83   & CSF.AB42.INNO < 656 & CSF.tTau.INNO > 378.65 & Burnham_class == "Non-AD pathological Change",
                                                            "Non-AD Change meeting all cut-offs",
                                                            ifelse(CSF.pTau.INNO >73.83   & CSF.AB42.INNO < 656 & CSF.tTau.INNO > 378.65 & Burnham_class == "Normal AD Biomarkers",
                                                                   "Normal AD Biomarkers meeting all cut-offs",
                                                                   ifelse(CSF.pTau.INNO >73.83   & CSF.AB42.INNO < 656 & Burnham_class == "AD",
                                                                          "AD meeting 2 cut offs",
                                                                          ifelse(CSF.AB42.INNO < 656 & CSF.tTau.INNO >378.65 & Burnham_class == "AD",
                                                                                 "AD meeting 2 cut offs",
                                                                                 ifelse(CSF.pTau.INNO >73.83  & CSF.tTau.INNO >378.65 & Burnham_class == "AD",
                                                                                        "AD meeting 2 cut offs",
                                                                                        ifelse(CSF.pTau.INNO >73.83   & CSF.AB42.INNO < 656 & Burnham_class == "Pathological Change",
                                                                                               "Pathological Change meeting 2 cut-offs",
                                                                                               ifelse(CSF.AB42.INNO < 656 & CSF.tTau.INNO >378.65 & Burnham_class == "Pathological Change",
                                                                                                      "Pathological Change meeting 2 cut-offs",
                                                                                                      ifelse(CSF.pTau.INNO >73.83  & CSF.tTau.INNO >378.65 & Burnham_class == "Pathological Change",
                                                                                                             "Pathological Change meeting 2 cut-offs",
                                                                                                             ifelse(CSF.pTau.INNO >73.83   & CSF.AB42.INNO < 656 & Burnham_class == "Non-AD pathological Change",
                                                                                                                    "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                    ifelse(CSF.AB42.INNO < 656 & CSF.tTau.INNO >378.65 & Burnham_class == "Non-AD pathological Change",
                                                                                                                           "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                           ifelse(CSF.pTau.INNO >73.83  & CSF.tTau.INNO >378.65 & Burnham_class == "Non-AD pathological Change",
                                                                                                                                  "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                                  ifelse(CSF.pTau.INNO >73.83   & CSF.AB42.INNO < 656 & Burnham_class == "Normal AD Biomarkers",
                                                                                                                                         "Normal AD Biomarkers meeting 2 cut-offs",
                                                                                                                                         ifelse(CSF.AB42.INNO < 656 & CSF.tTau.INNO >378.65 & Burnham_class == "Normal AD Biomarkers",
                                                                                                                                                "Normal AD Biomarkers meeting 2 cut-offs",
                                                                                                                                                ifelse(CSF.pTau.INNO >73.83  & CSF.tTau.INNO >378.65 & Burnham_class == "Normal AD Biomarkers",
                                                                                                                                                       "Normal AD Biomarkers meeting 2 cut-offs","unselected")))))))))))))))))



      df7$scat_col <- factor(df7$scat_col, levels = c("unselected",
                                                      "AD meeting all cut-offs",
                                                      "Pathological Change meeting all cut-offs",
                                                      "Non-AD Change meeting all cut-offs",
                                                      "Normal AD Biomarkers meeting all cut-offs",
                                                      "AD meeting 2 cut offs",
                                                      "Pathological Change meeting 2 cut-offs",
                                                      "Non-AD Pathological Change meeting 2 cut-offs",
                                                      "Normal AD Biomarkers meeting 2 cut-offs"))


      df_mesh_1 <- data.frame(X_VAL = c(656,  656,  min(df7$CSF.AB42.INNO),    min(df7$CSF.AB42.INNO),    656,   656,   min(df7$CSF.AB42.INNO),   min(df7$CSF.AB42.INNO)),
                              Y_VAL = c(73.83 ,max(df7$CSF.pTau.INNO),    73.83 ,  max(df7$CSF.pTau.INNO),    73.83, max(df7$CSF.pTau.INNO),   73.83, max(df7$CSF.pTau.INNO)),
                              Z_VAL = c(378.65 , 378.65 , 378.65 , 378.65 , max(df7$CSF.tTau.INNO),  max(df7$CSF.tTau.INNO),  max(df7$CSF.tTau.INNO),  max(df7$CSF.tTau.INNO)),
                              MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))

      # Make apoe4 a factor
      df7$apoe4 <- factor(df7$apoe4, levels = c(1,0))

      cube <- plot_ly()%>%
        add_markers(type = "scatter3d",
                    mode = "markers",
                    data = df7,
                    x = ~CSF.AB42.INNO,
                    y = ~CSF.pTau.INNO,
                    z = ~CSF.tTau.INNO,
                    color = ~scat_col,
                    colors = c('gray', "firebrick", "darkorange", "gold", "forestgreen","hotpink", "aquamarine", "chocolate1", "darkorchid")) %>%
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
      cube <- cube %>%
        layout(legend = LEGEND_1,
               scene = list(xaxis = axx, yaxis = axy, zaxis = axz))
      cube

    }

  })
  
  #############################################################################
  ############################### HIPPO CUT OFF ###############################
  #############################################################################
  
  # Regular ggplot for HIPPOCAMPUS
  
  output$plot_gg <- renderPlot({
    new.dat <- req(data_internal$raw)
    # new.dat <- req(my_data)
    new.dat$Diagnosis <- factor(new.dat$Diagnosis, levels = c("AD", "MCI", "HC"))
    p <- ggplot(new.dat, aes(x = Sum.hippo, color = Diagnosis, fill = Diagnosis))+
      geom_density(alpha = 0.5)+
      theme_bw()+
      scale_color_manual(values = c("red", "orange", "green"))+
      scale_fill_manual(values=c("red", "orange", "green"))+
      xlab(bquote('Hippocampus'~(mL^3)))+
      ylab("Density")+
      ggtitle("Density of Hippocampus by Diagnosis")
    p
  })
  
  output$intersect_plot <- renderPlot({
    new.dat <- req(data_internal$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippoframe <- x$hippoframe
    
    ggplot(hippoframe, aes(x=x))+
      theme_bw()+
      geom_line(aes(y = y, colour = "AD"), lwd = 2)+
      geom_line(aes(y=y2, colour = "HC"), lwd = 2)+
      geom_vline(xintercept =  hippo_cutoff, colour = "black", lwd = 1.5)+
      scale_colour_manual("", values = c("AD"="red", 
                                         "HC" = "green"))+
      labs(title = "Legend")+
      xlab(bquote('Hippocampus'~(mL^3)))+
      ylab("Density")+
      ggtitle("Density Curve of AD and HC with Intersecting Lines")
  })
  
  
  
  ############### RENDER TEXT FOR HIPPOCAMPUS OUTPUT 
  
  output$texthippo <- renderText({
    new.dat <- req(data_internal$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippoframe <- x$hippoframe
    
    paste0("The cut-off is ",round(hippo_cutoff,3),".")
  })

  #
  ##
  ###
  #############################################################################
  ################################## GROUP 2 ##################################
  #############################################################################
  ###
  ##
  #


  ################################ 2D Visualisation ###########################
  ################################ 2D RADIO GRAPH #############################

  o <- reactive({
    new.dat <- req(data_internal$raw)
    X_2_Input <- switch(input$X_2_Input,
                        Centiloid = new.dat$Centiloid,
                        ptau = new.dat$CSF.pTau.INNO,
                        Hippogroup2X1 = new.dat$Sum.hippo)

  })

  z <- reactive({
    new.dat <- req(data_internal$raw)
    Y_2_input <- switch(input$Y_2_input,
                        Centiloid = new.dat$Centiloid,
                        ptau = new.dat$CSF.pTau.INNO,
                        hipGroup2 = new.dat$Sum.hippo)
  })

  ############################### REACTIVE PLOT #################################.

  output$plot2 <- renderPlotly({
    new.dat <- req(data_internal$raw)
    X_2_Input <- input$X_2_Input
    Y_2_input <- input$Y_2_input

    X_LAB2 <- 'default'
    if (input$X_2_Input == "Centiloid"){
      X_LAB2 <- "Centiloid"
    }
    if (input$X_2_Input == "ptau"){
      X_LAB2 <- "CSF p-Tau pg/mL"
    }
    if (input$X_2_Input == "Hippogroup2X1"){
      X_LAB2 <- "Hippocampus mL<sup>3</sup>"
    }
    Y_LAB2 <- 'default'
    if (input$Y_2_input == "Centiloid"){
      Y_LAB2 <- "Centiloid"
    }
    if (input$Y_2_input == "ptau"){
      Y_LAB2 <- "CSF p-Tau pg/mL"
    }
    if (input$Y_2_input == "hipGroup2"){
      Y_LAB2 <- "Hippocampus mL<sup>3</sup>"
    }


    r <- ggplot(new.dat, aes_string(x = o(), y=z()))+
      geom_point(data = new.dat, aes(color = Clifford_class, group = ID))+
      theme_bw()+
      geom_smooth(method = "loess")+
      scale_color_manual(values = c("firebrick", "darkorange", "gold", "forestgreen"))+
      xlab(X_LAB2)+
      ylab(Y_LAB2)+
      labs(color = "ATN Clifford et. al \n Classification")+
      ggtitle(paste(X_LAB2,"vs",Y_LAB2, sep = "  "))

    ggplotly(r)
  })

  ############################# 3D Visualisation ##############################
  ############################ SCATTER AND PLANES #############################

  group2_animation <- reactiveValues(
    plot = NULL
  )

  observeEvent(input$G2staticP1, {
    new.dat <- req(data_internal$raw)
    group2_animation$plot <- ScatterPlotFunction(
      d = new.dat, 
      xdat = new.dat$Sum.hippo, 
      ydat = new.dat$CSF.pTau.INNO, 
      zdat = new.dat$Centiloid, 
      cols = new.dat$Clifford_class, 
      leg = LEGEND_2, 
      xax = axx2, 
      yax = axy2, 
      zax = axz2
    )
  })

  observeEvent(input$G2RotateP1, {
    new.dat <- req(data_internal$raw)
    group2_animation$plot <- AnimatedScatterPlotFunction(
      d = new.dat, 
      xdat = new.dat$Sum.hippo, 
      ydat = new.dat$CSF.pTau.INNO, 
      zdat = new.dat$Centiloid, 
      cols = new.dat$Clifford_class, 
      leg = LEGEND_2, 
      xax = axx2, 
      yax = axy2, 
      zax = axz2
    )

  })

  observeEvent(input$G2STATICP2, {
    new.dat <- req(data_internal$raw)
    ptau_threshold <- req(ptau_cutoff_lower$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippo_threshold <- hippo_cutoff
    new_data <- filter(new.dat, Age < 70)
    group2_animation$plot <- PlanesFunction(
      dat = new_data, 
      xinput = new_data$Sum.hippo, 
      yinput = new_data$CSF.pTau.INNO, 
      zinput = new_data$Centiloid, 
      cols = new_data$Clifford_class, 
      leg = LEGEND_2,
      xax = axx2,
      yax = axy2,
      zax = axz2,
      XCUT = hippo_threshold, 
      YCUT = ptau_threshold, 
      ZCUT = centiloid_threshold, 
      XNAME = HIPPO_cutoff_label, 
      YNAME = ptau_cutoff_label, 
      ZNAME = Centloid_cutoff_label
    )
  })

  observeEvent(input$G2ROTATEP2, {
    new.dat <- req(data_internal$raw)
    new_data_young <- filter(new.dat, Age < 70)
    ptau_threshold <- req(ptau_cutoff_lower$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippo_threshold <- hippo_cutoff
    group2_animation$plot <- AnimatedPlanesFunction(
      dat = new_data_young, 
      xinput = new_data_young$Sum.hippo, 
      yinput = new_data_young$CSF.pTau.INNO, 
      zinput = new_data_young$Centiloid, 
      cols = new_data_young$Clifford_class, 
      leg = LEGEND_2,
      xax = axx2,
      yax = axy2,
      zax = axz2,
      XCUT = hippo_threshold, 
      YCUT = ptau_threshold, 
      ZCUT = centiloid_threshold, 
      XNAME = HIPPO_cutoff_label, 
      YNAME = ptau_cutoff_label, 
      ZNAME = Centloid_cutoff_label
    )
  })

  observeEvent(input$G2STATICP3, {
    new.dat <- req(data_internal$raw)
    data_old <- filter(new.dat, Age > 70)
    
    ptau_threshold <- req(ptau_cutoff_upper$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippo_threshold <- hippo_cutoff
    
    group2_animation$plot <- PlanesFunction(
      dat = data_old, 
      xinput = data_old$Sum.hippo, 
      yinput = data_old$CSF.pTau.INNO, 
      zinput = data_old$Centiloid, 
      cols = data_old$Clifford_class, 
      leg = LEGEND_2,
      xax = axx2,
      yax = axy2,
      zax = axz2,
      XCUT = hippo_threshold, 
      YCUT = ptau_threshold, 
      ZCUT = centiloid_threshold, 
      XNAME = HIPPO_cutoff_label, 
      YNAME = ptau_cutoff_label, 
      ZNAME = Centloid_cutoff_label
    )

  })

  observeEvent(input$G3ROTATINGP3, {
    new.dat <- req(data_internal$raw)
    data_old <- filter(new.dat, Age > 70)
    
    ptau_threshold <- req(ptau_cutoff_upper$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippo_threshold <- hippo_cutoff

    group2_animation$plot <- AnimatedPlanesFunction(
      dat = data_old, 
      xinput = data_old$Sum.hippo, 
      yinput = data_old$CSF.pTau.INNO, 
      zinput = data_old$Centiloid, 
      cols = data_old$Clifford_class, 
      leg = LEGEND_2,
      xax = axx2,
      yax = axy2,
      zax = axz2,
      XCUT = hippo_threshold, 
      YCUT = ptau_threshold, 
      ZCUT = centiloid_threshold, 
      XNAME = HIPPO_cutoff_label, 
      YNAME = ptau_cutoff_label, 
      ZNAME = Centloid_cutoff_label
    )

  })

  output$CONDITIONPLOT <- renderPlotly({
    if (is.null(group2_animation$plot)) return()
    group2_animation$plot
  })

  ############################### CUBE PLOT  #################################.

  cube_g2_animate <- reactiveValues(
    plot = NULL
  )

  observeEvent(input$CUBESTATICG2, {
    new.dat <- req(data_internal$raw)
    df_young <- filter(new.dat, Age < 70)
    
    ptau_threshold <- req(ptau_cutoff_lower$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippo_threshold <- hippo_cutoff
    cube_g2_animate$plot <- G2CubeVisualisation(dat = df_young, 
                                             xinput = df_young$Sum.hippo,
                                             yinput = df_young$CSF.pTau.INNO, 
                                             zinput = df_young$Centiloid, 
                                             leg = LEGEND_2,
                                             xax=axx2,
                                             yax = axy2,
                                             zax = axz2, 
                                             XCUT = hippo_threshold, 
                                             YCUT = ptau_threshold, 
                                             ZCUT = centiloid_threshold)

  })


  observeEvent(input$CUBEROTATEG2, {
    new.dat <- req(data_internal$raw)
    df_young <- filter(new.dat, Age < 70)
    
    ptau_threshold <- req(ptau_cutoff_lower$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippo_threshold <- hippo_cutoff
    cube_g2_animate$plot <- AnimatedG2CubeVisualisation(dat = df_young, 
                                                xinput = df_young$Sum.hippo,
                                                yinput = df_young$CSF.pTau.INNO, 
                                                zinput = df_young$Centiloid, 
                                                leg = LEGEND_2,
                                                xax=axx2,
                                                yax = axy2,
                                                zax = axz2, 
                                                XCUT = hippo_threshold, 
                                                YCUT = ptau_threshold, 
                                                ZCUT = centiloid_threshold)
  })

  observeEvent(input$G2P2CUBESTATIC, {
    new.dat <- req(data_internal$raw)
    df_old <- filter(new.dat, Age > 70)
    
    ptau_threshold <- req(ptau_cutoff_upper$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippo_threshold <- hippo_cutoff
    cube_g2_animate$plot <- G2CubeVisualisation(dat = df_old, 
                                                xinput = df_old$Sum.hippo,
                                                yinput = df_old$CSF.pTau.INNO, 
                                                zinput = df_old$Centiloid, 
                                                leg = LEGEND_2,
                                                xax=axx2,
                                                yax = axy2,
                                                zax = axz2, 
                                                XCUT = hippo_threshold, 
                                                YCUT = ptau_threshold, 
                                                ZCUT = centiloid_threshold)
    

  })

  observeEvent(input$G2P2Rotate, {
    new.dat <- req(data_internal$raw)
    df_old <- filter(new.dat, Age > 70)
    
    ptau_threshold <- req(ptau_cutoff_upper$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippo_threshold <- hippo_cutoff
    cube_g2_animate$plot <- AnimatedG2CubeVisualisation(dat = df_old, 
                                                xinput = df_old$Sum.hippo,
                                                yinput = df_old$CSF.pTau.INNO, 
                                                zinput = df_old$Centiloid, 
                                                leg = LEGEND_2,
                                                xax=axx2,
                                                yax = axy2,
                                                zax = axz2, 
                                                XCUT = hippo_threshold, 
                                                YCUT = ptau_threshold, 
                                                ZCUT = centiloid_threshold)
  })

  output$plot5.P2 <- renderPlotly({
    if (is.null(cube_g2_animate$plot)) return()
    cube_g2_animate$plot
  })


  ############### ADD IN YOUR OWN DATA ##########################

  output$Group2AddIN <- renderPlotly({
    new.dat <- req(data_internal$raw)
    df8 <- new.dat[,c("Sum.hippo", "CSF.pTau.INNO", "Centiloid", "Clifford_class")]
    df9 <- na.omit(df8)
    New.Group2 <- data.frame(input$sums, input$ps, input$cents, "My own markers")
    names(New.Group2) <- c("Sum.hippo", "CSF.pTau.INNO", "Centiloid", "Clifford_class")
    newdf3 <- rbind(df9, New.Group2)
    newdf3$Clifford_class <- factor(newdf3$Clifford_class, levels = c("Stage 2, clinically asymptomatic",
                                                                      "Stage 1, preclinical AD stage",
                                                                      "SNAP",
                                                                      "MCI unlikely due to AD",
                                                                      "My own markers"))
    newdf4 <- mutate(newdf3, size_guide2 = ifelse(Clifford_class == "My own markers",18,12))
    newdf4$size_guide2 <- as.numeric(newdf4$size_guide2)
    if (input$DATAVIS2 == "3D Plot"){

      new_data_plot <- plot_ly(newdf4, x = ~CSF.pTau.INNO,
                               y = ~Sum.hippo,
                               z = ~Centiloid,
                               type = "scatter3d",
                               mode = "markers",
                               marker = list(size = ~size_guide2),
                               sizemode = 'Diameter',
                               color = ~Clifford_class,
                               colors = c("firebrick", "orange",'gold',"forestgreen","hotpink"))

      new_data_plot <- new_data_plot %>%
        layout(legend = LEGEND_2, scene = list(xaxis = axx2, yaxis = axy2, zaxis = axz2))
      new_data_plot


    }else if (input$DATAVIS2 == "A+/T+/N+ Visualisation"){

      AgeInput2 <- input$age

      if (AgeInput2 > 70){
        df_old <- filter(new.dat, Age > 70)
        df8 <- df_old[,c("Sum.hippo", "CSF.pTau.INNO", "Centiloid", "Clifford_class")]
        df9 <- na.omit(df8)
        New.Group2 <- data.frame(input$sums, input$ps, input$cents, "My own markers")
        names(New.Group2) <- c("Sum.hippo", "CSF.pTau.INNO", "Centiloid", "Clifford_class")
        newdf3 <- rbind(df9, New.Group2)
        newdf3$Clifford_class <- factor(newdf3$Clifford_class, levels = c("Stage 2, clinically asymptomatic",
                                                                          "Stage 1, preclinical AD stage",
                                                                          "SNAP",
                                                                          "MCI unlikely due to AD",
                                                                          "My own markers"))
        newdf4 <- mutate(newdf3, size_guide2 = ifelse(Clifford_class == "My own markers",18,12))
        newdf4$size_guide2 <- as.numeric(newdf4$size_guide2)
        new_group2 <- mutate(newdf4, scat_col = ifelse(CSF.pTau.INNO >73.83  & Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                       "selected - Stage 2 clinically asymptomatic",
                                                       ifelse(CSF.pTau.INNO >73.83  & Sum.hippo <5.402554 & Centiloid > 20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                              "selected - Stage 1 preclinical AD stage",
                                                              ifelse(CSF.pTau.INNO >73.83  & Sum.hippo < 5.402554 & Centiloid > 20 & Clifford_class == "SNAP",
                                                                     "selected - SNAP",
                                                                     ifelse(CSF.pTau.INNO >73.83  & Sum.hippo < 5.402554 & Centiloid > 20 & Clifford_class == "MCI unlikely due to AD",
                                                                            "selected - MCI unlikely due to AD",
                                                                            ifelse(Clifford_class == "My own markers", "My own markers", "unselected"))))))


        new_group2$scat_col <- factor(new_group2$scat_col, levels = c("unselected", "selected - Stage 2 clinically asymptomatic",
                                                                      "selected - Stage 1 preclinical AD stage", "selected - SNAP",
                                                                      "selected - MCI unlikely due to AD",
                                                                      "My own markers"))


        df_mesh_1 <- data.frame(X_VAL = c(max(new_group2$CSF.pTau.INNO), max(new_group2$CSF.pTau.INNO), 73.83,     73.83 ,   max(new_group2$CSF.pTau.INNO),  max(new_group2$CSF.pTau.INNO),  73.83,     73.83),
                                Y_VAL = c(min(new_group2$Sum.hippo, na.rm = T),  5.402554, min(new_group2$Sum.hippo, na.rm = T), 5.402554, min(new_group2$Sum.hippo, na.rm = T),   5.402554, min(new_group2$Sum.hippo, na.rm = T), 5.402554),
                                Z_VAL = c(20,      20,    20,      20,  max(new_group2$Centiloid, na.rm = T), max(new_group2$Centiloid, na.rm = T),   max(new_group2$Centiloid, na.rm = T),    max(new_group2$Centiloid, na.rm = T)),
                                MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))

        # Make apoe4 a factor

        cube_data_vis <- plot_ly()%>%
          add_markers(type = "scatter3d",
                      mode = "markers",
                      marker = list(size = ~size_guide2),
                      data = new_group2,
                      x = ~CSF.pTau.INNO,
                      y = ~Sum.hippo,
                      z = ~Centiloid,
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
          layout(legend = LEGEND_2, scene = list(xaxis = axx2, yaxis = axy2, zaxis = axz2))
        cube_data_vis
      }else {
        df_young <- filter(new.dat, Age <70)
        df8 <- df_young[,c("Sum.hippo", "CSF.pTau.INNO", "Centiloid", "Clifford_class")]
        df9 <- na.omit(df8)
        New.Group2 <- data.frame(input$sums, input$ps, input$cents, "My own markers")
        names(New.Group2) <- c("Sum.hippo", "CSF.pTau.INNO", "Centiloid", "Clifford_class")
        newdf3 <- rbind(df9, New.Group2)
        newdf3$Clifford_class <- factor(newdf3$Clifford_class, levels = c("Stage 2, clinically asymptomatic",
                                                                          "Stage 1, preclinical AD stage",
                                                                          "SNAP",
                                                                          "MCI unlikely due to AD",
                                                                          "My own markers"))
        newdf4 <- mutate(newdf3, size_guide2 = ifelse(Clifford_class == "My own markers",18,12))
        newdf4$size_guide2 <- as.numeric(newdf4$size_guide2)
        new_group2 <- mutate(newdf4, scat_col = ifelse(CSF.pTau.INNO >59.23  & Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                       "selected - Stage 2 clinically asymptomatic",
                                                       ifelse(CSF.pTau.INNO >59.23  & Sum.hippo <5.402554 & Centiloid > 20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                              "selected - Stage 1 preclinical AD stage",
                                                              ifelse(CSF.pTau.INNO >59.23  & Sum.hippo < 5.402554 & Centiloid > 20 & Clifford_class == "SNAP",
                                                                     "selected - SNAP",
                                                                     ifelse(CSF.pTau.INNO >59.23  & Sum.hippo < 5.402554 & Centiloid > 20 & Clifford_class == "MCI unlikely due to AD",
                                                                            "selected - MCI unlikely due to AD",
                                                                            ifelse(Clifford_class == "My own markers", "My own markers", "unselected"))))))


        new_group2$scat_col <- factor(new_group2$scat_col, levels = c("unselected", "selected - Stage 2 clinically asymptomatic",
                                                                      "selected - Stage 1 preclinical AD stage", "selected - SNAP",
                                                                      "selected - MCI unlikely due to AD",
                                                                      "My own markers"))


        df_mesh_1 <- data.frame(X_VAL = c(max(new_group2$CSF.pTau.INNO), max(new_group2$CSF.pTau.INNO), 59.23 , 59.23 ,   max(new_group2$CSF.pTau.INNO),  max(new_group2$CSF.pTau.INNO),  59.23,     59.23),
                                Y_VAL = c(min(new_group2$Sum.hippo, na.rm = T),  5.402554, min(new_group2$Sum.hippo, na.rm = T), 5.402554, min(new_group2$Sum.hippo, na.rm = T),   5.402554, min(new_group2$Sum.hippo, na.rm = T), 5.402554),
                                Z_VAL = c(20,      20,    20,      20,  max(new_group2$Centiloid, na.rm = T), max(new_group2$Centiloid, na.rm = T),   max(new_group2$Centiloid, na.rm = T),    max(new_group2$Centiloid, na.rm = T)),
                                MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))


        cube_data_vis <- plot_ly()%>%
          add_markers(type = "scatter3d",
                      mode = "markers",
                      marker = list(size = ~size_guide2),
                      data = new_group2,
                      x = ~CSF.pTau.INNO,
                      y = ~Sum.hippo,
                      z = ~Centiloid,
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
          layout(legend = LEGEND_2, scene = list(xaxis = axx2, yaxis = axy2, zaxis = axz2))
        cube_data_vis

      }
    }
  })

  ########################### Statistical Modelling  ##########################
  ############################## VISUALISATION ################################

  ################################ BOXPLOTS ###################################

  output$BOXPLOT2 <- renderPlot({
    new.dat <- req(data_internal$raw)
    # new.dat$apoe4 <- as.factor(new.dat$apoe4, levels = c("0","1"))
    new.dat$Age_binary <- factor(new.dat$Age_binary, levels = c("0","1"))
    if (input$BIOMARKERG2 == "Centiloid"){
      BoxplotFunction(new.dat, new.dat$Centiloid, input_title = axz2)

    } else if (input$BIOMARKERG2 == "CSF pTau pg/mL"){

      BoxplotFunction(new.dat, new.dat$CSF.pTau.INNO, input_title = axy)

    }else if (input$BIOMARKERG2 == "BOXHIP1"){

      BoxplotFunction(new.dat, new.dat$Sum.hippo, input_title = hippo.boxplot)
    }

  })

  output$SCATTER2 <- renderPlotly({
    new.dat <- req(data_internal$raw)

    if (input$SCATTERBIOMARKER == 'Centiloid'){
      m <- ggplot(new.dat, aes(x= Age, y = Centiloid, color = Clifford_class, group = ID))+
        geom_point()+
        theme_bw()+
        xlab("Age")+
        ylab("Centiloid") +
        ggtitle("Age vs Centiloid")+
        labs(color = "ATN Clifford et. al \n Classification")+
        scale_color_manual(values = c("firebrick", "darkorange", "yellow", "forestgreen"))+
        geom_smooth(method = "loess", se = T)
      ggplotly(m)
    }else if (input$SCATTERBIOMARKER == "CSF pTau pg/mL"){
      plot1 <- ggplot(new.dat, aes(x= Age, y = CSF.pTau.INNO, na.rm = TRUE, color = pTau.status))+
        geom_point()+
        theme_bw()+
        xlab("Age")+
        ylab("CSF pTau pg/mL") +
        ggtitle("Age vs CSF pTau pg/mL")+
        scale_color_manual(values = c("red", "forestgreen"))+
        geom_smooth(method = "loess", se = T)
      ggplotly(plot1)
    } else if (input$SCATTERBIOMARKER == "SCATTERHIP1"){
      m <- ggplot(new.dat, aes(x= Age, y = Sum.hippo, color = Clifford_class, group = ID))+
        geom_point()+
        theme_bw()+
        xlab("Age")+
        ylab("Hippocampus mL<sup>3</sup>")+
        ggtitle("Age vs Hippocampus")+
        labs(color = "ATN Clifford et. al \n Classification")+
        scale_color_manual(values = c("firebrick", "darkorange", "yellow", "forestgreen"))+
        geom_smooth(method = "loess", se = T)
      ggplotly(m)

    }
  })

  output$SURFACEG2 <- renderPlotly({
    new.dat <- req(data_internal$raw)

    if (input$DEMOGRAPHIC2 == "APOE-e4 allele"){
      new.dat$apoe4 <- factor(new.dat$apoe4, levels = c("1","0"))

      new_data_frame <- filter(new.dat, apoe4 == 1)

      apoe_4 <- plot_ly()
      apoe_4 <- add_markers(
        p = apoe_4,
        data = new.dat,
        x = ~CSF.pTau.INNO,
        y = ~ Sum.hippo,
        z = ~Centiloid,
        text = ~Diagnosis,
        type = "scatter3d",
        color = ~Clifford_class,
        colors = c("firebrick", "darkorange", "gold", "forestgreen"),
        mode = "markers"
      )


      my_lm <- lm(Centiloid ~ Sum.hippo+CSF.pTau.INNO, new_data_frame)
      # Set up Axis

      axis_x1 <- seq(min(new.dat$CSF.pTau.INNO), max(new.dat$CSF.pTau.INNO))
      axis_z1 <- seq(min(new.dat$Sum.hippo, na.rm = T), max(new.dat$Sum.hippo, na.rm = T))

      ## PLOT THE POINTS

      my_lm_surface <- expand.grid(CSF.pTau.INNO = axis_x1,Sum.hippo = axis_z1,KEEP.OUT.ATTRS = F)
      my_lm_surface$Centiloid <- predict.lm(my_lm, newdata = my_lm_surface)
      my_lm_surface <- acast(my_lm_surface, Sum.hippo ~ CSF.pTau.INNO, value.var = "Centiloid")

      apoe_4 <- add_trace(p = apoe_4,
                          z = my_lm_surface,
                          x = axis_x1,
                          y = axis_z1,
                          type = "surface",
                          colorscale = list(c(0,1), c("green","red")),
                          showscale = F,
                          name = "Regression for Carriers",
                          showlegend = T,
                          opacity = 0.6)

      # second regression for APOE4 non-carriers
      non_carriers <- filter(new.dat, apoe4 == 0)

      new_lm <- lm(Centiloid ~ Sum.hippo+CSF.pTau.INNO, non_carriers)
      # Set up Axis

      axis_x1 <- seq(min(new.dat$CSF.pTau.INNO), max(new.dat$CSF.pTau.INNO))
      axis_z1 <- seq(min(new.dat$Sum.hippo, na.rm = T), max(new.dat$Sum.hippo, na.rm = T))

      ## PLOT THE POINTS

      new_lm_surface <- expand.grid(CSF.pTau.INNO = axis_x1,Sum.hippo = axis_z1,KEEP.OUT.ATTRS = F)
      new_lm_surface$Centiloid <- predict.lm(new_lm, newdata = new_lm_surface)
      new_lm_surface <- acast(new_lm_surface, Sum.hippo ~ CSF.pTau.INNO, value.var = "Centiloid")

      apoe_4 <- add_trace(p = apoe_4,
                          z = new_lm_surface,
                          x = axis_x1,
                          y = axis_z1,
                          type = "surface",
                          colorscale = list(c(0,1), c("blue","tan")),
                          showscale = F,
                          name = "Regression for non-Carriers",
                          showlegend = T,
                          opacity = 0.6)
      apoe_4 <- apoe_4 %>%
        layout(legend = LEGEND_2,
               scene = list(xaxis = axx2, yaxis = axy2, zaxis = axz2))
      apoe_4
    }else if (input$DEMOGRAPHIC2 == 'Sex'){
      sex <- filter(new.dat, Sex == "Female")

      sex_plot <- plot_ly()
      sex_plot <- add_markers(
        p = sex_plot,
        data = new.dat,
        x = ~CSF.pTau.INNO,
        y = ~ Sum.hippo,
        z = ~Centiloid,
        text = ~Diagnosis,
        type = "scatter3d",
        color = ~Clifford_class,
        colors = c("firebrick", "darkorange", "gold", "forestgreen"),
        mode = "markers")

      sex_lm_f <- lm(Centiloid ~ Sum.hippo+CSF.pTau.INNO, sex)
      # Set up Axis

      x1 <- seq(min(new.dat$CSF.pTau.INNO), max(new.dat$CSF.pTau.INNO))
      z1 <- seq(min(new.dat$Sum.hippo, na.rm = T), max(new.dat$Sum.hippo, na.rm = T))

      ## PLOT THE POINTS

      sex_surface_1 <- expand.grid(CSF.pTau.INNO = x1,Sum.hippo = z1,KEEP.OUT.ATTRS = F)
      sex_surface_1$Centiloid <- predict.lm(sex_lm_f, newdata = sex_surface_1)
      sex_surface_1 <- acast(sex_surface_1, Sum.hippo ~ CSF.pTau.INNO, value.var = "Centiloid")

      sex_plot <- add_trace(p = sex_plot,
                            z = sex_surface_1,
                            x = x1,
                            y = z1,
                            type = "surface",
                            colorscale = list(c(0,1), c("green","red")),
                            showscale = F,
                            name = "Regression for Females",
                            showlegend = T,
                            opacity = 0.6)

      # second regression for APOE4 non-carriers
      sex_2 <- filter(new.dat, Sex == "Male")

      lm_for_males <- lm(Centiloid ~ Sum.hippo+CSF.pTau.INNO, sex_2)
      # Set up Axis

      ## PLOT THE POINTS

      surface_males <- expand.grid(CSF.pTau.INNO = x1,Sum.hippo = z1,KEEP.OUT.ATTRS = F)
      surface_males$Centiloid <- predict.lm(lm_for_males, newdata = surface_males)
      surface_males <- acast(surface_males, Sum.hippo ~ CSF.pTau.INNO, value.var = "Centiloid")

      sex_plot <- add_trace(p = sex_plot,
                            z = surface_males,
                            x = x1,
                            y = z1,
                            type = "surface",
                            colorscale = list(c(0,1), c("blue","tan")),
                            showscale = F,
                            name = "Regression for Males",
                            showlegend = T,
                            opacity = 0.6)
      sex_plot <- sex_plot %>%
        layout(legend = LEGEND_2,
               scene = list(xaxis = axx2, yaxis = axy2, zaxis = axz2))
      sex_plot
    }else if (input$DEMOGRAPHIC2 == "Age"){
      #####################################################
      ######################   Age   ######################

      age <- filter(new.dat, Age_binary == 1)

      age_plot <- plot_ly()
      age_plot <- add_markers(
        p = age_plot,
        data = new.dat,
        x = ~CSF.pTau.INNO,
        y = ~ Sum.hippo,
        z = ~Centiloid,
        text = ~Diagnosis,
        type = "scatter3d",
        color = ~Clifford_class,
        colors = c("firebrick", "darkorange", "gold", "forestgreen"),
        mode = "markers"
      )

      age_1_lm <- lm(Centiloid ~ Sum.hippo+CSF.pTau.INNO, age)
      # Set up Axis

      x1 <- seq(min(new.dat$CSF.pTau.INNO), max(new.dat$CSF.pTau.INNO))
      z1 <- seq(min(new.dat$Sum.hippo, na.rm = T), max(new.dat$Sum.hippo, na.rm = T))

      ## PLOT THE POINTS

      age_surface_1 <- expand.grid(CSF.pTau.INNO = x1,Sum.hippo = z1,KEEP.OUT.ATTRS = F)
      age_surface_1$Centiloid <- predict.lm(age_1_lm, newdata = age_surface_1)
      age_surface_1 <- acast(age_surface_1, Sum.hippo ~ CSF.pTau.INNO, value.var = "Centiloid")

      age_plot <- add_trace(p = age_plot,
                            z = age_surface_1,
                            x = x1,
                            y = z1,
                            type = "surface",
                            colorscale = list(c(0,1), c("green","red")),
                            showscale = F,
                            name = "Regression for Individuals with over 72.5 years of Age",
                            showlegend = T,
                            opacity = 0.6)

      # second regression for APOE4 non-carriers
      age_2 <- filter(new.dat, Age_binary == 0)

      age_0_lm <- lm(Centiloid ~ Sum.hippo+CSF.pTau.INNO, age_2)
      # Set up Axis

      ## PLOT THE POINTS

      age_surface_0 <- expand.grid(CSF.pTau.INNO = x1,Sum.hippo = z1,KEEP.OUT.ATTRS = F)
      age_surface_0$Centiloid <- predict.lm(age_0_lm, newdata = age_surface_0)
      age_surface_0 <- acast(age_surface_0, Sum.hippo ~ CSF.pTau.INNO, value.var = "Centiloid")

      age_plot <- add_trace(p = age_plot,
                            z = age_surface_0,
                            x = x1,
                            y = z1,
                            type = "surface",
                            colorscale = list(c(0,1), c("blue","tan")),
                            showscale = F,
                            name = "Regression for Individuals under the age of 72.5",
                            showlegend = T,
                            opacity = 0.6)
      age_plot <- age_plot %>%
        layout(legend = LEGEND_2,
               scene = list(xaxis = axx2, yaxis = axy2, zaxis = axz2))
      age_plot
    }else if (input$DEMOGRAPHIC2 == "Education"){
      education <- filter(new.dat, Education_binary == 1)

      education_plot <- plot_ly()
      education_plot <- add_markers(
        p = education_plot,
        data = new.dat,
        x = ~CSF.pTau.INNO,
        y = ~ Sum.hippo,
        z = ~Centiloid,
        text = ~Diagnosis,
        type = "scatter3d",
        color = ~Clifford_class,
        colors = c("firebrick", "darkorange", "gold", "forestgreen"),
        mode = "markers"
      )


      education_1_lm <- lm(Centiloid ~ Sum.hippo+CSF.pTau.INNO, education)
      # Set up Axis

      x1 <- seq(min(new.dat$CSF.pTau.INNO), max(new.dat$CSF.pTau.INNO))
      z1 <- seq(min(new.dat$Sum.hippo, na.rm = T), max(new.dat$Sum.hippo, na.rm = T))

      ## PLOT THE POINTS

      education_surface_1 <- expand.grid(CSF.pTau.INNO = x1,Sum.hippo = z1,KEEP.OUT.ATTRS = F)
      education_surface_1$Centiloid <- predict.lm(education_1_lm, newdata = education_surface_1)
      education_surface_1 <- acast(education_surface_1, Sum.hippo ~ CSF.pTau.INNO, value.var = "Centiloid")

      education_plot <- add_trace(p = education_plot,
                                  z = education_surface_1,
                                  x = x1,
                                  y = z1,
                                  type = "surface",
                                  colorscale = list(c(0,1), c("green","red")),
                                  showscale = F,
                                  name = "Regression for Individuals with over 12 years of Education",
                                  showlegend = T,
                                  opacity = 0.6)

      # second regression for APOE4 non-carriers
      education_2 <- filter(new.dat, Education_binary == 0)

      lm_0 <- lm(Centiloid ~ Sum.hippo+CSF.pTau.INNO, education_2)
      # Set up Axis

      ## PLOT THE POINTS

      surface_education_0 <- expand.grid(CSF.pTau.INNO = x1,Sum.hippo = z1,KEEP.OUT.ATTRS = F)
      surface_education_0$Centiloid <- predict.lm(lm_0, newdata = surface_education_0)
      surface_education_0 <- acast(surface_education_0, Sum.hippo ~ CSF.pTau.INNO, value.var = "Centiloid")

      education_plot <- add_trace(p = education_plot,
                                  z = surface_education_0,
                                  x = x1,
                                  y = z1,
                                  type = "surface",
                                  colorscale = list(c(0,1), c("blue","tan")),
                                  showscale = F,
                                  name = "Regression for Individuals with under 12 years of Education",
                                  showlegend = T,
                                  opacity = 0.6)
      education_plot <- education_plot %>%
        layout(legend = LEGEND_2,
               scene = list(xaxis = axx2, yaxis = axy2, zaxis = axz2))
      education_plot
    }

  })

  ############################ MODEL SUMMARIES ###############################
  output$LINREGG2 <- renderUI({
    new.dat <- req(data_internal$raw)
    if (input$LING2 == "Centiloid"){
      lm_Centiloid <- lm(Centiloid ~ Age+Sex+Education_binary+apoe4, data = new.dat)
      sumarry_pvals <- as.data.frame(summary(lm_Centiloid)$coefficients)
      new_sum <- sumarry_pvals %>%
        mutate(across(where(is.numeric), ~signif(.,digits = 2) #round(., digits = 6)
        ))
      rownames(new_sum) = c("Intercept","Age","Sex (Male)","Education Binary (under 12 years education)", "apoe4 (non-carrier)")
      mytableout = htmlTable (
        new_sum,
        caption = "Linear summary for Centiloid ~ Age+Sex+Education_binary+apoe4",
        tfoot = paste("R-squared value=",signif(summary(lm_Centiloid)$r.squared,2), "Adjusted R-squared value=",
                      signif(summary(lm_Centiloid)$adj.r.squared,2),sep = "     ")
      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }else if (input$LING2 == "CSF pTau pg/mL"){
      lm_tTau <- lm(CSF.pTau.INNO ~ Age+Sex+Education_binary+apoe4, data = new.dat)
      sumarry_pvals <- as.data.frame(summary(lm_tTau)$coefficients)
      new_sum <- sumarry_pvals %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2)#round(., digits = 6)
        ))
      rownames(new_sum) = c("Intercept","Age","Sex (Male)","Education Binary (under 12 years education)", "apoe4 (non-carrier)")
      mytableout = htmlTable (
        new_sum,
        caption = "Linear summary for Hippocampus ~ Age+Sex+Education_binary+apoe4",
        tfoot = paste("R-squared value=",signif(summary(lm_tTau)$r.squared,2), "Adjusted R-squared value=",
                      signif(summary(lm_tTau)$adj.r.squared,2),sep = "     ")
      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }else if (input$LING2 == "LINHIP1"){
      lm_Sum.hippo <- lm(Sum.hippo ~ Age+Sex+Education_binary+apoe4, data = new.dat)
      sumarry_pvals <- as.data.frame(summary(lm_Sum.hippo)$coefficients)
      new_sum <- sumarry_pvals %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2#round(., digits = 6, s
        )))
      rownames(new_sum) = c("Intercept","Age","Sex (Male)","Education Binary (under 12 years education)", "apoe4 (non-carrier)")
      mytableout = htmlTable (
        new_sum,
        caption = "Linear summary for Hippocampus ~ Age+Sex+Education_binary+apoe4",
        tfoot = paste("R-squared value=",signif(summary(lm_Sum.hippo)$r.squared,2), "Adjusted R-squared value=",
                      signif(summary(lm_Sum.hippo)$adj.r.squared,2),sep = "     ")      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }
  })
  ########################2. . Biomarker Interplay #########################

  output$INTERPLAY2 <- renderUI({
    new.dat <- req(data_internal$raw)
    if (input$INTERPLAYG2 == "Centiloid"){
      lm_AB <- lm(Centiloid ~ CSF.pTau.INNO+Sum.hippo, data = new.dat)
      p_value_df <- as.data.frame(summary(lm_AB)$coefficients)
      new_pvalue <- p_value_df %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
      rownames(new_pvalue) = c("Intercept","CSF pTau pg/mL","Hippocampus")
      mytableout = htmlTable (
        new_pvalue,
        caption = "Linear summary for Centiloid ~ CSF pTau + Hippocampus",
        tfoot = paste("R-squared value=",signif(summary(lm_AB)$r.squared,2), "Adjusted R-squared value=",
                      signif(summary(lm_AB)$adj.r.squared,2),sep = "     ")
      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }else if (input$INTERPLAYG2 == "CSF pTau pg/mL"){
      lm_AB <- lm(CSF.pTau.INNO ~ Centiloid+Sum.hippo, data = new.dat)
      p_value_df <- as.data.frame(summary(lm_AB)$coefficients)
      new_pvalue <- p_value_df %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
      rownames(new_pvalue) = c("Intercept","Centiloid","Sum.hippo")
      mytableout = htmlTable (
        new_pvalue,
        caption = "Linear summary for CSF pTau ~ Centiloid + Hippocampus",
        tfoot = paste("R-squared value=",signif(summary(lm_AB)$r.squared,2), "Adjusted R-squared value=",
                      signif(summary(lm_AB)$adj.r.squared,2),sep = "     ")
      )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }else if (input$INTERPLAYG2 == "MODELHIP1"){
      lm_AB <- lm(Sum.hippo ~ Centiloid+CSF.pTau.INNO, data = new.dat)
      p_value_df <- as.data.frame(summary(lm_AB)$coefficients)
      new_pvalue <- p_value_df %>%
        mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
      rownames(new_pvalue) = c("Intercept","Centiloid","CSF pTau pg/mL")
      mytableout = htmlTable (
        new_pvalue,
        caption = "Linear summary for Hippocampus ~ Centiloid + CSF pTau",
        tfoot = paste("R-squared value=",signif(summary(lm_AB)$r.squared,2), "Adjusted R-squared value=",
                      signif(summary(lm_AB)$adj.r.squared,2),sep = "     ")
        )
      mytableout %>%
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
        htmlTable
    }
  })

  ############################## DISCUSSION ##################################
  output$group2table <- renderUI({
    new.dat <- req(data_internal$raw)
    lm_pTau <- lm(CSF.pTau.INNO ~ Age+Sex+Education_binary+apoe4, data = new.dat)
    lm_Centiloid <- lm(Centiloid ~ Age+Sex+Education_binary+apoe4, data = new.dat)
    lm_Hippo <- lm(Sum.hippo ~ Age+Sex+Education_binary+apoe4, data = new.dat)
    newdf <- as.data.frame(summary(lm_Centiloid)$coefficients[,4])
    R_Squared <- as.data.frame(summary(lm_Centiloid)$r.squared)
    names(R_Squared) <- "summary(lm_Centiloid)$coefficients[, 4]"
    total <- rbind(newdf, R_Squared)
    newdf1 <- as.data.frame(summary(lm_pTau)$coefficients[,4])
    ptau_r_squared <- as.data.frame(summary(lm_pTau)$r.squared)
    names(ptau_r_squared) <- "summary(lm_pTau)$coefficients[, 4]"
    total1 <- rbind(newdf1,ptau_r_squared)
    newdf2 <- as.data.frame(summary(lm_Hippo)$coefficients[,4])
    hippo_squared <- as.data.frame(summary(lm_Hippo)$r.squared)
    names(hippo_squared) <- "summary(lm_Hippo)$coefficients[, 4]"
    total2 <- rbind(newdf2,hippo_squared)
    results <- cbind(total,total1,total2)
    results2 <- results %>%
      mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
    where <- (results2 <= 0.05)
    style <- 'background-color: yellow; color: black;'
    css.cell <- matrix('', nrow(results2), ncol(results2))
    css.cell[where] <- style
    names(results2) = c("Centiloid","CSF pTau pg/mL","Hippocampus mL<sup>3</sup>")
    rownames(results2) = c("Intercept","Age","Sex (Male)","Education Binary (under 12 years education)","apoe4 (non-carrier)", "R Squared Value")
    mytableout = htmlTable (
      results2,
      css.cell = css.cell,
      caption = "Table 5: P-Values summaries from all regression computed for Group 2",
    )
    mytableout
  })

  output$group2pvalue <- renderUI({
    new.dat <- req(data_internal$raw)
    lm_centiloid <- lm(Centiloid ~ CSF.pTau.INNO+Sum.hippo, data = new.dat)
    lm_ptau <- lm(CSF.pTau.INNO ~ Centiloid+Sum.hippo, data = new.dat)
    lm_hippo <- lm(Sum.hippo ~ Centiloid+CSF.pTau.INNO, data = new.dat)

    # Make a matrix for the data

    p_group1 <- matrix(NA, ncol=3, nrow=5)


    p_group1[1,1] = signif(summary(lm_centiloid)$coefficients[1,4],digits = 2)
    p_group1[1,2] = signif(summary(lm_ptau)$coefficients[1,4],digits = 2)
    p_group1[1,3] = signif(summary(lm_hippo)$coefficients[1,4],digits = 2)
    p_group1[2,1] = "NA"
    p_group1[2,2] = signif(summary(lm_ptau)$coefficients[2,4],digits = 2)
    p_group1[2,3] = signif(summary(lm_hippo)$coefficients[2,4],digits = 2)

    p_group1[3,1] = signif(summary(lm_centiloid)$coefficients[2,4],digits = 2)
    p_group1[3,2] = "NA"

    p_group1[3,3] = signif(summary(lm_hippo)$coefficients[3,4],digits = 2)

    p_group1[4,1] = signif(summary(lm_centiloid)$coefficients[3,4],digits=2)
    p_group1[4,2] = signif(summary(lm_ptau)$coefficients[3,4], digits = 2)
    p_group1[4,3] = "NA"

    p_group1[5,1] = signif(summary(lm_centiloid)$r.squared, digits = 2)
    p_group1[5,2] = signif(summary(lm_ptau)$r.squared, digits = 2)
    p_group1[5,3] = signif(summary(lm_hippo)$r.squared, digits =2)

    newdataframe <- as.matrix(p_group1)
    newdataframe <- as.numeric(newdataframe)
    # p_group1 <- p_group1 %>%
    #   mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
    where <- (newdataframe <= 0.05)
    style <- 'background-color: yellow; color: black;'
    css.cell <- matrix('', nrow(p_group1), ncol(p_group1))
    css.cell[where] <- style

    rownames(p_group1) = c("Intercept","Centiloid","CSF pTau pg/mL", "Hippocampus mL<sup>3</sup>", "R-squared Value")
    colnames(p_group1) = c("Centiloid", "CSF pTau pg/mL", "Hippocampus mL<sup>3</sup>")
    mytableout = htmlTable (
      p_group1,
      css.cell = css.cell,
      caption = "Table 6: p-value summaries for all Biomarker interplay"
    )


    mytableout

  })

  ########################## EXTRA A+/T+/N+ VIZ  ###########################

  output$EXTRACUBE <- renderPlotly({
    new.dat <- req(data_internal$raw)

    if (input$PopCube == "70+"){

      df_old <- filter(new.dat, Age > 70)
      df7 <- mutate(df_old, scat_col = ifelse(CSF.pTau.INNO >73.83   & Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                              "Stage 2 Clinically Asymptomatic meeting all cut-offs",
                                              ifelse(CSF.pTau.INNO >73.83   & Sum.hippo <5.402554 & Centiloid > 20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                     "Stage 1 Preclinical AD meeting all cut-offs",
                                                     ifelse(CSF.pTau.INNO >73.83   & Sum.hippo < 5.402554 & Centiloid > 20 & Clifford_class == "SNAP",
                                                            "SNAP meeting all cut-offs",
                                                            ifelse(CSF.pTau.INNO >73.83   & Sum.hippo < 5.402554 & Centiloid > 20 & Clifford_class == "MCI unlikely due to AD",
                                                                   "MCI unlikely due to AD meeting all cut-offs",
                                                                   ifelse(CSF.pTau.INNO >73.83   & Sum.hippo < 5.402554 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                          "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                          ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                                 "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                                 ifelse(CSF.pTau.INNO >73.83  & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                                        "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                                        ifelse(CSF.pTau.INNO >73.83   & Sum.hippo < 5.402554 & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                               "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                               ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                                      "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                                      ifelse(CSF.pTau.INNO >73.83  & Centiloid >20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                                             "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                                             ifelse(CSF.pTau.INNO >73.83   & Sum.hippo < 5.402554 & Clifford_class == "SNAP",
                                                                                                                    "SNAP meeting 2 cut-offs",
                                                                                                                    ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "SNAP",
                                                                                                                           "SNAP meeting 2 cut-offs",
                                                                                                                           ifelse(CSF.pTau.INNO >73.83  & Centiloid >20 & Clifford_class == "SNAP",
                                                                                                                                  "SNAP meeting 2 cut-offs",
                                                                                                                                  ifelse(CSF.pTau.INNO >73.83   & Sum.hippo < 5.402554 & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                         "MCI unlikely due to AD meeting 2 cut-offs",
                                                                                                                                         ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                                "MCI unlikely due to AD meeting 2 cut-offs",
                                                                                                                                                ifelse(CSF.pTau.INNO >73.83  & Centiloid >20 & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                                       "MCI unlikely due to AD meeting 2 cut-offs","unselected")))))))))))))))))



      df7$scat_col <- factor(df7$scat_col, levels = c("unselected", "Stage 2 Clinically Asymptomatic meeting all cut-offs",
                                                      "Stage 1 Preclinical AD meeting all cut-offs", "SNAP meeting all cut-offs",
                                                      "MCI unlikely due to AD meeting all cut-offs",
                                                      #"Patients who are within 10% of any cut-off",
                                                      "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                      "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                      "SNAP meeting 2 cut-offs",
                                                      "MCI unlikely due to AD meeting 2 cut-offs"))


      df_mesh_1 <- data.frame(X_VAL = c(max(df7$CSF.pTau.INNO), max(df7$CSF.pTau.INNO), 73.83 ,73.83 ,   max(df7$CSF.pTau.INNO),  max(df7$CSF.pTau.INNO), 73.83 ,73.83 ),
                              Y_VAL = c(min(df7$Sum.hippo, na.rm = T),  5.402554, min(df7$Sum.hippo, na.rm = T), 5.402554, min(df7$Sum.hippo, na.rm = T),   5.402554, min(df7$Sum.hippo, na.rm = T), 5.402554),
                              Z_VAL = c(20,      20,    20,      20,  max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T)),
                              MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))

      # Make apoe4 a factor
      df7$apoe4 <- factor(df7$apoe4, levels = c(1,0))

      cube2 <- plot_ly()%>%
        add_markers(type = "scatter3d",
                    mode = "markers",
                    data = df7,
                    x = ~CSF.pTau.INNO,
                    y = ~Sum.hippo,
                    z = ~Centiloid,
                    color = ~scat_col,
                    colors = c('gray', 'red', "gold","yellow", "green", "aquamarine", "chocolate1", "darkorchid1","deeppink")) %>%
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

      cube2 <- cube2 %>%
        layout(legend = LEGEND_2, scene = list(xaxis = axx2, yaxis = axy2, zaxis = axz2))
      cube2


    }else{

      df_young <- filter(new.dat, Age < 70)
      df7 <- mutate(df_young, scat_col = ifelse(CSF.pTau.INNO >59.23  & Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                "Stage 2 Clinically Asymptomatic meeting all cut-offs",
                                                ifelse(CSF.pTau.INNO >59.23  & Sum.hippo <5.402554 & Centiloid > 20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                       "Stage 1 Preclinical AD meeting all cut-offs",
                                                       ifelse(CSF.pTau.INNO >59.23  & Sum.hippo < 5.402554 & Centiloid > 20 & Clifford_class == "SNAP",
                                                              "SNAP meeting all cut-offs",
                                                              ifelse(CSF.pTau.INNO >59.23  & Sum.hippo < 5.402554 & Centiloid > 20 & Clifford_class == "MCI unlikely due to AD",
                                                                     "MCI unlikely due to AD meeting all cut-offs",
                                                                     ifelse(CSF.pTau.INNO >59.23  & Sum.hippo < 5.402554 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                            "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                            ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                                   "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                                   ifelse(CSF.pTau.INNO >59.23 & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                                          "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                                          ifelse(CSF.pTau.INNO >59.23  & Sum.hippo < 5.402554 & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                                 "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                                 ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                                        "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                                        ifelse(CSF.pTau.INNO >59.23 & Centiloid >20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                                               "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                                               ifelse(CSF.pTau.INNO >59.23  & Sum.hippo < 5.402554 & Clifford_class == "SNAP",
                                                                                                                      "SNAP meeting 2 cut-offs",
                                                                                                                      ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "SNAP",
                                                                                                                             "SNAP meeting 2 cut-offs",
                                                                                                                             ifelse(CSF.pTau.INNO >59.23 & Centiloid >20 & Clifford_class == "SNAP",
                                                                                                                                    "SNAP meeting 2 cut-offs",
                                                                                                                                    ifelse(CSF.pTau.INNO >59.23  & Sum.hippo < 5.402554 & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                           "MCI unlikely due to AD meeting 2 cut-offs",
                                                                                                                                           ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                                  "MCI unlikely due to AD meeting 2 cut-offs",
                                                                                                                                                  ifelse(CSF.pTau.INNO >59.23 & Centiloid >20 & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                                         "MCI unlikely due to AD meeting 2 cut-offs","unselected")))))))))))))))))



      df7$scat_col <- factor(df7$scat_col, levels = c("unselected", "Stage 2 Clinically Asymptomatic meeting all cut-offs",
                                                      "Stage 1 Preclinical AD meeting all cut-offs", "SNAP meeting all cut-offs",
                                                      "MCI unlikely due to AD meeting all cut-offs",
                                                      #"Patients who are within 10% of any cut-off",
                                                      "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                      "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                      "SNAP meeting 2 cut-offs",
                                                      "MCI unlikely due to AD meeting 2 cut-offs"))


      df_mesh_1 <- data.frame(X_VAL = c(max(df7$CSF.pTau.INNO), max(df7$CSF.pTau.INNO), 59.23,59.23,   max(df7$CSF.pTau.INNO),  max(df7$CSF.pTau.INNO), 59.23,59.23),
                              Y_VAL = c(min(df7$Sum.hippo, na.rm = T),  5.402554, min(df7$Sum.hippo, na.rm = T), 5.402554, min(df7$Sum.hippo, na.rm = T),   5.402554, min(df7$Sum.hippo, na.rm = T), 5.402554),
                              Z_VAL = c(20,      20,    20,      20,  max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T)),
                              MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))

      # Make apoe4 a factor
      df7$apoe4 <- factor(df7$apoe4, levels = c(1,0))

      cube2 <- plot_ly()%>%
        add_markers(type = "scatter3d",
                    mode = "markers",
                    data = df7,
                    x = ~CSF.pTau.INNO,
                    y = ~Sum.hippo,
                    z = ~Centiloid,
                    color = ~scat_col,
                    colors = c('gray', 'red', "gold","yellow", "green", "aquamarine", "chocolate1", "darkorchid1","deeppink")) %>%
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

      cube2 <- cube2 %>%
        layout(legend = LEGEND_2, scene = list(xaxis = axx2, yaxis = axy2, zaxis = axz2))
      cube2
    }
  })
  
  ############################ Demographic Summary ############################
  ################################# TABLE #####################################
  
  output$table1 <- renderTable({
    new.dat <- req(data_internal$raw)
    table_2 <- new_table_function(dat=new.dat)
    group_1_table <- table_2$group_1_table
    group_1_table}, hover = T, striped = T, bordered = T, 
                               width = "auto", align = "c", colnames = F, na = "")
  
  
  
  ############################ Demographic Summary ############################
  ################################# TABLE #####################################
  
  output$table.2 <- renderTable({
    new.dat <- req(data_internal$raw)
    table_2 <- new_table_function(dat = new.dat)
    group_2_table <- table_2$group_2_table
    group_2_table}, hover = T, striped = T, bordered = T,
                                width = "auto", align = "c", colnames = F, na = "")
  
  
  
############################# END OF SERVER FUNCTION ###########################  
}
