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
  
  #colnames(Example_csv)[7]<- "CSF.AB42"
  #colnames(Example_csv)[8]<- "CSF.pTau"
  #colnames(Example_csv)[9]<- "CSF.tTau"
  #colnames(simulated.data)[9]<- "CSF.tTau"
  #colnames(simulated.data)[7]<- "CSF.AB42"
  #colnames(simulated.data)[8]<- "CSF.pTau"
  #colnames(simulated.data)[9]<- "CSF.tTau"
  
  #save(Example_csv, file = "ExampleCSV.RData")
  #save(simulated.data, file = "simulated_data.Rdata")
  
  source("ScatterPlotFunction.R")
  source("CubeVisualisationG1.R")
  source("AnimatedScatterPlotFunction.R")
  source("PlanesFunction.R")
  source("AnimatedPlanesFunction.R")
  source("HippoFunction.R")
  source("AnimatedG1CubeVisualisation.R")
  source("G2CubeVisualisation.R")
  source("AnimatedG2CubeVisualisation.R")
  source("BoxplotFunction.R")
  source("new_table_function.R")
  source("LinearRegressionTables.R")
  source("ProcessingData.R")
  source("AddinDataG1.R")
  
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
  
  output$contents_3_3 <- renderTable({
    # Input INPUTfile will be NULL initiailly after the user selects 
    # and uploads a file, head of that data file will be shown 
    
    req(input$file_new_2)
    
    new_data_in2 <- read.csv(input$file_new_2$datapath, 
                             header = T)
    head(new_data_in2)
    
    
  })
  
  output$final_contents <- renderTable({
    # Input INPUTfile will be NULL initiailly after the user selects 
    # and uploads a file, head of that data file will be shown 
    
    req(input$file_new_2_brackets)
    
    new_data_in2 <- read.csv(input$file_new_2_brackets$datapath, 
                             header = T)
    head(new_data_in2)
    
    
  })
  
  data_internal <- reactiveValues(raw = NULL)
  
  # SUBMITTING FOR SAME THRESHOLDS
  observeEvent(input$newDataSUBMIT, {
    file1_input<- read.csv(
      file = input$file1$datapath, 
      header = T, 
      stringsAsFactors = F
    )
    
    x <- ProcessingData(file1_input = file1_input)
    file1_input <- x$file1_input
    
    data_internal$raw <- file1_input
    shinyjs::runjs("window.scrollTo(0, 100)")

  })
  # SUBMITTING FOR ONE THRESHOLD (NO AGE)
  observeEvent(input$SUMBITNEW, {
    file1_input<- read.csv(
      file = input$INPUTfile$datapath, 
      header = T, 
      stringsAsFactors = F
    )
    
    x <- ProcessingData(file1_input = file1_input)
    file1_input <- x$file1_input
    
    data_internal$raw <- file1_input
    shinyjs::runjs("window.scrollTo(0, 100)")
    
    
  })
  
  # SUBMTTING FOR 2 THRESHOLDS IN AGE DEPENDENCE 
  observeEvent(input$final_data_SUBMIT, {
    file1_input<- read.csv(
      file = input$file_new_2_brackets$datapath, 
      header = T, 
      stringsAsFactors = F
    )
    
    x <- ProcessingData(file1_input = file1_input)
    file1_input <- x$file1_input
    
    data_internal$raw <- file1_input
    shinyjs::runjs("window.scrollTo(0, 100)")
  })
  
  
  # SUBMITTING FOR 3 THRESHOLDS IN AGE DEPENDENCE
  observeEvent(input$final_data_SUBMIT_FINALL, {
    file1_input<- read.csv(
      file = input$file_new_2$datapath, 
      header = T, 
      stringsAsFactors = F
    )
    
    x <- ProcessingData(file1_input = file1_input)
    file1_input <- x$file1_input
    
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
  AB_age_dependent_2_1 <- reactiveValues(raw = NULL)
  AB_age_dependent_2_2 <- reactiveValues(raw = NULL)
  AB_age_dependent_3_1 <- reactiveValues(raw = NULL)
  AB_age_dependent_3_2 <- reactiveValues(raw = NULL)
  AB_age_dependent_3_3 <- reactiveValues(raw = NULL)
  ptau_age_dependet_2_1 <- reactiveValues(raw = NULL)
  ptau_age_dependent_2_2 <- reactiveValues(raw = NULL)
  ptau_age_dependent_3_1 <- reactiveValues(raw = NULL)
  ptau_age_dependent_3_2 <- reactiveValues(raw = NULL)
  ptau_age_dependent_3_3 <- reactiveValues(raw = NULL)
  ttau_age_dependent_2_1 <- reactiveValues(raw = NULL)
  ttau_age_dependent_2_2 <- reactiveValues(raw = NULL)
  ttau_age_dependent_3_1 <- reactiveValues(raw = NULL)
  ttau_age_dependent_3_2 <- reactiveValues(raw = NULL)
  ttau_age_dependent_3_3 <- reactiveValues(raw = NULL)
  
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
  
  # FOR AGE DEPENDENCE 2 INPUTS 
  observeEvent(input$Age_dependent_threshold_2, {
    AB_age_dependent_2_1 <- reactiveValues(raw = NULL)
    AB_age_dependent_2_2 <- reactiveValues(raw = NULL)
    ptau_age_dependet_2_1 <- reactiveValues(raw = NULL)
    ptau_age_dependent_2_2 <- reactiveValues(raw = NULL)
    ttau_age_dependent_2_1 <- reactiveValues(raw = NULL)
    ttau_age_dependent_2_2 <- reactiveValues(raw = NULL)
  })
  
  # FOR AGE DEPENDENCE 3 INPUTS 
  observeEvent(input$Age_dependent_threshold_3, {
    AB_age_dependent_3_1 <- reactiveValues(raw = NULL)
    AB_age_dependent_3_2 <- reactiveValues(raw = NULL)
    AB_age_dependent_3_3 <- reactiveValues(raw = NULL)
    ptau_age_dependent_3_1 <- reactiveValues(raw = NULL)
    ptau_age_dependent_3_2 <- reactiveValues(raw = NULL)
    ptau_age_dependent_3_3 <- reactiveValues(raw = NULL)
    ttau_age_dependent_3_1 <- reactiveValues(raw = NULL)
    ttau_age_dependent_3_2 <- reactiveValues(raw = NULL)
    ttau_age_dependent_3_3 <- reactiveValues(raw = NULL)
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
  
  ############# Read the TExt Input for ASSAY Type inputted ##########
  
  output$NewCUTOFFLabels <- renderText({
    new.dat <- req(data_internal$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippoframe <- x$hippoframe
    paste0("You have inputted new cut-off values for the CSF data. These points have been updated
           across the app. The new assay type analysed is ", input$AssayLabel, ". The cut-off thresholds
           have been updated below. To return to the supplied data or make a new selection, please refresh
           the website to clear all previous inputs.")
  })
    output$NewCUTOFFLabels2 <- renderText({
    new.dat <- req(data_internal$raw)
    paste0("You have inputted new cut-off values for the CSF data. These points have been updated
           across the app. The new assay type analysed is ", input$AssayLabel, ". The cut-off thresholds
           have been updated below. To return to the supplied data or make a new selection, please refresh
           the website to clear all previous inputs.")
  })
  

  output$ABTEXT <- renderText({
    new.dat <- req(data_internal$raw)
    AB_threshold <- req(AB_cutoff_singular$raw)
    paste0("A: is quantified by CSF AB1-42, positive participants have values less than ", AB_threshold)
  })
  
  output$PTAUTEXT <- renderText({
    new.dat <- req(data_internal$raw)
    ptau_threshold <- req(ptau_cutoff_singular$raw)
    paste0("T: is quantified by phosphor-Tau CSF p-181, positive participants have values greater than ", ptau_threshold)
  })
  
  output$TTAUTEXT <- renderText({
    new.dat <- req(data_internal$raw)
    ttau_threshold <- req(ttau_cutoff_singular$raw)
    paste0("N: is CSF t-Tau or total-Tau, positive particippants have values greater than ", ttau_threshold)
  })
  
  output$CENTILOIDTEXT <- renderText({
    new.dat <- req(data_internal$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    paste0("A: is quantified by Centiloid, positive particiapnts have values greater than ", centiloid_threshold)
  })
  
  output$PTAU2TEXT <- renderText({
    new.dat <- req(data_internal$raw)
    ptau_threshold <- req(ptau_cutoff_singular$raw)
    paste0("T: is quantified by phosphor-Tau CSF p-181, positive participants have values greater than ", ptau_threshold)
  })
  
  output$HIPPOCAMPUSTEXT <- renderText({
    new.dat <- req(data_internal$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippoframe <- x$hippoframe
    paste0("N: is quantified by hippocampus volume, positive participants have values less than ", round(hippo_cutoff,2))
  })
  
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
  
  ############################ Demographic Summary ############################
  ################################# TABLE #####################################
  
  output$table1 <- renderTable({
    new.dat <- req(data_internal$raw)
    table_2 <- new_table_function(dat=new.dat)
    group_1_table <- table_2$group_1_table
    group_1_table}, hover = T, striped = T, bordered = T, 
    width = "auto", align = "c", colnames = F, na = "")
  
  
  ################################ 2D Visualisation ###########################
  ################################ 2D RADIO GRAPH #############################
  d <- reactive({
    new.dat <- req(data_internal$raw)
    X_input <- switch(input$X_input,
                      AB = new.dat$CSF.AB42,
                      pTau = new.dat$CSF.pTau,
                      tTau = new.dat$CSF.tTau)
    
  })
  q <- reactive({
    new.dat <- req(data_internal$raw)
    Y_input <- switch(input$Y_input,
                      AB = new.dat$CSF.AB42,
                      pTau = new.dat$CSF.pTau,
                      tTau = new.dat$CSF.tTau)
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
      geom_point(data = new.dat, aes(color = Burnham_class))+
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
      xdat = new.dat$CSF.AB42, 
      ydat = new.dat$CSF.pTau, 
      zdat = new.dat$CSF.tTau, 
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
      xdat = new.dat$CSF.AB42, 
      ydat = new.dat$CSF.pTau, 
      zdat = new.dat$CSF.tTau, 
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
     xinput = df_young$CSF.AB42, 
     yinput = df_young$CSF.pTau, 
     zinput = df_young$CSF.tTau, 
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
     xinput = df_young$CSF.AB42, 
     yinput = df_young$CSF.pTau, 
     zinput = df_young$CSF.tTau, 
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
     xinput = df_old$CSF.AB42, 
     yinput = df_old$CSF.pTau, 
     zinput = df_old$CSF.tTau, 
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
     xinput = df_old$CSF.AB42, 
     yinput = df_old$CSF.pTau, 
     zinput = df_old$CSF.tTau, 
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
 
 
 
   #
   ##
   ###
   ####
   #####
   ################## CONDITIONAL PANELS FOR ADDING 1 THRESHOLD #################
   #####
   ####
   ###
   ##
   #
   
   ####################### GROUP 1 ###################
   ###################### PLANES VISUALISATION #################

   observeEvent(input$age_no_dependence_static, {
     new.dat <- req(data_internal$raw)
     AB_threshold <- req(AB_cutoff_singular$raw)
     ptau_threshold <- req(ptau_cutoff_singular$raw)
     ttau_treshold <- req(ttau_cutoff_singular$raw)
     v$plot <- PlanesFunction(
       dat = new.dat, 
       xinput = new.dat$CSF.AB42, 
       yinput = new.dat$CSF.pTau, 
       zinput = new.dat$CSF.tTau, 
       cols = new.dat$Burnham_class, 
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
 
 
 observeEvent(input$age_no_dependence_rotating, {
   new.dat <- req(data_internal$raw)
   AB_threshold <- req(AB_cutoff_singular$raw)
   ptau_threshold <- req(ptau_cutoff_singular$raw)
   ttau_treshold <- req(ttau_cutoff_singular$raw)
   v$plot <- AnimatedPlanesFunction(
     dat = new.dat, 
     xinput = new.dat$CSF.AB42, 
     yinput = new.dat$CSF.pTau, 
     zinput = new.dat$CSF.tTau, 
     cols = new.dat$Burnham_class, 
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
    cube_animate$plot <- CubeVisualisationG1(dat = df_young, 
                                             xinput = df_young$CSF.AB42,
                                             yinput = df_young$CSF.pTau, 
                                             zinput = df_young$CSF.tTau, 
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
                                             xinput = df_young$CSF.AB42,
                                             yinput = df_young$CSF.pTau, 
                                             zinput = df_young$CSF.tTau, 
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
    cube_animate$plot <- CubeVisualisationG1(dat = df_old, 
                                             xinput = df_old$CSF.AB42,
                                             yinput = df_old$CSF.pTau, 
                                             zinput = df_old$CSF.tTau, 
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
                                             xinput = df_old$CSF.AB42,
                                             yinput = df_old$CSF.pTau, 
                                             zinput = df_old$CSF.tTau, 
                                             leg = LEGEND_1,
                                             xax=axx,
                                             yax = axy,
                                             zax = axz, 
                                             XCUT = AB_threshold, 
                                             YCUT = ptau_threshold, 
                                             ZCUT = ttau_treshold)
  })
  
  #
  ##
  ###
  ####
  #####
  ################## CONDITIONAL PANELS FOR ADDING 1 THRESHOLD #################
  #####
  ####
  ###
  ##
  #
  
  ####################### GROUP 1 ###################
  ###################### CUBE VISUALISATION #################
  
  observeEvent(input$no_age_ATN, {
    new.dat <- req(data_internal$raw)
    AB_threshold <- req(AB_cutoff_singular$raw)
    ptau_threshold <- req(ptau_cutoff_singular$raw)
    ttau_treshold <- req(ttau_cutoff_singular$raw)
    cube_animate$plot <- CubeVisualisationG1(dat = new.dat, 
                                             xinput = new.dat$CSF.AB42,
                                             yinput = new.dat$CSF.pTau, 
                                             zinput = new.dat$CSF.tTau, 
                                             leg = LEGEND_1,
                                             xax=axx,
                                             yax = axy,
                                             zax = axz, 
                                             XCUT = AB_threshold, 
                                             YCUT = ptau_threshold, 
                                             ZCUT = ttau_treshold)
    
  })
  
  observeEvent(input$no_age_ATN_rotating, {
    new.dat <- req(data_internal$raw)
    AB_threshold <- req(AB_cutoff_singular$raw)
    ptau_threshold <- req(ptau_cutoff_singular$raw)
    ttau_treshold <- req(ttau_cutoff_singular$raw)
    cube_animate$plot <- AnimatedG1CubeVisualisation(dat = new.dat, 
                                             xinput = new.dat$CSF.AB42,
                                             yinput = new.dat$CSF.pTau, 
                                             zinput = new.dat$CSF.tTau, 
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

    df6 <- new.dat[,c("CSF.AB42", "CSF.pTau", "CSF.tTau", "Burnham_class")]
    New.Person <- data.frame(input$obs, input$ptau, input$ttau, "My own markers")
    names(New.Person) <- c("CSF.AB42", "CSF.pTau", "CSF.tTau", "Burnham_class")
    newDF <- rbind(df6, New.Person)
    newDF$Burnham_class <- factor(newDF$Burnham_class, levels = c("AD", "Pathological Change", "Non-AD pathological Change",
                                                      "Normal AD Biomarkers", "My own markers"))
    
    ADD_DATA <- mutate(newDF, size_guide = ifelse(Burnham_class == "My own markers",18,12))
    ADD_DATA$size_guide <- as.numeric(ADD_DATA$size_guide)

    if (input$DATAVIS =="3D Plot"){

      AddinDataG1(dat = ADD_DATA, 
                  xinput = ADD_DATA$CSF.AB42,
                  yinput = ADD_DATA$CSF.pTau, 
                  zinput = ADD_DATA$CSF.tTau, 
                  cols = ADD_DATA$Burnham_class, 
                  leg = LEGEND_1, 
                  xax = axx,
                  yax = axy,
                  zax = axz, 
                  size_guide_1 = ADD_DATA$size_guide)


    } else if(input$DATAVIS == "A+/T+/N+ Visualisation"){

      Ageinput <- input$age1

      if (Ageinput > 70){
        old_df <- filter(new.dat, Age >70)
        df6 <- old_df[,c("CSF.AB42", "CSF.pTau", "CSF.tTau", "Burnham_class")]
        New.Person <- data.frame(input$obs, input$ptau, input$ttau, "My own markers")
        names(New.Person) <- c("CSF.AB42", "CSF.pTau", "CSF.tTau", "Burnham_class")
        newDF <- rbind(df6, New.Person)
        newDF$Burnham_class <- factor(newDF$Burnham_class, levels = c("AD", "Pathological Change", "Non-AD pathological Change",
                                                                      "Normal AD Biomarkers", "My own markers"))
        ADD_DATA <- mutate(newDF, size_guide = ifelse(Burnham_class == "My own markers",18,12))
        ADD_DATA$size_guide <- as.numeric(ADD_DATA$size_guide)
        
        AB_threshold <- req(AB_cutoff$raw)
        ptau_threshold <- req(ptau_cutoff_upper$raw)
        ttau_treshold <- req(ttau_cutoff_upper$raw)
        
        ATNVisualisation(dat = ADD_DATA, xinput = ADD_DATA$CSF.AB42, yinput = ADD_DATA$CSF.pTau, 
                         zinput = ADD_DATA$CSF.tTau,
                         XCUT = AB_threshold, 
                         YCUT = ptau_threshold, 
                         ZCUT = ttau_treshold, 
                         leg = LEGEND_1, 
                         xax = axx,
                         yax = axy,
                         zax = axz, 
                         size_guide = ADD_DATA$size_guide)
        
      }else{
        df_young <- filter(new.dat, Age <70)
        df6 <- df_young[,c("CSF.AB42", "CSF.pTau", "CSF.tTau", "Burnham_class")]
        New.Person <- data.frame(input$obs, input$ptau, input$ttau, "My own markers")
        names(New.Person) <- c("CSF.AB42", "CSF.pTau", "CSF.tTau", "Burnham_class")
        newDF <- rbind(df6, New.Person)
        newDF$Burnham_class <- factor(newDF$Burnham_class, levels = c("AD", "Pathological Change", "Non-AD pathological Change",
                                                                      "Normal AD Biomarkers", "My own markers"))
        ADD_DATA <- mutate(newDF, size_guide = ifelse(Burnham_class == "My own markers",18,12))
        ADD_DATA$size_guide <- as.numeric(ADD_DATA$size_guide)
        
        AB_threshold <- req(AB_cutoff$raw)
        ptau_threshold <- req(ptau_cutoff_lower$raw)
        ttau_treshold <- req(ttau_cutoff_lower$raw)
        
        ATNVisualisation(dat = ADD_DATA, xinput = ADD_DATA$CSF.AB42, yinput = ADD_DATA$CSF.pTau, 
                         zinput = ADD_DATA$CSF.tTau,
                         XCUT = AB_threshold, 
                         YCUT = ptau_threshold, 
                         ZCUT = ttau_treshold, 
                         leg = LEGEND_1, 
                         xax = axx,
                         yax = axy,
                         zax = axz, 
                         size_guide = ADD_DATA$size_guide)

      }
    }
    
  })
  
  output$ADDINDATA_NO_AGE <- renderPlotly({
    new.dat <- req(data_internal$raw)
    AB_threshold <- req(AB_cutoff$raw)
    ptau_threshold <- req(ptau_cutoff_singular$raw)
    ttau_treshold <- req(ttau_cutoff_singular$raw)
    
    if(input$no_age_dependence == "no_age_atn_AYOB"){
      df6 <- new.dat[,c("CSF.AB42", "CSF.pTau", "CSF.tTau", "Burnham_class")]
      New.Person <- data.frame(input$AB_no_age_input, input$ptau_no_age_input, input$ttau_no_age_input, "My own markers")
      names(New.Person) <- c("CSF.AB42", "CSF.pTau", "CSF.tTau", "Burnham_class")
      newDF <- rbind(df6, New.Person)
      newDF$Burnham_class <- factor(newDF$Burnham_class, levels = c("AD", "Pathological Change", "Non-AD pathological Change",
                                                                    "Normal AD Biomarkers", "My own markers"))
      ADD_DATA <- mutate(newDF, size_guide = ifelse(Burnham_class == "My own markers",18,12))
      ADD_DATA$size_guide <- as.numeric(ADD_DATA$size_guide)
      
      
      
      ATNVisualisation(dat = ADD_DATA, xinput = ADD_DATA$CSF.AB42, yinput = ADD_DATA$CSF.pTau, 
                       zinput = ADD_DATA$CSF.tTau,
                       XCUT = AB_threshold, 
                       YCUT = ptau_threshold, 
                       ZCUT = ttau_treshold, 
                       leg = LEGEND_1, 
                       xax = axx,
                       yax = axy,
                       zax = axz, 
                       size_guide = ADD_DATA$size_guide)
    }  else if(input$no_age_dependence == "no_age_3D"){
      new.dat <- req(data_internal$raw)
      
      df6 <- new.dat[,c("CSF.AB42", "CSF.pTau", "CSF.tTau", "Burnham_class")]
      New.Person <- data.frame(input$AB_no_age_input, input$ptau_no_age_input, input$ttau_no_age_input, "My own markers")
      names(New.Person) <- c("CSF.AB42", "CSF.pTau", "CSF.tTau", "Burnham_class")
      newDF <- rbind(df6, New.Person)
      newDF$Burnham_class <- factor(newDF$Burnham_class, levels = c("AD", "Pathological Change", "Non-AD pathological Change",
                                                                    "Normal AD Biomarkers", "My own markers"))
      
      ADD_DATA <- mutate(newDF, size_guide = ifelse(Burnham_class == "My own markers",18,12))
      ADD_DATA$size_guide <- as.numeric(ADD_DATA$size_guide)
      AddinDataG1(dat = ADD_DATA, 
                  xinput = ADD_DATA$CSF.AB42,
                  yinput = ADD_DATA$CSF.pTau, 
                  zinput = ADD_DATA$CSF.tTau, 
                  cols = ADD_DATA$Burnham_class, 
                  leg = LEGEND_1, 
                  xax = axx,
                  yax = axy,
                  zax = axz, 
                  size_guide_1 = ADD_DATA$size_guide)
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
      
      BoxplotFunction(new.dat, new.dat$CSF.AB42, input_title = axx)
      
    }else if (input$BIOMARKER == "CSF pTau pg/mL"){
      BoxplotFunction(new.dat, new.dat$CSF.pTau, input_title = axy)

    }else if (input$BIOMARKER == "CSF tTau pg/mL"){
      
      BoxplotFunction(new.dat, new.dat$CSF.pTau, input_title = axz)
    }
  })

  ############################## 2D SCATTERS #################################

  output$SCATTERPLOT <- renderPlotly({
    new.dat <- req(data_internal$raw)
    if (input$BIOMARKER2 == "CSF AB1-42 pg/mL"){
      plot1 <- ggplot(new.dat, aes(x= Age, y = CSF.AB42, na.rm = TRUE, color = AB.status))+
        geom_point()+
        theme_bw()+
        xlab("Age")+
        ylab("CSF AB1-42 pg/mL") +
        ggtitle("Age vs CSF AB1-42 pg/mL")+
        scale_color_manual(values = c("forestgreen", "firebrick"))+
        geom_smooth(method = "loess", se = T)
      ggplotly(plot1)
    }else if (input$BIOMARKER2 == "CSF pTau pg/mL"){
      plot1 <- ggplot(new.dat, aes(x= Age, y = CSF.pTau, na.rm = TRUE, color = pTau.status))+
        geom_point()+
        theme_bw()+
        xlab("Age")+
        ylab("CSF pTau pg/mL") +
        ggtitle("Age vs CSF pTau pg/mL")+
        scale_color_manual(values = c("forestgreen", "firebrick"))+
        geom_smooth(method = "loess", se = T)
      ggplotly(plot1)
    }else if (input$BIOMARKER2 == "CSF tTau pg/mL"){
      plot1 <- ggplot(new.dat, aes(x= Age, y = CSF.tTau, na.rm = TRUE, color = tTau.status))+
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


  ############################ MODEL SUMMARIES ###############################

  ####################### 1. LINEAR FOR DEMOGRAPHICS #########################


  output$LinearREG <- renderUI({
    new.dat <- req(data_internal$raw)
    if (input$LINEAR1 == "CSF AB1-42 pg/mL"){
      LinearRegressionTables(dat = new.dat, biomarker = new.dat$CSF.AB42, 
                             title_biomarker = "Linear summary for CSF AB1-42 ~ Age+Sex+Education_binary+apoe4")
    }else if (input$LINEAR1 == "CSF pTau pg/mL"){
      LinearRegressionTables(dat = new.dat, biomarker = new.dat$CSF.pTau, 
                             title_biomarker = "Linear summary for CSF ptau ~ Age+Sex+Education_binary+apoe4")
    }else if (input$LINEAR1 == "CSF tTau pg/mL"){
      LinearRegressionTables(dat = new.dat, biomarker = new.dat$CSF.tTau, 
                             title_biomarker = "Linear summary for CSF tTau ~ Age+Sex+Education_binary+apoe4")
    }
  })
  ####################### 2. Logistic FOR DEMOGRAPHICS ########################

  output$Logistictable <- renderUI({
    new.dat <- req(data_internal$raw)
    new.dat <- mutate(new.dat, Binary_Abeta = ifelse(AB.status == "Positive", 1, 0))
    new.dat$Binary_Abeta <- factor(new.dat$Binary_Abeta, levels = c("1", "0"))
    new.dat <- mutate(new.dat, Binary_pTau = ifelse(pTau.status == "Positive", 1, 0))
    new.dat$Binary_pTau <- factor(new.dat$Binary_pTau, levels = c("1", "0"))
    new.dat <- mutate(new.dat, Binary_Tau = ifelse(tTau.status == "Positive", 1, 0))
    new.dat$Binary_Tau <- factor(new.dat$Binary_Tau, levels = c("1", "0"))

    if (input$LOG1 == "CSF AB1-42 pg/mL"){
      
      LogisticSummary(dat = new.dat, 
                      xinput = new.dat$Binary_Abeta, 
                      caption_title = "Logistic Summary for CSF AB1-42 ~ Age+Sex+Education_binary+apoe4")
      
      
    }else if (input$LOG1 == "CSF pTau pg/mL"){
      
      LogisticSummary(dat = new.dat, 
                      xinput = new.dat$Binary_pTau, 
                      caption_title = "Logistic Summary for CSF pTau ~ Age+Sex+Education_binary+apoe4")
      
    }else if (input$LOG1 == "CSF tTau pg/mL"){
      
      LogisticSummary(dat = new.dat, 
                      xinput = new.dat$Binary_Tau, 
                      caption_title = "Logistic Summary for CSF tTau ~ Age+Sex+Education_binary+apoe4")
    }


  })

  #################### 3. Biomarker interplay Summaries #####################

  output$BiomarkerReg <- renderUI({
    new.dat <- req(data_internal$raw)

    if (input$interplay == "CSF AB1-42 pg/mL"){
      
      BiomarkerInterplay(dat = new.dat,
                         biomarker1 = new.dat$CSF.AB42,
                         biomarker2 = new.dat$CSF.pTau, 
                         biomarker3 = new.dat$CSF.tTau, 
                         title_biomarker1 = "CSF AB1-42 pg/mL",
                         title_biomarker2 = "CSF pTau pg/mL", 
                         title_biomarker3 = "CSF tTau pg/mL", 
                         title_caption = "Linear summary for CSF AB1-42 ~ CSF pTau + CSF tTau")
      

    }else if(input$interplay == "CSF pTau pg/mL"){
      # Biomarker1 - ptau
      # Biomarker2 - AB
      # Biomarker3 - tTau
      BiomarkerInterplay(dat = new.dat,
                         biomarker1 = new.dat$CSF.pTau,
                         biomarker2 = new.dat$CSF.AB42, 
                         biomarker3 = new.dat$CSF.tTau, 
                         title_biomarker1 = "CSF ptau pg/mL",
                         title_biomarker2 = "CSF AB1-42 pg/mL", 
                         title_biomarker3 = "CSF tTau pg/mL", 
                         title_caption = "Linear summary for CSF pTau ~ CSF AB1-42 + CSF tTau")
      
    }else if(input$interplay == "CSF tTau pg/mL"){
      BiomarkerInterplay(dat = new.dat,
                         
                         biomarker1 = new.dat$CSF.pTau,
                         biomarker2 = new.dat$CSF.AB42, 
                         biomarker3 = new.dat$CSF.tTau, 
                         title_biomarker1 = "CSF ttau pg/mL",
                         title_biomarker2 = "CSF AB1-42 pg/mL", 
                         title_biomarker3 = "CSF pTau pg/mL", 
                         title_caption = "Linear summary for CSF tTau ~ CSF AB1-42 + CSF pTau")
    }

  })



  ####################### EXTRA A+/T+/N+ VISUALISATION ########################
  output$G1ExtraCube <- renderPlotly({
    new.dat <- req(data_internal$raw)
    if (input$cubeg1input == "60-70"){
      df_young <- filter(new.dat, Age < 70)
      df7 <- mutate(df_young, scat_col = ifelse(CSF.pTau >59.23    & CSF.AB42 < 656 & CSF.tTau >303.54  & Burnham_class == "AD",
                                                "AD meeting all cut-offs",
                                                ifelse(CSF.pTau >59.23    & CSF.AB42 <656 & CSF.tTau > 303.54  & Burnham_class == "Pathological Change",
                                                       "Pathological Change meeting all cut-offs",
                                                       ifelse(CSF.pTau >59.23    & CSF.AB42 < 656 & CSF.tTau > 303.54  & Burnham_class == "Non-AD pathological Change",
                                                              "Non-AD Pathological Change meeting all cut-offs",
                                                              ifelse(CSF.pTau >59.23    & CSF.AB42 < 656 & CSF.tTau > 303.54  & Burnham_class == "Normal AD Biomarkers",
                                                                     "Normal AD Biomarkers meeting all cut-offs",
                                                                     ifelse(CSF.pTau >59.23    & CSF.AB42 < 656 & Burnham_class == "AD",
                                                                            "AD meeting 2 cut offs",
                                                                            ifelse(CSF.AB42 < 656 & CSF.tTau >303.54  & Burnham_class == "AD",
                                                                                   "AD meeting 2 cut offs",
                                                                                   ifelse(CSF.pTau >59.23   & CSF.tTau >303.54  & Burnham_class == "AD",
                                                                                          "AD meeting 2 cut offs",
                                                                                          ifelse(CSF.pTau >59.23    & CSF.AB42 < 656 & Burnham_class == "Pathological Change",
                                                                                                 "Pathological Change meeting 2 cut-offs",
                                                                                                 ifelse(CSF.AB42 < 656 & CSF.tTau >303.54  & Burnham_class == "Pathological Change",
                                                                                                        "Pathological Change meeting 2 cut-offs",
                                                                                                        ifelse(CSF.pTau >59.23   & CSF.tTau >303.54  & Burnham_class == "Pathological Change",
                                                                                                               "Pathological Change meeting 2 cut-offs",
                                                                                                               ifelse(CSF.pTau >59.23    & CSF.AB42 < 656 & Burnham_class == "Non-AD pathological Change",
                                                                                                                      "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                      ifelse(CSF.AB42 < 656 & CSF.tTau >303.54  & Burnham_class == "Non-AD pathological Change",
                                                                                                                             "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                             ifelse(CSF.pTau >59.23   & CSF.tTau >303.54  & Burnham_class == "Non-AD pathological Change",
                                                                                                                                    "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                                    ifelse(CSF.pTau >59.23    & CSF.AB42 < 656 & Burnham_class == "Normal AD Biomarkers",
                                                                                                                                           "Normal AD Biomarker meeting 2 cut-offs",
                                                                                                                                           ifelse(CSF.AB42 < 656 & CSF.tTau >303.54  & Burnham_class == "Normal AD Biomarkers",
                                                                                                                                                  "Normal AD Biomarker meeting 2 cut-offs",
                                                                                                                                                  ifelse(CSF.pTau >59.23   & CSF.tTau >303.54  & Burnham_class == "Normal AD Biomarkers",
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


      df_mesh_1 <- data.frame(X_VAL = c(656,  656,  min(df7$CSF.AB42,na.rm = T),    min(df7$CSF.AB42,na.rm = T),    656,   656,   min(df7$CSF.AB42,na.rm = T),   min(df7$CSF.AB42,na.rm = T)),
                              Y_VAL = c(59.23  ,max(df7$CSF.pTau,na.rm = T),    59.23  ,  max(df7$CSF.pTau,na.rm = T),    59.23 , max(df7$CSF.pTau,na.rm = T),   59.23 , max(df7$CSF.pTau,na.rm = T)),
                              Z_VAL = c(303.54  , 303.54  , 303.54  , 303.54  , max(df7$CSF.tTau,na.rm = T),  max(df7$CSF.tTau,na.rm = T),  max(df7$CSF.tTau,na.rm = T),  max(df7$CSF.tTau,na.rm = T)),
                              MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))

      cube <- plot_ly()%>%
        add_markers(type = "scatter3d",
                    mode = "markers",
                    data = df7,
                    x = ~CSF.AB42,
                    y = ~CSF.pTau,
                    z = ~CSF.tTau,
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
      df7 <- mutate(df_old, scat_col = ifelse(CSF.pTau >73.83   & CSF.AB42 < 656 & CSF.tTau >378.65 & Burnham_class == "AD",
                                              "AD meeting all cut-offs",
                                              ifelse(CSF.pTau >73.83   & CSF.AB42 <656 & CSF.tTau > 378.65 & Burnham_class == "Pathological Change",
                                                     "Pathological Change meeting all cut-offs",
                                                     ifelse(CSF.pTau >73.83   & CSF.AB42 < 656 & CSF.tTau > 378.65 & Burnham_class == "Non-AD pathological Change",
                                                            "Non-AD Change meeting all cut-offs",
                                                            ifelse(CSF.pTau >73.83   & CSF.AB42 < 656 & CSF.tTau > 378.65 & Burnham_class == "Normal AD Biomarkers",
                                                                   "Normal AD Biomarkers meeting all cut-offs",
                                                                   ifelse(CSF.pTau >73.83   & CSF.AB42 < 656 & Burnham_class == "AD",
                                                                          "AD meeting 2 cut offs",
                                                                          ifelse(CSF.AB42 < 656 & CSF.tTau >378.65 & Burnham_class == "AD",
                                                                                 "AD meeting 2 cut offs",
                                                                                 ifelse(CSF.pTau >73.83  & CSF.tTau >378.65 & Burnham_class == "AD",
                                                                                        "AD meeting 2 cut offs",
                                                                                        ifelse(CSF.pTau >73.83   & CSF.AB42 < 656 & Burnham_class == "Pathological Change",
                                                                                               "Pathological Change meeting 2 cut-offs",
                                                                                               ifelse(CSF.AB42 < 656 & CSF.tTau >378.65 & Burnham_class == "Pathological Change",
                                                                                                      "Pathological Change meeting 2 cut-offs",
                                                                                                      ifelse(CSF.pTau >73.83  & CSF.tTau >378.65 & Burnham_class == "Pathological Change",
                                                                                                             "Pathological Change meeting 2 cut-offs",
                                                                                                             ifelse(CSF.pTau >73.83   & CSF.AB42 < 656 & Burnham_class == "Non-AD pathological Change",
                                                                                                                    "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                    ifelse(CSF.AB42 < 656 & CSF.tTau >378.65 & Burnham_class == "Non-AD pathological Change",
                                                                                                                           "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                           ifelse(CSF.pTau >73.83  & CSF.tTau >378.65 & Burnham_class == "Non-AD pathological Change",
                                                                                                                                  "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                                  ifelse(CSF.pTau >73.83   & CSF.AB42 < 656 & Burnham_class == "Normal AD Biomarkers",
                                                                                                                                         "Normal AD Biomarkers meeting 2 cut-offs",
                                                                                                                                         ifelse(CSF.AB42 < 656 & CSF.tTau >378.65 & Burnham_class == "Normal AD Biomarkers",
                                                                                                                                                "Normal AD Biomarkers meeting 2 cut-offs",
                                                                                                                                                ifelse(CSF.pTau >73.83  & CSF.tTau >378.65 & Burnham_class == "Normal AD Biomarkers",
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


      df_mesh_1 <- data.frame(X_VAL = c(656,  656,  min(df7$CSF.AB42,na.rm = T),    min(df7$CSF.AB42,na.rm = T),    656,   656,   min(df7$CSF.AB42,na.rm = T),   min(df7$CSF.AB42,na.rm = T)),
                              Y_VAL = c(73.83 ,max(df7$CSF.pTau,na.rm = T),    73.83 ,  max(df7$CSF.pTau,na.rm = T),    73.83, max(df7$CSF.pTau,na.rm = T),   73.83, max(df7$CSF.pTau,na.rm = T)),
                              Z_VAL = c(378.65 , 378.65 , 378.65 , 378.65 , max(df7$CSF.tTau,na.rm = T),  max(df7$CSF.tTau,na.rm = T),  max(df7$CSF.tTau,na.rm = T),  max(df7$CSF.tTau,na.rm = T)),
                              MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))

      # Make apoe4 a factor
      df7$apoe4 <- factor(df7$apoe4, levels = c(1,0))

      cube <- plot_ly()%>%
        add_markers(type = "scatter3d",
                    mode = "markers",
                    data = df7,
                    x = ~CSF.AB42,
                    y = ~CSF.pTau,
                    z = ~CSF.tTau,
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
  
  #
  ##
  ###
  ####
  #####
  ################## CONDITIONAL PANELS FOR ADDING 1 THRESHOLD #################
  #####
  ####
  ###
  ##
  #
  
  output$no_age_dependence_additional_cube <- renderPlotly({
    new.dat <- req(data_internal$raw)
    AB_threshold <- req(AB_cutoff$raw)
    ptau_threshold <- req(ptau_cutoff_singular$raw)
    ttau_treshold <- req(ttau_cutoff_singular$raw)
    df_young <- filter(new.dat, Age < 70)
    df7 <- mutate(new.dat, scat_col = ifelse(CSF.pTau >ptau_threshold    & CSF.AB42 < AB_threshold & CSF.tTau >ttau_treshold  & Burnham_class == "AD",
                                              "AD meeting all cut-offs",
                                              ifelse(CSF.pTau >ptau_threshold    & CSF.AB42 <AB_threshold & CSF.tTau > ttau_treshold  & Burnham_class == "Pathological Change",
                                                     "Pathological Change meeting all cut-offs",
                                                     ifelse(CSF.pTau >ptau_threshold    & CSF.AB42 < AB_threshold & CSF.tTau > ttau_treshold  & Burnham_class == "Non-AD pathological Change",
                                                            "Non-AD Pathological Change meeting all cut-offs",
                                                            ifelse(CSF.pTau >ptau_threshold    & CSF.AB42 < AB_threshold & CSF.tTau > ttau_treshold  & Burnham_class == "Normal AD Biomarkers",
                                                                   "Normal AD Biomarkers meeting all cut-offs",
                                                                   ifelse(CSF.pTau >ptau_threshold    & CSF.AB42 < AB_threshold & Burnham_class == "AD",
                                                                          "AD meeting 2 cut offs",
                                                                          ifelse(CSF.AB42 < AB_threshold & CSF.tTau >ttau_treshold  & Burnham_class == "AD",
                                                                                 "AD meeting 2 cut offs",
                                                                                 ifelse(CSF.pTau >ptau_threshold   & CSF.tTau >ttau_treshold  & Burnham_class == "AD",
                                                                                        "AD meeting 2 cut offs",
                                                                                        ifelse(CSF.pTau >ptau_threshold    & CSF.AB42 < AB_threshold & Burnham_class == "Pathological Change",
                                                                                               "Pathological Change meeting 2 cut-offs",
                                                                                               ifelse(CSF.AB42 < AB_threshold & CSF.tTau >ttau_treshold  & Burnham_class == "Pathological Change",
                                                                                                      "Pathological Change meeting 2 cut-offs",
                                                                                                      ifelse(CSF.pTau >59.23   & CSF.tTau >ttau_treshold  & Burnham_class == "Pathological Change",
                                                                                                             "Pathological Change meeting 2 cut-offs",
                                                                                                             ifelse(CSF.pTau >ptau_threshold    & CSF.AB42 < AB_threshold & Burnham_class == "Non-AD pathological Change",
                                                                                                                    "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                    ifelse(CSF.AB42 < AB_threshold & CSF.tTau >ttau_treshold  & Burnham_class == "Non-AD pathological Change",
                                                                                                                           "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                           ifelse(CSF.pTau >ptau_threshold   & CSF.tTau >ttau_treshold  & Burnham_class == "Non-AD pathological Change",
                                                                                                                                  "Non-AD Pathological Change meeting 2 cut-offs",
                                                                                                                                  ifelse(CSF.pTau >ptau_threshold    & CSF.AB42 < AB_threshold & Burnham_class == "Normal AD Biomarkers",
                                                                                                                                         "Normal AD Biomarker meeting 2 cut-offs",
                                                                                                                                         ifelse(CSF.AB42 < AB_threshold & CSF.tTau >ttau_treshold  & Burnham_class == "Normal AD Biomarkers",
                                                                                                                                                "Normal AD Biomarker meeting 2 cut-offs",
                                                                                                                                                ifelse(CSF.pTau >ptau_threshold   & CSF.tTau >ttau_treshold  & Burnham_class == "Normal AD Biomarkers",
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
    
    
    df_mesh_1 <- data.frame(X_VAL = c(AB_threshold,  AB_threshold,  min(df7$CSF.AB42,na.rm = T),    min(df7$CSF.AB42,na.rm = T),    AB_threshold,   AB_threshold,   min(df7$CSF.AB42,na.rm = T),   min(df7$CSF.AB42,na.rm = T)),
                            Y_VAL = c(ptau_threshold  ,max(df7$CSF.pTau,na.rm = T),    ptau_threshold  ,  max(df7$CSF.pTau,na.rm = T),    ptau_threshold , max(df7$CSF.pTau,na.rm = T),   ptau_threshold , max(df7$CSF.pTau,na.rm = T)),
                            Z_VAL = c(ttau_treshold  , ttau_treshold  , ttau_treshold  , ttau_treshold  , max(df7$CSF.tTau,na.rm = T),  max(df7$CSF.tTau,na.rm = T),  max(df7$CSF.tTau,na.rm = T),  max(df7$CSF.tTau,na.rm = T)),
                            MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))
    
    cube <- plot_ly()%>%
      add_markers(type = "scatter3d",
                  mode = "markers",
                  data = df7,
                  x = ~CSF.AB42,
                  y = ~CSF.pTau,
                  z = ~CSF.tTau,
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
    
    
    
    
  })
  
  #############################################################################
  ############################### HIPPO CUT OFF ###############################
  #############################################################################
  
  # Regular ggplot for HIPPOCAMPUS
  
  output$plot_gg <- renderPlot({
    new.dat <- req(data_internal$raw)
    # new.dat <- req(my_data)
    new.dat$Diagnosis <- factor(new.dat$Diagnosis, levels = c("AD", "MCI", "HC"))
    p <- ggplot(new.dat, aes(x = Sum.hippo, color = Diagnosis, fill = Diagnosis, na.rm = T))+
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
    
    ggplot(hippoframe, aes(x=x, na.rm = T))+
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
  
  ############################ Demographic Summary ############################
  ################################# TABLE #####################################
  
  output$table.2 <- renderTable({
    new.dat <- req(data_internal$raw)
    table_2 <- new_table_function(dat = new.dat)
    group_2_table <- table_2$group_2_table
    group_2_table}, hover = T, striped = T, bordered = T,
    width = "auto", align = "c", colnames = F, na = "")
  


  ################################ 2D Visualisation ###########################
  ################################ 2D RADIO GRAPH #############################

  o <- reactive({
    new.dat <- req(data_internal$raw)
    X_2_Input <- switch(input$X_2_Input,
                        Centiloid = new.dat$Centiloid,
                        ptau = new.dat$CSF.pTau,
                        Hippogroup2X1 = new.dat$Sum.hippo)

  })

  z <- reactive({
    new.dat <- req(data_internal$raw)
    Y_2_input <- switch(input$Y_2_input,
                        Centiloid = new.dat$Centiloid,
                        ptau = new.dat$CSF.pTau,
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
      geom_point(data = new.dat, aes(color = Clifford_class, na.rm = T))+
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
      ydat = new.dat$CSF.pTau, 
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
      ydat = new.dat$CSF.pTau, 
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
      yinput = new_data$CSF.pTau, 
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
      yinput = new_data_young$CSF.pTau, 
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
      yinput = data_old$CSF.pTau, 
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
      yinput = data_old$CSF.pTau, 
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
  
  #
  ##
  ###
  ####
  #####
  ################## CONDITIONAL PANELS FOR ADDING 1 THRESHOLD #################
  #####
  ####
  ###
  ##
  #
  
  observeEvent(input$no_age_G2, {
    new.dat <- req(data_internal$raw)

    ptau_threshold <- req(ptau_cutoff_singular$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippo_threshold <- hippo_cutoff
    
    group2_animation$plot <- PlanesFunction(
      dat = new.dat, 
      xinput = new.dat$Sum.hippo, 
      yinput = new.dat$CSF.pTau, 
      zinput = new.dat$Centiloid, 
      cols = new.dat$Clifford_class, 
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
  
  observeEvent(input$no_age_G2_rotating, {
    new.dat <- req(data_internal$raw)
    
    ptau_threshold <- req(ptau_cutoff_singular$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippo_threshold <- hippo_cutoff
    
    group2_animation$plot <- AnimatedPlanesFunction(
      dat = new.dat, 
      xinput = new.dat$Sum.hippo, 
      yinput = new.dat$CSF.pTau, 
      zinput = new.dat$Centiloid, 
      cols = new.dat$Clifford_class, 
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
                                             yinput = df_young$CSF.pTau, 
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
                                                yinput = df_young$CSF.pTau, 
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
                                                yinput = df_old$CSF.pTau, 
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
                                                yinput = df_old$CSF.pTau, 
                                                zinput = df_old$Centiloid, 
                                                leg = LEGEND_2,
                                                xax=axx2,
                                                yax = axy2,
                                                zax = axz2, 
                                                XCUT = hippo_threshold, 
                                                YCUT = ptau_threshold, 
                                                ZCUT = centiloid_threshold)
  })
  
  #
  ##
  ###
  ####
  #####
  ################## CONDITIONAL PANELS FOR ADDING 1 THRESHOLD #################
  #####
  ####
  ###
  ##
  #
  observeEvent(input$no_age_ATN_group_static, {
    new.dat <- req(data_internal$raw)
    ptau_threshold <- req(ptau_cutoff_singular$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippo_threshold <- hippo_cutoff
    cube_g2_animate$plot <- G2CubeVisualisation(dat = new.dat, 
                                                xinput = new.dat$Sum.hippo,
                                                yinput = new.dat$CSF.pTau, 
                                                zinput = new.dat$Centiloid, 
                                                leg = LEGEND_2,
                                                xax=axx2,
                                                yax = axy2,
                                                zax = axz2, 
                                                XCUT = hippo_threshold, 
                                                YCUT = ptau_threshold, 
                                                ZCUT = centiloid_threshold)
  })
  
  observeEvent(input$no_age_ATN_group_rotating, {
    new.dat <- req(data_internal$raw)
    ptau_threshold <- req(ptau_cutoff_singular$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippo_threshold <- hippo_cutoff
    cube_g2_animate$plot <- AnimatedG2CubeVisualisation(dat = new.dat, 
                                                xinput = new.dat$Sum.hippo,
                                                yinput = new.dat$CSF.pTau, 
                                                zinput = new.dat$Centiloid, 
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
    df8 <- new.dat[,c("Sum.hippo", "CSF.pTau", "Centiloid", "Clifford_class")]
    df9 <- na.omit(df8)
    New.Group2 <- data.frame(input$sums, input$ps, input$cents, "My own markers")
    names(New.Group2) <- c("Sum.hippo", "CSF.pTau", "Centiloid", "Clifford_class")
    newdf3 <- rbind(df9, New.Group2)
    newdf3$Clifford_class <- factor(newdf3$Clifford_class, levels = c("Stage 2, clinically asymptomatic",
                                                                      "Stage 1, preclinical AD stage",
                                                                      "SNAP",
                                                                      "MCI unlikely due to AD",
                                                                      "My own markers"))
    newdf4 <- mutate(newdf3, size_guide2 = ifelse(Clifford_class == "My own markers",18,12))
    newdf4$size_guide2 <- as.numeric(newdf4$size_guide2)
    if (input$DATAVIS2 == "3D Plot"){
      
      AddinDataG1(dat = newdf4, 
                  xinput = newdf4$Sum.hippo,
                  yinput = newdf4$CSF.pTau, 
                  zinput = newdf4$Centiloid, 
                  cols = newdf4$Clifford_class, 
                  leg = LEGEND_2, 
                  xax = axx2,
                  yax = axy2,
                  zax = axz2, 
                  size_guide_1 = newdf4$size_guide2)

      
    }else if (input$DATAVIS2 == "A+/T+/N+ Visualisation"){

      AgeInput2 <- input$age

      if (AgeInput2 > 70){
        df_old <- filter(new.dat, Age > 70)
        df8 <- df_old[,c("Sum.hippo", "CSF.pTau", "Centiloid", "Clifford_class")]
        df9 <- na.omit(df8)
        New.Group2 <- data.frame(input$sums, input$ps, input$cents, "My own markers")
        names(New.Group2) <- c("Sum.hippo", "CSF.pTau", "Centiloid", "Clifford_class")
        newdf3 <- rbind(df9, New.Group2)
        newdf3$Clifford_class <- factor(newdf3$Clifford_class, levels = c("Stage 2, clinically asymptomatic",
                                                                          "Stage 1, preclinical AD stage",
                                                                          "SNAP",
                                                                          "MCI unlikely due to AD",
                                                                          "My own markers"))
        newdf4 <- mutate(newdf3, size_guide2 = ifelse(Clifford_class == "My own markers",18,12))
        newdf4$size_guide2 <- as.numeric(newdf4$size_guide2)
        
        ptau_threshold <- req(ptau_cutoff_upper$raw)
        centiloid_threshold <- req(centiloid_cutoff$raw)
        x <- HippoFunction(dat = new.dat)
        hippo_cutoff <- x$hippocampus_threshold
        hippo_threshold <- hippo_cutoff
        
        G2ATNVisualisation(dat = newdf4, xinput = newdf4$Sum.hippo, yinput = newdf4$CSF.pTau, 
                         zinput = newdf4$Centiloid,
                         XCUT = hippo_threshold, 
                         YCUT = ptau_threshold, 
                         ZCUT = centiloid_threshold, 
                         leg = LEGEND_2, 
                         xax = axx2,
                         yax = axy2,
                         zax = axz2, 
                         size_guide = newdf4$size_guide)
        
        
      }else {
        df_young <- filter(new.dat, Age <70)
        df8 <- df_young[,c("Sum.hippo", "CSF.pTau", "Centiloid", "Clifford_class")]
        df9 <- na.omit(df8)
        New.Group2 <- data.frame(input$sums, input$ps, input$cents, "My own markers")
        names(New.Group2) <- c("Sum.hippo", "CSF.pTau", "Centiloid", "Clifford_class")
        newdf3 <- rbind(df9, New.Group2)
        newdf3$Clifford_class <- factor(newdf3$Clifford_class, levels = c("Stage 2, clinically asymptomatic",
                                                                          "Stage 1, preclinical AD stage",
                                                                          "SNAP",
                                                                          "MCI unlikely due to AD",
                                                                          "My own markers"))
        newdf4 <- mutate(newdf3, size_guide2 = ifelse(Clifford_class == "My own markers",18,12))
        newdf4$size_guide2 <- as.numeric(newdf4$size_guide2)
        
        ptau_threshold <- req(ptau_cutoff_lower$raw)
        centiloid_threshold <- req(centiloid_cutoff$raw)
        x <- HippoFunction(dat = new.dat)
        hippo_cutoff <- x$hippocampus_threshold
        hippo_threshold <- hippo_cutoff
        
        G2ATNVisualisation(dat = newdf4, xinput = newdf4$Sum.hippo, yinput = newdf4$CSF.pTau, 
                           zinput = newdf4$Centiloid,
                           XCUT = hippo_threshold, 
                           YCUT = ptau_threshold, 
                           ZCUT = centiloid_threshold, 
                           leg = LEGEND_2, 
                           xax = axx2,
                           yax = axy2,
                           zax = axz2, 
                           size_guide = newdf4$size_guide)

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

      BoxplotFunction(new.dat, new.dat$CSF.pTau, input_title = axy)

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
      plot1 <- ggplot(new.dat, aes(x= Age, y = CSF.pTau, na.rm = TRUE, color = pTau.status))+
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
        x = ~CSF.pTau,
        y = ~ Sum.hippo,
        z = ~Centiloid,
        text = ~Diagnosis,
        type = "scatter3d",
        color = ~Clifford_class,
        colors = c("firebrick", "darkorange", "gold", "forestgreen"),
        mode = "markers"
      )


      my_lm <- lm(Centiloid ~ Sum.hippo+CSF.pTau, new_data_frame)
      # Set up Axis

      axis_x1 <- seq(min(new.dat$CSF.pTau), max(new.dat$CSF.pTau))
      axis_z1 <- seq(min(new.dat$Sum.hippo, na.rm = T), max(new.dat$Sum.hippo, na.rm = T))

      ## PLOT THE POINTS

      my_lm_surface <- expand.grid(CSF.pTau = axis_x1,Sum.hippo = axis_z1,KEEP.OUT.ATTRS = F)
      my_lm_surface$Centiloid <- predict.lm(my_lm, newdata = my_lm_surface)
      my_lm_surface <- acast(my_lm_surface, Sum.hippo ~ CSF.pTau, value.var = "Centiloid")

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

      new_lm <- lm(Centiloid ~ Sum.hippo+CSF.pTau, non_carriers)
      # Set up Axis

      axis_x1 <- seq(min(new.dat$CSF.pTau), max(new.dat$CSF.pTau))
      axis_z1 <- seq(min(new.dat$Sum.hippo, na.rm = T), max(new.dat$Sum.hippo, na.rm = T))

      ## PLOT THE POINTS

      new_lm_surface <- expand.grid(CSF.pTau = axis_x1,Sum.hippo = axis_z1,KEEP.OUT.ATTRS = F)
      new_lm_surface$Centiloid <- predict.lm(new_lm, newdata = new_lm_surface)
      new_lm_surface <- acast(new_lm_surface, Sum.hippo ~ CSF.pTau, value.var = "Centiloid")

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
        x = ~CSF.pTau,
        y = ~ Sum.hippo,
        z = ~Centiloid,
        text = ~Diagnosis,
        type = "scatter3d",
        color = ~Clifford_class,
        colors = c("firebrick", "darkorange", "gold", "forestgreen"),
        mode = "markers")

      sex_lm_f <- lm(Centiloid ~ Sum.hippo+CSF.pTau, sex)
      # Set up Axis

      x1 <- seq(min(new.dat$CSF.pTau), max(new.dat$CSF.pTau))
      z1 <- seq(min(new.dat$Sum.hippo, na.rm = T), max(new.dat$Sum.hippo, na.rm = T))

      ## PLOT THE POINTS

      sex_surface_1 <- expand.grid(CSF.pTau = x1,Sum.hippo = z1,KEEP.OUT.ATTRS = F)
      sex_surface_1$Centiloid <- predict.lm(sex_lm_f, newdata = sex_surface_1)
      sex_surface_1 <- acast(sex_surface_1, Sum.hippo ~ CSF.pTau, value.var = "Centiloid")

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

      lm_for_males <- lm(Centiloid ~ Sum.hippo+CSF.pTau, sex_2)
      # Set up Axis

      ## PLOT THE POINTS

      surface_males <- expand.grid(CSF.pTau = x1,Sum.hippo = z1,KEEP.OUT.ATTRS = F)
      surface_males$Centiloid <- predict.lm(lm_for_males, newdata = surface_males)
      surface_males <- acast(surface_males, Sum.hippo ~ CSF.pTau, value.var = "Centiloid")

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
        x = ~CSF.pTau,
        y = ~ Sum.hippo,
        z = ~Centiloid,
        text = ~Diagnosis,
        type = "scatter3d",
        color = ~Clifford_class,
        colors = c("firebrick", "darkorange", "gold", "forestgreen"),
        mode = "markers"
      )

      age_1_lm <- lm(Centiloid ~ Sum.hippo+CSF.pTau, age)
      # Set up Axis

      x1 <- seq(min(new.dat$CSF.pTau), max(new.dat$CSF.pTau))
      z1 <- seq(min(new.dat$Sum.hippo, na.rm = T), max(new.dat$Sum.hippo, na.rm = T))

      ## PLOT THE POINTS

      age_surface_1 <- expand.grid(CSF.pTau = x1,Sum.hippo = z1,KEEP.OUT.ATTRS = F)
      age_surface_1$Centiloid <- predict.lm(age_1_lm, newdata = age_surface_1)
      age_surface_1 <- acast(age_surface_1, Sum.hippo ~ CSF.pTau, value.var = "Centiloid")

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

      age_0_lm <- lm(Centiloid ~ Sum.hippo+CSF.pTau, age_2)
      # Set up Axis

      ## PLOT THE POINTS

      age_surface_0 <- expand.grid(CSF.pTau = x1,Sum.hippo = z1,KEEP.OUT.ATTRS = F)
      age_surface_0$Centiloid <- predict.lm(age_0_lm, newdata = age_surface_0)
      age_surface_0 <- acast(age_surface_0, Sum.hippo ~ CSF.pTau, value.var = "Centiloid")

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
        x = ~CSF.pTau,
        y = ~ Sum.hippo,
        z = ~Centiloid,
        text = ~Diagnosis,
        type = "scatter3d",
        color = ~Clifford_class,
        colors = c("firebrick", "darkorange", "gold", "forestgreen"),
        mode = "markers"
      )


      education_1_lm <- lm(Centiloid ~ Sum.hippo+CSF.pTau, education)
      # Set up Axis

      x1 <- seq(min(new.dat$CSF.pTau), max(new.dat$CSF.pTau))
      z1 <- seq(min(new.dat$Sum.hippo, na.rm = T), max(new.dat$Sum.hippo, na.rm = T))

      ## PLOT THE POINTS

      education_surface_1 <- expand.grid(CSF.pTau = x1,Sum.hippo = z1,KEEP.OUT.ATTRS = F)
      education_surface_1$Centiloid <- predict.lm(education_1_lm, newdata = education_surface_1)
      education_surface_1 <- acast(education_surface_1, Sum.hippo ~ CSF.pTau, value.var = "Centiloid")

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

      lm_0 <- lm(Centiloid ~ Sum.hippo+CSF.pTau, education_2)
      # Set up Axis

      ## PLOT THE POINTS

      surface_education_0 <- expand.grid(CSF.pTau = x1,Sum.hippo = z1,KEEP.OUT.ATTRS = F)
      surface_education_0$Centiloid <- predict.lm(lm_0, newdata = surface_education_0)
      surface_education_0 <- acast(surface_education_0, Sum.hippo ~ CSF.pTau, value.var = "Centiloid")

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
      
      LinearRegressionTables(dat = new.dat, biomarker = new.dat$Centiloid, 
                             title_biomarker = "Linear summary for Centiloid ~ Age+Sex+Education_binary+apoe4")
      
    }else if (input$LING2 == "CSF pTau pg/mL"){
      
      LinearRegressionTables(dat = new.dat, biomarker = new.dat$CSF.pTau, 
                             title_biomarker = "Linear summary for CSF pTau ~ Age+Sex+Education_binary+apoe4")
      
    }else if (input$LING2 == "LINHIP1"){
      
      LinearRegressionTables(dat = new.dat, biomarker = new.dat$Sum.hippo, 
                             title_biomarker = "Linear summary for Hippocampus ~ Age+Sex+Education_binary+apoe4")
    }
  })
  ########################2. . Biomarker Interplay #########################

  output$INTERPLAY2 <- renderUI({
    new.dat <- req(data_internal$raw)
    if (input$INTERPLAYG2 == "Centiloid"){
      # Biomarker1 = centiloid
      # 2 = ptau
      # 3 - Sumhippo
      BiomarkerInterplay(dat = new.dat,
                         biomarker1 = new.dat$Centiloid,
                         biomarker2 = new.dat$CSF.pTau, 
                         biomarker3 = new.dat$Sum.hippo, 
                         title_biomarker1 = "Centiloid",
                         title_biomarker2 = "CSF ptau pg/mL", 
                         title_biomarker3 = "Hippocampus", 
                         title_caption = "Linear summary for Centiloid ~ CSF pTau + Hippocampus")
      
      
    }else if (input$INTERPLAYG2 == "CSF pTau pg/mL"){
      # 1 - ptau
      # 2 - centiloid
      # 3 - hippocampus
      BiomarkerInterplay(dat = new.dat,
                         biomarker1 = new.dat$CSF.pTau,
                         biomarker2 = new.dat$Centiloid, 
                         biomarker3 = new.dat$Sum.hippo, 
                         title_biomarker1 = "CSF ptau pg/mL",
                         title_biomarker2 = "Centiloid", 
                         title_biomarker3 = "Hippocampus", 
                         title_caption = "Linear summary for CSF pTau ~ Centiloid + Hippocampus") 
      
      
    }else if (input$INTERPLAYG2 == "MODELHIP1"){
      # 1 - sum.hippo 
      # 2 = centiloid 
      # 3 - ptau
      
      BiomarkerInterplay(dat = new.dat,
                         biomarker1 = new.dat$Sum.hippo,
                         biomarker2 = new.dat$Centiloid, 
                         biomarker3 = new.dat$CSF.pTau, 
                         title_biomarker1 = "Hippocampus",
                         title_biomarker2 = "Centiloid", 
                         title_biomarker3 = "CSF ptau pg/mL", 
                         title_caption = "Linear summary for Hippocampus ~ Centiloid + CSF pTau") 
    }
  })



  ########################## EXTRA A+/T+/N+ VIZ  ###########################

  output$EXTRACUBE <- renderPlotly({
    new.dat <- req(data_internal$raw)

    if (input$PopCube == "70+"){

      df_old <- filter(new.dat, Age > 70)
      df7 <- mutate(df_old, scat_col = ifelse(CSF.pTau >73.83   & Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                              "Stage 2 Clinically Asymptomatic meeting all cut-offs",
                                              ifelse(CSF.pTau >73.83   & Sum.hippo <5.402554 & Centiloid > 20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                     "Stage 1 Preclinical AD meeting all cut-offs",
                                                     ifelse(CSF.pTau >73.83   & Sum.hippo < 5.402554 & Centiloid > 20 & Clifford_class == "SNAP",
                                                            "SNAP meeting all cut-offs",
                                                            ifelse(CSF.pTau >73.83   & Sum.hippo < 5.402554 & Centiloid > 20 & Clifford_class == "MCI unlikely due to AD",
                                                                   "MCI unlikely due to AD meeting all cut-offs",
                                                                   ifelse(CSF.pTau >73.83   & Sum.hippo < 5.402554 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                          "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                          ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                                 "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                                 ifelse(CSF.pTau >73.83  & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                                        "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                                        ifelse(CSF.pTau >73.83   & Sum.hippo < 5.402554 & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                               "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                               ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                                      "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                                      ifelse(CSF.pTau >73.83  & Centiloid >20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                                             "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                                             ifelse(CSF.pTau >73.83   & Sum.hippo < 5.402554 & Clifford_class == "SNAP",
                                                                                                                    "SNAP meeting 2 cut-offs",
                                                                                                                    ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "SNAP",
                                                                                                                           "SNAP meeting 2 cut-offs",
                                                                                                                           ifelse(CSF.pTau >73.83  & Centiloid >20 & Clifford_class == "SNAP",
                                                                                                                                  "SNAP meeting 2 cut-offs",
                                                                                                                                  ifelse(CSF.pTau >73.83   & Sum.hippo < 5.402554 & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                         "MCI unlikely due to AD meeting 2 cut-offs",
                                                                                                                                         ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                                "MCI unlikely due to AD meeting 2 cut-offs",
                                                                                                                                                ifelse(CSF.pTau >73.83  & Centiloid >20 & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                                       "MCI unlikely due to AD meeting 2 cut-offs","unselected")))))))))))))))))



      df7$scat_col <- factor(df7$scat_col, levels = c("unselected", "Stage 2 Clinically Asymptomatic meeting all cut-offs",
                                                      "Stage 1 Preclinical AD meeting all cut-offs", "SNAP meeting all cut-offs",
                                                      "MCI unlikely due to AD meeting all cut-offs",
                                                      #"Patients who are within 10% of any cut-off",
                                                      "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                      "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                      "SNAP meeting 2 cut-offs",
                                                      "MCI unlikely due to AD meeting 2 cut-offs"))


      df_mesh_1 <- data.frame(X_VAL = c(max(df7$CSF.pTau,na.rm = T), max(df7$CSF.pTau,na.rm = T), 73.83 ,73.83 ,   max(df7$CSF.pTau,na.rm = T),  max(df7$CSF.pTau,na.rm = T), 73.83 ,73.83 ),
                              Y_VAL = c(min(df7$Sum.hippo, na.rm = T),  5.402554, min(df7$Sum.hippo, na.rm = T), 5.402554, min(df7$Sum.hippo, na.rm = T),   5.402554, min(df7$Sum.hippo, na.rm = T), 5.402554),
                              Z_VAL = c(20,      20,    20,      20,  max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T)),
                              MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))

      # Make apoe4 a factor
      df7$apoe4 <- factor(df7$apoe4, levels = c(1,0))

      cube2 <- plot_ly()%>%
        add_markers(type = "scatter3d",
                    mode = "markers",
                    data = df7,
                    x = ~CSF.pTau,
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
      df7 <- mutate(df_young, scat_col = ifelse(CSF.pTau >59.23  & Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                "Stage 2 Clinically Asymptomatic meeting all cut-offs",
                                                ifelse(CSF.pTau >59.23  & Sum.hippo <5.402554 & Centiloid > 20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                       "Stage 1 Preclinical AD meeting all cut-offs",
                                                       ifelse(CSF.pTau >59.23  & Sum.hippo < 5.402554 & Centiloid > 20 & Clifford_class == "SNAP",
                                                              "SNAP meeting all cut-offs",
                                                              ifelse(CSF.pTau >59.23  & Sum.hippo < 5.402554 & Centiloid > 20 & Clifford_class == "MCI unlikely due to AD",
                                                                     "MCI unlikely due to AD meeting all cut-offs",
                                                                     ifelse(CSF.pTau >59.23  & Sum.hippo < 5.402554 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                            "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                            ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                                   "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                                   ifelse(CSF.pTau >59.23 & Centiloid >20 & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                                          "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                                          ifelse(CSF.pTau >59.23  & Sum.hippo < 5.402554 & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                                 "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                                 ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                                        "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                                        ifelse(CSF.pTau >59.23 & Centiloid >20 & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                                               "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                                               ifelse(CSF.pTau >59.23  & Sum.hippo < 5.402554 & Clifford_class == "SNAP",
                                                                                                                      "SNAP meeting 2 cut-offs",
                                                                                                                      ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "SNAP",
                                                                                                                             "SNAP meeting 2 cut-offs",
                                                                                                                             ifelse(CSF.pTau >59.23 & Centiloid >20 & Clifford_class == "SNAP",
                                                                                                                                    "SNAP meeting 2 cut-offs",
                                                                                                                                    ifelse(CSF.pTau >59.23  & Sum.hippo < 5.402554 & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                           "MCI unlikely due to AD meeting 2 cut-offs",
                                                                                                                                           ifelse(Sum.hippo < 5.402554 & Centiloid >20 & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                                  "MCI unlikely due to AD meeting 2 cut-offs",
                                                                                                                                                  ifelse(CSF.pTau >59.23 & Centiloid >20 & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                                         "MCI unlikely due to AD meeting 2 cut-offs","unselected")))))))))))))))))



      df7$scat_col <- factor(df7$scat_col, levels = c("unselected", "Stage 2 Clinically Asymptomatic meeting all cut-offs",
                                                      "Stage 1 Preclinical AD meeting all cut-offs", "SNAP meeting all cut-offs",
                                                      "MCI unlikely due to AD meeting all cut-offs",
                                                      #"Patients who are within 10% of any cut-off",
                                                      "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                      "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                      "SNAP meeting 2 cut-offs",
                                                      "MCI unlikely due to AD meeting 2 cut-offs"))


      df_mesh_1 <- data.frame(X_VAL = c(max(df7$CSF.pTau,na.rm = T), max(df7$CSF.pTau,na.rm = T), 59.23,59.23,   max(df7$CSF.pTau,na.rm = T),  max(df7$CSF.pTau,na.rm = T), 59.23,59.23),
                              Y_VAL = c(min(df7$Sum.hippo, na.rm = T),  5.402554, min(df7$Sum.hippo, na.rm = T), 5.402554, min(df7$Sum.hippo, na.rm = T),   5.402554, min(df7$Sum.hippo, na.rm = T), 5.402554),
                              Z_VAL = c(20,      20,    20,      20,  max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T)),
                              MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))

      # Make apoe4 a factor
      df7$apoe4 <- factor(df7$apoe4, levels = c(1,0))

      cube2 <- plot_ly()%>%
        add_markers(type = "scatter3d",
                    mode = "markers",
                    data = df7,
                    x = ~CSF.pTau,
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
  
  output$NO_AGE_CUBE_NEW_G2 <- renderPlotly({
    new.dat <- req(data_internal$raw)
    ptau_threshold <- req(ptau_cutoff_singular$raw)
    centiloid_threshold <- req(centiloid_cutoff$raw)
    x <- HippoFunction(dat = new.dat)
    hippo_cutoff <- x$hippocampus_threshold
    hippo_threshold <- hippo_cutoff
    
    df7 <- mutate(new.dat, scat_col = ifelse(CSF.pTau >ptau_threshold  & Sum.hippo < hippo_threshold & Centiloid >centiloid_threshold & Clifford_class == "Stage 2, clinically asymptomatic",
                                              "Stage 2 Clinically Asymptomatic meeting all cut-offs",
                                              ifelse(CSF.pTau >ptau_threshold  & Sum.hippo <hippo_threshold & Centiloid > centiloid_threshold & Clifford_class == "Stage 1, preclinical AD stage",
                                                     "Stage 1 Preclinical AD meeting all cut-offs",
                                                     ifelse(CSF.pTau >ptau_threshold  & Sum.hippo < hippo_threshold & Centiloid > centiloid_threshold & Clifford_class == "SNAP",
                                                            "SNAP meeting all cut-offs",
                                                            ifelse(CSF.pTau >ptau_threshold  & Sum.hippo < hippo_threshold & Centiloid > centiloid_threshold & Clifford_class == "MCI unlikely due to AD",
                                                                   "MCI unlikely due to AD meeting all cut-offs",
                                                                   ifelse(CSF.pTau >ptau_threshold  & Sum.hippo < hippo_threshold & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                          "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                          ifelse(Sum.hippo < hippo_threshold & Centiloid >centiloid_threshold & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                                 "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                                 ifelse(CSF.pTau >ptau_threshold & Centiloid >centiloid_threshold & Clifford_class == "Stage 2, clinically asymptomatic",
                                                                                        "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                                                        ifelse(CSF.pTau >ptau_threshold  & Sum.hippo < hippo_threshold & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                               "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                               ifelse(Sum.hippo < hippo_threshold & Centiloid >centiloid_threshold & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                                      "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                                      ifelse(CSF.pTau >ptau_threshold & Centiloid >centiloid_threshold & Clifford_class == "Stage 1, preclinical AD stage",
                                                                                                             "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                                                                             ifelse(CSF.pTau >ptau_threshold  & Sum.hippo < hippo_threshold & Clifford_class == "SNAP",
                                                                                                                    "SNAP meeting 2 cut-offs",
                                                                                                                    ifelse(Sum.hippo < hippo_threshold & Centiloid >centiloid_threshold & Clifford_class == "SNAP",
                                                                                                                           "SNAP meeting 2 cut-offs",
                                                                                                                           ifelse(CSF.pTau >ptau_threshold & Centiloid >centiloid_threshold & Clifford_class == "SNAP",
                                                                                                                                  "SNAP meeting 2 cut-offs",
                                                                                                                                  ifelse(CSF.pTau >ptau_threshold  & Sum.hippo < hippo_threshold & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                         "MCI unlikely due to AD meeting 2 cut-offs",
                                                                                                                                         ifelse(Sum.hippo < hippo_threshold & Centiloid >centiloid_threshold & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                                "MCI unlikely due to AD meeting 2 cut-offs",
                                                                                                                                                ifelse(CSF.pTau >ptau_threshold & Centiloid >centiloid_threshold & Clifford_class == "MCI unlikely due to AD",
                                                                                                                                                       "MCI unlikely due to AD meeting 2 cut-offs","unselected")))))))))))))))))
    
    
    
    df7$scat_col <- factor(df7$scat_col, levels = c("unselected", "Stage 2 Clinically Asymptomatic meeting all cut-offs",
                                                    "Stage 1 Preclinical AD meeting all cut-offs", "SNAP meeting all cut-offs",
                                                    "MCI unlikely due to AD meeting all cut-offs",
                                                    #"Patients who are within 10% of any cut-off",
                                                    "Stage 2 Clinically Asymptomatic meeting 2 cut-offs",
                                                    "Stage 1 Preclinical AD meeting 2 cut-offs",
                                                    "SNAP meeting 2 cut-offs",
                                                    "MCI unlikely due to AD meeting 2 cut-offs"))
    
    
    df_mesh_1 <- data.frame(X_VAL = c(max(df7$CSF.pTau,na.rm = T), max(df7$CSF.pTau,na.rm = T), ptau_threshold,ptau_threshold,   max(df7$CSF.pTau,na.rm = T),  max(df7$CSF.pTau,na.rm = T), ptau_threshold,ptau_threshold),
                            Y_VAL = c(min(df7$Sum.hippo, na.rm = T),  hippo_threshold, min(df7$Sum.hippo, na.rm = T), hippo_threshold, min(df7$Sum.hippo, na.rm = T),   hippo_threshold, min(df7$Sum.hippo, na.rm = T), hippo_threshold),
                            Z_VAL = c(centiloid_threshold,      centiloid_threshold,    centiloid_threshold,      centiloid_threshold,  max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T),max(df7$Centiloid, na.rm = T)),
                            MESH_COL = factor(rep("CUBE", 8), levels = c("CUBE")))
    
    # Make apoe4 a factor
    df7$apoe4 <- factor(df7$apoe4, levels = c(1,0))
    
    cube2 <- plot_ly()%>%
      add_markers(type = "scatter3d",
                  mode = "markers",
                  data = df7,
                  x = ~CSF.pTau,
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
    
    
    
  })
  
  
  ######################## 3D PREDICTIVE SURFACES ############################
  
  output$SURFACEPLOT <- renderPlotly({
    new.dat <- req(data_internal$raw)
    
    if (input$DEMOGRAPHIC == "APOE-e4 allele"){
      new_data_frame <- filter(new.dat, apoe4 == 1)
      
      my_lm <- lm(CSF.tTau ~ CSF.AB42+CSF.pTau, new_data_frame)
      # Set up Axis
      
      axis_x1 <- seq(min(new.dat$CSF.AB42), max(new.dat$CSF.AB42))
      axis_z1 <- seq(min(new.dat$CSF.pTau), max(new.dat$CSF.pTau))
      
      ## PLOT THE POINTS
      
      my_lm_surface <- expand.grid(CSF.AB42 = axis_x1,CSF.pTau = axis_z1,KEEP.OUT.ATTRS = F)
      my_lm_surface$CSF.tTau <- predict.lm(my_lm, newdata = my_lm_surface)
      my_lm_surface <- acast(my_lm_surface, CSF.pTau ~ CSF.AB42, value.var = "CSF.tTau")
      
      my_plot <- plot_ly()
      my_plot <- add_markers(
        p = my_plot,
        data = new.dat,
        x = ~CSF.AB42,
        y = ~ CSF.pTau,
        z = ~CSF.tTau,
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
      
      my_lm_2 <- lm(CSF.tTau ~ CSF.AB42+CSF.pTau, new_data_frame_2)
      # Set up Axis
      
      axis_x2 <- seq(min(new.dat$CSF.AB42), max(new.dat$CSF.AB42))
      axis_z2 <- seq(min(new.dat$CSF.pTau), max(new.dat$CSF.pTau))
      
      ## PLOT THE POINTS
      
      my_lm_surface_2 <- expand.grid(CSF.AB42 = axis_x2,CSF.pTau = axis_z2,KEEP.OUT.ATTRS = F)
      my_lm_surface_2$CSF.tTau <- predict.lm(my_lm_2, newdata = my_lm_surface_2)
      my_lm_surface_2 <- acast(my_lm_surface_2, CSF.pTau ~ CSF.AB42, value.var = "CSF.tTau")
      
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
      
      my_lm_female <- lm(CSF.tTau ~ CSF.AB42+CSF.pTau, female_data)
      # Set up Axis
      
      axis_x1_female <- seq(min(new.dat$CSF.AB42), max(new.dat$CSF.AB42))
      axis_z1_female <- seq(min(new.dat$CSF.pTau), max(new.dat$CSF.pTau))
      
      ## PLOT THE POINTS
      
      my_lm_surface_female <- expand.grid(CSF.AB42 = axis_x1_female,CSF.pTau = axis_z1_female,KEEP.OUT.ATTRS = F)
      my_lm_surface_female$CSF.tTau <- predict.lm(my_lm_female, newdata = my_lm_surface_female)
      my_lm_surface_female <- acast(my_lm_surface_female, CSF.pTau ~ CSF.AB42, value.var = "CSF.tTau")
      
      my_plot <- plot_ly()
      my_plot <- add_markers(
        p = my_plot,
        data = new.dat,
        x = ~CSF.AB42,
        y = ~ CSF.pTau,
        z = ~CSF.tTau,
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
      
      my_lm_male <- lm(CSF.tTau ~ CSF.AB42+CSF.pTau, male_data)
      # Set up Axis
      
      axis_x2_male <- seq(min(new.dat$CSF.AB42), max(new.dat$CSF.AB42))
      axis_z2_male <- seq(min(new.dat$CSF.pTau), max(new.dat$CSF.pTau))
      
      ## PLOT THE POINTS
      
      my_lm_surface_male <- expand.grid(CSF.AB42 = axis_x2_male,CSF.pTau = axis_z2_male,KEEP.OUT.ATTRS = F)
      my_lm_surface_male$CSF.tTau <- predict.lm(my_lm_male, newdata = my_lm_surface_male)
      my_lm_surface_male <- acast(my_lm_surface_male, CSF.pTau ~ CSF.AB42, value.var = "CSF.tTau")
      
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
      
      age_lm_1 <- lm(CSF.tTau ~ CSF.AB42+CSF.pTau, Age_binary)
      # Set up Axis
      
      x1 <- seq(min(new.dat$CSF.AB42), max(new.dat$CSF.AB42))
      z1 <- seq(min(new.dat$CSF.pTau), max(new.dat$CSF.pTau))
      
      ## PLOT THE POINTS
      
      age_surface_1 <- expand.grid(CSF.AB42 = x1,CSF.pTau = z1,KEEP.OUT.ATTRS = F)
      age_surface_1$CSF.tTau <- predict.lm(age_lm_1, newdata = age_surface_1)
      age_surface_1 <- acast(age_surface_1, CSF.pTau ~ CSF.AB42, value.var = "CSF.tTau")
      
      my_plot <- plot_ly()
      my_plot <- add_markers(
        p = my_plot,
        data = new.dat,
        x = ~CSF.AB42,
        y = ~ CSF.pTau,
        z = ~CSF.tTau,
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
      
      age_lm_2 <- lm(CSF.tTau ~ CSF.AB42+CSF.pTau, age_0)
      # Set up Axis
      
      x2 <- seq(min(new.dat$CSF.AB42), max(new.dat$CSF.AB42))
      z2 <- seq(min(new.dat$CSF.pTau), max(new.dat$CSF.pTau))
      
      ## PLOT THE POINTS
      
      age_surface_2 <- expand.grid(CSF.AB42 = x2,CSF.pTau = z2,KEEP.OUT.ATTRS = F)
      age_surface_2$CSF.tTau <- predict.lm(age_lm_2, newdata = age_surface_2)
      age_surface_2 <- acast(age_surface_2, CSF.pTau ~ CSF.AB42, value.var = "CSF.tTau")
      
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
      
      my_education_1 <- lm(CSF.tTau ~ CSF.AB42+CSF.pTau, eduction_data)
      # Set up Axis
      
      axis_x1_education <- seq(min(new.dat$CSF.AB42), max(new.dat$CSF.AB42))
      axis_z1_education <- seq(min(new.dat$CSF.pTau), max(new.dat$CSF.pTau))
      
      ## PLOT THE POINTS
      
      my_lm_surface_education <- expand.grid(CSF.AB42 = axis_x1_education,CSF.pTau = axis_z1_education,KEEP.OUT.ATTRS = F)
      my_lm_surface_education$CSF.tTau <- predict.lm(my_education_1, newdata = my_lm_surface_education)
      my_lm_surface_education <- acast(my_lm_surface_education, CSF.pTau ~ CSF.AB42, value.var = "CSF.tTau")
      
      my_plot <- plot_ly()
      my_plot <- add_markers(
        p = my_plot,
        data = new.dat,
        x = ~CSF.AB42,
        y = ~ CSF.pTau,
        z = ~CSF.tTau,
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
      
      my_lm_ed_0 <- lm(CSF.tTau ~ CSF.AB42+CSF.pTau, education_data_0)
      # Set up Axis
      
      axis_x2 <- seq(min(new.dat$CSF.AB42), max(new.dat$CSF.AB42))
      axis_z2 <- seq(min(new.dat$CSF.pTau), max(new.dat$CSF.pTau))
      
      ## PLOT THE POINTS
      
      my_lm_surface_ed_0 <- expand.grid(CSF.AB42 = axis_x2,CSF.pTau = axis_z2,KEEP.OUT.ATTRS = F)
      my_lm_surface_ed_0$CSF.tTau <- predict.lm(my_lm_ed_0, newdata = my_lm_surface_ed_0)
      my_lm_surface_ed_0 <- acast(my_lm_surface_ed_0, CSF.pTau ~ CSF.AB42, value.var = "CSF.tTau")
      
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
  


############################# END OF SERVER FUNCTION ###########################  
}
