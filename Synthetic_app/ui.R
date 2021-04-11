#
##
###
#############################################################################
######################### SIMULATED DATA UI CODE ############################
#############################################################################
###
##
#
############################################
# Synthetic ATN data
# Lucinda Nott
# Marcela Cespedes 




# this is the user interface file - starting with a very basic layout 
## Load Libraries 
library(shiny)
library(reshape2)
library(dplyr)
library(ggplot2)
library(visdat)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
library(shinycssloaders)
library(data.table)
library(gridExtra)
library(networkD3)
library(shinydashboard) # for dashboard related functions and features
library(shinyBS)
library(htmlTable)
library(htmlwidgets)
library(shinymanager)
library(shinydisconnect)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

share <- list(
  image = ""
)

# User Interface
ui<- 
  #secure_app(head_auth = tags$script(inactivity),
  fluidPage(
    useShinyjs(),
    shinydisconnect::disconnectMessage2(),
  tags$style(HTML("
    .tabbable > .nav > li > a {display:flex;
    justify-content:center;
    flex-direction:row;
    font-size:18PX;height = 40PX;display:inline-block;
    margin-left:auto; margin-right}
  ")),
  br(),
  br(),
  titlePanel(
    windowTitle = "De-binarising the ATN Framework: Continuous modelling of biomarkers in 3D",
    title = h1(strong("De-binarising the ATN Framework: Continuous modelling of biomarkers in 3D (Synthetic Simulated Data Version)"), align = "center")),
  useShinyalert(),
  useShinyjs(),
  br(), 
  br(),
  br(),
  fixedRow(
    column(1, tags$a(img(src = "blankspace.png", height = "10px", width = "60px"))), 
    column(3, tags$a(href = "https://aibl.csiro.au/", 
                     img(src = "Capture.PNG", height = "100px", 
                         title = "aibl Homepage"))), 
    column(1, tags$a(img(src = "blankspace.png", height = "10px", width = "60px"))), 
    column(1, tags$a(img(src = "blankspace.png", height = "10px", width = "60px"))), 
    column(2, tags$a(href = 'https://www.csiro.au/', 
                     img(src = 'CSIRO_logo1.png', 
                         title = 'CSIRO Home Page'))), 
    column(1, tags$a(img(src = "blankspace.png", height = "10px", width = "60px"))), 
    column(3, tags$a(href = "https://aehrc.com/", 
                     img(src = "EHealth_logo.png", 
                         title = "The Australian eHealth Research Centre Homepage", align = "center"))),
  ), 
  br(),
  br(),
  br(),
  tabsetPanel(type = "tabs", # create Tabs
              
              
              ## Panel 1 -----------------------------------                
              tabPanel(title ="Background: ATN model project", 
                       h2(strong("Background"), align = "center"),
                       br(),
                       fixedRow(
                         column(2,tags$a(img(src = "blankspace.png", height = "10px", width = "60px"))), 
                         column(8, 
                                p(strong("The synthetic data in this app was randomly generated and bears no clinical relevance. 
                                  Data used in this app is to demostrate app features, and does not represent true features or characteristics of the AIBL biomarkers
                                  in the ATN framework", style ="text-align: justify;width:100%;font-size:20px;color:red")),
                                p("Alzheimer's Disease (AD) is one of the most common forms of dementia. With no cure, scientists are 
                         still grappling to better understand how this disease affects the human brain prior to and during the 
                         development of dementia.", style ="width:100%;font-size:18px"),
                                p("The methods used to assess the pathology and classify stages of AD have been standardised to help 
                         provide a guide for clinicians. Standard measures of pathology refer to the build up of Amyloid 
                         (AB) plaques, neurofibrillary tangles of Tau protein in the affected brain regions and the progressive 
                         loss of structure or function of neurons, known as Neurodegeneration. The hippocampus, a complex brain structure located inside the medial lobe 
                         is a key 
                         measure of neurodegeneration as it is one of the earliest brain regions to start 
                         deteriorating (the hippocampus volume used here is the sum of the left and right hippocampi, adjusted for the intracranial volume [4]). What makes the ATN framework specific to 
                         AD is the combination of the three pathologies. Some dementias have AB-Amyloid pathology but not Tau, others have Tau 
                         build up but are not associated with AD (including brain injuries). If a patient has all three - then it is 
                         highly probable they are on the AD pathway. 
                         ", style ="text-align: justify;width:100%;font-size:18px")),
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "60px")))                                 
                       ),
                       
                       br(),
                       # tags$a(img(src = "AD2.png")),
                       fixedRow(
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "60px"))), 
                         column(8, tags$a(href = "http://www.lidsen.com/journals/neurobiology/neurobiology-03-04-046", 
                                          img(src = "AD2.png", 
                                              title = "Adaption from de Loof & Schoofs (2019)", 
                                              width = "100%", 
                                              height = "auto"))),
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "60px"))) 
                       ),
                       
                       # HTML('<center><img src="AD2.png"></center>'),
                       fixedRow(
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "100%"))), 
                         column(8, p("Figure 1: Major pathological hallmarks of AD are plaques and neurofibrillary tangles that are absent in health 
                                               brain tissue. (A) shows the AB-Amyloid and Tau of a normal brain, (B) shows the tangles and plaques of an AD brain.
                                               (C) shows the massive apoptosis that occurs in the later stages of AD in the human brain (brain slices). 
                                               Sources: de Loof & Schoofs [5].",style = "text-align: justify;font-size:15px;width:100%")), 
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "60px")))
                       ),
                       
                       br(),
                       h2(strong("Research Problem"), align = "center"),
                       fixedRow(
                         column(2,tags$a(img(src = "blankspace.png", height = "10px", width = "60px"))), 
                         column(8, p("Previously, the biomarkers for AD (AB-Amyloid, Tau and Neurodegeneration) have been used to descibe the different stages 
                         of AD by Clifford and colleagues [3]. In this, an individual is described as being positive or negative for AB-Amyloid, Tau and Neurodegeneration, 
                        forming 8 different groups. This approach considers the binary form of these 
                         biomarkers in the terms of an individual surpassing a certain threshold and does not provide 
                         information on the interplay of the AD biomarkers in their continuous biomarker form.",style ="text-align: justify;width:100%;font-size:18px")), 
                         column(2,tags$a(img(src = "blankspace.png", height = "10px", width = "60px"))), 
                       ),
                       br(),
                       h2(strong('Aim of this Project'),align = "center"), 
                       fixedRow(
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "100%"))), 
                         column(8,
                                p("This project aims to investigate the joint associations of Amyloid, Tau and Neurodegeneration over the 
                         AD domain to better understand the complex dynamics along a continuous domain using mathematical and 
                         statistical 3D modelling. Moreover, this project aims to deliver results using an interactive tool to 
                         effectively visualise and better communicate the results from complex 3D models. It further aims to 
                         better inform and aid clinicians and neuroscientists on the aetiology of AD and facilitate new knowledge 
                         on the interplay of key AD biomarkers.",style ="text-align: justify;width:100%;font-size:18px")
                         ),
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "100%"))), 
                       ),
                       HTML('<center><img src="input.JPG" width="600"></center>'),
                       fixedRow(
                         column(4, tags$a(img(src = "blankspace.png", height = "10px", width = "40px"))),
                         column(4, p("Figure 2: Flowchart and general breakdown of the project."))
                       ), 
                       br(),
                       h2(strong('Layout of App'), align = "center"), 
                       br(),
                       fixedRow(
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "100%"))), 
                         column(8,
                                p("There are two general approaches for measuring the biomarkers of the AT(N) framework; via cerebrospinal fluid 
                       (CSF) or via medical imaging such as positron emisson topography (PET) and magnetic resonance imaging (MRI). Thus, 
                       this project analysed AT(N) biomarkers in two distinct groups. Group 1 pertains to AT(N) biomarkers measured exclusively via 
                       CSF and Group 2 combines imaging and CSF AT(N) biomarkers. Analysis and results for each group can be found in the corresponding 
                       App tabs. A visual breakdown of the Groups and how the corresponding biomarkers were measured can be seen below.",style ="text-align: justify;width:100%;font-size:18px")),
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "100%")))
                       ),
                       fixedRow(
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "100%"))), 
                         column(8,
                                sankeyNetworkOutput("SankeyDiagram", height = 300, width = "80%")
                         )
                       ),
                       fixedRow(
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "40px"))),
                         column(8, p("Figure 3: Sankey Diagram that describes the biomarker breakdown for each group analysed."))
                       ), 
                       h2(strong("Description of Data"), align = "center"),
                       fixedRow(
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "100%"))), 
                         column(8,
                                p("In this project, we analysed data from the Australian Imaging, Biomarkers and Lifestyle flagship (AIBL)
                         study of ageing [7]. The AIBL study is the largest longitudinal study of ageing in Australia. With over 2,000
                         participants, the focus of AIBL is to better the nature of AD research to enable early detection and lifestyle interventions. 
                         As the data used is derived from a clinical study, the initial findings and App will not be accessible to the public. Instead, this publically available App consists of synthetic 
                         simulated data that can be found ", a(href = "https://github.com/lucindanott",
                                                                                                    'here', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end"),style ="text-align: justify;width:100%;font-size:18px")),
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "100%"))), 
                       ),
                       br(), 
                       h2("References", style = "font-size:18px", align = "center"),
                       fixedRow(
                         column(2, tags$a(img(src = "blankspace.png", height = "10px", width = "100%"))), 
                         column(8,
                                p("[1] Bourgeat, P., Dore, V., Fripp, J., Villemagne, V.L., Rowe, C.C. and Salvado, O., 2015, March. 
                        Computational analysis of PET by AIBL (CapAIBL): a cloud-based processing pipeline for the quantification of PET images. 
                        In Medical Imaging 2015: Image Processing (Vol. 9413, p. 94132V). International Society for Optics and Photonics.", style = "font-size:10px"),
                                p("[2] Burnham, S.C., Coloma, P.M., Li, Q.X., Collins, S., Savage, G., Laws, S., Doecke, J., Maruff, P., Martins, R.N., Ames, D. and Rowe, C.C., 2019. 
                         Application of the NIA-AA research framework: towards a biological definition of Alzheimer's disease using cerebrospinal fluid biomarkers in the AIBL 
                         study. The journal of prevention of Alzheimer's disease, 6(4), pp.248-255.", style = "font-size:10px"),
                                p("[3] Jack, C.R., Bennett, D.A., Blennow, K., Carrillo, M.C., Feldman, H.H., Frisoni, G.B., 
                        Hampel, H., Jagust, W.J., Johnson, K.A., Knopman, D.S. and Petersen, R.C., 2016. 
                        A/T/N: an unbiased descriptive classification scheme for Alzheimer disease biomarkers. Neurology, 87(5), pp.539-547.
                         ", style = "font-size:10px"),
                                p("[4] Jack Jr, C.R., Knopman, D.S., Jagust, W.J., Petersen, R.C., Weiner, M.W., Aisen, P.S., Shaw, L.M., Vemuri, P., Wiste, H.J., Weigand, S.D. and Lesnick, T.G., 2013. 
                        Update on hypothetical model of Alzheimer's disease biomarkers. Lancet neurology, 12(2), p.207.", style = "font-size:10px"),
                                p("[5] De Loof, A. and Schoofs, L., 2019. Alzheimer's disease: 
                        Is a dysfunctional mevalonate biosynthetic pathway the master-inducer of deleterious changes in cell physiology?. 
                        OBM Neurobiology, 3(4).", style = "font-size:10px"),
                                p("[6] Doecke, J.D., Ward, L., Burnham, S.C., Villemagne, V.L., Li, Q.X., Collins, S., Fowler, C.J., Manuilova, E., 
                           Widmann, M., Rainey-Smith, S.R. and Martins, R.N., 2020. Elecsys CSF biomarker immunoassays demonstrate concordance with 
                           amyloid-PET imaging. Alzheimer's research & therapy, 12, pp.1-10..", style = "font-size:10px"),
                                p("[7] Dore, V., Bourgeat, P., Cummins, T.L., Raniga, P., Williams, S., Amadoru, S., MacLean, C., Hinton, F., Shepherd, C., Halliday, G.M. and Leyton, C.E., 
                        2018. P1-405: VISUAL ASSESSMENT OF BETA-AMYLOID PET SCAN IS IMPROVED BY CAPAIBL. Alzheimer's & Dementia, 14(7S_Part_8),
                        pp.P459-P459.", style = "font-size:10px"),
                                p("[8] Ellis, K., Bush, A., Darby, D., De Fazio, D., Foster, J., Hudson, P., Lautenschlager, N., Lenzo, N., Martins, R., 
                        Maruff, P. and Masters, C., 2009. The Australian Imaging, Biomarkers and Lifestyle (AIBL) study of aging: methodology and 
                        baseline characteristics of 1112 individuals recruited for a longitudinal study of Alzheimer's disease.", style = "font-size:10px"),
                                p("[9] Li, Q.X., Villemagne, V.L., Doecke, J.D., Rembach, A., Sarros, S., Varghese, S., McGlade, A., Laughton, K.M., Pertile, K.K., Fowler, C.J.
                         and Rumble, R.L., 2015. Alzheimer's disease normative cerebrospinal fluid biomarkers validated in PET amyloid-Beta
                         characterized subjects from the Australian Imaging, Biomarkers and Lifestyle (AIBL) study. Journal of Alzheimer's Disease, 48(1), pp.175-187.", style = "font-size:10px"),
                                p("[10] Jack Jr, C.R., Knopman, D.S., Jagust, W.J., Petersen, R.C., Weiner, M.W., Aisen, P.S., Shaw, L.M., Vemuri, P., Wiste, H.J., 
                        Weigand, S.D. and Lesnick, T.G., 2013. Tracking pathophysiological processes in Alzheimer's disease: an updated hypothetical model of 
                        dynamic biomarkers. The Lancet Neurology, 12(2), pp.207-216.", style = "font-size:10px"),
                                p("[11] Medical Animation (2013). Cerebrospinal Fluid In Brain - 3D Medical Animation. Available at: http://www.medical-animations.com/contact.html. ", style = "font-size:10px"),
                                p("[12] The Swedish BioFINDER Study. 2021. Tau PET Imaging - The Swedish Biofinder Study. [online] Available at: https://biofinder.se/data-biomarkers/tau-pet-imaging/.", style = "font-size:10px"),
                                p("[13] Villemagne, V., 2014. En Attendant Centiloid. Advances in Research, 2(12), pp.723-729.", style = "font-size:10px")
                         )
                       )
                       
                       
              ), # end first panel
              
              ########################################################
              ########################################################
              
              ## Panel 2 -------------------------------------
              tabPanel(title ="Group 1: CSF ATN Biomarkers",
                       br(), 
                       box(solidHeader = F, 
                           collapsible = F, 
                           width = 12, 
                           fluidRow(
                             column(width = 9,
                                    conditionalPanel(
                                      condition = "input.SUMBITNEW",
                                      textOutput("NewCUTOFFLabels"),
                                      tags$head(tags$style("#NewCUTOFFLabels{
                                 font-size: 18px;color:#6C3483;font-weight:bold                             }"
                                      )
                                      ),
                                      br(),
                                    ),
                                    conditionalPanel(
                                      condition = "input.newDataSUBMIT", 
                                      p("You have selected to use INNOTEST assay cut-off values for the CSF data. These points are the same as 
                                        used in the supplied data set and have been updated across the app. To return to supplied data or make a new selection, please
                                        refresh the website to clear all previous inputs.", style = 'text-align: justify;width:100%;font-size:18px;color:#6C3483;font-weight:bold ')
                                    ),
                                    p("The National Institute on Aging and Alzheimer's Association (NIA-AA) proposed a new 
                                      research framework: Towards a Biological definition of Alzheimer's disease which uses the 
                                      three-biomarker previously mentioned: AB-amyloid, Tau and Neurodegeneration, AT(N), to 
                                      generate an eight group biomarker based definition of Alzheimer's disease [2]. These eight groups
                                      were then collapsed into four main groups corresponding to the classifications of Burnham et al. [2]. The table below 
                                      illustrates the groups.",style ="text-align: justify;width:100%;font-size:18px"),
                                    HTML('<style type="text/css">
                  .tg  {border-collapse:collapse;border-color:#ccc;border-spacing:0;}
                          .tg td{background-color:#fff;border-color:#ccc;border-style:solid;border-width:1px;color:#333;
                                  font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;word-break:normal;}
                      .tg th{background-color:#f0f0f0;border-color:#ccc;border-style:solid;border-width:1px;color:#333;
                              font-family:Arial, sans-serif;font-size:14px;font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
                      .tg .tg-mcqj{border-color:#000000;font-weight:bold;text-align:left;vertical-align:top}
                              .tg .tg-73oq{border-color:#000000;text-align:left;vertical-align:top}
                                      .tg .tg-xwyw{border-color:#000000;text-align:center;vertical-align:middle}
                                              </style>
                                              <table class="tg">
                                                  <thead>
                                                  <tr>
                                                  <th class="tg-mcqj">NIA-AA Research Framework</th>
                                                      <th class="tg-mcqj">Burnham Classification</th>
                                                          </tr>
                                                          </thead>
                                                          <tbody>
                                                          <tr>
                                                          <td class="tg-73oq">A+/T+/N+</td>
                                                              <td class="tg-xwyw" rowspan="2">AD</td>
                                                                  </tr>
                                                                  <tr>
                                                                  <td class="tg-73oq">A+/T+/N-</td>
                                                                      </tr>
                                                                      <tr>
                                                                      <td class="tg-73oq">A+/T-/N+</td>
                                                                          <td class="tg-xwyw" rowspan="2">Pathological Change</td>
                                                                              </tr>
                                                                              <tr>
                                                                              <td class="tg-73oq">A+/T-/N-</td>
                                                                                  </tr>
                                                                                  <tr>
                                                                                  <td class="tg-73oq">A-/T+/N+</td>
                                                                                      <td class="tg-xwyw" rowspan="3">Non-AD Pathological Change</td>
                                                                                          </tr>
                                                                                          <tr>
                                                                                          <td class="tg-73oq">A-/T-/N+</td>
                                                                                              </tr>
                                                                                              <tr>
                                                                                              <td class="tg-73oq">A-/T+/N-</td>
                                                                                                  </tr>
                                                                                                  <tr>
                                                                                                  <td class="tg-73oq">A-/T-/N-</td>
                                                                                                      <td class="tg-xwyw">Normal AD Biomarkers</td>
                                                                                                          </tr>
                                                                                                          </tbody>
                                                                                                          </table>'),
                                    p("Table 1: Group 1 Classifications adapted from Burham et al. [2]"),
                                    br(),
                                    conditionalPanel(
                                      condition = "input.sample_or_real == 'sample' || input.newDataSUBMIT", 
                                      p("Several imaging
                                      and CSF techniques can be used to detect these biomarkers. Imaging 
                                      techniques include MRI and PET
                                      scans. Lumbar punctures are used to collect CSF data. In this panel, we apply the AT(N) classification 
                                      system to CSF biomarkers from data collected in AIBL. The type of assay used in AD research is important. Here, single-analyte ELISA tests (or INNOTEST)
                                      assay for the 
                                      CSF data is used. This means that the threshold cut-offs for positive and negative individuals are dependent on being under or over the age of 70. 
                                        Group 1 is described by the following biomarkers and cut-off thresholds:",style ="text-align: justify;width:100%;font-size:18px"), 
                                      tags$div(
                                        tags$ul(
                                          tags$li("A: is quantified by CSF AB1-42, positive participants have values less than 656 and are over 60 years old. ",style ="width:100%;font-size:18px"), 
                                          tags$li("T: is quantified by phosphor-Tau CSF p-181, positive participants aged 60-70 have values greater than 59.23 and individuals aged 70+ have values greater than 73.83",style ="width:100%;font-size:18px"), 
                                          tags$li("N: is CSF t-Tau or total-Tau, positive participants aged 60-70 have values greater than 303.54 and individuals aged 70+ have values greater than 378.65",style ="width:100%;font-size:18px")
                                        )
                                      ), 
                                    ),
                                    conditionalPanel(
                                      condition = "input.SUMBITNEW", 
                                      p("Several imaging
                                      and CSF techniques can be used to detect these biomarkers. Imaging 
                                      techniques include MRI and PET
                                      scans. Lumbar punctures are used to collect CSF data. In this panel, we apply the AT(N) classification 
                                      system to CSF biomarkers from data collected in AIBL. The type of assay used in AD research is important.
                                        Group 1 is described by the following biomarkers and cut-off thresholds:",style ="text-align: justify;width:100%;font-size:18px"), 
                                      tags$div(
                                        tags$ul(
                                          tags$li(textOutput("ABTEXT"), tags$head(tags$style("#ABTEXT{
                                 font-size: 18px;                             }"
                                          )
                                          )
                                          ), 
                                          tags$li(textOutput("PTAUTEXT"), tags$head(tags$style("#PTAUTEXT{
                                 font-size: 18px;                             }"
                                          )
                                          )
                                          ),  
                                          tags$li(textOutput("TTAUTEXT"), tags$head(tags$style("#TTAUTEXT{
                                 font-size: 18px;                             }"
                                          )
                                          )
                                          ),  
                                        )
                                      ),
                                    ),
                                    p("These biomarker cut-offs are cited in Doecke et al. [6] and Li et al. [9]. For more information on INNO assay and the biomarker cut-offs please see ",
                                      a(href = "https://www.fujirebio.com/sites/default/files/2019-09/INNOTEST_Alzheimer_brochure.pdf",'here', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end"),style ="text-align: justify;width:100%;font-size:18px"),
                                    p("This tab is broken down into eight components; data selection, a demographic summary, a reactive 2D plot, 
                                      interactive 3D plots (with the options of adding in the positive thresholds for the 
                                      biomarkers), an A+/T+/N+ positive area visual, Add in your own data point option and statistical modelling.",style ="text-align: justify;width:100%;font-size:18px"),
                                    # p("For more information on INNO assay and the biomarker cut-offs please see ",a(href = "https://www.fujirebio.com/sites/default/files/2019-09/INNOTEST_Alzheimer_brochure.pdf",
                                    #                                                                                'here', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end")
                                    #   )
                             ),
                             
                             column(width = 3,
                                    align = "left",
                                    tags$a(href = "http://www.medical-animations.com/contact.html",
                                           img(src = "CSF.gif", height = "230px", width = "265px", title = "Adaption from Medical Animation Australia")),
                                    # textOutput("TEXT1")
                                    p("Figure 4: a visual representation of the cerebrospinal fluid that surrounds the brain and travels down the
                                      spinal cord. A lumbar puncture would be performed at the lower back, in the lumbar region. Source: Medical Animation [11].", style = "text-align: justify;width:265px")
                             )
                           )
                       ),
                       br(),
                       br(),
                       
                       tabsetPanel(type = "tabs", 
                                   tabPanel("Data Selection",
                                            br(), 
                                            h3(strong("Download Data")),
                                            hr(),
                                            p("The data for this synthetic app was randomly generated, bearing no clinical relevance.
                                              All data files associated with the synthetic app are publicly available and you can download the CSV file
                                              containing the simulated synthetic data used.", style = "width:75%; font-size:18px;text-align:justify"),
                                            p("To download the dataset, please click the download button below.",style = "width:100%; font-size:18px;text-align:justify"), 
                                            downloadButton("downloadData", "Download", 
                                                           class = "LN"), 
                                            tags$head(tags$style(".LN{background-color:#5bc0de;} .LN{color: white;} .LN{width:250px}")), # background color and font color
                                            br(), 
                                            br(), 
                                            p(""),
                                            sidebarLayout(
                                              sidebarPanel(
                                                h3(strong("Upload Your Own Data")),
                                                hr(),
                                              p("Additionally, you can select to analyse your own data (uploaded from a CSV file) or use the synthetic data supplied.
                                                             The data selected will be analysed across all tabs. If the user does not want to add data in,
                                                             the app is automatically generated for the synthetic data supplied and no further action is needed.
                                                ", 
                                                             style = "width:100%; font-size:18px;text-align:justify"),
                                                           # selectInput("addDATAin", "Select the data anaylsed:", 
                                                           #             choices = c("Synthetic Simulated Data Supplied", 
                                                           #                         "Upload my own data"),
                                                           #             # selected = "Synthetic Simulated Data Supplied", 
                                                           #             multiple = F), 
                                              radioButtons(
                                                "sample_or_real", h4("Which Data to use?"), 
                                                choices = list("Supplied Synthetic Data" = "sample", 
                                                               "Upload from .csv format (spreadsheet)" = "user")
                                              )
                                             
                                              
                                               
                                            ), 
                                            mainPanel(
                                              br(),
                                              conditionalPanel(
                                                condition = "input.sample_or_real == 'sample'", 
                                                p(strong("You have selected to use the Synthetic Data Supplied. No further action is needed.", 
                                                         style = "width:100%;font-size:17px;color:red")), 
                                                p("Note that in the synthetic data all CSF ATN biomarkers for Group 1 were analysed with INNOTEST and the cut-offs are dependent on age. Group 2 pertains to 
          a combined modality of Centiloid PET Scan, CSF pTau and hippocampus volume with the cut-offs evident in the corresponding tab.",
                                                  style = "width:80%;font-size:17px")
                                              ), 
                                              conditionalPanel(
                                                condition = "input.sample_or_real == 'user'", 
                                                p(strong("You have selected upload your own data. Please follow the instructions 
                                                         and questions below to upload the data correctly.",
                                                         style = "width:80%;font-size:17px;color:red")), 
                                                br(), 
                                                p("In this app, INNOTEST assay was used for all CSF data. This makes the thresholds dependent on
                                                  and individual being under or over the age of 70.", style = "width:100%;font-size:17px"), 
                                                p(strong("Do you wish to use the INNOTEST assay cut offs for CSF data?", 
                                                  style = "width:80%;font-size:17px;color:red")), 
                                                radioButtons(
                                                  "INNOTEST", "Do you wish to use the INNOTEST assay cut-offs for CSF data?", 
                                                  choices = list(
                                                    "Yes" = "yes_innotest", 
                                                    "No" = "no_innotest"
                                                  ), 
                                                  selected = character(0)
                                                ), 
                                                conditionalPanel(
                                                  condition = "input.INNOTEST == 'yes_innotest'", 
                                                  p(strong("You have selected to use the same measurements of data analysed for group 1 and 2 with the same thresholds. Please following careful the instructions below for uploading
                                                  your CSV file.", style = "width:80%;font-size:17px;color:red")), 
                                                  p("The CSV file must include the following headers in the first row. Order is not important, however, please ensure the spelling is the same.", style = "width:100%;font-size:16px"), 
                                                  tags$div(
                                                    tags$ul(
                                                      tags$li("Age - numerical input", style = "width:100%;font-size:16px"), 
                                                      tags$li("apoe4 - categorical input where 1 is a carrier, 0 is non-carrier", style = "width:100%;font-size:16px"), 
                                                      tags$li("Sex - categorial input including Male or Female.", style = "width:100%;font-size:16px"), 
                                                      tags$li("Education_binary - binary input where 1 is over 12 years education and 0 under 12 years.", style = "width:100%;font-size:16px"),
                                                      tags$li("Diagnosis - categorial input for clinical diagnosis consisting of AD, MCI and HC", style = "width:100%;font-size:16px"),
                                                      tags$li("CSF.AB42 - numerical input", style = "width:100%;font-size:16px"),
                                                      tags$li("CSF.pTau - numerical input", style = "width:100%;font-size:16px"), 
                                                      tags$li("CSF.tTau - numerical input", style = "width:100%;font-size:16px"), 
                                                      tags$li("Sum.hippo - numerical input", style = "width:100%;font-size:16px"), 
                                                      tags$li("Centiloid - numerical input", style = "width:100%;font-size:16px"), 
                                                      tags$li("AB.status - categorial input consisting of negative or positive", style = "width:100%;font-size:16px"), 
                                                      tags$li("pTau.status - categorial input consisting of negative or positive ", style = "width:100%;font-size:16px"), 
                                                      tags$li("tTau.status - categorial input consisting of negative or positive", style = "width:100%;font-size:16px")
                                                    )
                                                  ),
                                                  p("The following data points must be numerical unless specificed above for categorical inputs. \n
                                                  No additional characters, such as $,#,&,*,(,!, are permitted. \n 
                                                  Please ensure that capitalising of inputs and row headers are followed as above. ", style = "width:100%;font-size:16px"), 
                                                  p("You can also downloand (through the button below) an example CSV file to check the inputs required.", 
                                                    style = "width:100%;font-size:16px"),
                                                  downloadButton("DOWNLOADEXAMPLE", "Download Example CSV File",
                                                                 class = "LN"),
                                                  tags$head(tags$style(".LN{background-color:#5bc0de;} .LN{color: white;} .LN{width:250px}")), # background color and font color
                                                  br(),
                                                  br(),
                                                  p("Once uploaded, the first 5 rows of data will be displayed. Double check the inputs and press the submit button when ready.", 
                                                    style = "width:100%;font-size:16px"),
                                                  p(strong("If the file does not adhere to the instructions above, the website will disconnect due to an error in the file", 
                                                           style = "width:100%;font-size:16px;color:red")),
                                                  fileInput("file1", "Choose CSV File",
                                                            multiple = F,
                                                            accept = c("text/csv",
                                                                       "text/comma-separated-values, text/plain",
                                                                       ".csv"
                                                            )
                                                            
                                                  ), 
                                                  tableOutput("contents"), 
                                              
                                                  actionGroupButtons("newDataSUBMIT", "SUBMIT", status = "info", size = "lg"), 
                                               
                                                ), 
                                                # FOR DIFFERENT CUT OFF VALUES
                                                conditionalPanel(
                                                  condition = "input.INNOTEST == 'no_innotest'", 
                                                  p(strong("You have selected to use the same measurements of data and groups but with different assay and/or thresholds. 
                     Please follow carefully the instructions below.",
                                                           style = "width:80%;font-size:17px;color:red")), 
                                                  textInput("AssayLabel", 
                                                            label = "Please input the name of the assay used:", 
                                                            value = ""), 
                                                  radioButtons(
                                                    "new_AGE", "Are the cut-offs for this assay age dependent?", 
                                                    choices = list(
                                                      "Yes" = 'Age_yes', 
                                                      "No" = "age_no"
                                                    ), 
                                                    selected = character(0)
                                                  ), 
                                                  conditionalPanel(
                                                    condition = "input.new_AGE == 'Age_yes'", 
                                                    p("This assay is age dependent. Please select the number of bins (different age
                                                      brackets) for each CSF biomarkers (for example if CSF AB1-42 pg/mL was dependent
                                                      on an individual being 60-70 or 70+, there would be 2 age brackets.)", 
                                                      style ="width:100%;font-size:17px"), 
                                                    # radioButtons("AB_bins", "How many bins for CSF AB 1-42 pg/mL",
                                                    #              choices = c("1","2","3","4+"),
                                                    #              selected = character(0))
                                                    fluidRow(
                                                      column(4, radioButtons("AB_bins", "How many bins for CSF AB1-42 pg/mL",
                                                                             choices = c("2","3"),
                                                                             selected = character(0))
                                                      ), 
                                                      column(4, radioButtons("pTau_bins", "How many bins for CSF pTau pg/mL",
                                                                             choices = c("2","3"),
                                                                             selected = character(0))
                                                      ), 
                                                      column(4, radioButtons("tTau_bins", "How many bins for CSF tTau pg/mL",
                                                                             choices = c("2","3"),
                                                                             selected = character(0))
                                                      ),
                                                    ), 
                                                    conditionalPanel(
                                                      condition = "input.AB_bins == '2'", 
                                                      p(strong("For CSF AB 1-42 pg/mL, please enter the following", 
                                                               style ="width:100%;font-size:17px;color:#7FB3D5")),
                                                      fluidRow(
                                                        column(4, 
                                                               numericInput(
                                                                 "AB_input_age", "Please input the 1st cut-off point", 
                                                                 value = 1, min = 1, max = NA
                                                               )
                                                               ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "AB_text", "Please specify the lower age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               )
                                                               ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "AB_second_bracket_2", "Please specify the higher age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               ))
                                                      ), 
                                                      fluidRow(
                                                        column(4, 
                                                               numericInput(
                                                                 "AB_input_age_second", "Please input the 2nd cut-off point", 
                                                                 value = 1, min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "AB_text_second", "Please specify the lower age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "AB_input_age_bracket_2", "Please specify the higher age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               )
                                                               )
                                                      ),
                                                      
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.AB_bins == '3'", 
                                                      p(strong("For CSF AB 1-42 pg/mL, please enter the following", 
                                                               style ="width:100%;font-size:17px;color:#7FB3D5")),
                                                      fluidRow(
                                                        column(4, 
                                                               numericInput(
                                                                 "AB_AGE", "Please input the 1st cut-off point", 
                                                                 value = 1, min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "Text_for_AB", "Please specify the lower age in the bracket", value = ""
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput("input_AB_higher_bracket_2", "Please specify the higher age in the bracket", value = "", 
                                                                            min = 1, max = NA))
                                                      ), 
                                                      fluidRow(
                                                        column(4, 
                                                               numericInput(
                                                                 "AB_AGE_2", "Please input the 2nd cut-off point", 
                                                                 value = 1, min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "TEXT_FOR_AB_2", "Please specify the lower age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "text_input_AB_2_upper", "Please specify the upper age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               ))
                                                      ),
                                                      fluidRow(
                                                        column(4, 
                                                               numericInput(
                                                                 "AB_input_age_third", "Please input the 3rd cut-off point", 
                                                                 value = 1, min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "AB_text_third", "Please specify the lower age in the bracket", value = ""
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "AB_text_bracket_input_3", "Please specify the upper age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               ))
                                                      ),
                                                      
                                                    ),
                                                    
                                                    
                                                    conditionalPanel(
                                                      condition = "input.pTau_bins == '2'", 
                                                      p(strong("For CSF pTau pg/mL, please enter the following", 
                                                               style ="width:100%;font-size:17px;color:#1ABC9C")),
                                                      fluidRow(
                                                        column(4, 
                                                               numericInput(
                                                                 "ptau_input_age", "Please input the 1st cut-off point", 
                                                                 value = 1, min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "ptau_text", "Please specify the lower age in the bracket", value = "",
                                                                 min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4,
                                                               numericInput(
                                                                 "ptau_input_upper_age_2", "Please specify the upper age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               ))
                                                      ), 
                                                      fluidRow(
                                                        column(4, 
                                                               numericInput(
                                                                 "ptau_input_age_second", "Please input the 2nd cut-off point", 
                                                                 value = 1, min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "ptau_text_second", "Please specify the lower age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "ptau_input_upper_age_3", "Please specify the upper age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               ))
                                                      ),
                                                      
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.pTau_bins == '3'", 
                                                      p(strong("For CSF pTau pg/mL, please enter the following", 
                                                               style ="width:100%;font-size:17px;color:#1ABC9C")),
                                                      fluidRow(
                                                        column(4, 
                                                               numericInput(
                                                                 "AGE_Ptau", "Please input the 1st cut-off point", 
                                                                 value = 1, min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "TEXT_PTAU_2", "Please specify the lower age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "ptau_input_upper_age_3_1", "Please specify the upper age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               )
                                                               )
                                                      ), 
                                                      fluidRow(
                                                        column(4, 
                                                               numericInput(
                                                                 "AGE_PTAU_2", "Please input the 2nd cut-off point", 
                                                                 value = 1, min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "TEXT_PTAU_2", "Please specify the lower age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "input_ptau_upper_age_2", "Please specify the upper age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               ))
                                                      ),
                                                      fluidRow(
                                                        column(4, 
                                                               numericInput(
                                                                 "AGE_PTAU_3", "Please input the 3rd cut-off point", 
                                                                 value = 1, min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "TEXT_PTAU_3", "Please specify the lower age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, numericInput(
                                                               "ptau_input_upper_age_3_3", "Please specify the upper age in the bracket", value = "", 
                                                               min = 1, max = NA)
                                                               )
                                                      ),
                                                      
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.tTau_bins == '2'", 
                                                      p(strong("For CSF tTau pg/mL, please enter the following", 
                                                               style ="width:100%;font-size:17px;color:#6C3483")),
                                                      fluidRow(
                                                        column(4, 
                                                               numericInput(
                                                                 "ttau_input_age", "Please input the 1st cut-off point", 
                                                                 value = 1, min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "ttau_text", "Please specify the lower age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "ttau_upper_age_input_2_1", "Please specify the upper age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               ))
                                                      ), 
                                                      fluidRow(
                                                        column(4, 
                                                               numericInput(
                                                                 "ttau_input_age_second", "Please input the 2nd cut-off point", 
                                                                 value = 1, min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "ttau_text_second", "Please specify the lower age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               )
                                                        ), 
                                                        column(4, 
                                                               numericInput(
                                                                 "ttau_upper_age_input_2_2", "Please specify the upper age in the bracket", value = "", 
                                                                 min = 1, max = NA
                                                               ))
                                                      ),
                                                      p("Please double check the cut-offs are correct and submit when ready.", 
                                                        style = "width:100%;font-size:16px"),
                                                      actionGroupButtons("Age_dependent_threshold_2", "Submit Cut-offs", 
                                                                         status = "info", size = "lg"),
                                                      br(),
                                                      br(),
                                                      p("Once you have submitted the cut-offs, please follow the instructions for 
                                                      uploading the data file.", style = "width:100%;font-size:16px"),
                                                      p("The CSV file must include the following headers in the first row. 
                                                      Order is not important, however, please ensure the spelling is the same.",
                                                        style = "width:100%;font-size:16px"), 
                                                      tags$div(
                                                        tags$ul(
                                                          tags$li("Age - numerical input", style = "width:100%;font-size:16px"), 
                                                          tags$li("apoe4 - categorical input where 1 is a carrier, 0 is non-carrier", style = "width:100%;font-size:16px"), 
                                                          tags$li("Sex - categorial input including Male or Female.", style = "width:100%;font-size:16px"), 
                                                          tags$li("Education_binary - binary input where 1 is over 12 years education and 0 under 12 years.", style = "width:100%;font-size:16px"),
                                                          tags$li("Diagnosis - categorial input for clinical diagnosis consisting of AD, MCI and HC", style = "width:100%;font-size:16px"),
                                                          tags$li("CSF.AB42 - numerical input", style = "width:100%;font-size:16px"),
                                                          tags$li("CSF.pTau - numerical input", style = "width:100%;font-size:16px"), 
                                                          tags$li("CSF.tTau - numerical input", style = "width:100%;font-size:16px"), 
                                                          tags$li("Sum.hippo - numerical input", style = "width:100%;font-size:16px"), 
                                                          tags$li("Centiloid - numerical input", style = "width:100%;font-size:16px"), 
                                                          tags$li("AB.status - categorial input consisting of negative or positive", style = "width:100%;font-size:16px"), 
                                                          tags$li("pTau.status - categorial input consisting of negative or positive ", style = "width:100%;font-size:16px"), 
                                                          tags$li("tTau.status - categorial input consisting of negative or positive", style = "width:100%;font-size:16px")
                                                        )
                                                      ),
                                                      p("The following data points must be numerical unless specificed above for categorical inputs. \n
                                                  No additional characters, such as $,#,&,*,(,!, are permitted. \n 
                                                  Please ensure that capitalising of inputs and row headers are followed as above. ", style = "width:100%;font-size:16px"), 
                                                      p("You can also downloand (through the button below) an example CSV file to check the inputs required.", 
                                                        style = "width:100%;font-size:16px"),
                                                      downloadButton("final_download_example", "Download Example CSV File",
                                                                     class = "LN"),
                                                      tags$head(tags$style(".LN{background-color:#5bc0de;} .LN{color: white;} .LN{width:250px}")), # background color and font color
                                                      br(),
                                                      br(),
                                                      p("Once uploaded, the first 5 rows of data will be displayed. Double check the inputs and press the submit button when ready.", 
                                                        style = "width:100%;font-size:16px"),
                                                      p(strong("If the file does not adhere to the instructions above, the website will disconnect due to an error in the file", 
                                                               style = "width:100%;font-size:16px;color:red")),
                                                      fileInput("file_new_2_brackets", "Choose CSV File",
                                                                multiple = F,
                                                                accept = c("text/csv",
                                                                           "text/comma-separated-values, text/plain",
                                                                           ".csv"
                                                                )
                                                                
                                                      ), 
                                                      tableOutput("final_contents"),

                                                      actionGroupButtons("final_data_SUBMIT", "SUBMIT", status = "info", size = "lg")
                                                      
                                                      
                                                      
                                                    ),
                                                  conditionalPanel(
                                                    condition = "input.tTau_bins == '3'", 
                                                    p(strong("For CSF tTau pg/mL, please enter the following", 
                                                             style ="width:100%;font-size:17px;color:#6C3483")),
                                                    fluidRow(
                                                      column(4, 
                                                             numericInput(
                                                               "AGE_TTAU", "Please input the 1st cut-off point", 
                                                               value = 1, min = 1, max = NA
                                                             )
                                                      ), 
                                                      column(4, 
                                                             numericInput(
                                                               "TEXT_Ttau", "Please specify the lower age in the bracket", value = "", 
                                                               min = 1, max = NA
                                                             )
                                                      ), 
                                                      column(4, 
                                                             numericInput(
                                                               "ttau_upper_age_3_1", "Please specify the upper age in the bracket", value = "", 
                                                               min = 1, max = NA
                                                             ))
                                                    ), 
                                                    fluidRow(
                                                      column(4, 
                                                             numericInput(
                                                               "AGE_TTAU_2", "Please input the 2nd cut-off point", 
                                                               value = 1, min = 1, max = NA
                                                             )
                                                      ), 
                                                      column(4, 
                                                             numericInput(
                                                               "TEXT_TTAU_2", "Please specify the lower age in the bracket", value = "", 
                                                               min = 1, max = NA
                                                             )
                                                      ), 
                                                      column(4, 
                                                             numericInput(
                                                               "ttau_upper_age_3_2", "Please specify the upper age in the bracket", value = "", 
                                                               min = 1, max = NA
                                                             ))
                                                    ),
                                                    fluidRow(
                                                      column(4, 
                                                             numericInput(
                                                               "AGE_TTAU_2", "Please input the 3rd cut-off point", 
                                                               value = 1, min = 1, max = NA
                                                             )
                                                      ), 
                                                      column(4, 
                                                             numericInput(
                                                               "TEXT_TTAU_3", "Please specify the lower age in the bracket", value = "", 
                                                               min = 1, max = NA
                                                             )
                                                      ), 
                                                      column(4, 
                                                             numericInput(
                                                               "ttau_upper_age_3_3", "Please specify the upper age in the bracket", value = "", 
                                                               min = 1, max = NA
                                                             ))
                                                    ),
                                                    p("Please double check the cut-offs are correct and submit when ready.", 
                                                      style = "width:100%;font-size:16px"),
                                                    actionGroupButtons("Age_dependent_threshold_3", "Submit Cut-offs", 
                                                                       status = "info", size = "lg"),
                                                    br(),
                                                    br(),
                                                    p("Once you have submitted the cut-offs, please follow the instructions for 
                                                      uploading the data file.", style = "width:100%;font-size:16px"),
                                                    p("The CSV file must include the following headers in the first row. 
                                                      Order is not important, however, please ensure the spelling is the same.",
                                                      style = "width:100%;font-size:16px"), 
                                                    tags$div(
                                                      tags$ul(
                                                        tags$li("Age - numerical input", style = "width:100%;font-size:16px"), 
                                                        tags$li("apoe4 - categorical input where 1 is a carrier, 0 is non-carrier", style = "width:100%;font-size:16px"), 
                                                        tags$li("Sex - categorial input including Male or Female.", style = "width:100%;font-size:16px"), 
                                                        tags$li("Education_binary - binary input where 1 is over 12 years education and 0 under 12 years.", style = "width:100%;font-size:16px"),
                                                        tags$li("Diagnosis - categorial input for clinical diagnosis consisting of AD, MCI and HC", style = "width:100%;font-size:16px"),
                                                        tags$li("CSF.AB42 - numerical input", style = "width:100%;font-size:16px"),
                                                        tags$li("CSF.pTau - numerical input", style = "width:100%;font-size:16px"), 
                                                        tags$li("CSF.tTau - numerical input", style = "width:100%;font-size:16px"), 
                                                        tags$li("Sum.hippo - numerical input", style = "width:100%;font-size:16px"), 
                                                        tags$li("Centiloid - numerical input", style = "width:100%;font-size:16px"), 
                                                        tags$li("AB.status - categorial input consisting of negative or positive", style = "width:100%;font-size:16px"), 
                                                        tags$li("pTau.status - categorial input consisting of negative or positive ", style = "width:100%;font-size:16px"), 
                                                        tags$li("tTau.status - categorial input consisting of negative or positive", style = "width:100%;font-size:16px")
                                                      )
                                                    ),
                                                    p("The following data points must be numerical unless specificed above for categorical inputs. \n
                                                  No additional characters, such as $,#,&,*,(,!, are permitted. \n 
                                                  Please ensure that capitalising of inputs and row headers are followed as above. ", style = "width:100%;font-size:16px"), 
                                                    p("You can also downloand (through the button below) an example CSV file to check the inputs required.", 
                                                      style = "width:100%;font-size:16px"),
                                                    downloadButton("Download_example_again", "Download Example CSV File",
                                                                   class = "LN"),
                                                    tags$head(tags$style(".LN{background-color:#5bc0de;} .LN{color: white;} .LN{width:250px}")), # background color and font color
                                                    br(),
                                                    br(),
                                                    p("Once uploaded, the first 5 rows of data will be displayed. Double check the inputs and press the submit button when ready.", 
                                                      style = "width:100%;font-size:16px"),
                                                    p(strong("If the file does not adhere to the instructions above, the website will disconnect due to an error in the file", 
                                                             style = "width:100%;font-size:16px;color:red")),
                                                    fileInput("file_new_2", "Choose CSV File",
                                                              multiple = F,
                                                              accept = c("text/csv",
                                                                         "text/comma-separated-values, text/plain",
                                                                         ".csv"
                                                              )
                                                              
                                                    ), 
                                                    tableOutput("contents_3_3"),
                                                    
                                                    # useShinyjs(), 
                                                    # extendShinyjs(text = "shinyjs.button = function() {window.scrollTo(0, 0)}"),
                                                    actionGroupButtons("final_data_SUBMIT_FINALL", "SUBMIT", status = "info", size = "lg")
                                                    
                                                    
                                                  ),
                                                  ), 
                                                  conditionalPanel(
                                                    condition = "input.new_AGE == 'age_no'", 
                                                    p("This assay is not age dependent, please
                                                      input the numeric value for each CSF data point.", style = "width:100%;font-size:17px"), 
                                                    numericInput(
                                                      "ABthreshold", "Enter your CSF AB1-42 pg/mL cut-off value here:", 
                                                      value = 1, min = 1, max = NA
                                                    ), 
                                                    numericInput(
                                                      "pTauThreshold", "Enter your CSF pTau pg/mL cut-off value here:", 
                                                      value = 1, min = 1, max = NA
                                                    ), 
                                                    numericInput(
                                                      "tTautheshold", "Enter your CSF tTau pg/mL cut-off value here:", 
                                                      value = 1, min = 1, max = NA
                                                    ), 
                                                    p("Please press the submit button when you are happy with the cut-offs inputted and follow
                                                      the instructions below.", style = "width:100%;font-size:16px"),
                                                    actionGroupButtons("THRESHOLDSUB", "SUBMIT CUT-OFFS", status = "info", size = "lg"),
                                                    br(),
                                                    p(strong("After inputting the cut-offs, please upload your CSV file through the following instructions:", style = "width:100%;font-size:16px;color:red")), 
                                                    p("The CSV file must include the following headers in the first row. Order is not important, however, please ensure the spelling is the same.", style = "width:100%;font-size:16px"),
                                                    tags$div(
                                                      tags$ul(
                                                        tags$li("Age - numerical input", style = "width:100%;font-size:16px"),
                                                        tags$li("apoe4 - categorical input where 1 is a carrier, 0 is non-carrier", style = "width:100%;font-size:16px"),
                                                        tags$li("Sex - categorial input including Male or Female.", style = "width:100%;font-size:16px"),
                                                        tags$li("Education_binary - binary input where 1 is over 12 years education and 0 under 12 years.", style = "width:100%;font-size:16px"),
                                                        tags$li("Diagnosis - categorial input for clinical diagnosis consisting of AD, MCI and HC", style = "width:100%;font-size:16px"),
                                                        tags$li("CSF.AB42 - numerical input", style = "width:100%;font-size:16px"),
                                                        tags$li("CSF.pTau - numerical input", style = "width:100%;font-size:16px"),
                                                        tags$li("CSF.tTau - numerical input", style = "width:100%;font-size:16px"),
                                                        tags$li("Sum.hippo - numerical input", style = "width:100%;font-size:16px"),
                                                        tags$li("Centiloid - numerical input", style = "width:100%;font-size:16px"),
                                                        tags$li("AB.status - categorial input consisting of negative or positive", style = "width:100%;font-size:16px"),
                                                        tags$li("pTau.status - categorial input consisting of negative or positive ", style = "width:100%;font-size:16px"),
                                                        tags$li("tTau.status - categorial input consisting of negative or positive", style = "width:100%;font-size:16px")
                                                      )
                                                    ),
                                                    p("The following data points must be numerical unless specificed above for categorical inputs. \n
                                                  No additional characters, such as $,#,&,*,(,!, are permitted. \n
                                                  Please ensure that capitalising of inputs and row headers are followed as above.", 
                                                      style = "width:100%;font-size:16px"),
                                                    p("You can also downloand (through the button below) an example CSV file to check the inputs required.", style = "width:100%;font-size:16px"),
                                                    downloadButton("DOWNLOADEXAMPLE2", "Download Example CSV File",
                                                                   class = "NEW"),
                                                    tags$head(tags$style(".NEW{background-color:#5bc0de;} .NEW{color: white;} .NEW{width:250px}")), # background color and font color
                                                    br(),
                                                    br(),
                                                    p("Once uploaded, the first 5 rows of data will be displayed. Double check the inputs and press the submit button when ready.", style = "width:100%;font-size:16px"),
                                                    p(strong("If the file does not adhere to the instructions above, the website will disconnect due to an error in the file", style = "width:100%;font-size:16px;color:red")),
                                                    fileInput("INPUTfile", "Choose CSV File",
                                                              multiple = F,
                                                              accept = c("text/csv",
                                                                         "text/comma-separated-values, text/plain",
                                                                         ".csv"
                                                              )
                                                              
                                                    ),
                                                    tableOutput("contents2"),
                                                    actionGroupButtons("SUMBITNEW", "SUBMIT", status = "info", size = "lg"),
                                                    br()
                                                  )
                                                )
                                              )
                                            )
                                            )
                                              
                                   ),
                                   
                                   tabPanel("Demographic Summary", 
                                            br(), 
                                            p("Demographic summaries for Group 1 split into ATN Burnham et al. classification groups. Standard
                                              deviation denoted by SD. Biomarker Positive and Negative Status defined by their respective cut-offs above.", style = "text-align: justify;width:100%;font-size:18px"),
                                            br(), tableOutput("table1")),
                                   
                                   ###########################
                                   ########################### subpanel
                                   tabPanel("2D Visualistion", 
                                            br(),
                                            htmlOutput("ASSAYreminder1"),
                                            br(),
                                            
                                            sidebarLayout(
                                              sidebarPanel(
                                                p("Select 2 Biomarkers for 2D plot"), 
                                                br(), 
                                                radioButtons("X_input", "X Value:", 
                                                             c("CSF AB1-42 pg/mL" = "AB", 
                                                               "CSF p-Tau pg/mL" = "pTau", 
                                                               "CSF t-Tau pg/mL" = "tTau"), 
                                                             selected = "AB"), 
                                                br(), 
                                                radioButtons("Y_input", "Y Value:", 
                                                             c("CSF AB1-42 pg/mL" = "AB", 
                                                               "CSF p-Tau pg/mL" = "pTau", 
                                                               "CSF t-Tau pg/mL" = "tTau"), 
                                                             selected = "pTau")
                                              ), 
                                              mainPanel(
                                                shinycssloaders:: withSpinner(
                                                  plotlyOutput("plot", height = 600)
                                                )
                                              )
                                            )
                                   ),
                                   
                                   #####################
                                   ##################### subpanel
                                   
                                   tabPanel("3D Visualisation", 
                                            br(),
                                            htmlOutput("ASSAYreminder2"),
                                            br(),
                                            sidebarLayout(
                                              sidebarPanel(
                                                selectInput(
                                                  "plotType", "Plot Type",
                                                  c("Normal 3D Scatter Plot" = "Normal",
                                                    "3D Scatter Plot with Cut-offs"= "planes"
                                                    )),
                                                
                                                conditionalPanel(
                                                  condition = "input.plotType == 'Normal'", 
                                                  p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                  actionGroupButtons(
                                                    inputIds = c("Static","rotating"), 
                                                    labels = list("Interactive Static", "Rotating Animation"), 
                                                    status = "info",
                                                    size = 'lg',
                                                    fullwidth = T)
                                                  
                                                ),
                                                
                                                conditionalPanel(
                                                  condition = "input.sample_or_real == 'sample' || input.newDataSUBMIT", 
                                                  conditionalPanel(
                                                    condition = "input.plotType == 'planes'",
                                                    selectInput(
                                                      "AGEgroup", "Select the specific Age Group",
                                                      c("60-70",
                                                        "70+")),
                                                  ),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.plotType == 'planes'",
                                                    
                                                    conditionalPanel(
                                                      condition = "input.AGEgroup == '60-70'",
                                                      p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load or update from the previous plot
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                      actionGroupButtons(
                                                        inputIds = c("staticP2", "rotateP2"), 
                                                        labels = list("Interactive Static", "Rotating Animation"),
                                                        status = "info",
                                                        size = 'lg',
                                                        fullwidth = T
                                                      ), 
                                                      br(),
                                                      p("The thresholds are as above. The user can remove the cut-offs to the right side of the plot at their discretion",style ="text-align: justify;width:100%;font-size:16px"),
                                                      # actionButton("animG1P2", "Click here for animated version", icon=icon("fas fa-sync-alt"))
                                                    ), 
                                                    
                                                    conditionalPanel(
                                                      condition = "input.AGEgroup == '70+'", 
                                                      p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load or update from the previous plot
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                      actionGroupButtons(
                                                        inputIds = c("staticP3", "rotateP3"), 
                                                        labels = list("Interactive Static", "Rotating Animation"), 
                                                        status = "info",
                                                        size = 'lg',
                                                        fullwidth = T
                                                      ),
                                                      br(),
                                                      p("The thresholds are as above. The user can remove the cut-offs to the right side of the plot at their discretion",style ="text-align: justify;width:100%;font-size:16px")
                                                      # actionButton("animG1P3", "Click here for animated version", icon=icon("fas fa-sync-alt"))
                                                    )
                                                  )
                                                
                                                ),
                                                conditionalPanel(
                                                  condition = "input.SUMBITNEW",
                                                  conditionalPanel(
                                                    condition = "input.plotType == 'planes'", 
                                                    p(strong("The assay inputted by the user is not age dependent.",
                                                             style ="text-align: justify;width:100%;font-size:16px")), 
                                                    p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load or update from the previous plot
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                    actionGroupButtons(
                                                      inputIds = c("age_no_dependence_static", "age_no_dependence_rotating"), 
                                                      labels = list("Interactive Static", "Rotating Animation"), 
                                                      status = "info",
                                                      size = 'lg',
                                                      fullwidth = T
                                                    ),
                                                    br(),
                                                    
                                                    
                                                  )
                                                )
                                                
                                              ),
                                              
                                              mainPanel(br(), br(),shinycssloaders:: withSpinner(
                                                plotlyOutput("PLOTLYOUTPUT", height = 600)))
                                            )
                                            
                                            
                                   ),
                                   
                                   ##################
                                   ################## subpanel
                                   
                                   tabPanel("A+/T+/N+ Visualisation",
                                            br(),
                                            htmlOutput("ASSAYreminder3"),
                                            br(),
                                            sidebarLayout(
                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.sample_or_real == 'sample' || input.newDataSUBMIT",
                                                  pickerInput("CUBE1", "Select the Specific Age Group", 
                                                              choices = c("60-70", "70+"),
                                                              selected = "60-70", 
                                                              multiple = F),
                                                  conditionalPanel(
                                                    condition = "input.CUBE1 == '60-70'", 
                                                    p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load or update from the previous plot
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                    actionGroupButtons(
                                                      inputIds = c("ATNStatic", "ATNrotating"), 
                                                      labels = list("Interactive Static", "Rotating Animation"), 
                                                      status = "info",
                                                      size = 'lg',
                                                      fullwidth = T
                                                    )
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.CUBE1 == '70+'", 
                                                    p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load or update from the previous plot
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                    actionGroupButtons(
                                                      inputIds = c("ATNStaticP2", "ATNrotatingP2"), 
                                                      labels = list("Interactive Static", "Rotating Animation"), 
                                                      status = "info",
                                                      size = 'lg',
                                                      fullwidth = T
                                                    )
                                                  ),
                                                  br(),
                                                  p("In this plot, the selected individuals (in red) are those that 
                                                  meet all 3 biomarker's definition of being biomarker positive (in the A+/T+/N+ area). The unselected (in grey) are those that don't meet
                                                  all 3 definitions of being biomarker positive.",style ="text-align: justify;width:100%;font-size:18px"),
                                                  br(),
                                                  p("Alzheimer's disease is not as simple as these binary classifications and cut-offs. In previous 2D and 3D plots, there is a lot overlap between data points
                                                  of different classifications, emphasising that AD is not described by a specific cut-off, but a pathway that is unique to each individual.",style ="text-align: justify;width:100%;font-size:18px"), 
                                                  p(strong("Please click the button below to identify borderline individuals.",style ="text-align: justify;width:100%;font-size:18px")), 
                                                  actionButton("g1cube", "Borderline A+/T+/N+ Visualisation", icon=icon("fas fa-cubes")), 
                                                  bsModal(id = "G1CUBE", 
                                                          title = "Borderline A+/T+/N+ Visualisation", 
                                                          trigger = "g1cube", 
                                                          size = "large", 
                                                          p("In this plot, those who met the requirements of two  cut-offs are shown. For 70+ Age Group, xx% of the data have meet only 2 of the 3 cut-offs. For 60-70 Age Group, xx% of the data 
                                                          have meet 2 out of the 3 cut-offs. This plot highlights the pitfalls of classification of individuals from binarized cut-offs and demonstrates the inappropriateness of 
                                                          categorising individuals based on these predetermined cut-offs.",style ="text-align: justify;width:100%;font-size:18px"),
                                                          p("As before, please select the specific age group to view the plot. The plots produced below are an interactive static plot for the user
                                                          to move at their discretion.",style ="text-align: justify;width:100%;font-size:18px"), 
                                                          selectInput("cubeg1input", "Select the Specific Age Group:",
                                                                      choices = c("60-70", "70+"),
                                                                      selected = "70+",
                                                                      multiple = F),
                                                          shinycssloaders:: withSpinner(plotlyOutput("G1ExtraCube", height = 600))
                                                  )
                                                  
                                                ), 
                                                conditionalPanel(
                                                  condition = "input.SUMBITNEW", 
                                                  p(strong("The assay inputted by the user is not age dependent.",
                                                           style ="text-align: justify;width:100%;font-size:16px")), 
                                                  p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load or update from the previous plot
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                  actionGroupButtons(
                                                    inputIds = c("no_age_ATN", "no_age_ATN_rotating"), 
                                                    labels = list("Interactive Static", "Rotating Animation"), 
                                                    status = "info",
                                                    size = 'lg',
                                                    fullwidth = T
                                                  ), 
                                                  br(),
                                                  p("In this plot, the selected individuals (in red) are those that 
                                                  meet all 3 biomarker's definition of being biomarker positive (in the A+/T+/N+ area). The unselected (in grey) are those that don't meet
                                                  all 3 definitions of being biomarker positive.",style ="text-align: justify;width:100%;font-size:18px"),
                                                  br(),
                                                  p("Alzheimer's disease is not as simple as these binary classifications and cut-offs. In previous 2D and 3D plots, there is a lot overlap between data points
                                                  of different classifications, emphasising that AD is not described by a specific cut-off, but a pathway that is unique to each individual.",style ="text-align: justify;width:100%;font-size:18px"), 
                                                  p(strong("Please click the button below to identify borderline individuals.",style ="text-align: justify;width:100%;font-size:18px")), 
                                                  actionButton("ANothercube", "Borderline A+/T+/N+ Visualisation", icon=icon("fas fa-cubes")), 
                                                  bsModal(id = "ANOTHERG1CUBE", 
                                                          title = "Borderline A+/T+/N+ Visualisation", 
                                                          trigger = "ANothercube", 
                                                          size = "large", 
                                                          p("In this plot, those who met the requirements of two  cut-offs are shown. This plot highlights the pitfalls of classification of individuals from binarized cut-offs and demonstrates the inappropriateness of 
                                                          categorising individuals based on these predetermined cut-offs.",style ="text-align: justify;width:100%;font-size:18px"),
                                                          p("The plots produced below are an interactive static plot for the user
                                                          to move at their discretion.",style ="text-align: justify;width:100%;font-size:18px"), 
                                                          shinycssloaders:: withSpinner(plotlyOutput("no_age_dependence_additional_cube", height = 600))
                                                  )
                                                )
                                                
                                                
                                                
                                              ), 
                                              mainPanel(br(), br(), shinycssloaders::withSpinner(
                                                plotlyOutput("plot5.P3", height = 600)
                                              )
                                              )# 10 blue 12
                                            ) # 15 blue, 26 pink ish for 70+
                                   ),
                                   
                                   ######################
                                   ###################### subpanel
                                   
                                   tabPanel("Add in your own data point!",
                                            br(),
                                            htmlOutput("ASSAYreminder4"),
                                            br(),
                                            sidebarLayout(
                                              sidebarPanel(
                                                p('Enter your own ATN biomarker data to see your standing in the cohort',style ="text-align: justify;width:100%;font-size:18px"),
                                                br(),
                                                conditionalPanel(
                                                  condition = "input.sample_or_real == 'sample' || input.newDataSUBMIT",
                                                  numericInput("age1", "Please enter your age:", 
                                                               50,
                                                               min = 1, 
                                                               max = 200),
                                                  numericInput("obs", "CSF AB1-42 pg/mL", 
                                                               800,
                                                               min = 1,
                                                               max = 2000),
                                                  numericInput("ptau", "CSF pTau pg/mL", 
                                                               30,
                                                               min = -100,
                                                               max = 1000), 
                                                  numericInput("ttau", "CSF tTau pg/mL", 
                                                               300, 
                                                               min = 1, 
                                                               max = 1000), 
                                                  pickerInput("DATAVIS", "Select the form of visualisation:", 
                                                              choices = c("3D Plot", "A+/T+/N+ Visualisation"),
                                                              selected = "3D Plot",
                                                              multiple = F),
                                                ),
                                                conditionalPanel(
                                                  condition = "input.SUMBITNEW", 
                                                  numericInput("no_age_dependence", "Please enter your age:", 
                                                               50,
                                                               min = 1, 
                                                               max = 200),
                                                  numericInput("AB_no_age_input", "CSF AB1-42 pg/mL", 
                                                               800,
                                                               min = 1,
                                                               max = 2000),
                                                  numericInput("ptau_no_age_input", "CSF pTau pg/mL", 
                                                               30,
                                                               min = -100,
                                                               max = 1000), 
                                                  numericInput("ttau_no_age_input", "CSF tTau pg/mL", 
                                                               300, 
                                                               min = 1, 
                                                               max = 1000), 
                                                  pickerInput("no_age_dependence", "Select the form of visualisation:", 
                                                              choices = c("3D Plot" = "no_age_3D", "A+/T+/N+ Visualisation" = "no_age_atn_AYOB"),
                                                              selected = "3D Plot",
                                                              multiple = F),
                                                ),
                                                p("The plots produced are an interactive static plot for the user
                                                          to move at their discretion.", style = "width:100%;font-size:18px")
                                              ), 
                                              mainPanel(br(), br(),
                                                        conditionalPanel(
                                                          condition = "input.sample_or_real == 'sample' || input.newDataSUBMIT", 
                                                          shinycssloaders:: withSpinner(
                                                            plotlyOutput("OWNBIOMARKERS", height = 600)
                                                          )
                                                        ), 
                                                        conditionalPanel(
                                                          condition = "input.SUMBITNEW", 
                                                          shinycssloaders:: withSpinner(
                                                            plotlyOutput("ADDINDATA_NO_AGE", height = 600)
                                                          )
                                                        )
                                                         )
                                            )
                                   ), 
                                   tabPanel("Statistical Modelling",
                                            br(),
                                            htmlOutput("ASSAYreminder5"),
                                            br(),
                                            p(strong("As the synthetic data is randomly generated, the statisical models that are fitted and corresponding
                                              regression coefficients/p-values are meaningless and do not reflect any features of the AIBL's ATN biomarkers.", style = "width:100%;font-size:18px;color:red")),
                                            p("In this tab, 3 models are reported on:",style ="width:100%;font-size:18px"),
                                            tags$div(
                                              tags$ol(
                                                tags$li("Linear models for demographic characteristics on AT(N) biomarkers",style ="width:100%;font-size:18px"), 
                                                tags$li("Logistic models for demographic characteristics on AT(N) biomarkers",style ="width:100%;font-size:18px"), 
                                                tags$li("Linear models for the interplay between AT(N) biomarkers",style ="width:100%;font-size:18px")
                                              )
                                            ),
                                            p("Linear regression is used to predict continuous dependent variables using a given
                                            set of independent variables and logistic is used to predict the binary dependent variables. For more information on Linear and Logistic regressions,
                                            please see ",a(href = "https://www.javatpoint.com/linear-regression-vs-logistic-regression-in-machine-learning#:~:text=Linear%20regression%20is%20used%20to,given%20set%20of%20independent%20variables.&text=The%20output%20for%20Linear%20Regression%20must%20be%20a%20continuous%20value,as%20price%2C%20age%2C%20etc.",
                                                           'here', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end"),style ="text-align: justify;width:100%;font-size:18px"),
                                            p("The outcome for logistic regressions is the status (positive or negative) for the corresponding ATN biomarker.  
                                            The independent variables that are considered for both regressions are APOE-e4 allele carrier status, sex, age and education. Education was categorised as under and over 12 years of education. 
                                            The side panel below breaks down this tab further into components comprising of
                                               visualisation, model summaries and discussion.",style ="text-align: justify;width:100%;font-size:18px"),
                                            br(),
                                            navlistPanel(
                                              tabPanel("Visualisation", 
                                                       p("Visualising Boxplots, 2D and 3D plots can be a useful aid to understand the fundamental variable in a regression.",style ="text-align: justify;width:100%;font-size:18px"), 
                                                       p("Below, you can select the type of visual aid (Boxplots, 2D Scatter Plots or 3D Scatter Surface Plots).",style ="text-align: justify;width:100%;font-size:18px"), 
                                                       selectInput("REGRESPLOT", "Select the Type of Visual:", 
                                                                   c("Boxplots" = "boxes", 
                                                                     "2D Scatter Plots" = "scatter",
                                                                     "3D Scatter with Regression Surfaces" = "surfaces"
                                                                   )),
                                                       # only show this panel if the plot type is boxplots
                                                       conditionalPanel(
                                                         condition = "input.REGRESPLOT == 'boxes'", 
                                                         p("You can view the corresponding boxplots for each specific biomarker.",style ="text-align: justify;width:100%;font-size:18px"), 
                                                         selectInput(
                                                           "BIOMARKER", "Select the specific Biomarker:", 
                                                           c("CSF AB1-42 pg/mL", 
                                                             "CSF pTau pg/mL", 
                                                             "CSF tTau pg/mL")
                                                         ), 
                                                         br(),
                                                         shinycssloaders:: withSpinner(plotOutput("G1BOXPLOTS", width = 900, height = 700))
                                                       ), 
                                                       conditionalPanel(
                                                         condition = "input.REGRESPLOT == 'scatter'",
                                                         p("You can view the corresponding 2D Scatter Plots for each specific biomarker.",style ="text-align: justify;width:100%;font-size:18px"), 
                                                         
                                                         selectInput(
                                                           "BIOMARKER2", "Select the specific Biomarker:", 
                                                           c("CSF AB1-42 pg/mL", 
                                                             "CSF pTau pg/mL", 
                                                             "CSF tTau pg/mL")
                                                         ), 
                                                         br(),
                                                         shinycssloaders:: withSpinner(plotlyOutput("SCATTERPLOT", width = 1000, height = 700))
                                                         
                                                       ),
                                                       
                                                       conditionalPanel(
                                                         condition = "input.REGRESPLOT == 'surfaces'", 
                                                         p("You can view the corresponding 3D scatter plot surface for each of the specific demographic Characteristics.",style ="text-align: justify;width:100%;font-size:18px"), 
                                                         selectInput(
                                                           "DEMOGRAPHIC", "Select the specific Demographic Characteristic:", 
                                                           c("APOE-e4 allele", 
                                                             "Sex", 
                                                             "Age",
                                                             "Education")
                                                         ), 
                                                         p("In order to view predictive planes, age was broken down into over 72.5 and under 72.5. Education was categorised as under and over 12 years of education.",style ="text-align: justify;width:100%;font-size:18px"),
                                                         p("The plots produced are an interactive static plot for the user
                                                          to move at their discretion.", style = "width:100%,font-size:17px"),
                                                         br(),
                                                         shinycssloaders:: withSpinner(plotlyOutput("SURFACEPLOT", width = 1000, height = 700))
                                                         
                                                         
                                                       ), 
                                                       
                                                       
                                                       
                                              ),
                                              tabPanel("Model Summaries",
                                                       p("Please select which of the three models to view:", style ="text-align: justify;width:100%;font-size:18px"), 
                                                       selectInput("ModelregG1", "Select the model reported on:", 
                                                                   c("1. Linear model for demographic characteristics" = "LING1", 
                                                                     "2. Logistic model for demographic characteristics" = "LOGG1", 
                                                                     "3. Linear model for biomarker interplay" = "ATI1")
                                                       ), 
                                                       conditionalPanel(
                                                         condition = "input.ModelregG1 == 'LING1'",
                                                         pickerInput("LINEAR1", "Select the dependent biomarker",
                                                                     choices = c("CSF AB1-42 pg/mL", "CSF pTau pg/mL", "CSF tTau pg/mL"), 
                                                                     selected = "CSF AB1-42 pg/mL", 
                                                                     multiple = F), 
                                                         shinycssloaders:: withSpinner(tableOutput("LinearREG"))
                                                         
                                                         
                                                       ), 
                                                       conditionalPanel(
                                                         condition = "input.ModelregG1 == 'LOGG1'",
                                                         pickerInput("LOG1", "Select the dependent biomarker",
                                                                     choices = c("CSF AB1-42 pg/mL", "CSF pTau pg/mL", "CSF tTau pg/mL"), 
                                                                     selected = "CSF AB1-42 pg/mL", 
                                                                     multiple = F), 
                                                         shinycssloaders:: withSpinner(tableOutput("Logistictable"))
                                                         
                                                         
                                                       ), 
                                                       conditionalPanel(
                                                         condition = "input.ModelregG1 == 'ATI1'",
                                                         pickerInput("interplay", "Select the dependent biomarker",
                                                                     choices = c("CSF AB1-42 pg/mL", "CSF pTau pg/mL", "CSF tTau pg/mL"), 
                                                                     selected = "CSF AB1-42 pg/mL", 
                                                                     multiple = F), 
                                                         shinycssloaders:: withSpinner(tableOutput("BiomarkerReg"))
                                                         
                                                         
                                                       )

                                              ),
                                              
                                              # ),
                                              
                                              tabPanel("Discussion", 
                                                       p(strong("Please note that no discussion on statistical modelling in this App will be given as this data is randomly generated and does 
                                                         not hold any clinicial meaning.", style = "width:100%; font-size:18px;color:red"))
                                              )
                                            )
                                            
                                            
                                            
                                   )
                       )
                       
              ),
              
              # Panel 3 -------------------------------------
              tabPanel(title ="Group 2: Combined Modality Data",
                       br(), 
                         box(solidHeader = F, 
                             collapsible = F, 
                             width = 12, 
                             fluidRow(
                               column(width = 9, 
                                      conditionalPanel(
                                        condition = "input.SUMBITNEW", 
                                        textOutput("NewCUTOFFLabels2"),
                                        tags$head(tags$style("#NewCUTOFFLabels2{
                                 font-size: 18px;color:#6C3483;font-weight:bold                             }"
                                        )
                                        ),
                                        br(),
                                      ),
                                      conditionalPanel(
                                        condition = "input.newDataSUBMIT", 
                                        p("You have selected to use INNOTEST assay cut-off values for the CSF data. These points are the same as 
                                        used in the supplied data set and have been updated across the app. To return to supplied data or make a new selection, please
                                        refresh the website to clear all previous inputs.", style = 'text-align: justify;width:100%;font-size:18px;color:#6C3483;font-weight:bold ')
                                      ),
                                      p("Similarly to Group 1, the NIA-AA Framework can be collapsed according to the groups established in Clifford et al. [3]. The groups
                                    are shown in the table below.",style ="text-align: justify;width:100%;font-size:18px"), 
                                      # HTML('<center><img src="Capture.JPG"></center>'), 
                                      HTML('<style type="text/css">
.tg  {border-collapse:collapse;border-color:#ccc;border-spacing:0;}
.tg td{background-color:#fff;border-color:#ccc;border-style:solid;border-width:1px;color:#333;
  font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{background-color:#f0f0f0;border-color:#ccc;border-style:solid;border-width:1px;color:#333;
  font-family:Arial, sans-serif;font-size:14px;font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-mcqj{border-color:#000000;font-weight:bold;text-align:left;vertical-align:top}
.tg .tg-73oq{border-color:#000000;text-align:left;vertical-align:top}
.tg .tg-xwyw{border-color:#000000;text-align:center;vertical-align:middle}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-mcqj">NIA-AA Research Framework</th>
    <th class="tg-mcqj">Clifford Classification</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-73oq">A+/T+/N+</td>
    <td class="tg-xwyw" rowspan="2">Stage 2, Clinically Asymptomatic</td>
  </tr>
  <tr>
    <td class="tg-73oq">A+/T+/N-</td>
  </tr>
  <tr>
    <td class="tg-73oq">A+/T-/N+</td>
    <td class="tg-xwyw" rowspan="2">Stage 1, Preclinical AD Stage</td>
  </tr>
  <tr>
    <td class="tg-73oq">A+/T-/N-</td>
  </tr>
  <tr>
    <td class="tg-73oq">A-/T+/N+</td>
    <td class="tg-xwyw" rowspan="3">SNAP (Suspected Non-Alzheimers <br>Disease Person)</td>
                                      </tr>
                                      <tr>
                                      <td class="tg-73oq">A-/T-/N+</td>
                                      </tr>
                                      <tr>
                                      <td class="tg-73oq">A-/T+/N-</td>
                                      </tr>
                                      <tr>
                                      <td class="tg-73oq">A-/T-/N-</td>
                                      <td class="tg-xwyw">MCI unlikely due to AD</td>
                                      </tr>
                                      </tbody>
                                      </table>'),
                                      p("Table 4: Group 2 Classifications adapted from Clifford et al. [3]"),
                                      br(),
                                      p("In this group, AB-Amyloid plaques (A) are quantified by the Centiloid value measured through PET imaging. Tau (T) was measured through phosphor-Tau CSF p-181 and Neurodegeneration (N)
                                      was quantified via the hippocampus volume, measured  by MRI imaging.",style ="text-align: justify;width:100%;font-size:18px"),
                                      p("The thresholds for Amyloid, Tau and Neurodegeneration were derived from literature. The Centiloid cut-off was cited in Villemagne et al. [13],Dore et al. [6] and
                                    Bourgeat et al. [1]. The CSF pTau 
                                    cut-off is specified in Doecke et al.[6] and Li et al.[9]. The cut-off for hippocampus is established in Clifford et al. [4]. For more imformation on the derivation of the 
                                    hippocampus, please click the View Derivation button below.",style ="text-align: justify;width:100%;font-size:18px"), 
                                      p("Group 2 is therefore described below with the 
                                    corresponding cut-off thresholds:",style ="text-align: justify;width:100%;font-size:18px"),
                                      conditionalPanel(
                                        condition = "input.sample_or_real == 'sample' || input.newDataSUBMIT", 
                                        tags$div(
                                          tags$ul(
                                            tags$li("A: is quantified by Centiloid, positive participants have values greater than 20",style ="text-align: justify;width:100%;font-size:18px"),
                                            tags$li("T: is quantified by phosphor-Tau CSF p-181, positive participants aged 60-70 have values greater than 59.23 and individuals aged 70+ have values greater than 73.83",style ="text-align: justify;width:100%;font-size:18px"), 
                                            tags$li("N: is quantified by hippocampus volume, positive participants have values less than 5.4.",style ="text-align: justify;width:100%;font-size:18px")
                                          )
                                        ),
                                      ), 
                                      conditionalPanel(
                                        condition = "input.SUMBITNEW", 
                                        tags$div(
                                          tags$ul(
                                            tags$li(textOutput("CENTILOIDTEXT"), tags$head(tags$style("#CENTILOIDTEXT{
                                 font-size: 18px;                             }"
                                            )
                                            )
                                            ), 
                                            tags$li(textOutput("PTAU2TEXT"), tags$head(tags$style("#PTAU2TEXT{
                                 font-size: 18px;                             }"
                                            )
                                            )
                                            ), 
                                            tags$li(textOutput("HIPPOCAMPUSTEXT"), tags$head(tags$style("#HIPPOCAMPUSTEXT{
                                 font-size: 18px;                             }"
                                            )
                                            )
                                            ),
                                          )
                                        ),
                                      ),
                                      p("This tab is broken down into seven components; a demographic summary, a reactive 2D plot, 
                                      interactive 3D plots (with the options of adding in the positive thresholds for the 
                                      biomarkers), an A+/T+/N+ positive area visual, Add in your own data point option and statistical modelling.",style ="text-align: justify;width:100%;font-size:18px"),
                                      actionButton("hipplot", "View Derivation", icon=icon("bar-chart")),
                                      br(),
                                      # Shiny BS Modal to display the Dataset inside a MOdal with spinner added 
                                      bsModal(id = "HIPPLOT", 
                                              title = "Derivation of hippocampus Cut-off", 
                                              trigger = "hipplot", 
                                              size = "large", 
                                              p("The hippocampus volume cut-off was derived from mixture methods. This method was established in Clifford et al. [4].
                                            The cut-off is the intersection (in black) of the normal curves of the hippocampus volume for Alzheimer's Disease (AD) and Healthy Cognitive (HC), as shown below.",style ="text-align: justify;width:100%;font-size:18px"),
                                              textOutput("texthippo"),
                                              tags$head(tags$style("#texthippo{
                                 font-size: 18px;                                 }"
                                              )
                                              ),
                                              plotOutput("plot_gg"), plotOutput('intersect_plot')
                                      ),
                                      br(),
                               ), 
                               column(width = 3,
                                      align = "left",
                                      tags$a(href = "https://biofinder.se/data-biomarkers/tau-pet-imaging/",
                                             img(src = "TauPET.gif", height = "230px", width = "260px",
                                                 title = "Adaption from Sweish Biofinder Study")
                                      ),
                                      p("Figure 5: This image gives a visual representation of a PET scan. In a PET scan, radioactive substances, known as
                                      radiotracers are used to visualise and measure changes in metabolic process. This animation shows a PET scan
                                      of a patient with Alzheimer's Disease measuring Tau (indicated by red sections). Source: the Swedish BioFINDER study [12].", style = "text-align: justify;width:260px"))
                             )
                       ), 
                       
                       # Sidebar layout with input and output definitions
                       tabsetPanel(type = "tabs",
                                   tabPanel("Demographic Summary", br(),
                                            p("Demographic summaries for Group 2 split into ATN Clifford et al. classification groups. Standard
                                              deviation denoted by SD.",style ="text-align: justify;width:100%;font-size:18px"),
                                            br(), tableOutput("table.2")),
                                   
                                   #########################
                                   ######################### subpanel
                                   
                                   tabPanel("2D Visualisation", 
                                            br(),
                                            htmlOutput("ASSAYreminder6"),
                                            br(),
                                            sidebarLayout(
                                              sidebarPanel(
                                                p("Select 2 Biomarkers for 2D plot"), 
                                                br(), 
                                                tags$div(
                                                  id="X_2_Input", class="form-group shiny-input-radiogroup shiny-input-container",
                                                  tags$label(class="control-label", `for`="X_2_Input", "X Value:"),
                                                  tags$div(class="shiny-options-group",
                                                           tags$div(class="radio",
                                                                    tags$label(
                                                                      tags$input(type="radio", name="X_2_Input", value="Centiloid", checked="checked",
                                                                                 tags$span(HTML("Centiloid")))
                                                                    )
                                                           ),
                                                           tags$div(class="radio",
                                                                    tags$label(
                                                                      tags$input(type="radio", name="X_2_Input", value="ptau",
                                                                                 tags$span(HTML("CSF pTau pg/mL")))
                                                                    )
                                                           ),
                                                           tags$div(class="radio",
                                                                    tags$label(
                                                                      tags$input(type="radio", name="X_2_Input", value="Hippogroup2X1",
                                                                                 tags$span(HTML("Hippocampus mL<sup>3</sup>")))
                                                                    )
                                                           )
                                                  )
                                                ),
                                                
                                                br(), 

                                                tags$div(
                                                  id="Y_2_input", class="form-group shiny-input-radiogroup shiny-input-container",
                                                  tags$label(class="control-label", `for`="Y_2_input", "Y Value:"),
                                                  tags$div(class="shiny-options-group",
                                                           tags$div(class="radio",
                                                                    tags$label(
                                                                      tags$input(type="radio", name="Y_2_input", value="Centiloid",
                                                                                 tags$span(HTML("Centiloid")))
                                                                    )
                                                           ),
                                                           tags$div(class="radio",
                                                                    tags$label(
                                                                      tags$input(type="radio", name="Y_2_input", value="ptau",
                                                                                 tags$span(HTML("CSF pTau pg/mL")))
                                                                    )
                                                           ),
                                                           tags$div(class="radio",
                                                                    tags$label(
                                                                      tags$input(type="radio", name="Y_2_input", value="hipGroup2",checked = "checked",
                                                                                 tags$span(HTML("Hippocampus mL<sup>3</sup>")))
                                                                    )
                                                           )
                                                  )
                                                )
                                                
                                              ), 
                                              mainPanel(br(), shinycssloaders:: withSpinner(
                                                plotlyOutput("plot2", height = 600)
                                              )
                                              )
                                            )
                                   ),
                                   
                                   #########################
                                   ######################### subpanel
                                   
                                   tabPanel("3D Visualisation", 
                                            br(),
                                            htmlOutput("ASSAYreminder7"),
                                            br(),
                                            sidebarLayout(
                                              sidebarPanel(
                                                selectInput(
                                                  "plotType2", "Plot Type",
                                                  c("Normal 3D Scatter Plot" = "Normal2",
                                                    "3D Scatter Plot with Cut-offs"= "planes2")),
                                                
                                            
                                                conditionalPanel(
                                                  condition = "input.plotType2 == 'Normal2'", 
                                                  p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                  actionGroupButtons(
                                                    inputIds = c("G2staticP1","G2RotateP1"), 
                                                    labels = list("Interactive Static", "Rotating Animation"), 
                                                    status = "info",
                                                    size = 'lg',
                                                    fullwidth = T)
                                                ), 
                                                conditionalPanel(
                                                  condition = "input.plotType2 == 'planes2'", 
                                                  conditionalPanel(
                                                    condition = "input.sample_or_real == 'sample'||input.newDataSUBMIT", 
                                                    selectInput(
                                                      "AGEgroup2", "Select the specific Age Group",
                                                      c("60-70",
                                                        "70+")), 
                                                    conditionalPanel(
                                                      condition = "input.AGEgroup2 == '60-70'", 
                                                      p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load or update
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                      actionGroupButtons(
                                                        inputIds = c("G2STATICP2", "G2ROTATEP2"), 
                                                        labels = list("Interactive Static", "Rotating Animation"), 
                                                        status = "info", 
                                                        size = "lg", 
                                                        fullwidth = T
                                                      ),
                                                      br(),
                                                      p("The thresholds are followed above.The user can remove the cut-offs to the right side of the plot at their discretion.",
                                                        style ="text-align: justify;width:100%;font-size:18px")
                                                    ), 
                                                    conditionalPanel(
                                                      condition = "input.AGEgroup2 == '70+'", 
                                                      p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load or update
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                      actionGroupButtons(
                                                        inputIds = c("G2STATICP3", "G3ROTATINGP3"), 
                                                        labels = list("Interactive Static", "Rotating Animation"),
                                                        status = "info", 
                                                        size = "lg", 
                                                        fullwidth = T
                                                      ),
                                                      br(),
                                                      p("The thresholds are followed above.The user can remove the cut-offs to the right side of the plot at their discretion.",
                                                        style ="text-align: justify;width:100%;font-size:18px")
                                                    )
                                                  ), 
                                                  conditionalPanel(
                                                    condition = "input.SUMBITNEW", 
                                                    p(strong("The assay inputted by the user is not age dependent.",
                                                             style ="text-align: justify;width:100%;font-size:16px")), 
                                                    p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load or update
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                    actionGroupButtons(
                                                      inputIds = c("no_age_G2", "no_age_G2_rotating"), 
                                                      labels = list("Interactive Static", "Rotating Animation"),
                                                      status = "info", 
                                                      size = "lg", 
                                                      fullwidth = T
                                                    ),
                                                    br(),
                                                    p("The thresholds are followed above.The user can remove the cut-offs to the right side of the plot at their discretion.",
                                                      style ="text-align: justify;width:100%;font-size:18px")
                                                  )
                                                )
                                                
                                              ),
                                              
                                              mainPanel(br(), br(), shinycssloaders:: withSpinner(
                                                plotlyOutput("CONDITIONPLOT", height = 600))
                                                
                                              )
                                            )
                                            
                                            
                                   ),
                                   
                                   #########################
                                   ######################### subpanel
                                   
                                   tabPanel("A+/T+/N+ Visualisation",
                                            br(),
                                            htmlOutput("ASSAYreminder8"),
                                            br(),
                                            sidebarLayout(
                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.sample_or_real == 'sample' || input.newDataSUBMIT",
                                                  pickerInput("CUBE2", "Select the Specific Age Group", 
                                                              choices = c("60-70", "70+"), 
                                                              selected = "60-70", 
                                                              multiple = F), 
                                                  conditionalPanel(
                                                    condition = "input.CUBE2 == '60-70'", 
                                                    p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load or update
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                    actionGroupButtons(
                                                      inputIds = c("CUBESTATICG2", "CUBEROTATEG2"), 
                                                      labels = list("Interactive Static", "Rotating Animation"), 
                                                      status = "info", 
                                                      size = "lg", 
                                                      fullwidth = T
                                                    )
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.CUBE2 == '70+'", 
                                                    p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load or update
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                    actionGroupButtons(
                                                      inputIds = c("G2P2CUBESTATIC", "G2P2Rotate"), 
                                                      labels = list("Interactive Static", "Rotating Animation"),
                                                      status = "info",
                                                      size = "lg", 
                                                      fullwidth = T
                                                    )
                                                  ),
                                                  br(),
                                                  p("In the visual aid, the selected individuals are those that 
                                                  meet all 3 biomarker's definition of being positive. The unselected are those they don't meet
                                                  all 3 definitions of being positive.",style ="text-align: justify;width:100%;font-size:18px"),
                                                  br(),
                                                  br(),
                                                  p("Alzheimer's disease is not as simple as these binary classifications and cut-offs. In previous 2D and 3D plots, there is a lot overlap between data points
                                                  of different classifications. This emphasises that AD is not described by a specific cut-off, but a pathway that is unique to each individual.",style ="text-align: justify;width:100%;font-size:18px"), 
                                                  # p("Instead of a standard A+/T+/N+ visualisation, we can add individuals who met 2 of the 3 cut-offs."), 
                                                  p(strong("Please click the button below to identify borderline individuals."), style = "text-align: justify;width:100%;font-size:18px"), 
                                                  actionButton("cubic", "Borderline A+/T+/N+ Visualisation", icon=icon("fas fa-cubes")), 
                                                  br(), 
                                                  bsModal(id = "CUBEG21", 
                                                          title = "Borderline A+/T+/N+ Visualisation", 
                                                          trigger = "cubic", 
                                                          size = "large", 
                                                          p("In this plot, those who met the requirements of two cut-offs and their respective 
                                                          classification are shown. For 70+ Age Group, xx% of the data have meet only 2 of the 3 cut-offs. For 60-70 Age Group, xx% of the data 
                                                          have meet 2 out of the 3 cut-offs. This plot highlights the pitfalls of classification of individuals from binarized cut-offs and demonstrates the inappropriateness of 
                                                          categorising individuals based on these predetermined cut-offs",style ="text-align: justify;width:100%;font-size:18px"),
                                                          p("As before, please select the specific age group to view the plot. The plots produced below are an interactive static plot for the user
                                                          to move at their discretion.",style ="text-align: justify;width:100%;font-size:18px"), 
                                                          selectInput("PopCube", "Select the Specific Age Group:",
                                                                      choices = c("60-70", "70+"),
                                                                      selected = "70+",
                                                                      multiple = F),
                                                          shinycssloaders:: withSpinner(plotlyOutput("EXTRACUBE", height = 600))
                                                  )
                                                ), 
                                                conditionalPanel(
                                                  condition = "input.SUMBITNEW", 
                                                  p(strong("The assay inputted by the user is not age dependent.",
                                                           style ="text-align: justify;width:100%;font-size:16px")), 
                                                  p(strong("Please select whether you would like to view as an interactive static plot or a rotating animation. Note: the plot will not load or update
                                                    until a selection is made.", style ="text-align: justify;width:100%;font-size:16px;color:red")),
                                                  actionGroupButtons(
                                                    inputIds = c("no_age_ATN_group_static", "no_age_ATN_group_rotating"), 
                                                    labels = list("Interactive Static", "Rotating Animation"),
                                                    status = "info",
                                                    size = "lg", 
                                                    fullwidth = T
                                                  ), 
                                                  br(),
                                                  p("In the visual aid, the selected individuals are those that 
                                                  meet all 3 biomarker's definition of being positive. The unselected are those they don't meet
                                                  all 3 definitions of being positive.",style ="text-align: justify;width:100%;font-size:18px"),
                                                  br(),
                                                  br(),
                                                  p("Alzheimer's disease is not as simple as these binary classifications and cut-offs. In previous 2D and 3D plots, there is a lot overlap between data points
                                                  of different classifications. This emphasises that AD is not described by a specific cut-off, but a pathway that is unique to each individual.",style ="text-align: justify;width:100%;font-size:18px"), 
                                                  # p("Instead of a standard A+/T+/N+ visualisation, we can add individuals who met 2 of the 3 cut-offs."), 
                                                  p(strong("Please click the button below to identify borderline individuals."), style = "text-align: justify;width:100%;font-size:18px"), 
                                                  actionButton("NEW_NO_AGE_CUBE", "Borderline A+/T+/N+ Visualisation", icon=icon("fas fa-cubes")), 
                                                  br(), 
                                                  bsModal(id = "NO_AGE_CUBE", 
                                                          title = "Borderline A+/T+/N+ Visualisation", 
                                                          trigger = "NEW_NO_AGE_CUBE", 
                                                          size = "large", 
                                                          p("In this plot, those who met the requirements of two cut-offs and their respective 
                                                          classification are shown.  This plot highlights the pitfalls of classification of individuals from binarized cut-offs and demonstrates the inappropriateness of 
                                                          categorising individuals based on these predetermined cut-offs",style ="text-align: justify;width:100%;font-size:18px"),
                                                          p("The plots produced below are an interactive static plot for the user
                                                          to move at their discretion.",style ="text-align: justify;width:100%;font-size:18px"), 
                                                          shinycssloaders:: withSpinner(plotlyOutput("NO_AGE_CUBE_NEW_G2", height = 600))
                                                  )
                                                  
                                                )
                                              ), 
                                              mainPanel(shinycssloaders:: withSpinner(
                                                plotlyOutput("plot5.P2", height = 600))
                                              )
                                            )
                                   ),
                                   
                                   #########################
                                   ######################### subpanel
                                   
                                   tabPanel("Add in your own data point!",
                                            br(),
                                            htmlOutput("ASSAYreminder9"),
                                            br(),
                                            sidebarLayout(
                                              sidebarPanel(
                                                p('Enter your own ATN  biomarker data to see your standing in the cohort.',style ="text-align: justify;width:100%;font-size:18px"),
                                                br(),
                                                numericInput("age", "Please enter your age", 
                                                             50,
                                                             min = 1, 
                                                             max = 200),
                                                numericInput("sums", HTML("Hippocampus mL<sup>3</sup>"), 
                                                             5,
                                                             min = 1,
                                                             max = 2000),
                                                numericInput("ps", "CSF pTau pg/mL", 
                                                             23,
                                                             min = 50,
                                                             max = 1000), 
                                                numericInput("cents", "Centiloid", 
                                                             30, 
                                                             min = 1, 
                                                             max = 1000), 
                                                pickerInput("DATAVIS2", "Select the form of visualisation:", 
                                                            choices = c("3D Plot", "A+/T+/N+ Visualisation"),
                                                            selected = "3D Plot",
                                                            multiple = F), 
                                                p("The plots produced are an interactive static plot for the user
                                                          to move at their discretion.", style = "width:100%;font-size:18px")
                                              ), 
                                              mainPanel(br(), br(),shinycssloaders:: withSpinner(
                                                plotlyOutput("Group2AddIN", height = 600)
                                              )
                                              )
                                            )
                                   ), 
                                   tabPanel("Statistical Modelling", 
                                            br(),
                                            p(strong("As the synthetic data is randomly generated, the statisical models that are fitted and corresponding
                                              regression coefficients/p-values are meaningless and do not reflect any features of the AIBL's ATN biomarkers.", style = "width:100%;font-size:18px;color:red")),
                                            p("In this tab, two models are reported on:",style ="text-align: justify;width:100%;font-size:18px"),
                                            tags$div(
                                              tags$ol(
                                                tags$li("Linear models for the deomgraphic characteristics on AT(N) biomarkers",style ="text-align: justify;width:100%;font-size:18px"),
                                                tags$li("Linear models for the interplay between AT(N) biomarkers",style ="text-align: justify;width:100%;font-size:18px")
                                              )
                                            ),
                                            p("
                                            The side panel below breaks down this tab further into components comprising of
                                               visualisation, model summaries and discussion.",style ="text-align: justify;width:100%;font-size:18px"),
                                            br(),
                                            navlistPanel(
                                              tabPanel("Visualisation", 
                                                       p("Visualising Boxplots, 2D and 3D plots can be a useful aid to understand the fundamental variable in a regression.",style ="text-align: justify;width:100%;font-size:18px"), 
                                                       p("Below, you can select the type of visual aid (Boxplots, 2D Scatter Plots or 3D Scatter Surface Plots).",style ="text-align: justify;width:100%;font-size:18px"), 
                                                       selectInput("REGRESPLOT2", "Select the Type of Visual:", 
                                                                   c("Boxplots" = "boxes", 
                                                                     "2D Scatter Plots" = "scatter",
                                                                     "3D Scatter with Regression Surfaces" = "surfaces"
                                                                   )),
                                                       # only show this panel if the plot type is boxplots
                                                       conditionalPanel(
                                                         condition = "input.REGRESPLOT2 == 'boxes'", 
                                                         p("You can view the corresponding boxplots for each specific biomarker."), 
                                                         selectInput(
                                                           "BIOMARKERG2", "Select the specific Biomarker:", 
                                                           c("Centiloid", 
                                                             "CSF pTau pg/mL", 
                                                             "Hippocampus mL\xB3" = "BOXHIP1")
                                                         ),
                                                         shinycssloaders:: withSpinner(plotOutput("BOXPLOT2", width = 1000, height = 700))
                                                       ), 
                                                       conditionalPanel(
                                                         condition = "input.REGRESPLOT2 == 'scatter'",
                                                         p("You can view the corresponding 2D Scatter Plots for each specific biomarker."), 
                                                         
                                                         selectInput(
                                                           "SCATTERBIOMARKER", "Select the specific Biomarker:", 
                                                           choices <- c("Centiloid", 
                                                                        "CSF pTau pg/mL", 
                                                                        "Hippocampus mL\xB3"="SCATTERHIP1"), 
                                                           
                                                         ), 
                                                         shinycssloaders:: withSpinner(plotlyOutput("SCATTER2",width = 1000, height = 700))
                                                         
                                                       ),
                                                       
                                                       conditionalPanel(
                                                         condition = "input.REGRESPLOT2 == 'surfaces'", 
                                                         p("You can view the corresponding 3D scatter plot surface for each of the specific demographic Characteristics."), 
                                                         selectInput(
                                                           "DEMOGRAPHIC2", "Select the specific Demographic Characteristic:", 
                                                           c("APOE-e4 allele", 
                                                             "Sex", 
                                                             "Age",
                                                             "Education")
                                                         ), 
                                                         p("The surfaces produced are predictive planes from regressions computed for the binarised demographic characteristic."),
                                                         p("The plots produced are an interactive static plot for the user
                                                          to move at their discretion.", style = "width:100%,font-size:17px"),
                                                         shinycssloaders:: withSpinner(plotlyOutput("SURFACEG2",width = 1000, height = 700))
                                                         
                                                         
                                                       ), 
                                              ), 
                                              tabPanel("Model Summaries", 
                                                       p("Please select which of the two models to view:"), 
                                                       selectInput("REGRESSIONG2", "Select the model reported on:", 
                                                                   c("1. Linear models for demographic characteristics" = "DMC2", 
                                                                     "2. Linear models for biomarker interplay" = "BINTER2")), 
                                                       conditionalPanel(
                                                         condition = "input.REGRESSIONG2 == 'DMC2'", 
                                                         pickerInput("LING2", "Please select the specific dependent biomarker:", 
                                                                     choices = c("Centiloid", 
                                                                                 "CSF pTau pg/mL",
                                                                                 "Hippocampus mL\xB3"="LINHIP1")), 
                                                         shinycssloaders:: withSpinner(tableOutput("LINREGG2"))
                                                       ), 
                                                       conditionalPanel(
                                                         condition = "input.REGRESSIONG2 == 'BINTER2'", 
                                                         pickerInput("INTERPLAYG2", "Please select the specific dependent biomarker", 
                                                                     choices = c("Centiloid", 
                                                                                 "CSF pTau pg/mL", 
                                                                                 "Hippocampus mL\xB3"="MODELHIP1")), 
                                                         shinycssloaders:: withSpinner(tableOutput("INTERPLAY2"))
                                                         
                                                       )
                                                       
                                                       
                                              ),
                                              
                                              tabPanel("Discussion",
                                                       p(strong("Please note that no discussion on statistical modelling in this App will be given as this data is randomly generated and does 
                                                         not hold any clinicial meaning.", style = "width:100%; font-size:18px;color:red"))
                                                       
                                              )
                                              
                                              
                                              
                                            )
                                   )
                       )
              ) 
  )
)






