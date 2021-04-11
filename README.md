# CSIRO Vacation Project

This repository contains the files for the synthetic simulated R Shiny App. This project was for summer 2020/2021. 

**Project Title:** De-binarising the ATN Framework: Continuous modelling of biomarkers in 3D. 

## Acknowledgements 

This App was created during a CSIRO vacation internship. Supervised by Dr Marcela Cespedes and Dr James Doecke. 

Special thanks to Marcela for her guidance, help and supportive attitude for the whole vacation period. My first R shiny app would not be up or running without her (fingers crossed it still runs!). Additional gratitude to CSIRO for funding under the Biostatistics team. 

The web application is available here: https://lucinda-nott.shinyapps.io/simulated_data_app/

## What this Repository Contains

This repository contains: 
- R code to simulate synthetic data 
- snytheticProcess to process the data and form tables 
- Functions for creating all 3D visualisation through plotly 
- UI and serve file to run the App
- www file and any additional png
- References and literature related to the Project

**This Repository does not contain any clinical data associated with the AIBL. Data used in this app is to demostrate app features, and does not represent true features or characteristics of the AIBL biomarkers in the ATN framework**

## Research Problem and Project Aim

Previously, the biomarkers for AD (AB-Amyloid, Tau and Neurodegeneration) have been used to descibe the different stages of AD by Clifford and colleagues. In this, an individual is described as being positive or negative for AB-Amyloid, Tau and Neurodegeneration, forming 8 different groups. This approach considers the binary form of these biomarkers in the terms of an individual surpassing a certain threshold and does not provide information on the interplay of the AD biomarkers in their continuous biomarker form.

This project aims to investigate the joint associations of Amyloid, Tau and Neurodegeneration over the AD domain to better understand the complex dynamics along a continuous domain using mathematical and statistical 3D modelling. Moreover, this project aims to deliver results using an interactive tool to effectively visualise and better communicate the results from complex 3D models. It further aims to better inform and aid clinicians and neuroscientists on the aetiology of AD and facilitate new knowledge on the interplay of key AD biomarkers.

## Data Analysed in the App 
In this project, we analysed data from the Australian Imaging, Biomarkers and Lifestyle flagship (AIBL) study of ageing. The AIBL study is the largest longitudinal study of ageing in Australia. With over 2,000 participants, the focus of AIBL is to better the nature of AD research to enable early detection and lifestyle interventions. As the data used is derived from a clinical study, the initial findings and App will not be accessible to the public. Instead, this publically available App consists of synthetic simulated data. 


## References 

Bourgeat, P., Dore, V., Fripp, J., Villemagne, V.L., Rowe, C.C. and Salvado, O., 2015, March. Computational analysis of PET by AIBL (CapAIBL): a cloud-based processing pipeline for the quantification of PET images. In Medical Imaging 2015: Image Processing (Vol. 9413, p. 94132V). International Society for Optics and Photonics.


Burnham, S.C., Coloma, P.M., Li, Q.X., Collins, S., Savage, G., Laws, S., Doecke, J., Maruff, P., Martins, R.N., Ames, D. and Rowe, C.C., 2019. Application of the NIA-AA research framework: towards a biological definition of Alzheimer's disease using cerebrospinal fluid biomarkers in the AIBL study. The journal of prevention of Alzheimer's disease, 6(4), pp.248-255.


Jack, C.R., Bennett, D.A., Blennow, K., Carrillo, M.C., Feldman, H.H., Frisoni, G.B.,Hampel, H., Jagust, W.J., Johnson, K.A., Knopman, D.S. and Petersen, R.C., 2016. A/T/N: an unbiased descriptive classification scheme for Alzheimer disease biomarkers. Neurology, 87(5), pp.539-547.

 Jack Jr, C.R., Knopman, D.S., Jagust, W.J., Petersen, R.C., Weiner, M.W., Aisen, P.S., Shaw, L.M., Vemuri, P., Wiste, H.J., Weigand, S.D. and Lesnick, T.G., 2013. Update on hypothetical model of Alzheimer's disease biomarkers. Lancet neurology, 12(2), p.207.
 
 
De Loof, A. and Schoofs, L., 2019. Alzheimer's disease: Is a dysfunctional mevalonate biosynthetic pathway the master-inducer of deleterious changes in cell physiology?.OBM Neurobiology, 3(4).


Doecke, J.D., Ward, L., Burnham, S.C., Villemagne, V.L., Li, Q.X., Collins, S., Fowler, C.J., Manuilova, E., Widmann, M., Rainey-Smith, S.R. and Martins, R.N., 2020. Elecsys CSF biomarker immunoassays demonstrate concordance with amyloid-PET imaging. Alzheimer's research & therapy, 12, pp.1-10.


Dore, V., Bourgeat, P., Cummins, T.L., Raniga, P., Williams, S., Amadoru, S., MacLean, C., Hinton, F., Shepherd, C., Halliday, G.M. and Leyton, C.E., 2018. P1-405: VISUAL ASSESSMENT OF BETA-AMYLOID PET SCAN IS IMPROVED BY CAPAIBL. Alzheimer's & Dementia, 14(7S_Part_8), pp.P459-P459.


Ellis, K., Bush, A., Darby, D., De Fazio, D., Foster, J., Hudson, P., Lautenschlager, N., Lenzo, N., Martins, R.,Maruff, P. and Masters, C., 2009. The Australian Imaging, Biomarkers and Lifestyle (AIBL) study of aging: methodology and baseline characteristics of 1112 individuals recruited for a longitudinal study of Alzheimer's disease.


Li, Q.X., Villemagne, V.L., Doecke, J.D., Rembach, A., Sarros, S., Varghese, S., McGlade, A., Laughton, K.M., Pertile, K.K., Fowler, C.J.and Rumble, R.L., 2015. Alzheimer's disease normative cerebrospinal fluid biomarkers validated in PET amyloid-Betacharacterized subjects from the Australian Imaging, Biomarkers and Lifestyle (AIBL) study. Journal of Alzheimer's Disease, 48(1), pp.175-187.


Jack Jr, C.R., Knopman, D.S., Jagust, W.J., Petersen, R.C., Weiner, M.W., Aisen, P.S., Shaw, L.M., Vemuri, P., Wiste, H.J., Weigand, S.D. and Lesnick, T.G., 2013. Tracking pathophysiological processes in Alzheimer's disease: an updated hypothetical model of dynamic biomarkers. The Lancet Neurology, 12(2), pp.207-216.



