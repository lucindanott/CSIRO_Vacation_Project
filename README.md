# CSIRO Vacation Project

This repository contains the files for the synthetic simulated R Shiny App. This project was for the 2020/2021. 

**Project Title:** De-binarising the ATN Framework: Continuous modelling of biomarkers in 3D. 

## Acknowledgements 

This App was created during a CSIRO vacation internship. Supervised by Dr Marcela Cespedes and Dr James Doecke. 

Special thanks to Marcela for her guidance, help and supportive attitude for the whole vacation period. My first R shiny app would not be up or running without her (fingers crossed it still runs!). Additional gratitude to CSIRO for funding under the Biostatistics team. 

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

