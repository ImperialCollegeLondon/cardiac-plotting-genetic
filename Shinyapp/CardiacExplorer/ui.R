######################################################
################# ui.R ###############################
######################################################
######################################################
# rm(list = ls(all = TRUE))
#start with an empty environment
#install.packages("shiny")
#install.packages('shinyFiles')
#library(shinyFiles)
#setwd("~/cardiac/Experiments_of_Maria/ShinyApp/")
library(remotes)
# remotes::install_github("rstudio/d3heatmap", force=TRUE)
library(shiny)
library(plotly)
library(data.table)
library(dplyr)
library(d3heatmap)
library(visNetwork)
library(igraph)
data_all<-read.csv("2D/2d_association_lasso.csv")
data_all<-data_all[,-1]
colnames(data_all)[c(21:23,17:19,30)]<-c("Blood_pressure_medication","Cholesterol_medication", "Insulin_medication","Duration_of_moderate_activity (min/day)", 
                                         "Duration_of_vigorous_activity (min/day)", "Assessment_centre","Heart_failure")
colnames(data_all)[47:131]<-c("Longitudinal_PDSR (s-1)","Radial_PDSR (s-1)", "Ecc_AHA_1 (%)","Ecc_AHA_2 (%)", "Ecc_AHA_3 (%)",
                          "Ecc_AHA_4 (%)","Ecc_AHA_5 (%)","Ecc_AHA_6 (%)","Ecc_AHA_7 (%)","Ecc_AHA_8 (%)","Ecc_AHA_9 (%)",
                          "Ecc_AHA_10 (%)","Ecc_AHA_11 (%)","Ecc_AHA_12 (%)","Ecc_AHA_13 (%)","Ecc_AHA_14 (%)","Ecc_AHA_15 (%)",
                          "Ecc_AHA_16 (%)","Ecc_Global (%)", "Err_AHA_1 (%)", "Err_AHA_2 (%)","Err_AHA_3 (%)","Err_AHA_4 (%)",
                          "Err_AHA_5 (%)","Err_AHA_6 (%)","Err_AHA_7 (%)","Err_AHA_8 (%)","Err_AHA_9 (%)","Err_AHA_10 (%)","Err_AHA_11 (%)",
                          "Err_AHA_12 (%)","Err_AHA_13 (%)","Err_AHA_14 (%)","Err_AHA_15 (%)","Err_AHA_16 (%)","Err_Global (%)",                  
                          "Ell_1 (%)","Ell_2 (%)","Ell_3 (%)","Ell_4 (%)","Ell_5 (%)","Ell_6 (%)", "Ell_Global (%)","WT_AHA_1 (mm)",
                          "WT_AHA_2 (mm)","WT_AHA_3 (mm)","WT_AHA_4 (mm)","WT_AHA_5 (mm)","WT_AHA_6 (mm)","WT_AHA_7 (mm)","WT_AHA_8 (mm)",                   
                          "WT_AHA_9 (mm)","WT_AHA_10 (mm)","WT_AHA_11 (mm)","WT_AHA_12 (mm)","WT_AHA_13 (mm)","WT_AHA_14 (mm)",                  
                          "WT_AHA_15 (mm)","WT_AHA_16 (mm)","WT_Global (mm)","AAo distensibility (10-3 mmHg-1)", "DAo distensibility (10-3 mmHg-1)",
                          "AAo max area (mm2)","AAo min area (mm2)","DAo max area (mm2)","DAo min area (mm2)","LVEDVi (mL/m2)","LVESVi (mL/m2)",
                          "LVSVi (mL/m2)","LVMi (g/m2)","LAVi max (mL/m2)","LAVi min (mL/m2)","LASVi (mL/m2)","LVEF (%)","LVCO (L/min)","LVCI (L/min/m2)",
                          "LAEF (%)","RVEDVi (mL/m2)","RVESVi (mL/m2)","RVSVi (mL/m2)","RAVi max (mL/m2)","RAVi min (mL/m2)",                    
                          "RASVi (mL/m2)","RVEF (%)","RAEF (%)")
rownames(data_all)<-colnames(data_all)

extractNames <- c("Race", "Sex", "Age", "Smoking", "SBP")
extractNames2 <-c("Age","Sex F0/M1","BSA","BMI", "Weight", "Height", "Body fat (%)", "Whole body fat", "Waist circumference", 
                  "Hip circumference","SBP", "DBP", "Pulse rate", "Pulse wave stiffness","Impedance of whole body",
                  "Smoking status","Alcohol drinker status", "High blood pressure" ,"Angina", "Stroke", 
                  "Heart attack","Asthma","Bronchitis", "Hypertension","Serious medical condition or disability diagnosed by doctor", 
                  "High cholesterol","Hypertrophic cardiomyopathy", "Heart failure", "Cardiac disease", 
                  "Pulmonary embolism", "Rheumatoid arthritis","COPD", "Peripheral vascular disease", "Depression", "Dementia",
                  "Parkinson's disease","Cancer diagnosed by doctor", "Diabetes diagnosed by doctor","Number of medication taken",
                  "Blood pressure medication uptake","Cholesterol medication uptake", "Insulin medication uptake" ,
                  "Duration of moderate activity (in mins/day)", "Duration of vigorous activity (in mins/day)", "Assessment centre")

points = c("Endocardial", "Epicardial","Full shape")
fluidPage(
  navbarPage("Digital Heart",
             tabPanel("Home",
                      titlePanel(h1("Cardiac Phenotype Explorer", align = "center")),
                      wellPanel(h1("Introduction"),
                                HTML("<p><h4>The primary aim of the website is to support the visualisation of cardiac imaging phenotypes derived from UK Biobank cardiovascular magnetic resonance (CMR) 
                                     images for over 26,000 subjects and to enable the exploration of their links to other non-imaging phenotypes that characterise the structure and function for the 
                                     the right and left ventricle (RV and LV).<br>"),
                                
                                HTML("<p><h4>The CMR imaging data is available from the <a href='https://imaging.ukbiobank.ac.uk/'>UK Biobank</a> and acquired following a standardised imaging 
                                protocol (<a href='https://jcmr-online.biomedcentral.com/articles/10.1186/s12968-016-0227-4'>Petersen et al. JCMR 2016</a>). Briefly, the analysis pipeline consists of several parts, including performing segmentation on short-axis, long-axis and aortic cine images using 
                                convolutional neural networks (<a href='https://jcmr-online.biomedcentral.com/articles/10.1186/s12968-018-0471-x'>Bai et al. JCMR 2018</a>, <a href='https://rd.springer.com/chapter/10.1007%2F978-3-030-00937-3_67'>Bai et al. MICCAI 2018</a>), evaluating volumetric measures and myocardial wall thickness, 
                                performing motion tracking using image registration, evaluating strains etc.<br>"),
                                HTML("<p><h4>The imaging phenotypes are derived using an automated image analysis pipeline, followed by manual quality control, characterise the structure and function 
                                for the four cardiac chambers and two sections of the aorta, 
                                including the left ventricle (LV), right ventricle (RV), left atrium (LA) and right atrium (RA), the ascending aorta (AAo) and descending aorta (DAo). 
                                The myocardial wall thickness and radial (Err), circumferential (Ecc) and longitudinal (Ell) strains are evaluated from the 2D segmentation analysis both globally and regionally for each segment.
                                     The radial and longitudinal peak distolic strain rates (PDSR) were calculated from a peak detection algorithm to obtain the distolic peak.<br>"),
                                HTML("<p><h4>To provide a 3D shape model of the heart, the LV was segmented using previous knowledge of cardiac anatomy, from a set of manually annotated atlases. 
                                Each segmentation was coregistered to ensure anatomical consistency between subjects (<a href='https://www.ncbi.nlm.nih.gov/pubmed/26387054'>Bai et al., Med Image Anal 2015</a>).
                                Wall thickness (WT) and surface to surface (S2S) were calculated at over 40,000 points in the 3D model at end-diastole and were measured as the 
                                     distance between the endocardial and epicardial surfaces perpendicular to the midwall plane and as the differences in the endocardial and epicardial 
                                     surfaces relative to an average cardiac shape, respectively.<br>"),
                                HTML("<p><h4> The visualisation results are organised into two parts: '2D Associations' and '3D Associations', explained below. You can go to each part by clicking the corresponding tab at the top of this website.<br>")),
                      wellPanel(h1("2D Associations"),
                                HTML("<p><h4>The associations between 2D imaging phenotypes (LV, RV, LA, RA, AAo, DAo, WT, Err, Ecc, Ell and PDSR) and the non-imaging phenotypes were assessed using a linear regression model adjusted for age, sex, race, body surface area and systolic blood pressure.
                                <h4>The 2D phenotype associations can be visualised from the R packages 'd3heatmap' and 'visNetwork'. <br>")),
                      wellPanel(h1("3D Associations"),
                                HTML("<p><h4>The associations between one 3D imaging phenotype (WT, S2S) and one non-imaging phenotype for each point in the 3D datasets were assessed using a linear regression model adjusted for age, sex, race, body surface area, systolic blood pressure and diastolic blood pressure.
                                <h4>Multiple testing with Benjamini-Hochberg (BH) procedure to control from false discovery rate (FDR) and the threshold-free cluster enhancement (TFCE) technique to boost the areas of signal with spatial contiguity, was used in the LV points with significant associations (<a href='https://academic.oup.com/bioinformatics/article/34/1/97/4103396'>Biffi et al., Bioinformatics 2018</a>). <br>")),
                      mainPanel(HTML('<img src = "ic_ukbb.png" height = "65" width = "350">'),                                # author info
                                shiny::hr(),
                                span("Feedback:  "),
                                a("Email ", href = "mailto:mthanaj@ic.ac.uk"),
                                span("| Website by Marjola Thanaj (Last updated: 24 November 2020)"),
                                br()
                      )),
             tabPanel("2D Associations",
                      titlePanel("2D associations plots"),
                      tags$hr(),
                      tabsetPanel(type="tabs",
                                  tabPanel("d3heatmap",
                                           tags$hr(),
                                           helpText("Each association was adjusted for age, sex, race, body surface area and
                                        systolic blood pressure."),
                                           tags$hr(),
                                           d3heatmapOutput("heatmap", height="700px", width="100%")),
                                  tabPanel("visNetwork",
                                           tags$hr(),
                                           div(style="display:inline-block;vertical-align:top;",
                                               fluidRow(
                                                 column(2,
                                                        selectInput("plotmod", "Select graph layout:",
                                                                    choices = c("Smooth","Circle"),
                                                                    selected="Smooth")
                                                 ),
                                                 column(10,
                                                        selectInput("cov", "Select Imaging or Non-imaging phenotypes to explore:",
                                                                    choices = unique(as.character(colnames(data_all))),
                                                                    selected="Longitudinal_PDSR (s-1)"),
                                                        helpText("Adjusted for age, sex, race, body surface area and 
                                                                 systolic blood pressure."),
                                                        visNetworkOutput("network",height="600px", width="100%")))
                                           ))
                      ), 
                      br(),br(),
                      downloadButton("downloadData2", "Download 2D associations matrix"),
                      br(),br()
             ),
             tabPanel("3D Associations",
                      titlePanel("3D plot of LV Surface"),
                      tags$hr(),
                      actionButton('helpButton', 'Help', icon("question-circle"), style = "background-color:limegreen;color:white;position:fixed;right:40px;top:10px;z-index:100000000000", onclick="startHelp();"),
                      sidebarLayout(
                        sidebarPanel(
                          actionButton("goButton", "Plot", class = "btn-primary"),
                          tags$hr(),
                          selectInput("phen", "Select imaging phenotype:",
                                      choices =c("WT","S2S"),
                                      selected="WT"),
                          tags$hr(),
                          
                          selectInput("typeInput2", "Select non-imaging phenotype to extract:",
                                      choices = unique(extractNames2),
                                      selected = "Age"),
                          tags$hr(),
                          
                          checkboxInput("checkbox", label = "Adjust for cofounders.", value = TRUE),
                          helpText("Adjusted for age, sex, race, body surface area,
                                        systolic blood pressure and diastolic blood pressure.
                                     This is applicable if non-imaging phenotype is a continuous variable."),
                          tags$hr(),
                          selectInput("whichee", "Select points file:",
                                      choices = unique(points),
                                      selected="Full shape"),
                          tags$hr(),
                          checkboxInput("checkbox1", label = "BH", value = T),
                          checkboxInput("checkbox2", label = "TFCE", value = T),
                          helpText("Multiple Testing correction and Thresholding technique"),
                          uiOutput("HelpBox"),
                          tags$hr(),
                          selectInput("dataset", "Choose a dataset to download:",
                                      choices = c("Beta coefficients", "P-values")),
                          downloadButton("downloadData", "Download data")
                        ),
                        mainPanel(
                          plotlyOutput(outputId = "coolplot"),
                          htmlOutput(outputId ="adjust"),
                          br(),br(),br(),br(),br(), br(),br(),br(),br(), br(),br(), br(),
                          htmlOutput(outputId ="text")
                        )
                      )
                      
             )
             
             
  )
)

#
