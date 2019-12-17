######################################################
################# ui.R ##########################
######################################################
######################################################
# rm(list = ls(all = TRUE))  
#start with an empty environment
#install.packages("shiny")
#install.packages('shinyFiles')
#library(shinyFiles)
# setwd("Z:/Experiments_of_Maria/20191024_ShinyApp/")
library(shiny)
library(plotly)
extractNames <- c("Race", "Sex", "Age", "Smoking", "SBP")
points = c("Endocardial", "Epicardial","Full shape")

fluidPage(
  navbarPage("UKBB",
             tabPanel("Home",
                      titlePanel(h1("Cardiac Phenotype Explorer", align = "center")),
                      wellPanel(h1("Introduction"),
                                h4("The primary aim of the website is to support the visualisation of cardiac imaging phenotypes derived from UK Biobank cardiovascular magnetic resonance (CMR) images for over 8,000 subjects and to enable the exploration of their links to other non-imaging phenotypes "),
                                h4("that characterise the structure and function for the the left ventricle (LV)."),
                                h4("The CMR imaging data is available from the UK Biobank and acquired following a standardised imaging protocol. Briefly, the heart was segmented from the 3D images using previous knowledge of cardiac anatomy from a set of manually annotated atlases. Each segmentation"), 
                                h4("was coregistered to ensure anatomical consistency between subjects. Wall thickness (WT) was calculated at over 40,000 points in the 3D model at end-diastole and was measured as the distance between the endocardial and epicardial surfaces perpendicular to the midwall"),
                                h4(" plane.")),
                      wellPanel(h1("Linear Regression"),
                                h4("The associations between one imaging phenotype (WT, S2S) and one non-imaging phenotype for each point in the 3D datasets were assessed using a regression model adjusted for the other non-imaging phenotypes with correction to control the false discovery rate. Threshold- "),
                                h4("free cluster enhancement (TFCE) was used in the cardiac atlas to boost the areas of signal with spatial contiguity.")),
                      mainPanel(img(src = "ic.png", height = 90, width = 220),img(src = "ukbb.png", height = 80, width = 200),
                                h5("Feedback: Email | Website by Marjola Thanaj (Last updated: 16 Dec 2019)"))),
             tabPanel("Associations",
                      titlePanel("3D plot of LV Surface"),
                      sidebarLayout(
                         sidebarPanel(
                          # actionButton("start", "Start process", class = "btn-primary", onclick="Shiny.onInputChange('stopThis',false)"),
                          # actionButton("stop", "Stop", class = "btn-danger", onclick="Shiny.onInputChange('stopThis',true)"),
                          
                          # textOutput('timeleft'),
                          # verbatimTextOutput("value"),
                          selectInput("phen", "Select phenotype:",
                                      choices =c("WT","S2S"),
                                      selected="WT"),
                          selectInput("typeInput", "Select covariates to extract:",
                                      choices = c("Age","BSA","SBP","Duration of Vigorous activity (in mins)"),
                                      selected = "SBP"),
                          checkboxInput("checkbox3", label = "Control for", value = T),
                          conditionalPanel(
                            condition = "input.checkbox3 == true ",
                            selectInput("typecontrol", "Select covariates to control for:",
                                        choices =(unique(extractNames[1:5])),
                                        selected = "Race")),
                          # uiOutput('typecontrol'),
                          selectInput("whichee", "Select points file:",
                                      choices = unique(points),
                                      selected="Epicardial"),
                          checkboxInput("checkbox1", label = "BH", value = T),
                          checkboxInput("checkbox2", label = "TFCE", value = FALSE),
                          helpText("Multiple Testing correction and Thesolding technique"),
                          uiOutput("Box"), 
                          selectInput("dataset", "Choose a dataset:",
                                      choices = c("Beta", "Pvalues")),
                          downloadButton("downloadData", "Download data")
                        ),
                        mainPanel(
                          plotlyOutput("coolplot"),
                          br(), br(),
                          htmlOutput("text"),
                        )
                      )
             )
  )
)

#