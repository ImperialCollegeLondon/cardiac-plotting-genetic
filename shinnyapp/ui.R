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
# library(shinyalert)
library(plotly)
extractNames <- c("Race", "Sex", "Age", "Smoking", "SBP")
points = c("Endocardial", "Epicardial","Full shape")

fluidPage(
  navbarPage("Digital Heart",
             tabPanel("Home",
                      titlePanel(h1("Cardiac Phenotype Explorer", align = "center")),
                      wellPanel(h1("Introduction"),
                                HTML("<p><h4>The primary aim of the website is to support the visualisation of cardiac imaging phenotypes derived from UK Biobank cardiovascular magnetic resonance (CMR) images for over 8,000 subjects and to enable the exploration of their links to other non-imaging phenotypes that characterise the structure and function for the the left ventricle (LV).<br>"),
                                
                                HTML("<p><h4>The CMR imaging data is available from the <a href='https://imaging.ukbiobank.ac.uk/'>UK Biobank</a> and acquired following a standardised imaging protocol. Briefly, the heart was segmented from the 3D images using previous knowledge of cardiac anatomy from a set of manually annotated atlases. Each segmentation was coregistered to ensure anatomical consistency between subjects (<a href='https://www.ncbi.nlm.nih.gov/pubmed/26387054'>Bai et al., 2015</a>).
                                <h4> Wall thickness (WT) and surface to surface (S2S) were calculated at over 40,000 points in the 3D model at end-diastole and were measured as the distance between the endocardial and epicardial surfaces perpendicular to the midwall plane and as the differences in the endocardial and epicardial surfaces relative to an average cardiac shape, respectively.<br>")),
                                
                      wellPanel(h1("Linear Regression"),
                                HTML("<p><h4>The associations between one imaging phenotype (WT, S2S) and one non-imaging phenotype for each point in the 3D datasets were assessed using a linear regression model adjusted for the other non-imaging phenotypes.  
                                <h4>Multiple testing with Benjamini-Hochberg (BH) procedure to control from false discovery rate (FDR) and the threshold-free cluster enhancement (TFCE) technique to boost the areas of signal with spatial contiguity, was used in the LV points with significant associations (<a href='https://academic.oup.com/bioinformatics/article/34/1/97/4103396'>Biffi et al., 2018</a>). <br>")),
                      mainPanel(HTML('<img src = "ic_ukbb.png" height = "65" width = "350">'),#HTML('<img src = "ukbb.png", height = "65", width = "200">'),
                                # author info
                                shiny::hr(),
                                span("Feedback:  "),
                                a("Email ", href = "mailto:mthanaj@ic.ac.uk"),
                                span("| Website by Marjola Thanaj (Last updated: 09 Jan 2020)"),
                                br()
                      )),
             # h5("Feedback: Email: mthanaj@ic.ac.uk | Website by Marjola Thanaj (Last updated: 07 Jan 2020)"))),
             tabPanel("Associations",
                      titlePanel("3D plot of LV Surface"),
                      tags$hr(),
                      # useShinyalert(),
                      # tags$head(tags$script(src = "message-handler.js")),
                      actionButton('helpButton', 'Help', icon("question-circle"), style = "background-color:limegreen;color:white;position:fixed;right:40px;top:10px;z-index:100000000000", onclick="startHelp();"),
                      sidebarLayout(
                        sidebarPanel(
                          actionButton("goButton", "Plot", class = "btn-primary"),
                          tags$hr(),
                          selectInput("data", "Select dataset to plot:",
                                      choices = c("9k", "25k"),
                                      selected = "9k"),
                          tags$hr(),
                          selectInput("phen", "Select phenotype:",
                                      choices =c("WT","S2S"),
                                      selected="WT"),
                          tags$hr(),
                      
                          
                          conditionalPanel(
                            condition = "input.data == '9k'",
                            selectInput("typeInput", "Select covariates to extract:",
                                        choices = c("Age","BSA","SBP","Duration of Vigorous activity (in mins)"),
                                        selected = "Age"),
                            tags$hr(),
                            
                            checkboxInput("checkbox3", label = "Control for ", value = F),
                            
                            conditionalPanel(
                              condition = "input.checkbox3 == true ",
                              selectInput("typecontrol", "Select covariates to control for:",
                                          choices =(unique(extractNames[1:5]))
                              ))),
                          conditionalPanel(
                            condition = "input.data == '25k'",
                            selectInput("typeInput2", "Select covariates to extract:",
                                        choices = c("Age","BSA","SBP", "Diabetes", "long_PDSR", "radial_PDSR"),
                                        selected = "Age"),
                           tags$hr()),

                          
                          checkboxInput("checkbox", label = "Adjust for cofounders.", value = TRUE),
                          tags$hr(),
                          selectInput("whichee", "Select points file:",
                                      choices = unique(points),
                                      selected="Epicardial"),
                          tags$hr(),
                          checkboxInput("checkbox1", label = "BH", value = T),
                          checkboxInput("checkbox2", label = "TFCE", value = FALSE),
                          helpText("Multiple Testing correction and Thesolding technique"),
                          uiOutput("HelpBox"),
                          tags$hr(),
                          selectInput("dataset", "Choose a dataset:",
                                      choices = c("Beta coefficients", "P-values")),
                          downloadButton("downloadData", "Download data")
                        ),
                        mainPanel(
                          plotlyOutput(outputId = "coolplot"),
                          br(), br(),br(), br(), br(), br(), br(), br(), br(),br(), br(),br(), br(),br(),br(), br(),br(), br(),
                          htmlOutput(outputId ="text"),
                        )
                      )
                    
                        
             )
             
             
  )
)

#