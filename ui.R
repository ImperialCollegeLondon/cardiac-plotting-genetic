######################################################
################# ui,R ##########################
######################################################
######################################################
rm(list = ls(all = TRUE))  
#start with an empty environment
#install.packages("shiny")
setwd("~/cardiac/Experiments_of_Maria/20191024_ShinyApp/")
library(shiny)


fluidPage(
  titlePanel("3D plot of LV Surface"),
  sidebarLayout(
    sidebarPanel(
      actionButton("start", "Start process", class = "btn-primary", onclick="Shiny.onInputChange('stopThis',false)"),
      actionButton("stop", "Stop", class = "btn-danger", onclick="Shiny.onInputChange('stopThis',true)"),
      helpText(" 
               "),
      textOutput('timeleft'),
      verbatimTextOutput("value"),
      selectInput("phen", "Select phenotype:",
                  choices =c("WT","S2S","Curvature"),
                  selected="WT"),
      selectInput("typeInput", "Select covariates to extract:",
                  choices =(unique(extractNames)),
                  selected = "SBP"),
      radioButtons("group", "Select group:",
                   choices = (unique(choice)),
                   selected = choice[2]),
      checkboxInput("checkbox3", label = "Control for", value = T),
      conditionalPanel(
        condition = "input.checkbox3 == true",
        selectInput("typecontrol", "Select covariates to control for:",
                    choices =(unique(extractNames[1:6])),
                    selected = "Race")),

      selectInput("whichee", "Select points file:",
                  choices =list("1","2","3"),
                  selected="2"),
      helpText("1 endocardial, 2 epicardial, 3 full shape"),
      checkboxInput("checkbox1", label = "BH", value = T),
      checkboxInput("checkbox2", label = "TFCE", value = FALSE),
      conditionalPanel(
        condition = "input.checkbox2 == true",
        sliderInput("obs", "Number of permutations:", 0, 10000, 100, step = 100)),
      helpText("Multiple Testing correction and Thesolding technique"),
      uiOutput("Box"), 
      downloadButton("downloadData", "Download data")
    ),
    mainPanel(
      plotlyOutput("coolplot"),
      br(), br(),
      htmlOutput("text"))
  )
)

#