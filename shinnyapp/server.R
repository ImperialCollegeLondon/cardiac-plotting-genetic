######################################################
################# server.R ##########################
######################################################
######################################################
#rm(list = ls(all = TRUE))  
#start with an empty environment
#install.packages("shiny")
# setwd("Z:/Experiments_of_Maria/20191024_ShinyApp/")
library(shiny)
library(plotly)
library(data.table)
library(dplyr)
choices <- c("Age","BSA","SBP","Duration of Vigorous activity (in mins)")
choices2 <-c("Age", "Sex F0/M1","BSA","SBP","Pulse rate","Smoking status","Alcohol drinker status","Asthma","Bronchitis", "Hypertension",
             "Serious medical condition or diasability diagnosed by doctor", "Hypertrophic cardiomyopathy", "Heart attack",
             "Cancer diagnosed by doctor", "Diabetes diagnosed by doctor","Stroke","Number of medication taken",
             "Duration of moderate activity (in mins)", "Duration of vigorous activity (in mins)")
extractname<-c("Age","Sex","BSA","SBP","Pulse_rate","Smoking","Alcohol","Asthma","Bronchitis", "Hypertension",
               "Disability", "HCM", "Heart_attack",
               "Cancer", "Diabetes","Stroke","Medication",
               "Moderate", "Vigorous")
points <- c("Endocardial", "Epicardial","Full shape")
pv_names <- c("pvalues","BHpvalues","BHpvaluesTFCE")
function(input, output,session) {
  
  phen <- reactive(input$phen)
  whichpoint <- reactive({input$whichee})
  Sel2 <- reactive(input$typeInput2)
  datasetInput <- reactive(input$dataset)
  Adj <- reactive(input$checkbox)
  BH <- reactive(input$checkbox1)
  TFCE <- reactive(input$checkbox2)
  endoEpi <- read.table("endo_epi.txt")
  vert2print <- list(which(endoEpi[,4]==0),which(endoEpi[,4]==1),1:length(endoEpi[,4]))
  
  observeEvent(input$helpButton, {
    # on click, send custom message to start help
    showModal(modalDialog(
      title = "CardiacExplorer Help",
      HTML("<ul><li>Select Wall thickness (WT) or Surface to surface (S2S) imaging phenotype.<br><br> 
      </li><li>Select the non-imaging covariates for the 3D mass univariate regression analysis.<br><br> 
      </li><li>Tick <b>'Adjust for cofounders'</b> checkbox to adjust for other non-imaging phenotypes. <br><br>
      </li><li>Choose points files (Epicardial with 27623 points, Endocardial with 19185 points and Full mesh with 46808 points)<br><br>
      </li><li>Tick <b>'BH'</b> checkbox to use Benjamini-Hochberg procedure.<br><br>
      </li><li>Tick <b>'TFCE'</b> chechbox to use the threshold-free cluster enhancement technique.<br><br>
      </li><li>Press <b>'Plot'</b> every time input parameters are altered.<br><br> 
      <ul><li>Plots might take a few seconds before displayed.<br><br>
      </li><li>Red areas indicate positive associations, blue areas indicate negative associations
           and grey areas indicate non significant correlations.<br><br>
      </li><li>You can download the full mesh coordinates with the beta coefficients or the <br>
           p-values after choosing the dataset to download and press <b>'Download data'</b>.<br>"
      ),
      easyClose = TRUE)
    )})
  v <- reactiveValues(doPlot = FALSE, dotext=" ")
  observeEvent(input$typeInput2, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  observeEvent(input$checkbox, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  observeEvent(input$checkbox1, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  observeEvent(input$checkbox2, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  observeEvent(input$phen, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  observeEvent(input$whichee, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  
  observeEvent(input$goButton, {
    v$doPlot <- input$goButton
    v$dotext <- input$goButton
  })
  observe({
    #################################
    print(paste("Phenotype: ",phen()))
    print(paste("Covariate 2: ",Sel2()))
    
    ##### plot ##########
    #####################
    output$coolplot <- renderPlotly({
      if (v$doPlot == FALSE) return()
      if (Sel2()==choices2[2]){
        choices =extractname
        Sel2<-"Sex"
        ns<-c("23,681")
      } else if (Sel2()==choices2[5]){
        choices =extractname
        Sel2<-"Pulse_rate"
        ns<-c("23,680")
      } else if (Sel2()==choices2[6]){
        choices =extractname
        Sel2<-"Smoking"
        ns<-c("23,548")
      } else if (Sel2()==choices2[7]){
        choices =extractname
        Sel2<-"Alcohol"
        ns<-c("23,548")
      } else if (Sel2()==choices2[11]){
        choices =extractname
        Sel2<-"Disability"
        ns<-c("23,547")
      }else if (Sel2()==choices2[12]){
        choices =extractname
        Sel2<-"HCM"
        ns<-c("18,647")
      } else if (Sel2()==choices2[13]){
        choices =extractname
        Sel2<-"Heart_attack"
        ns<-c("18,647")
      } else if (Sel2()==choices2[14]){
        choices =extractname
        Sel2<-"Cancer"
        ns<-c("23,548")
      } else if (Sel2()==choices2[15]){
        choices =extractname
        Sel2<-"Diabetes"
        ns<-c("23,548")
      } else if (Sel2()==choices2[17]){
        choices =extractname
        Sel2<-"Medication"
        ns<-c("23,681")
      } else if (Sel2()==choices2[18]){
        choices =extractname
        Sel2<-"Moderate"
        ns<-c("21,280")
      }else if (Sel2()==choices2[19]){
        choices =extractname
        Sel2<-"Vigorous"
        ns<-c("16,106")
      } else{
        choices=c("Age","Sex F0/M1","BSA","SBP","Pulse rate","Smoking status","Alcohol drinker status","Asthma","Bronchitis", "Hypertension",
                  "Serious medical condition or disability diagnosed by doctor", "Hypertrophic cardiomyopathy", "Heart attack",
                  "Cancer diagnosed by doctor", "Diabetes diagnosed by doctor","Stroke","Number of medication taken",
                  "Duration of moderate activity (in mins)", "Duration of vigorous activity (in mins)")
        
        Sel2<-input$typeInput2
      }
      if (Sel2()==choices2[1] || Sel2()==choices2[3] || Sel2()==choices2[4]){ns<-c("23,681")}
      if (Sel2()==choices2[8] || Sel2()==choices2[9] || Sel2()==choices2[10] || Sel2()==choices2[16]){ns<-c("18,647")}
      num_sel<-which(choices==Sel2)
      datafile<-"24k" 
      # }
      if (Adj() == TRUE){
        filepath<-paste(datafile,choices[num_sel],choices[num_sel],sep = "/")
      } else {
        adjpath<-paste("Adjust_0",choices[num_sel], sep = "/")
        filepath<-paste(datafile,choices[num_sel],adjpath,sep = "/")
      }
      isolate({
        if (TFCE() == FALSE & BH() == FALSE) {
          
          Data <- read.table(paste(filepath,"_beta_",phen,".txt", sep = ""), quote="\"", comment.char="")
          colnames(Data) <- c("x", "y", "z", "beta")
          Data_pvalues <- read.table(paste(filepath,"_pvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
          colnames(Data_pvalues) <- c("x","y","z","pvalues")
          
          if (whichpoint()==points[1]){
            Data <- Data[vert2print[[1]],]
            Data_pvalues <- Data_pvalues[vert2print[[1]],]
          } else if (whichpoint()==points[2]){
            
            Data <- Data[vert2print[[2]],]
            Data_pvalues <- Data_pvalues[vert2print[[2]],]
          } else if (whichpoint()==points[3]){
            
            Data <- Data[vert2print[[3]],]
            Data_pvalues <- Data_pvalues[vert2print[[3]],]
          }
          observeEvent(input$dataset, {
            if (datasetInput() == "Beta coefficients") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_beta_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data, file, col.names = TRUE, row.names = FALSE)
                }
              )
            } else if (datasetInput() == "P-values") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_pvalues_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data_pvalues, file, col.names = TRUE, row.names = FALSE)
                }
              )
            }
          })
        } else if (TFCE() == FALSE & BH() == TRUE){
          
          Data <- read.table(paste(filepath,"_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
          colnames(Data) <- c("x", "y", "z", "beta")
          Data_pvalues <- read.table(paste(filepath,"_BHpvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
          colnames(Data_pvalues) <- c("x","y","z","pvalues")
          
          if (whichpoint()==points[1]){
            Data <- Data[vert2print[[1]],]
            Data_pvalues <- Data_pvalues[vert2print[[1]],]
          } else if (whichpoint()==points[2]){
            
            Data <- Data[vert2print[[2]],]
            Data_pvalues <- Data_pvalues[vert2print[[2]],]
          } else if (whichpoint()==points[3]){
            
            Data <- Data[vert2print[[3]],]
            Data_pvalues <- Data_pvalues[vert2print[[3]],]
          }
          observeEvent(input$dataset, {
            
            if (datasetInput() == "Beta coefficients") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_beta_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data, file, col.names = TRUE, row.names = FALSE)
                }
              )
            } else if (datasetInput() == "P-values") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_BHpvalues_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data_pvalues, file, col.names = TRUE, row.names = FALSE)
                }
              )
            }
          })
          
        } else if (TFCE() == TRUE & BH() == TRUE) {
          
          Data <- read.table(paste(filepath,"_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
          colnames(Data) <- c("x", "y", "z", "beta")
          Data_pvalues <- read.table(paste(filepath,"_BHpvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
          colnames(Data_pvalues) <- c("x","y","z","pvalues")
          
          if (whichpoint()==points[1]){
            Data <- Data[vert2print[[1]],]
            Data_pvalues <- Data_pvalues[vert2print[[1]],]
          } else if (whichpoint()==points[2]){
            
            Data <- Data[vert2print[[2]],]
            Data_pvalues <- Data_pvalues[vert2print[[2]],]
          } else if (whichpoint()==points[3]){
            
            Data <- Data[vert2print[[3]],]
            Data_pvalues <- Data_pvalues[vert2print[[3]],]
          }
          observeEvent(input$dataset, {
            if (datasetInput() == "Beta coefficients") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_beta_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data, file, col.names = TRUE, row.names = FALSE)
                }
              )
            } else if (datasetInput() == "P-values") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_BHpvaluesTFCE_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data_pvalues, file, col.names = TRUE, row.names = FALSE)
                }
              )
            }
          })
        } else if (TFCE() == TRUE & BH() == FALSE) {
          
          Data <- read.table(paste(filepath,"_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
          colnames(Data) <- c("x", "y", "z", "beta")
          
          Data_pvalues <- read.table(paste(filepath,"_pvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
          colnames(Data_pvalues) <- c("x","y","z","pvalues")
          
          if (whichpoint()==points[1]){
            Data <- Data[vert2print[[1]],]
            Data_pvalues <- Data_pvalues[vert2print[[1]],]
          } else if (whichpoint()==points[2]){
            
            Data <- Data[vert2print[[2]],]
            Data_pvalues <- Data_pvalues[vert2print[[2]],]
          } else if (whichpoint()==points[3]){
            
            Data <- Data[vert2print[[3]],]
            Data_pvalues <- Data_pvalues[vert2print[[3]],]
          }
          
          observeEvent(input$dataset, {
            
            if (datasetInput() == "Beta coefficients") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_beta_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data, file, col.names = TRUE, row.names = FALSE)
                }
              )
            } else if (datasetInput() == "P-values") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_pvaluesTFCE_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data_pvalues, file, col.names = TRUE, row.names = FALSE)
                }
              )
            }
          })
        }
        pos<-which(Data_pvalues$pvalues < 0.05)
        sig <- (length(pos)/nrow(Data))*100
        significance_area<-format(round(sig, 0), nsmall = 0)
        average_beta <- format(round(mean(Data$beta[pos]), 3), nsmall = 2)
        print(significance_area)
        if (length(pos) == 0) {
          output$text <- renderUI({
            if (v$dotext == " ") return()
            str1 <- paste("<br><br>Beta coefficient for",phen()," vs ", Sel2(),"in UKBB subjects, n = ",ns,". Not significant associations.")
            HTML(paste(str1))
          })
          ax <- list(
            title = "",
            zeroline = F,
            showline = F,
            showticklabels = F,
            showgrid = F)
          p<-plot_ly(Data, x = ~x , y = ~y , z = ~z, height = 650,# width = 1200,
                     marker = list(color = ~beta,colorbar=list(title='beta coefficient - (a.u.)',len = 0.55, x = 1.13, y = 0.4), 
                                   colorscale = c('#1972A4', '#FF7070'),
                                   cauto = F,
                                   cmin = -max(abs(Data$beta)),
                                   cmax = max(abs(Data$beta)),showscale = T)) %>%
            add_markers() %>%
            layout(scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable",
                                camera = list(eye = list(x = cos(3.3)*2, y = sin(3.3)*2, z= 0.23))))
          p
        } else {
          
          output$text <- renderUI({
            if (v$dotext == " ") return()
            str1 <- paste("<br><br>Beta coefficients for",phen()," vs ", Sel2(),"with average beta coefficient = ",average_beta, "and significant area = ", significance_area, "%, in UKBB subjects, n = ",ns,".")
            HTML(paste(str1))#}
          })
          ax <- list(
            title = "",
            zeroline = F,
            showline = F,
            showticklabels = F,
            showgrid = F)
          p<-plot_ly(Data, x = ~x , y = ~y , z = ~z, height = 650, #width = 1200,
                     marker = list(color = "lightgrey",colorbar=list(title='beta coefficient - (a.u.)',len = 0.55, x = 1.00, y = 0.8), 
                                   colorscale = c('#1972A4', '#FF7070'),
                                   cauto = F,
                                   cmin = -max(abs(Data$beta))*6,
                                   cmax = max(abs(Data$beta))*6,showscale = F)) %>%
            add_markers() %>%
            layout(scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable",
                                camera = list(eye = list(x = cos(3.3)*2, y = sin(3.3)*2, z= 0.23))))
          
          p<- add_trace(p = p,
                        z = Data[pos,3],
                        x = Data[pos,1],
                        y = Data[pos,2],type = "scatter3d", mode = "markers",inherit = T,
                        marker = list(color =  Data[pos,4],colorbar=list(title='beta coefficient - (a.u.)',len = 0.55, x = 1.13, y = 0.4),
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto= F,
                                      cmin = -max(abs(Data[pos,4])),
                                      cmax = max(abs(Data[pos,4])), showscale = T))%>%
            layout(showlegend = F)
          p
        }
      })
    })
  })
}

