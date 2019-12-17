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
# library(multtest)
# library(mutools3D)
choices <- c("Age","BSA","SBP","Duration of Vigorous activity (in mins)")
# extractNames <- c("Race", "Sex", "Age", "Smoking", "SBP")
points <- c("Endocardial", "Epicardial","Full shape")
pv_names <- c("pvalues","BHpvalues","BHpvaluesTFCE")
function(input, output,session) {
  
  phen <- reactive(input$phen)
  # nPermutations <- reactive(input$obs)
  whichpoint <- reactive({input$whichee})
  Sel <- reactive(input$typeInput)
  Cntrl <- reactive(input$typecontrol)
  datasetInput <- reactive(input$dataset)
  BH <- reactive(input$checkbox1)
  TFCE <- reactive(input$checkbox2)
  ctrl <- reactive(input$checkbox3)
  observe({
    
    #################################
    endoEpi <- read.table("endo_epi.txt")
    vert2print <- list(which(endoEpi[,4]==0),which(endoEpi[,4]==1),1:length(endoEpi[,4]))
    
    
    if (input$checkbox3 == FALSE) {
      output$coolplot <- renderPlotly({
        print(paste("Phenotype: ",phen()))
        print(paste("Covariate: ",Sel()))
        print(paste("Control for: ",Cntrl()))
        ##### plot ##########
        #####################
        # setwd("Z:/Experiments_of_Maria/20191128_Shinyapp_interactive_phenotypic_applications/All_final")
        if (Sel() == choices[4]){
          choices = c("Age","BSA","SBP","Vigorous")
          Sel<-"Vigorous"
        } else {
          choices = c("Age","BSA","SBP","Duration of Vigorous activity (in mins)")
          Sel<-Sel()
        }
        print(choices)
        if (TFCE() == FALSE & BH() == FALSE) {
          
          num_sel<-which(choices==Sel)
          # print(num_sel)
          filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
          # print(filepath)
          Data <- read.table(paste(filepath,"_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
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
          
          if (datasetInput() == "Beta") {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste(choices[num_sel],"_beta_",phen(),".txt", sep = "")
              },
              content = function(file) {
                write.table(Data, file, col.names = TRUE, row.names = FALSE)
              }
            )
          } else if (datasetInput() == "Pvalues") {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste(choices[num_sel],"_pvalues_",phen(),".txt", sep = "")
              },
              content = function(file) {
                write.table(Data_pvalues, file, col.names = TRUE, row.names = FALSE)
              }
            )
          }
          # ax <- list(
          #   title = "",
          #   zeroline = F,
          #   showline = F,
          #   showticklabels = F,
          #   showgrid = F)
          # p<-plot_ly(Data, x = ~x , y = ~y , z = ~z,
          #            marker = list(color = ~beta,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.00, y = 0.8), 
          #                          colorscale = c('#1972A4', '#FF7070'),
          #                          cauto = F,
          #                          cmin = -max(Data$beta),
          #                          cmax = max(Data$beta),showscale = T)) %>%
          #   add_markers() %>%
          #   layout(scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable",
          #                       camera = list(eye = list(x = cos(3.3)*2, y = sin(3.3)*2, z= 0.23))))
          # p
          # 
        } else if (TFCE() == FALSE & BH() == TRUE){
          
          
          num_sel<-which(choices==Sel)
          filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
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
          
          if (datasetInput() == "Beta") {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste(choices[num_sel],"_beta_",phen(),".txt", sep = "")
              },
              content = function(file) {
                write.table(Data, file, col.names = TRUE, row.names = FALSE)
              }
            )
          } else if (datasetInput() == "Pvalues") {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste(choices[num_sel],"_BHpvalues_",phen(),".txt", sep = "")
              },
              content = function(file) {
                write.table(Data_pvalues, file, col.names = TRUE, row.names = FALSE)
              }
            )
          }
          
        } else if (TFCE() == TRUE & BH() == TRUE) {
          num_sel<-which(choices==Sel)
          filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
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
          
          if (datasetInput() == "Beta") {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste(choices[num_sel],"_beta_",phen(),".txt", sep = "")
              },
              content = function(file) {
                write.table(Data, file, col.names = TRUE, row.names = FALSE)
              }
            )
          } else if (datasetInput() == "Pvalues") {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste(choices[num_sel],"_BHpvaluesTFCE_",phen(),".txt", sep = "")
              },
              content = function(file) {
                write.table(Data_pvalues, file, col.names = TRUE, row.names = FALSE)
              }
            )
          }
          
        } else if (TFCE() == TRUE & BH() == FALSE) {
          num_sel<-which(choices==Sel)
          filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
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
          
          
          if (datasetInput() == "Beta") {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste(choices[num_sel],"_beta_",phen(),".txt", sep = "")
              },
              content = function(file) {
                write.table(Data, file, col.names = TRUE, row.names = FALSE)
              }
            )
          } else if (datasetInput() == "Pvalues") {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste(choices[num_sel],"_pvaluesTFCE_",phen(),".txt", sep = "")
              },
              content = function(file) {
                write.table(Data_pvalues, file, col.names = TRUE, row.names = FALSE)
              }
            )
          }
          
        }
        
        pos<-which(Data_pvalues$pvalues < 0.05)
        sig <- (length(pos)/nrow(Data))*100
        significance_area<-format(round(sig, 0), nsmall = 0)
        average_beta <- format(round(mean(Data$beta[pos]), 2), nsmall = 2)
        print(significance_area)
        
        if (length(pos) == 0) {
          output$text <- renderUI({
            str1 <- paste("Beta coefficient for",phen()," vs ", Sel(),"in UKBB subjects, n= 8212")
            HTML(paste(str1))
            
          })
          ax <- list(
            title = "",
            zeroline = F,
            showline = F,
            showticklabels = F,
            showgrid = F)
          p<-plot_ly(Data, x = ~x , y = ~y , z = ~z,
                     marker = list(color = ~beta,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.00, y = 0.8), 
                                   colorscale = c('#1972A4', '#FF7070'),
                                   cauto = F,
                                   cmin = -abs(max(Data$beta)),
                                   cmax = abs(max(Data$beta)),showscale = T)) %>%
            add_markers() %>%
            layout(scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable",
                                camera = list(eye = list(x = cos(3.3)*2, y = sin(3.3)*2, z= 0.23))))
          p
        } else {
          
          output$text <- renderUI({
            str1 <- paste("<b>","Beta coefficient for ", phen()," vs ", Sel(),"in UKBB subjects,","<b>")
            str2 <- paste("<b>","beta = ",average_beta, "and significant area = ",significance_area, "%, n= 8212<b>")
            
            HTML(paste(str1, str2, sep = '<br/>'))
            
          })
          
          ax <- list(
            title = "",
            zeroline = F,
            showline = F,
            showticklabels = F,
            showgrid = F)
          p<-plot_ly(Data, x = ~x , y = ~y , z = ~z,
                     marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.00, y = 0.8), 
                                   colorscale = c('#1972A4', '#FF7070'),
                                   cauto = F,
                                   cmin = -abs(max(Data$beta))*6,
                                   cmax = abs(max(Data$beta))*6,showscale = F)) %>%
            add_markers() %>%
            layout(scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable",
                                camera = list(eye = list(x = cos(3.3)*2, y = sin(3.3)*2, z= 0.23))))
          
          p<- add_trace(p = p,
                        z = Data[pos,3],
                        x = Data[pos,1],
                        y = Data[pos,2],type = "scatter3d", mode = "markers",inherit = T,
                        marker = list(color =  Data[pos,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto= F,
                                      cmin = -abs(max(Data[pos,4])),
                                      cmax = abs(max(Data[pos,4])), showscale = T))%>%
            layout(showlegend = F)
          p
          
          
        }
        
      })
      
    } else {
      
      if (Sel() == choices[4]){
        choices = c("Age","BSA","SBP","Vigorous")
        Sel<-"Vigorous"
      } else {
        choices = c("Age","BSA","SBP","Duration of Vigorous activity (in mins)")
        Sel<-Sel()
      }
      observe({
        x <- input$typeInput
        print(x)
        
        # Can use character(0) to remove all choices
        if (x == "SBP"){
          
          # Can also set the label and select items
          updateSelectInput(session, "typecontrol",
                            "Select covariates to control for:",
                            choices = c("Race", "Sex", "Age", "Smoking"),
                            selected = "Race"
          )
        } else if (x == "Age"){
          
          # Can also set the label and select items
          updateSelectInput(session, "typecontrol",
                            "Select covariates to control for:",
                            choices = c("Race", "Sex", "Smoking", "SBP"),
                            selected = "Race"
          )
        }
        output$coolplot <- renderPlotly({
          #   #####for control ##########
          if (Cntrl()=="Race"){ # Race
            
            if (TFCE() == FALSE & BH() == FALSE) {
              
              num_sel<-which(choices==Sel)
              # print(num_sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              # print(filepath)
              Data <- read.table(paste(filepath,"_Race_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Race_pvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Race_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Race_pvalues_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == FALSE & BH() == TRUE){
              
              
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_Race_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Race_BHpvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Race_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Race_BHpvalues_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == TRUE & BH() == TRUE) {
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_Race_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Race_BHpvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Race_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Race_BHpvaluesTFCE_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == TRUE & BH() == FALSE) {
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_Race_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Race_pvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Race_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Race_pvaluesTFCE_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            }
            colnames(Data) <- c("x", "y", "z", "beta0", "beta1","beta2","beta3","beta4")
            colnames(Data_pvalues) <- c("x", "y", "z", "pvalues0", "pvalues1","pvalues2","pvalues3","pvalues4")
            
            avb <- matrix(0,ncol = 5, nrow = 1)
            signif <- matrix(0,ncol = 5, nrow = 1)
            iS<-4
            iE<-0
            for (iP in 4:8){
              iE<-iE+1
              Dat<-Data[,c(1:3,iS)]
              colnames(Dat) <- c("x", "y", "z", "beta")
              Datp<-Data_pvalues[,c(1:3,iS)]
              colnames(Datp) <- c("x", "y", "z", "pvalues")
              pos<-which(Datp$pvalues < 0.05)
              print(length(pos))
              sig <- (length(pos)/nrow(Dat))*100
              significance_area<-format(round(sig, 0), nsmall = 0)
              average_beta <- format(round(mean(Dat$beta[pos]), 2), nsmall = 2)
              iS<-iS+1
              avb[,iE]<- average_beta
              signif[,iE] <-significance_area
            }
            #     
            output$text <- renderUI({
              str1 <- paste("Beta coefficient for ", phen()," vs ", Sel(),"in UKBB subjects,")
              str2 <- paste("average beta for White = ",avb[,1], "and significant area = ", signif[,1], "%,")
              str3 <- paste("average beta for Asian = ",avb[,2], "and significant area = ", signif[,2], "%,")
              str4 <- paste("average beta for Chinese = ",avb[,3], "and significant area = ", signif[,3], "%,")
              str5 <- paste("average beta for African = ",avb[,4], "and significant area = ", signif[,4], "%,")
              str6 <- paste("average beta for Mixed = ",avb[,5], "and significant area = ", signif[,5], "%, n = 8212")
              
              HTML(paste(str1, str2, str3, str4, str5, str6, sep = '<br/>'))
              #       
            })
            ax <- list(title = "",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax1 <- list(title = "White ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax2 <- list(title = "Asian ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax3 <- list(title = "Chinese ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax4 <- list(title = "African ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax5 <- list(title = "Mixed ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            
            pos<-which(Data_pvalues$pvalues0 < 0.05)
            Data0<-Data[,1:4]
            if (length(pos)==0) {
              p1<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                          marker = list(color = ~beta0,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = T,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = T)) %>%
                add_markers(showlegend = FALSE)
            } else {
              p1<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = T,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data0[pos,3],x = Data0[pos,1], y = Data0[pos,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data0[pos,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = T))
            }
            pos1<-which(Data_pvalues$pvalues1 < 0.05)
            Data1<-Data[,c(1:3,5)]
            if (length(pos1)==0) {
              p2<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                          marker = list(color = ~beta1,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = F)) %>%
                add_markers(showlegend = FALSE) 
            } else {
              p2<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data1[pos1,3],x = Data1[pos1,1], y = Data1[pos1,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data1[pos1,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = F))
            }
            pos2<-which(Data_pvalues$pvalues2 < 0.05)
            Data2<-Data[,c(1:3,6)]
            if (length(pos2)==0) {
              p3<-plot_ly(Data2, x = ~x , y = ~y , z = ~z,scene='scene3',
                          marker = list(color = ~beta2,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = F)) %>%
                add_markers(showlegend = FALSE) 
            } else {
              p3<-plot_ly(Data2, x = ~x , y = ~y , z = ~z,scene='scene3',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data2[pos2,3],x = Data2[pos2,1], y = Data2[pos2,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data2[pos2,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = F))
            }
            pos3<-which(Data_pvalues$pvalues3 < 0.05)
            Data3<-Data[,c(1:3,7)]
            if (length(pos3)==0) {
              p4<-plot_ly(Data3, x = ~x , y = ~y , z = ~z,scene='scene4',
                          marker = list(color = ~beta3,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = F)) %>%
                add_markers(showlegend = FALSE) 
            } else {
              p4<-plot_ly(Data3, x = ~x , y = ~y , z = ~z,scene='scene4',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data3[pos3,3],x = Data3[pos3,1], y = Data3[pos3,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data3[pos3,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = F))
            }
            pos4<-which(Data_pvalues$pvalues4 < 0.05)
            Data4<-Data[,c(1:3,8)]
            if (length(pos4)==0) {
              p5<-plot_ly(Data4, x = ~x , y = ~y , z = ~z,scene='scene5',
                          marker = list(color = ~beta4,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = F)) %>%
                add_markers(showlegend = FALSE) 
            } else {
              p5<-plot_ly(Data4, x = ~x , y = ~y , z = ~z,scene='scene5',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data4[pos4,3],x = Data4[pos4,1], y = Data4[pos4,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data4[pos4,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = F))
              
            }
            p<-subplot(p1,p2,p3,p4,p5, nrows = 2) %>%
              layout(title = "",
                     scene = list(domain=list(x=c(0,0.35),y=c(0.5,1)),xaxis = ax,yaxis = ax, zaxis = ax1, dragmode="turntable"),
                     scene2 = list(domain=list(x=c(0.3,0.8),y=c(0.5,1)),xaxis = ax,yaxis = ax, zaxis = ax2, dragmode="turntable"),
                     scene3 = list(domain=list(x=c(0.2,0),y=c(0.5,1)),xaxis = ax,yaxis = ax, zaxis = ax3, dragmode="turntable"),
                     scene4 = list(domain=list(x=c(0,0.35),y=c(0,0.5)),xaxis = ax,yaxis = ax, zaxis = ax4, dragmode="turntable"),
                     scene5 = list(domain=list(x=c(0.3,0.8),y=c(0,0.5)),xaxis = ax,yaxis = ax, zaxis = ax5, dragmode="turntable"),showlegend = FALSE)
            p
            
          } else if (Cntrl()=="Sex"){ # Sex
            
            if (TFCE() == FALSE & BH() == FALSE) {
              
              num_sel<-which(choices==Sel)
              # print(num_sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              # print(filepath)
              Data <- read.table(paste(filepath,"_Sex_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Sex_pvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Sex_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Sex_pvalues_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == FALSE & BH() == TRUE){
              
              
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_Sex_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Sex_BHpvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Sex_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Sex_BHpvalues_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == TRUE & BH() == TRUE) {
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_Sex_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Sex_BHpvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Sex_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Sex_BHpvaluesTFCE_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == TRUE & BH() == FALSE) {
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_Sex_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Sex_pvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Sex_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Sex_pvaluesTFCE_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            }
            colnames(Data) <- c("x", "y", "z", "beta0", "beta1")
            colnames(Data_pvalues) <- c("x", "y", "z", "pvalues0", "pvalues1")
            
            avb <- matrix(0,ncol = 2, nrow = 1)
            signif <- matrix(0,ncol = 2, nrow = 1)
            iS<-4
            iE<-0
            for (iP in 4:5){
              iE<-iE+1
              Dat<-Data[,c(1:3,iS)]
              colnames(Dat) <- c("x", "y", "z", "beta")
              Datp<-Data_pvalues[,c(1:3,iS)]
              colnames(Datp) <- c("x", "y", "z", "pvalues")
              pos<-which(Datp$pvalues < 0.05)
              print(length(pos))
              sig <- (length(pos)/nrow(Dat))*100
              significance_area<-format(round(sig, 0), nsmall = 0)
              average_beta <- format(round(mean(Dat$beta[pos]), 2), nsmall = 2)
              iS<-iS+1
              avb[,iE]<- average_beta
              signif[,iE] <-significance_area
            }
            
            output$text <- renderUI({
              str1 <- paste("Beta coefficient for ", phen()," vs ", Sel(),"in UKBB subjects,")
              str2 <- paste("average beta for Female = ",avb[,1], "and significant area = ",signif[,1], "%,")
              str3 <- paste("average beta for Male = ",avb[,2], "and significant area = ",signif[,2], "%, n = 8212")
              
              HTML(paste(str1, str2, str3, sep = '<br/>'))
              
            })
            ax <- list(title = "",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax1 <- list(title = "Female ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax2 <- list(title = "Male ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            
            pos<-which(Data_pvalues$pvalues0 < 0.05)
            Data0<-Data[,1:4]
            if (length(pos)==0) {
              p1<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                          marker = list(color = ~beta0,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = T,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = T)) %>%
                add_markers(showlegend = FALSE)
            } else {
              p1<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = T,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data0[pos,3],x = Data0[pos,1], y = Data0[pos,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data0[pos,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = T))
            }
            pos1<-which(Data_pvalues$pvalues1 < 0.05)
            Data1<-Data[,c(1:3,5)]
            if (length(pos1)==0) {
              p2<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                          marker = list(color = ~beta1,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = F)) %>%
                add_markers(showlegend = FALSE) 
            } else {
              p2<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data1[pos1,3],x = Data1[pos1,1], y = Data1[pos1,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data1[pos1,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = F))
            }
            p <- subplot(p1, p2) %>%
              layout(title="",scene = list(domain=list(x=c(0,0.48),y=c(0.1,1)),xaxis = ax,yaxis = ax, zaxis = ax1, dragmode="turntable"),
                     scene2 = list(domain=list(x=c(0.48,1),y=c(0.1,1)),xaxis = ax,yaxis = ax, zaxis = ax2, dragmode="turntable"),showlegend = FALSE)
            p
            
          } else if (Cntrl()=="Age"){ # Age
            
            if (TFCE() == FALSE & BH() == FALSE) {
              
              num_sel<-which(choices==Sel)
              # print(num_sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              # print(filepath)
              Data <- read.table(paste(filepath,"_Age_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Age_pvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Age_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Age_pvalues_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == FALSE & BH() == TRUE){
              
              
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_Age_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Age_BHpvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Age_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Age_BHpvalues_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == TRUE & BH() == TRUE) {
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_Age_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Age_BHpvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Age_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Age_BHpvaluesTFCE_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == TRUE & BH() == FALSE) {
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_Age_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Age_pvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Age_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Age_pvaluesTFCE_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            }
            colnames(Data) <- c("x", "y", "z", "beta0", "beta1","beta2")
            colnames(Data_pvalues) <- c("x", "y", "z", "pvalues0", "pvalues1","pvalues2")
            
            avb <- matrix(0,ncol = 3, nrow = 1)
            signif <- matrix(0,ncol = 3, nrow = 1)
            iS<-4
            iE<-0
            for (iP in 4:6){
              iE<-iE+1
              Dat<-Data[,c(1:3,iS)]
              colnames(Dat) <- c("x", "y", "z", "beta")
              Datp<-Data_pvalues[,c(1:3,iS)]
              colnames(Datp) <- c("x", "y", "z", "pvalues")
              pos<-which(Datp$pvalues < 0.05)
              print(length(pos))
              sig <- (length(pos)/nrow(Dat))*100
              significance_area<-format(round(sig, 0), nsmall = 0)
              average_beta <- format(round(mean(Dat$beta[pos]), 2), nsmall = 2)
              iS<-iS+1
              avb[,iE]<- average_beta
              signif[,iE] <-significance_area
              print(avb)
              print(signif)
            }
            
            output$text <- renderUI({
              str1 <- paste("Beta coefficient for ", phen()," vs ", Sel(),"in UKBB subjects,")
              str2 <- paste("average beta for 40 to 50 y.o. = ",avb[,1], "and significant area = ",signif[,1], "%,")
              str3 <- paste("average beta for 50 to 60 y.o = ",avb[,2], "and significant area = ",signif[,2], "%,")
              str4 <- paste("average beta for 60 to 70 y.o. = ",avb[,3], "and significant area = ",signif[,3], "%, n=8212")
              
              HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
              
            })
            ax <- list(title = "",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax1 <- list(title = "40 to 50 y.o. ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax2 <- list(title = "50 to 60 y.o. ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax3 <- list(title = "60 to 70 y.o. ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            
            pos<-which(Data_pvalues$pvalues0 < 0.05)
            Data0<-Data[,1:4]
            if (length(pos)==0) {
              p1<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                          marker = list(color = ~beta0,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = T,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = T)) %>%
                add_markers(showlegend = FALSE)
            } else {
              p1<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = T,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data0[pos,3],x = Data0[pos,1], y = Data0[pos,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data0[pos,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = T))
            }
            pos1<-which(Data_pvalues$pvalues1 < 0.05)
            Data1<-Data[,c(1:3,5)]
            if (length(pos1)==0) {
              p2<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                          marker = list(color = ~beta1,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = F)) %>%
                add_markers(showlegend = FALSE) 
            } else {
              p2<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data1[pos1,3],x = Data1[pos1,1], y = Data1[pos1,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data1[pos1,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = F))
            }
            pos2<-which(Data_pvalues$pvalues2 < 0.05)
            Data2<-Data[,c(1:3,6)]
            if (length(pos2)==0) {
              p3<-plot_ly(Data2, x = ~x , y = ~y , z = ~z,scene='scene3',
                          marker = list(color = ~beta2,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = F)) %>%
                add_markers(showlegend = FALSE) 
            } else {
              p3<-plot_ly(Data2, x = ~x , y = ~y , z = ~z,scene='scene3',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data2[pos2,3],x = Data2[pos2,1], y = Data2[pos2,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data2[pos2,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = F))
            }
            p<-subplot(p1,p2,p3, nrows = 2) %>%
              layout(title = "",
                     scene = list(domain=list(x=c(0,0.5),y=c(0.5,1)),xaxis = ax,yaxis = ax, zaxis = ax1, dragmode="turntable"),
                     scene2 = list(domain=list(x=c(0.5,1),y=c(0.5,1)),xaxis = ax,yaxis = ax, zaxis = ax2, dragmode="turntable"),
                     scene3 = list(domain=list(x=c(0,0.5),y=c(0,0.5)),xaxis = ax,yaxis = ax, zaxis = ax3, dragmode="turntable"),showlegend = FALSE)
            p
          } else if (Cntrl()=="Smoking"){ # Smoking
            
            if (TFCE() == FALSE & BH() == FALSE) {
              
              num_sel<-which(choices==Sel)
              # print(num_sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              # print(filepath)
              Data <- read.table(paste(filepath,"_Smoking_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Smoking_pvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Smoking_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Smoking_pvalues_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == FALSE & BH() == TRUE){
              
              
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_Smoking_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Smoking_BHpvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Smoking_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Smoking_BHpvalues_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == TRUE & BH() == TRUE) {
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_Smoking_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Age_BHpvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Smoking_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Smoking_BHpvaluesTFCE_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == TRUE & BH() == FALSE) {
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_Smoking_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_Smoking_pvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Smoking_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_Smoking_pvaluesTFCE_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            }
            colnames(Data) <- c("x", "y", "z", "beta0", "beta1","beta2")
            colnames(Data_pvalues) <- c("x", "y", "z", "pvalues0", "pvalues1","pvalues2")
            
            avb <- matrix(0,ncol = 3, nrow = 1)
            signif <- matrix(0,ncol = 3, nrow = 1)
            iS<-4
            iE<-0
            for (iP in 4:6){
              iE<-iE+1
              Dat<-Data[,c(1:3,iS)]
              colnames(Dat) <- c("x", "y", "z", "beta")
              Datp<-Data_pvalues[,c(1:3,iS)]
              colnames(Datp) <- c("x", "y", "z", "pvalues")
              pos<-which(Datp$pvalues < 0.05)
              print(length(pos))
              sig <- (length(pos)/nrow(Dat))*100
              significance_area<-format(round(sig, 0), nsmall = 0)
              average_beta <- format(round(mean(Dat$beta[pos]), 2), nsmall = 2)
              iS<-iS+1
              avb[,iE]<- average_beta
              signif[,iE] <-significance_area
            }
            print(avb)
            print(signif)
            output$text <- renderUI({
              str1 <- paste("Beta coefficient for ", phen()," vs ", Sel(),"in UKBB subjects,")
              str2 <- paste("average beta for Non-smokers = ",avb[,1], "and significant area = ", signif[,1], "%,")
              str3 <- paste("average beta for Previous smokers = ",avb[,2], "and significant area = ", signif[,2], "%")
              str4 <- paste("average beta for Current smokers = ",avb[,3], "and significant area = ", signif[,3], "%,n = 8212")
              
              HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
              
            })
            
            ax <- list(title = "",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax1 <- list(title = "Never ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax2 <- list(title = "Previous ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax3 <- list(title = "Current ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            pos<-which(Data_pvalues$pvalues0 < 0.05)
            Data0<-Data[,1:4]
            if (length(pos)==0) {
              p1<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                          marker = list(color = ~beta0,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = T,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = T)) %>%
                add_markers(showlegend = FALSE)
            } else {
              p1<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = T,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data0[pos,3],x = Data0[pos,1], y = Data0[pos,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data0[pos,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = T))
            }
            pos1<-which(Data_pvalues$pvalues1 < 0.05)
            Data1<-Data[,c(1:3,5)]
            if (length(pos1)==0) {
              p2<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                          marker = list(color = ~beta1,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = F)) %>%
                add_markers(showlegend = FALSE) 
            } else {
              p2<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data1[pos1,3],x = Data1[pos1,1], y = Data1[pos1,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data1[pos1,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = F))
            }
            pos2<-which(Data_pvalues$pvalues2 < 0.05)
            Data2<-Data[,c(1:3,6)]
            if (length(pos2)==0) {
              p3<-plot_ly(Data2, x = ~x , y = ~y , z = ~z,scene='scene3',
                          marker = list(color = ~beta2,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = F)) %>%
                add_markers(showlegend = FALSE) 
            } else {
              p3<-plot_ly(Data2, x = ~x , y = ~y , z = ~z,scene='scene3',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data2[pos2,3],x = Data2[pos2,1], y = Data2[pos2,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data2[pos2,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = F))
            }
            p<-subplot(p1,p2,p3, nrows = 2) %>%
              layout(title = "",
                     scene = list(domain=list(x=c(0,0.5),y=c(0.5,1)),xaxis = ax,yaxis = ax, zaxis = ax1, dragmode="turntable"),
                     scene2 = list(domain=list(x=c(0.5,1),y=c(0.5,1)),xaxis = ax,yaxis = ax, zaxis = ax2, dragmode="turntable"),
                     scene3 = list(domain=list(x=c(0,0.5),y=c(0,0.5)),xaxis = ax,yaxis = ax, zaxis = ax3, dragmode="turntable"),showlegend = FALSE)
            p
            
          } else if (Cntrl()=="SBP"){ # SBP
            
            if (TFCE() == FALSE & BH() == FALSE) {
              
              num_sel<-which(choices==Sel)
              # print(num_sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              # print(filepath)
              Data <- read.table(paste(filepath,"_SBP_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_SBP_pvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_SBP_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_SBP_pvalues_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == FALSE & BH() == TRUE){
              
              
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_SBP_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_SBP_BHpvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_SBP_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_SBP_BHpvalues_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == TRUE & BH() == TRUE) {
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_SBP_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_SBP_BHpvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_SBP_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_SBP_BHpvaluesTFCE_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            } else if (TFCE() == TRUE & BH() == FALSE) {
              num_sel<-which(choices==Sel)
              filepath<-paste(choices[num_sel],choices[num_sel],sep = "/")
              Data <- read.table(paste(filepath,"_SBP_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
              Data_pvalues <- read.table(paste(filepath,"_SBP_pvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
              
              
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
              
              
              if (datasetInput() == "Beta") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_SBP_beta_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              } else if (datasetInput() == "Pvalues") {
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste(choices[num_sel],"_SBP_pvaluesTFCE_",phen(),".txt", sep = "")
                  },
                  content = function(file) {
                    write.table(Data_pvalues, file, col.names = FALSE, row.names = FALSE)
                  }
                )
              }
              
            }
            colnames(Data) <- c("x", "y", "z", "beta0", "beta1","beta2")
            colnames(Data_pvalues) <- c("x", "y", "z", "pvalues0", "pvalues1","pvalues2")
            
            avb <- matrix(0,ncol = 3, nrow = 1)
            signif <- matrix(0,ncol = 3, nrow = 1)
            iS<-4
            iE<-0
            for (iP in 4:6){
              iE<-iE+1
              Dat<-Data[,c(1:3,iS)]
              colnames(Dat) <- c("x", "y", "z", "beta")
              Datp<-Data_pvalues[,c(1:3,iS)]
              colnames(Datp) <- c("x", "y", "z", "pvalues")
              pos<-which(Datp$pvalues < 0.05)
              print(length(pos))
              sig <- (length(pos)/nrow(Dat))*100
              significance_area<-format(round(sig, 0), nsmall = 0)
              average_beta <- format(round(mean(Dat$beta[pos]), 2), nsmall = 2)
              iS<-iS+1
              avb[,iE]<- average_beta
              signif[,iE] <-significance_area
            }
            output$text <- renderUI({
              str1 <- paste("Beta coefficient for ", phen()," vs ", Sel(),"in UKBB subjects,")
              str2 <- paste("average beta for Normotensive = ",avb[,1], "and significant area = ", signif[,1], "%,")
              str3 <- paste("average beta for Prehypertensive = ",avb[,2], "and significant area = ", signif[,2], "%,")
              str4 <- paste("average beta for Hypertensive = ",avb[,3], "and significant area = ", signif[,3], "%, n = 8212")
              
              HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
              
            })
            
            ax <- list(title = "",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax1 <- list(title = "Normotensive ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax2 <- list(title = "Prehypertensive ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            ax3 <- list(title = "Hypertensive ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
            
            pos<-which(Data_pvalues$pvalues0 < 0.05)
            Data0<-Data[,1:4]
            if (length(pos)==0) {
              p1<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                          marker = list(color = ~beta0,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = T,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = T)) %>%
                add_markers(showlegend = FALSE)
            } else {
              p1<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = T,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data0[pos,3],x = Data0[pos,1], y = Data0[pos,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data0[pos,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = T))
            }
            pos1<-which(Data_pvalues$pvalues1 < 0.05)
            Data1<-Data[,c(1:3,5)]
            if (length(pos1)==0) {
              p2<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                          marker = list(color = ~beta1,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = F)) %>%
                add_markers(showlegend = FALSE) 
            } else {
              p2<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data1[pos1,3],x = Data1[pos1,1], y = Data1[pos1,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data1[pos1,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = F))
            }
            pos2<-which(Data_pvalues$pvalues2 < 0.05)
            Data2<-Data[,c(1:3,6)]
            if (length(pos2)==0) {
              p3<-plot_ly(Data2, x = ~x , y = ~y , z = ~z,scene='scene3',
                          marker = list(color = ~beta2,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0)),
                                        cmax = abs(max(Data$beta0)),showscale = F)) %>%
                add_markers(showlegend = FALSE) 
            } else {
              p3<-plot_ly(Data2, x = ~x , y = ~y , z = ~z,scene='scene3',
                          marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto = F,
                                        cmin = -abs(max(Data$beta0))*5,
                                        cmax = abs(max(Data$beta0))*5,showscale = F)) %>%
                add_markers() %>%
                layout(title = "")%>%
                add_trace(z = Data2[pos2,3],x = Data2[pos2,1], y = Data2[pos2,2],type = "scatter3d", mode = "markers",
                          marker = list(color =  Data2[pos2,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                        colorscale = c('#1972A4', '#FF7070'),
                                        cauto= F,
                                        cmin = -abs(max(Data0[pos,4])),
                                        cmax = abs(max(Data0[pos,4])), showscale = F))
            }
            p<-subplot(p1,p2,p3, nrows = 2) %>%
              layout(title = "",
                     scene = list(domain=list(x=c(0,0.5),y=c(0.5,1)),xaxis = ax,yaxis = ax, zaxis = ax1, dragmode="turntable"),
                     scene2 = list(domain=list(x=c(0.5,1),y=c(0.5,1)),xaxis = ax,yaxis = ax, zaxis = ax2, dragmode="turntable"),
                     scene3 = list(domain=list(x=c(0,0.5),y=c(0,0.5)),xaxis = ax,yaxis = ax, zaxis = ax3, dragmode="turntable"),showlegend = FALSE)
            p
            
          }
        })
      }) 
      
    }    
    
  })
}

