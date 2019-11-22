######################################################
################# server.R ##########################
######################################################
######################################################
#rm(list = ls(all = TRUE))  
#start with an empty environment
#install.packages("shiny")
setwd("~/cardiac/Experiments_of_Maria/20191024_ShinyApp/")
library(shiny)


function(input, output) {
  
  phen <- reactive(input$phen)
  nPermutations <- reactive(input$obs)
  whichEE <- reactive({input$whichee})
  Sel <- reactive(input$typeInput)
  Cntrl <- reactive(input$typecontrol)
  Group <- reactive(input$group)
  
  BH <- reactive(input$checkbox1)
  TFCE <- reactive(input$checkbox2)
  ctrl <- reactive(input$checkbox3)
  #timer <- reactiveVal(10)
  #active <- reactiveVal(FALSE)
  observeEvent(input$start, {
    initial.ok <- 0
    #active <- reactiveVal(FALSE)
    showModal(modalDialog(
      title = "Important message",
      "Data loading. This will take 5 minutes"
    ))
    
    start.time <- Sys.time()
    if (initial.ok < input$stop) {
      initial.ok <- initial.ok + 1
      stop("Interrupted")
    }
    print(paste("Phenotype: ",phen()))
    print(paste("Covariate: ",Sel()))
    print(paste("Control for: ",Cntrl()))
    print(paste("Group: ",Group()))
    print(paste("Number of permutations: ",nPermutations()))
    # X,Y,Z coordinates for each individual
    Xcoord <- readRDS("~/cardiac/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/WT/WTedXcoordinate.rds")
    Ycoord <- readRDS("~/cardiac/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/WT/WTedYcoordinate.rds")
    Zcoord <- readRDS("~/cardiac/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/WT/WTedZcoordinate.rds")
    
    if (phen()=="WT"){
      setwd("~/cardiac/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/WT")
      #IMAGING DATA MATRIX
      #NCOL = N POINTS ON THE ATLAS
      #NROW = N PATIENTS
      Y <- readRDS("WTedLV.rds")
      #DATA PRE-PROCESSING
      Y <- scale(Y)
      dim(Y)
      #CLINICAL DATA MATRIX
      #NCOL = N COVARIATES UNDER STUDY
      #NROW = N PATIENTS
      X <- readRDS(paste("WTedmodel.rds"))
      dim(X)
      inputClinical <- data.frame(readRDS(paste("WTedLVclinicalData.rds")))
    } else if (phen()=="S2S"){
      setwd("~/cardiac/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/S2S")
      #IMAGING DATA MATRIX
      #NCOL = N POINTS ON THE ATLAS
      #NROW = N PATIENTS
      Y <- readRDS("S2SedLV.rds")
      #DATA PRE-PROCESSING
      Y <- scale(Y)
      dim(Y)
      #CLINICAL DATA MATRIX
      #NCOL = N COVARIATES UNDER STUDY
      #NROW = N PATIENTS
      X <- readRDS(paste("S2Sedmodel.rds"))
      dim(X)
      inputClinical <- data.frame(readRDS(paste("S2SedLVclinicalData.rds")))
    } else if (phen()=="Curvature"){
      setwd("~/cardiac/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/Curvature")
      #IMAGING DATA MATRIX
      #NCOL = N POINTS ON THE ATLAS
      #NROW = N PATIENTS
      Y <- readRDS("CurvatureedLV.rds")
      #DATA PRE-PROCESSING
      Y <- scale(Y)
      dim(Y)
      #CLINICAL DATA MATRIX
      #NCOL = N COVARIATES UNDER STUDY
      #NROW = N PATIENTS
      X <- readRDS(paste("Curvatureedmodel.rds"))
      dim(X)
      inputClinical <- data.frame(readRDS(paste("CurvatureedLVclinicalData.rds")))
    }
    
    
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken) 
    showModal(modalDialog(
      title = "Important message",
      "Loading completed!"
    ))
    
    #print(dim(mesh_Coordinates))
    extract <- which(extractNames == Sel())
    iSNP <- extract
    output$coolplot <- renderPlotly({
      endoEpi <- read.table("~/cardiac/Experiments_of_Maria/20191024_ShinyApp/shinyplot/endo_epi.txt")
      vert2print <- list(which(endoEpi[,4]==0),which(endoEpi[,4]==1),1:length(endoEpi[,4]))
      
      if (whichEE()==1){
        ##READ THE NNLIST ASSOCIATED TO EACH VERTEX
        NNmatrix <- readRDS("~/cardiac/Experiments_of_Maria/20191024_ShinyApp/shinyplot/redendoNNmatrix.rds")
        ##READ AREAS ASSOCIATED TO EACH VERTEX
        A <- readRDS("~/cardiac/Experiments_of_Maria/20191024_ShinyApp/shinyplot/endoLVareas.rds")
        
        Yout <- Y[,vert2print[[1]]]
        x_Coordinates <- Xcoord[,vert2print[[1]]]
        y_Coordinates <- Ycoord[,vert2print[[1]]]
        z_Coordinates <- Zcoord[,vert2print[[1]]]
        
      } else if (whichEE()==2){
        ##READ THE NNLIST ASSOCIATED TO EACH VERTEX
        NNmatrix <- readRDS("~/cardiac/Experiments_of_Maria/20191024_ShinyApp/shinyplot/redepiNNmatrix.rds")
        ##READ AREAS ASSOCIATED TO EACH VERTEX
        A <- readRDS("~/cardiac/Experiments_of_Maria/20191024_ShinyApp/shinyplot/epiLVareas.rds")
        Yout <- Y[,vert2print[[2]]]
        x_Coordinates <- Xcoord[,vert2print[[2]]]
        y_Coordinates <- Ycoord[,vert2print[[2]]]
        z_Coordinates <- Zcoord[,vert2print[[2]]]
        
      } else if (whichEE()==3){
        ##READ THE NNLIST ASSOCIATED TO EACH VERTEX
        NNmatrix <- readRDS("~/cardiac/Experiments_of_Maria/20191024_ShinyApp/shinyplot/redNNmatrix.rds")
        ##READ AREAS ASSOCIATED TO EACH VERTEX
        A <- readRDS("~/cardiac/Experiments_of_Maria/20191024_ShinyApp/shinyplot/LVarea.rds")
        Yout <- Y
        x_Coordinates <- Xcoord[,vert2print[[3]]]
        y_Coordinates <- Ycoord[,vert2print[[3]]]
        z_Coordinates <- Zcoord[,vert2print[[3]]]
      }
      
      if (Group() == choice[1]){
        
        X_coordinates <- colMeans(x_Coordinates)
        Y_coordinates <- colMeans(y_Coordinates)
        Z_coordinates <- colMeans(z_Coordinates)
        mesh_Coordinates <- data.frame(X_coordinates, Y_coordinates, Z_coordinates)
        colnames(mesh_Coordinates) <- c("x", "y", "z")
        Yfinal <- Yout
        Xout <- X
        inputClinicalout <- inputClinical
      } else if (Group() == choice[2]){
        normotensive<-which((inputClinical$SBP<120))
        
        Xco <- x_Coordinates[normotensive,]
        Yco <- y_Coordinates[normotensive,]
        Zco <- z_Coordinates[normotensive,]
        X_coordinates <- colMeans(Xco)
        Y_coordinates <- colMeans(Yco)
        Z_coordinates <- colMeans(Zco)
        mesh_Coordinates <- data.frame(X_coordinates, Y_coordinates, Z_coordinates)
        colnames(mesh_Coordinates) <- c("x", "y", "z")
        Yfinal <- Yout[normotensive,]
        Xout <- X[normotensive,]
        inputClinicalout <- inputClinical[normotensive,]
      } else if (Group() == choice[3]){
        prehpertesive<-which((inputClinical$SBP>=120) & (inputClinical$SBP<=139))
        
        Xco <- x_Coordinates[prehpertesive,]
        Yco <- y_Coordinates[prehpertesive,]
        Zco <- z_Coordinates[prehpertesive,]
        X_coordinates <- colMeans(Xco)
        Y_coordinates <- colMeans(Yco)
        Z_coordinates <- colMeans(Zco)
        mesh_Coordinates <- data.frame(X_coordinates, Y_coordinates, Z_coordinates)
        colnames(mesh_Coordinates) <- c("x", "y", "z")
        Yfinal <- Yout[prehpertesive,]
        Xout <- X[prehpertesive,]
        inputClinicalout <- inputClinical[prehpertesive,]
      } else if (Group() == choice[4]){
        hypertesive<-which((inputClinical$SBP>=140))
        
        Xco <- x_Coordinates[hypertesive,]
        Yco <- y_Coordinates[hypertesive,]
        Zco <- z_Coordinates[hypertesive,]
        X_coordinates <- colMeans(Xco)
        Y_coordinates <- colMeans(Yco)
        Z_coordinates <- colMeans(Zco)
        mesh_Coordinates <- data.frame(X_coordinates, Y_coordinates, Z_coordinates)
        colnames(mesh_Coordinates) <- c("x", "y", "z")
        Yfinal <- Yout[hypertesive,]
        Xout <- X[hypertesive,]
        inputClinicalout <- inputClinical[hypertesive,]
      }
      
      if (ctrl() == FALSE) {
        
        extract <- which(extractNames == Sel())
        iSNP <- extract
        extract <- extract+1
        if (TFCE() == FALSE & BH() == FALSE) {
          start.time <- Sys.time()
          result <- murHC4m(Xout,Yfinal,extract)
          end.time <- Sys.time()
          time.taken <- end.time - start.time
          print(time.taken) 
          pvalues <- result[,3]
          beta <- result[,1]
          
        }
        iEx <-1
        if (BH() == TRUE){
          #MULTIPLE TESTING CORRECTION
          start.time <- Sys.time()
          result <- murHC4m(Xout,Yfinal,extract)
          end.time <- Sys.time()
          time.taken <- end.time - start.time
          print(time.taken)       
          beta <- result[,1]
          corrected <- mt.rawp2adjp(result[,3+(iEx-1)*3], proc=c("BH"), na.rm = FALSE)
          pvalueADJ5tsbh <- array(dim = length(result[,3+(iEx-1)*3]))
          pvalues <- corrected$adjp[order(corrected$index),][,2]
        } 
        if (TFCE() == TRUE){
          start.time <- Sys.time()
          result <- murHC4m(Xout,Yfinal,extract)
          end.time <- Sys.time()
          time.taken <- end.time - start.time
          print(time.taken)       
          beta <- result[,1]
          start.time1 <- Sys.time()
          withProgress(message = 'Calculation in progress',
                       value = 0, {
                         n <- nPermutations()
                         EventTime <- Sys.time() + ((n*time.taken)/60)
                         for (i in 1:n) {
                           incProgress(1/n,detail = paste("The time remaining is",round(difftime(EventTime, Sys.time(), units='auto')),'mins'))
                           #Sys.sleep(time.taken)
                           sign <- permFL(Xout,Yfinal,extract,A,NNmatrix,nPermutations(), FALSE, TRUE, nofCores, E = 0.5, H = 2)
                         }
                         if (i==n) {incProgress(1/n,detail = paste("TFCE Completed!\n"))
                           Sys.sleep(2)}
                       })
          
          
          end.time1 <- Sys.time()
          time.taken1 <- end.time1 - start.time1
          print(time.taken1)      
          pvalues <- sign[,1]
        } 
        if (TFCE() == TRUE & BH() == TRUE){
          start.time <- Sys.time()
          result <- murHC4m(Xout,Yfinal,extract)
          end.time <- Sys.time()
          time.taken <- end.time - start.time
          print(time.taken)       
          beta <- result[,1]
          start.time1 <- Sys.time()
          withProgress(message = 'Calculation in progress',
                       value = 0, {
                         n <- nPermutations()
                         EventTime <- Sys.time() + ((n*time.taken)/60)
                         for (i in 1:n) {
                           incProgress(1/n,detail = paste("The time remaining is",round(difftime(EventTime, Sys.time(), units='auto')),'mins'))
                           #Sys.sleep(time.taken)
                           sign <- permFL(Xout,Yfinal,extract,A,NNmatrix,nPermutations(), FALSE, TRUE, nofCores, E = 0.5, H = 2)
                         }
                         if (i==n) {incProgress(1/n,detail = paste("TFCE Completed!\n"))
                           Sys.sleep(2)}
                       })
          
          
          end.time1 <- Sys.time()
          time.taken1 <- end.time1 - start.time1
          print(time.taken1)
          #MULTIPLE TESTING CORRECTION
          pfdr5TSBH <- mt.rawp2adjp(sign[,1+(iEx-1)*2], proc=c("BH"), na.rm = FALSE)
          pvalueADJ5tsbh <- array(dim = length(sign[,1+(iEx-1)*2]))
          pvalues <- pfdr5TSBH$adjp[order(pfdr5TSBH$index),][,2]
        } 
        
        ###### add in mesh coordinates ####
        #################################
        
        mesh_Coordinates <- cbind(mesh_Coordinates,99999)
        
        Data <- data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z, beta)
        colnames(Data) <- c("x", "y", "z", "w")
        D <- which(Data$w < 9999)
        Data <- Data[D,]
        
        Datapvalues<-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z, pvalues)
        colnames(Datapvalues) <- c("x", "y", "z", "p")
        Datapvalues <- Datapvalues[D,]
        
        pos<-which(Datapvalues$p < 0.05)
        sig <- (length(pos)/nrow(Data))*100
        significance_area<-format(round(sig, 0), nsmall = 0)
        average_beta <- format(round(mean(Data$w[pos]), 2), nsmall = 2)
        print(significance_area)
        
        ##### plot ##########
        #####################
        if (TFCE() == FALSE & BH() == FALSE) {
          output$text <- renderUI({
            str1 <- paste("Beta coefficient for WT vs ", Sel(),"in UKBB", Group(),"subjects, n=",nrow(Xout))
            HTML(paste(str1))
            
          })
          
          Data_beta <- data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z, beta)
          colnames(Data_beta) <- c("x", "y", "z", "beta")
          D <- which(Data_beta$beta < 9999)
          Data_beta <- Data_beta[D,]
          output$downloadData <- downloadHandler(
            filename = function() {
              paste(extractNames[iSNP],"_beta.txt",sep="")
            },
            content = function(file) {
              write.table(Data_beta, file, col.names = TRUE, row.names = FALSE)
            }
          )
          
          ax <- list(
            title = "",
            zeroline = F,
            showline = F,
            showticklabels = F,
            showgrid = F)
          p<-plot_ly(Data, x = ~x , y = ~y , z = ~z,
                     marker = list(color = ~w,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.00, y = 0.6), 
                                   colorscale = c('#1972A4', '#FF7070'),
                                   cauto = F,
                                   cmin = -max(Data$w),
                                   cmax = max(Data$w),showscale = T)) %>%
            add_markers() %>%
            layout(scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable",
                                camera = list(eye = list(x = cos(3.3)*2, y = sin(3.3)*2, z= 0.23))))
          p
          
        } else {
          if (BH() == TRUE){
            
            Data_all <- data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z, beta, pvalues)
            colnames(Data_all) <- c("x", "y", "z", "beta","BHpvalues")
            D <- which(Data_all$beta < 9999)
            Data_all <- Data_all[D,]
            output$downloadData <- downloadHandler(
              filename = function() {
                paste(extractNames[iSNP],"_betaBHpvalues.txt",sep="")
              },
              content = function(file) {
                write.table(Data_all, file, col.names = TRUE, row.names = FALSE)
              }
            )
          } else {Data_all <- data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z, beta, pvalues)
          colnames(Data_all) <- c("x", "y", "z", "beta","BHpvaluesTFCE")
          D <- which(Data_all$beta < 9999)
          Data_all <- Data_all[D,]
          output$downloadData <- downloadHandler(
            filename = function() {
              paste(extractNames[iSNP],"_betaBHpvaluesTFCE.txt",sep="")
            },
            content = function(file) {
              write.table(Data_all, file, col.names = TRUE, row.names = FALSE)
            }
          )
          }
          if (length(pos) == 0) {
            output$text <- renderUI({
              str1 <- paste("Beta coefficient for WT vs ", Sel(),"in UKBB", Group(),"subjects, n=",nrow(Xout))
              HTML(paste(str1))
              
            })
            ax <- list(
              title = "",
              zeroline = F,
              showline = F,
              showticklabels = F,
              showgrid = F)
            p<-plot_ly(Data, x = ~x , y = ~y , z = ~z,
                       marker = list(color = ~w,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.00, y = 0.6), 
                                     colorscale = c('#1972A4', '#FF7070'),
                                     cauto = F,
                                     cmin = -max(Data$w),
                                     cmax = max(Data$w),showscale = T)) %>%
              add_markers() %>%
              layout(scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable",
                                  camera = list(eye = list(x = cos(3.3)*2, y = sin(3.3)*2, z= 0.23))))
            p
          } else {
            
            output$text <- renderUI({
              str1 <- paste("Beta coefficient for WT vs ", Sel(),"in UKBB", Group(),"subjects,")
              str2 <- paste("beta = ",average_beta, "and significant area = ",significance_area, "%, n=",nrow(Xout))
              
              HTML(paste(str1, str2, sep = '<br/>'))
              
            })
            
            ax <- list(
              title = "",
              zeroline = F,
              showline = F,
              showticklabels = F,
              showgrid = F)
            p<-plot_ly(Data, x = ~x , y = ~y , z = ~z,
                       marker = list(color = ~w,colorbar=list(title='beta - (a.u)',len = 0.55, x = 1.00, y = 0.6), 
                                     colorscale = c('#1972A4', '#FF7070'),
                                     cauto = F,
                                     cmin = -max(Data$w)*6,
                                     cmax = max(Data$w)*6,showscale = F)) %>%
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
                                        cmin = -max(Data[pos,4]),
                                        cmax = max(Data[pos,4]), showscale = T))%>%
              layout(showlegend = F)
            p
            
            
          }
        }
      } else {
        
        ##### plot for control ##########
        #################################
        SelectControls <- function(Xin,Yfinal,extractNames){ 
          extract <- which(colnames(Xin) == Sel())
          iSNP <- extract
          bgn <-length(extractNames)+1
          fns <- ncol(Xin)
          iE<-0
          
          start.time <- Sys.time()
          beta_f<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Yfinal))
          pvalues_f<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Yfinal))
          for(ic in bgn:fns){
            iE<-iE+1
            result<- murHC4m(Xin[,c(1:length(extractNames),ic)],Yfinal,extract)
            beta_f[,iE]<-result[,1]
            pvalues_f[,iE]<-result[,3]
          }
          end.time <- Sys.time()
          time.taken <- end.time - start.time
          print(time.taken) 
          
          if (TFCE() == FALSE & BH() == FALSE) {
            Beta<-beta_f
            Pvalues<- pvalues_f
            
          } else if (BH() == TRUE){
            iE<-0
            #Beta<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Yfinal))
            Pvalues<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Yfinal))
            start.time <- Sys.time()
            for(ic in bgn:fns){
              #MULTIPLE TESTING CORRECTION
              iE<-iE+1
              corrected <- mt.rawp2adjp(pvalues_f[,1+(iE-1)], proc=c("BH"), na.rm = FALSE)
              pvalueADJ5tsbh <- array(dim = length(pvalues_f[,1+(iE-1)]))
              pvalues <- corrected$adjp[order(corrected$index),][,2]
              Pvalues[,iE]<-pvalues
            }
            Beta<-beta_f
            end.time <- Sys.time()
            time.taken <- end.time - start.time
            print(time.taken)
          } else if (TFCE() == TRUE){
            pvalues_f<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Yfinal))
            for(ic in bgn:fns){
              start.time1 <- Sys.time()
              
              sign <- permFL(Xin[,c(1:length(extractNames),ic)],Yfinal,extract,A,NNmatrix,nPermutations(), FALSE, TRUE, nofCores, E = 0.5, H = 2)
              
              end.time1 <- Sys.time()
              time.taken1 <- end.time1 - start.time1
              print(time.taken1)      
              pvalues_f[,iE]<-sign[,1]
            }
            if (BH() == TRUE){
              iE<-0
              #Beta<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Yfinal))
              Pvalues<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Yfinal))
              start.time <- Sys.time()
              for(ic in bgn:fns){
                iE<-iE+1
                #MULTIPLE TESTING CORRECTION
                pfdr5TSBH <- mt.rawp2adjp(pvalues_f[,1+(iE-1)], proc=c("BH"), na.rm = FALSE)
                pvalueADJ5tsbh <- array(dim = length(pvalues_f[,1+(iE-1)]))
                pvalues <- pfdr5TSBH$adjp[order(pfdr5TSBH$index),][,2]
                Pvalues[,iE]<-pvalues
              }
              Beta<-beta_f
              end.time <- Sys.time()
              time.taken <- end.time - start.time
            } else {
              Beta<-beta_f
              Pvalues<- pvalues_f
            }
          }
          
          Controls <- list(Beta, Pvalues)
          return(Controls) 
        }
        
        Doplot<-function(ax,ax1,ax2,ax3,ax4,Data,Datapvalues){
          pos<-which(Datapvalues$pvalues0 < 0.05)
          Data0<-Data[,1:4]
          if (length(pos)==0) {
            p1<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                        marker = list(color = ~beta0,colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = T,
                                      cmin = -max(Data$beta0)*5,
                                      cmax = max(Data$beta0)*5,showscale = T)) %>%
              add_markers(showlegend = FALSE)
          } else {
            p1<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                        marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = T,
                                      cmin = -max(Data$beta0)*5,
                                      cmax = max(Data$beta0)*5,showscale = F)) %>%
              add_markers() %>%
              layout(title = paste("<b> <b>"))%>%
              add_trace(z = Data0[pos,3],x = Data0[pos,1], y = Data0[pos,2],type = "scatter3d", mode = "markers",
                        marker = list(color =  Data0[pos,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto= F,
                                      cmin = -max(Data0[pos,4]),
                                      cmax = max(Data0[pos,4]), showscale = T))
          }
          pos1<-which(Datapvalues$pvalues1 < 0.05)
          Data1<-Data[,c(1:3,5)]
          if (length(pos1)==0) {
            p2<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                        marker = list(color = ~beta1,colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta0)*5,
                                      cmax = max(Data$beta0)*5,showscale = F)) %>%
              add_markers(showlegend = FALSE) 
          } else {
            p2<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                        marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta1)*5,
                                      cmax = max(Data$beta1)*5,showscale = F)) %>%
              add_markers() %>%
              layout(title = paste("<b> <b>"))%>%
              add_trace(z = Data1[pos1,3],x = Data1[pos1,1], y = Data1[pos1,2],type = "scatter3d", mode = "markers",
                        marker = list(color =  Data1[pos1,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto= F,
                                      cmin = -max(Data0[pos,4]),
                                      cmax = max(Data0[pos,4]), showscale = F))
          }
          pos2<-which(Datapvalues$pvalues2 < 0.05)
          Data2<-Data[,c(1:3,6)]
          if (length(pos2)==0) {
            p3<-plot_ly(Data2, x = ~x , y = ~y , z = ~z,scene='scene3',
                        marker = list(color = ~beta2,colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta2)*5,
                                      cmax = max(Data$beta2)*5,showscale = F)) %>%
              add_markers(showlegend = FALSE) 
          } else {
            p3<-plot_ly(Data2, x = ~x , y = ~y , z = ~z,scene='scene3',
                        marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta0)*5,
                                      cmax = max(Data$beta0)*5,showscale = F)) %>%
              add_markers() %>%
              layout(title = paste("<b> <b>"))%>%
              add_trace(z = Data2[pos2,3],x = Data2[pos2,1], y = Data2[pos2,2],type = "scatter3d", mode = "markers",
                        marker = list(color =  Data2[pos2,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto= F,
                                      cmin = -max(Data0[pos,4]),
                                      cmax = max(Data0[pos,4]), showscale = F))
          }
          pos3<-which(Datapvalues$pvalues3 < 0.05)
          Data3<-Data[,c(1:3,7)]
          if (length(pos3)==0) {
            p4<-plot_ly(Data3, x = ~x , y = ~y , z = ~z,scene='scene4',
                        marker = list(color = ~beta3,colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta3)*5,
                                      cmax = max(Data$beta3)*5,showscale = F)) %>%
              add_markers(showlegend = FALSE) 
          } else {
            p4<-plot_ly(Data3, x = ~x , y = ~y , z = ~z,scene='scene4',
                        marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta3)*5,
                                      cmax = max(Data$beta3)*5,showscale = F)) %>%
              add_markers() %>%
              layout(title = paste("<b> <b>"))%>%
              add_trace(z = Data3[pos3,3],x = Data3[pos3,1], y = Data3[pos3,2],type = "scatter3d", mode = "markers",
                        marker = list(color =  Data3[pos3,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto= F,
                                      cmin = -max(Data0[pos,4]),
                                      cmax = max(Data0[pos,4]), showscale = F))
          }
          p<-subplot(p1,p2,p3,p4, nrows = 2) %>%
            layout(title = "",
                   scene = list(domain=list(x=c(0,0.5),y=c(0.5,1)),xaxis = ax,yaxis = ax, zaxis = ax1, dragmode="turntable"),
                   scene2 = list(domain=list(x=c(0.5,1),y=c(0.5,1)),xaxis = ax,yaxis = ax, zaxis = ax2, dragmode="turntable"),
                   scene3 = list(domain=list(x=c(0,0.5),y=c(0,0.5)),xaxis = ax,yaxis = ax, zaxis = ax3, dragmode="turntable"),
                   scene4 = list(domain=list(x=c(0.5,1),y=c(0,0.5)),xaxis = ax,yaxis = ax, zaxis = ax4, dragmode="turntable"),showlegend = FALSE)
          p
          return(p)
        }
        
        ###### create dummie variable for each category ####
        ####################################################
        
        if (Cntrl()==extractNames[1]){ # Race
          inputClinicalin <-as.data.frame.array(Xout[,-1])
          inputClinicalin$race0 <- sapply(inputClinicalin$Race, function (x){ if(x==0) return(1) else return(0)})
          inputClinicalin$race1 <- sapply(inputClinicalin$Race, function (x){ if(x==1) return(1) else return(0)})
          inputClinicalin$race2 <- sapply(inputClinicalin$Race, function (x){ if(x==2) return(1) else return(0)})
          inputClinicalin$race3 <- sapply(inputClinicalin$Race, function (x){ if(x==3) return(1) else return(0)})
          inputClinicalin$race4 <- sapply(inputClinicalin$Race, function (x){ if(x==4) return(1) else return(0)})
          inputClinicalin$race5 <- sapply(inputClinicalin$Race, function (x){ if(x==5) return(1) else return(0)})
          Xin <- data.matrix(inputClinicalin)
          Xin <- cbind(1, Xin)
          race_pos<-which(colnames(Xin)==extractNames[1])
          Xin<-Xin[,-race_pos]
          Xin<-as.matrix(Xin)
          Controls <- SelectControls(Xin,Yfinal,extractNames)
          
          Beta<-Controls[[1]]
          Pvalues<-Controls[[2]]
          #Data <-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          Data<-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          namevector <- c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5")
          Data[ , namevector] <- NA
          Data$beta0 <- Beta[,1]
          Data$beta1 <- Beta[,2]
          Data$beta2 <- Beta[,3]
          Data$beta3 <- Beta[,4]
          Data$beta4 <- Beta[,5]
          Data$beta5 <- Beta[,6]
          colnames(Data) <- c("x", "y", "z", "beta0", "beta1", "beta2", "beta3", "beta4", "beta5")
          D <- which(Data$beta0 < 9999)
          Data <- Data[D,]
          Datapvalues<-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          namevectorp <- c("pvalues0", "pvalues1", "pvalues2", "pvalues3", "pvalues4", "pvalues5")
          Datapvalues[ , namevectorp] <- NA
          Datapvalues$pvalues0 <- Pvalues[,1]
          Datapvalues$pvalues1 <- Pvalues[,2]
          Datapvalues$pvalues2 <- Pvalues[,3]
          Datapvalues$pvalues3 <- Pvalues[,4]
          Datapvalues$pvalues4 <- Pvalues[,5]
          Datapvalues$pvalues5 <- Pvalues[,6]
          colnames(Datapvalues) <- c("x", "y", "z", "pvalues0", "pvalues1", "pvalues2", "pvalues3", "pvalues4", "pvalues5")
          Datapvalues <- Datapvalues[D,]
          print(head(Data))
          print(head(Datapvalues))
          
          output$downloadData <- downloadHandler(
            filename = function() {
              paste(extractNames[iSNP],"_BHbeta.txt",sep="")
            },
            content = function(file) {
              write.table(Datapvalues, file, col.names = TRUE, row.names = FALSE)
            }
          )
          bgn <-length(extractNames)+1
          fns <- ncol(Xin)
          avb <- matrix(0,ncol = length(namevector), nrow = 1)
          signif <- matrix(0,ncol = length(namevector), nrow = 1)
          iS<-4
          iE<-0
          for (iP in bgn:fns){
            iE<-iE+1
            Dat<-Data[,c(1:3,iS)]
            colnames(Dat) <- c("x", "y", "z", "beta")
            Datp<-Datapvalues[,c(1:3,iS)]
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
            str1 <- paste("Beta coefficient for WT vs ", Sel(),"in UKBB", Group(),"subjects,")
            str2 <- paste("average beta for White = ",avb[,1], "and significant area = ", signif[,1], "%,")
            str3 <- paste("average beta for Asian = ",avb[,2], "and significant area = ", signif[,2], "%,")
            str4 <- paste("average beta for Chinese = ",avb[,3], "and significant area = ", signif[,3], "%,")
            str5 <- paste("average beta for African = ",avb[,4], "and significant area = ", signif[,4], "%, n=",nrow(Xout))
            
            HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
            
          })
          
          
          ax <- list(title = "",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax1 <- list(title = "White ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax2 <- list(title = "Asian ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax3 <- list(title = "Chinese ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax4 <- list(title = "African ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          #ax5 <- list(title = "Mixed ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          #ax6 <- list(title = "Other ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          
          p <- Doplot(ax,ax1,ax2,ax3,ax4,Data,Datapvalues)
          
        } else if (Cntrl()==extractNames[2]){ # Sex
          inputClinicalin <-as.data.frame.array(Xout[,-1])
          inputClinicalin$sex0 <- sapply(inputClinicalin$Sex, function (x){ if(x==0) return(1) else return(0)})
          inputClinicalin$sex1 <- sapply(inputClinicalin$Sex, function (x){ if(x==1) return(1) else return(0)})
          Xin <- data.matrix(inputClinicalin)
          Xin <- cbind(1, Xin)
          sex_pos<-which(colnames(Xin)==extractNames[2])
          Xin<-Xin[,-sex_pos]
          Xin<-as.matrix(Xin)
          Controls <- SelectControls(Xin,Yfinal,extractNames)
          
          Beta<-Controls[[1]]
          Pvalues<-Controls[[2]]
          
          Data<-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          namevector <- c("beta0", "beta1")
          Data[ , namevector] <- NA
          Data$beta0 <- Beta[,1]
          Data$beta1 <- Beta[,2]
          
          colnames(Data) <- c("x", "y", "z", "beta0", "beta1")
          
          D <- which(Data$beta0 < 9999)
          Data <- Data[D,]
          Datapvalues<-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          namevectorp <- c("pvalues0", "pvalues1")
          Datapvalues[ , namevectorp] <- NA
          Datapvalues$pvalues0 <- Pvalues[,1]
          Datapvalues$pvalues1 <- Pvalues[,2]
          
          colnames(Datapvalues) <- c("x", "y", "z", "pvalues0", "pvalues1")
          Datapvalues <- Datapvalues[D,]
          print(head(Data))
          print(head(Datapvalues))
          
          output$downloadData <- downloadHandler(
            filename = function() {
              paste(extractNames[iSNP],"_beta.txt",sep="")
            },
            content = function(file) {
              write.table(Data, file, col.names = TRUE, row.names = FALSE)
            }
          )
          
          bgn <-length(extractNames)+1
          fns <- ncol(Xin)
          avb <- matrix(0,ncol = length(namevector), nrow = 1)
          signif <- matrix(0,ncol = length(namevector), nrow = 1)
          iS<-4
          iE<-0
          for (iP in bgn:fns){
            iE<-iE+1
            Dat<-Data[,c(1:3,iS)]
            colnames(Dat) <- c("x", "y", "z", "beta")
            Datp<-Datapvalues[,c(1:3,iS)]
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
            str1 <- paste("Beta coefficient for WT vs ", Sel(),"in UKBB", Group(),"subjects,")
            str2 <- paste("average beta for Female = ",avb[,1], "and significant area = ",signif[,1], "%,")
            str3 <- paste("average beta for Male = ",avb[,2], "and significant area = ",signif[,2], "%, n=",nrow(Xout))
            
            HTML(paste(str1, str2, str3, sep = '<br/>'))
            
          })
          ax <- list(title = "",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax1 <- list(title = "Female ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax2 <- list(title = "Male ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          
          pos<-which(Datapvalues$pvalues0 < 0.05)
          Data0<-Data[,1:4]
          if (length(pos)==0) {
            p0<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                        marker = list(color = ~beta0,colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta0),
                                      cmax = max(Data$beta0),showscale = T)) %>%
              add_markers(showlegend = FALSE) %>%
              layout(title = paste("<b>Female <b>"))
          } else {
            p0<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                        marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta0)*5,
                                      cmax = max(Data$beta0)*5,showscale = F)) %>%
              add_markers(showlegend = FALSE) %>%
              layout(title = paste("<b>Female <b>"))%>%
              add_trace(z = Data0$z[pos],x = Data0$x[pos], y = Data0$y[pos],type = "scatter3d", mode = "markers",
                        marker = list(color =  ~Data0$beta0[pos],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto= F,
                                      cmin = -max(Data0$beta0[pos]),
                                      cmax = max(Data0$beta0[pos]), showscale = T))
          } 
          pos1<-which(Datapvalues$pvalues1 < 0.05)
          Data1<-Data[,c(1:3,5)]
          if (length(pos1)==0) {
            p1<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                        marker = list(color = ~beta1,colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta1),
                                      cmax = max(Data$beta1),showscale = F)) %>%
              add_markers(showlegend = FALSE) %>%
              layout(title = paste("<b>Male <b>"))
          } else {
            p1<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                        marker = list(color = "lightgrey",colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta1)*5,
                                      cmax = max(Data$beta1)*5,showscale = F)) %>%
              add_markers() %>%
              layout(title = paste("<b>Male <b>"))%>%
              add_trace(z = Data1$z[pos1],x = Data1$x[pos1], y = Data1$y[pos1],type = "scatter3d", mode = "markers",
                        marker = list(color =  ~Data1$beta1[pos1],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto= F,
                                      cmin = -max(Data1$beta0[pos1]),
                                      cmax = max(Data1$beta0[pos1]), showscale = F))
          } 
          
          p <- subplot(p0, p1) %>%
            layout(title="",scene = list(domain=list(x=c(0,0.48),y=c(0.1,1)),xaxis = ax,yaxis = ax, zaxis = ax1, dragmode="turntable"),
                   scene2 = list(domain=list(x=c(0.48,1),y=c(0.1,1)),xaxis = ax,yaxis = ax, zaxis = ax2, dragmode="turntable"),showlegend = FALSE)
          p  
          
          
          
        } else if (Cntrl()==extractNames[3]){ # Age
          inputClinicalin <-as.data.frame.array(inputClinicalout[,-1])
          a1<-(inputClinicalin$Age <= 44) & (inputClinicalin$Age >= 35)
          a2<-(inputClinicalin$Age <= 54) & (inputClinicalin$Age >= 45)
          a3<-(inputClinicalin$Age <= 64) & (inputClinicalin$Age >= 55)
          a4<-(inputClinicalin$Age <= 74) & (inputClinicalin$Age >= 65)
          age1 <- ifelse(a1,1,0)
          age2 <- ifelse(a2,2,age1)
          age3 <- ifelse(a3,3,age2)
          age4 <- ifelse(a4,4,age3)
          age_value <-age4
          Xin <- data.frame(Xout)
          Xin$Age1 <- sapply(age_value, function (x){ if(x==1) return(1) else return(0)})
          Xin$Age2 <- sapply(age_value, function (x){ if(x==2) return(1) else return(0)})
          Xin$Age3 <- sapply(age_value, function (x){ if(x==3) return(1) else return(0)})
          Xin$Age4 <- sapply(age_value, function (x){ if(x==4) return(1) else return(0)})
          #Xin <- cbind(1, Xin)
          age_pos<-which(colnames(Xin)==extractNames[3])
          Xin<-Xin[,-age_pos]
          Xin<-as.matrix(Xin)
          
          print(dim(Xin))
          print(dim(Yfinal))
          Controls <- SelectControls(Xin,Yfinal,extractNames)
          
          Beta<-Controls[[1]]
          Pvalues<-Controls[[2]]
          
          Data<-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          namevector <- c("beta0", "beta1", "beta2", "beta3")
          Data[ , namevector] <- NA
          Data$beta0 <- Beta[,1]
          Data$beta1 <- Beta[,2]
          Data$beta2 <- Beta[,3]
          Data$beta3 <- Beta[,4]
          
          colnames(Data) <- c("x", "y", "z", "beta0", "beta1", "beta2", "beta3")
          
          D <- which(Data$beta0 < 9999)
          Data <- Data[D,]
          Datapvalues<-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          namevectorp <- c("pvalues1", "pvalues2", "pvalues3", "pvalues4")
          Datapvalues[ , namevectorp] <- NA
          Datapvalues$pvalues0 <- Pvalues[,1]
          Datapvalues$pvalues1 <- Pvalues[,2]
          Datapvalues$pvalues2 <- Pvalues[,3]
          Datapvalues$pvalues3 <- Pvalues[,4]
          
          colnames(Datapvalues) <- c("x", "y", "z", "pvalues1", "pvalues2", "pvalues3", "pvalues4")
          Datapvalues <- Datapvalues[D,]
          
          output$downloadData <- downloadHandler(
            filename = function() {
              paste(extractNames[iSNP],"_beta.txt",sep="")
            },
            content = function(file) {
              write.table(Data, file, col.names = TRUE, row.names = FALSE)
            }
          )
          bgn <-length(extractNames)+1
          fns <- ncol(Xin)
          avb <- matrix(0,ncol = length(namevector), nrow = 1)
          signif <- matrix(0,ncol = length(namevector), nrow = 1)
          iS<-4
          iE<-0
          for (iP in bgn:fns){
            iE<-iE+1
            Dat<-Data[,c(1:3,iS)]
            colnames(Dat) <- c("x", "y", "z", "beta")
            Datp<-Datapvalues[,c(1:3,iS)]
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
            str1 <- paste("Beta coefficient for WT vs ", Sel(),"in UKBB", Group(),"subjects,")
            str2 <- paste("average beta for 35 to 44 y.o. = ",avb[,1], "and significant area = ",signif[,1], "%,")
            str3 <- paste("average beta for 45 to 54 y.o = ",avb[,2], "and significant area = ",signif[,2], "%,")
            str4 <- paste("average beta for 55 to 64 y.o. = ",avb[,3], "and significant area = ",signif[,3], "%,")
            str5 <- paste("average beta for 65 to 74 y.o. = ",avb[,4], "and significant area = ",signif[,4], "%, n=",nrow(Xout))
            
            HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
            
          })
          ax <- list(title = "",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax1 <- list(title = "35 to 44 y.o. ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax2 <- list(title = "45 to 54 y.o. ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax3 <- list(title = "55 to 64 y.o. ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax4 <- list(title = "65 to 74 y.o. ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          
          p <- Doplot(ax,ax1,ax2,ax3,ax4,Data,Datapvalues)
          
          
          
          
        } else if (Cntrl()==extractNames[4]){ # Smoking
          inputClinicalin <-as.data.frame.array(Xout[,-1])
          inputClinicalin$Smoking0 <- sapply(inputClinicalin$Smoking, function (x){ if(x==0) return(1) else return(0)})
          inputClinicalin$Smoking1 <- sapply(inputClinicalin$Smoking, function (x){ if(x==1) return(1) else return(0)})
          inputClinicalin$Smoking2 <- sapply(inputClinicalin$Smoking, function (x){ if(x==2) return(1) else return(0)})
          inputClinicalin$Smoking3 <- sapply(inputClinicalin$Smoking, function (x){ if(x==3) return(1) else return(0)})
          
          Xin <- data.matrix(inputClinicalin)
          Xin <- cbind(1, Xin)
          race_pos<-which(colnames(Xin)==extractNames[4])
          Xin<-Xin[,-race_pos]
          Xin<-as.matrix(Xin)
          
          Controls <- SelectControls(Xin,Yfinal,extractNames)
          Beta<-Controls[[1]]
          Pvalues<-Controls[[2]]
          Data<-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          namevector <- c("beta0", "beta1", "beta2", "beta3")
          Data[ , namevector] <- NA
          Data$beta0 <- Beta[,1]
          Data$beta1 <- Beta[,2]
          Data$beta2 <- Beta[,3]
          Data$beta3 <- Beta[,4]
          colnames(Data) <- c("x", "y", "z", "beta0", "beta1", "beta2", "beta3")
          D <- which(Data$beta0 < 9999)
          Data <- Data[D,]
          Datapvalues<-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          namevectorp <- c("pvalues0", "pvalues1", "pvalues2", "pvalues3")
          Datapvalues[ , namevectorp] <- NA
          Datapvalues$pvalues0 <- Pvalues[,1]
          Datapvalues$pvalues1 <- Pvalues[,2]
          Datapvalues$pvalues2 <- Pvalues[,3]
          Datapvalues$pvalues3 <- Pvalues[,4]
          
          colnames(Datapvalues) <- c("x", "y", "z", "pvalues0", "pvalues1", "pvalues2", "pvalues3")
          Datapvalues <- Datapvalues[D,]
          
          output$downloadData <- downloadHandler(
            filename = function() {
              paste(extractNames[iSNP],"_beta.txt",sep="")
            },
            content = function(file) {
              write.table(Data, file, col.names = TRUE, row.names = FALSE)
            }
          )
          bgn <-length(extractNames)+1
          fns <- ncol(Xin)
          avb <- matrix(0,ncol = length(namevector), nrow = 1)
          signif <- matrix(0,ncol = length(namevector), nrow = 1)
          iS<-4
          iE<-0
          for (iP in bgn:fns){
            iE<-iE+1
            Dat<-Data[,c(1:3,iS)]
            colnames(Dat) <- c("x", "y", "z", "beta")
            Datp<-Datapvalues[,c(1:3,iS)]
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
            str1 <- paste("Beta coefficient for WT vs ", Sel(),"in UKBB", Group(),"subjects,")
            str2 <- paste("average beta for Non-smokers = ",avb[,1], "and significant area = ", signif[,1], "%,")
            str3 <- paste("average beta for Previous smokers = ",avb[,2], "and significant area = ", signif[,2], "%,")
            str4 <- paste("average beta for Current smokers = ",avb[,3], "and significant area = ", signif[,3], "%,")
            str5 <- paste("average beta for NA = ",avb[,4], "and significant area = ", signif[,4], "%, n=",nrow(Xout))
            
            HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
            
          })
          
          ax <- list(title = "",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax1 <- list(title = "Never ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax2 <- list(title = "Previous ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax3 <- list(title = "Current ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax4 <- list(title = "NA ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          p <- Doplot(ax,ax1,ax2,ax3,ax4,Data,Datapvalues)
          
        } else if (Cntrl()==extractNames[5]){ # BSA
          inputClinicalin <-as.data.frame.array(inputClinicalout[,-1])
          b1<-(inputClinicalin$BSA > 1.6) & (inputClinicalin$Sex == 0)
          b2<-(inputClinicalin$BSA <= 1.6) & (inputClinicalin$Sex == 0)
          b3<-(inputClinicalin$BSA > 1.9) & (inputClinicalin$Sex == 1)
          b4<-(inputClinicalin$BSA <= 1.9) & (inputClinicalin$Sex == 1)
          
          bsa1 <- ifelse(b1,1,0)
          bsa2 <- ifelse(b2,2,bsa1)
          bsa3 <- ifelse(b3,3,bsa2)
          bsa4 <- ifelse(b4,4,bsa3)
          bsa_value <-bsa4
          Xin <- data.frame(Xout)
          Xin$BSA1 <- sapply(bsa_value, function (x){ if(x==1) return(1) else return(0)})
          Xin$BSA2 <- sapply(bsa_value, function (x){ if(x==2) return(1) else return(0)})
          Xin$BSA3 <- sapply(bsa_value, function (x){ if(x==3) return(1) else return(0)})
          Xin$BSA4 <- sapply(bsa_value, function (x){ if(x==4) return(1) else return(0)})
          #Xin <- cbind(1, Xin)
          bsa_pos<-which(colnames(Xin)==extractNames[5])
          Xin<-Xin[,-bsa_pos]
          Xin<-as.matrix(Xin)
          Controls <- SelectControls(Xin,Yfinal,extractNames)
          Beta<-Controls[[1]]
          Pvalues<-Controls[[2]]
          #Data <-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          Data<-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          namevector <- c("beta0", "beta1", "beta2", "beta3")
          Data[ , namevector] <- NA
          Data$beta0 <- Beta[,1]
          Data$beta1 <- Beta[,2]
          Data$beta2 <- Beta[,3]
          Data$beta3 <- Beta[,4]
          colnames(Data) <- c("x", "y", "z", "beta0", "beta1", "beta2", "beta3")
          D <- which(Data$beta0 < 9999)
          Data <- Data[D,]
          Datapvalues<-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          namevectorp <- c("pvalues0", "pvalues1", "pvalues2", "pvalues3")
          Datapvalues[ , namevectorp] <- NA
          Datapvalues$pvalues0 <- Pvalues[,1]
          Datapvalues$pvalues1 <- Pvalues[,2]
          Datapvalues$pvalues2 <- Pvalues[,3]
          Datapvalues$pvalues3 <- Pvalues[,4]
          
          colnames(Datapvalues) <- c("x", "y", "z", "pvalues0", "pvalues1", "pvalues2", "pvalues3")
          Datapvalues <- Datapvalues[D,]
          
          
          output$downloadData <- downloadHandler(
            filename = function() {
              paste(extractNames[iSNP],"_beta.txt",sep="")
            },
            content = function(file) {
              write.table(Data, file, col.names = TRUE, row.names = FALSE)
            }
          )
          bgn <-length(extractNames)+1
          fns <- ncol(Xin)
          avb <- matrix(0,ncol = length(namevector), nrow = 1)
          signif <- matrix(0,ncol = length(namevector), nrow = 1)
          iS<-4
          iE<-0
          for (iP in bgn:fns){
            iE<-iE+1
            Dat<-Data[,c(1:3,iS)]
            colnames(Dat) <- c("x", "y", "z", "beta")
            Datp<-Datapvalues[,c(1:3,iS)]
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
            str1 <- paste("Beta coefficient for WT vs ", Sel(),"in UKBB", Group(),"subjects,")
            str2 <- paste("average beta for Female with high BSA = ",avb[,1], "and significant area = ", signif[,1], "%,")
            str3 <- paste("average beta for Female with low BSA = ",avb[,2], "and significant area = ", signif[,2], "%,")
            str4 <- paste("average beta for Male with high BSA = ",avb[,3], "and significant area = ", signif[,3], "%,")
            str5 <- paste("average beta for Male with low BSA = ",avb[,4], "and significant area = ", signif[,4], "%, n=",nrow(Xout))
            
            HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
            
          })
          
          ax <- list(title = "",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax1 <- list(title = "Female with high BSA ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax2 <- list(title = "Female with low BSA ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax3 <- list(title = "Male with high BSA ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax4 <- list(title = "Male with low BSA",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          p <- Doplot(ax,ax1,ax2,ax3,ax4,Data,Datapvalues)
          
        } else if (Cntrl()==extractNames[6]){ # SBP
          inputClinicalin <-as.data.frame.array(inputClinicalout[,-1])
          s1<-(inputClinicalin$SBP<120)
          s2<-((inputClinicalin$SBP>=120) & (inputClinicalin$SBP<=139))
          s3<-(inputClinicalin$SBP>=140)
          
          sbp1 <- ifelse(s1,1,0)
          sbp2 <- ifelse(s2,2,sbp1)
          sbp3 <- ifelse(s3,3,sbp2)
          sbp_value <-sbp3
          Xin <- data.frame(Xout)
          Xin$SBP1 <- sapply(sbp_value, function (x){ if(x==1) return(1) else return(0)})
          Xin$SBP2 <- sapply(sbp_value, function (x){ if(x==2) return(1) else return(0)})
          Xin$SBP3 <- sapply(sbp_value, function (x){ if(x==3) return(1) else return(0)})
          sbp_pos<-which(colnames(Xin)==extractNames[6])
          Xin<-Xin[,-sbp_pos]
          Xin<-as.matrix(Xin)
          Controls <- SelectControls(Xin,Yfinal,extractNames)
          Beta<-Controls[[1]]
          Pvalues<-Controls[[2]]
          #Data <-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          Data<-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          namevector <- c("beta0", "beta1", "beta2")
          Data[ , namevector] <- NA
          Data$beta0 <- Beta[,1]
          Data$beta1 <- Beta[,2]
          Data$beta2 <- Beta[,3]
          colnames(Data) <- c("x", "y", "z", "beta0", "beta1", "beta2")
          D <- which(Data$beta0 < 9999)
          Data <- Data[D,]
          Datapvalues<-data.frame(mesh_Coordinates$x, mesh_Coordinates$y, mesh_Coordinates$z)
          namevectorp <- c("pvalues0", "pvalues1", "pvalues2")
          Datapvalues[ , namevectorp] <- NA
          Datapvalues$pvalues0 <- Pvalues[,1]
          Datapvalues$pvalues1 <- Pvalues[,2]
          Datapvalues$pvalues2 <- Pvalues[,3]
          
          colnames(Datapvalues) <- c("x", "y", "z", "pvalues0", "pvalues1", "pvalues2")
          Datapvalues <- Datapvalues[D,]
          
          
          output$downloadData <- downloadHandler(
            filename = function() {
              paste(extractNames[iSNP],"_beta.txt",sep="")
            },
            content = function(file) {
              write.table(Data, file, col.names = TRUE, row.names = FALSE)
            }
          )
          bgn <-length(extractNames)+1
          fns <- ncol(Xin)
          avb <- matrix(0,ncol = length(namevector), nrow = 1)
          signif <- matrix(0,ncol = length(namevector), nrow = 1)
          iS<-4
          iE<-0
          for (iP in bgn:fns){
            iE<-iE+1
            Dat<-Data[,c(1:3,iS)]
            colnames(Dat) <- c("x", "y", "z", "beta")
            Datp<-Datapvalues[,c(1:3,iS)]
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
            str1 <- paste("Beta coefficient for WT vs ", Sel(),"in UKBB", Group(),"subjects,")
            str2 <- paste("average beta for Normotensive = ",avb[,1], "and significant area = ", signif[,1], "%,")
            str3 <- paste("average beta for Prehypertensive = ",avb[,2], "and significant area = ", signif[,2], "%,")
            str4 <- paste("average beta for Hypertensive = ",avb[,3], "and significant area = ", signif[,3], "%, n=",nrow(Xout))
            
            HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
            
          })
          
          ax <- list(title = "",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax1 <- list(title = "Normotensive ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax2 <- list(title = "Prehypertensive ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          ax3 <- list(title = "Hypertensive ",zeroline = F,showline = F,showticklabels = F,showgrid = F)
          pos<-which(Datapvalues$pvalues0 < 0.05)
          Data0<-Data[,1:4]
          if (length(pos)==0) {
            p0<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                        marker = list(color = ~beta0,colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta0),
                                      cmax = max(Data$beta0),showscale = T)) %>%
              add_markers(showlegend = FALSE) %>%
              layout(title = paste("<b>Female <b>"))
          } else {
            p0<-plot_ly(Data0, x = ~x , y = ~y , z = ~z,scene='scene',
                        marker = list(color = "grey",colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = T,
                                      cmin = -max(Data$beta0)*5,
                                      cmax = max(Data$beta0)*5,showscale = F)) %>%
              add_markers(showlegend = FALSE) %>%
              layout(title = paste("<b>Female <b>"))%>%
              add_trace(z = Data0[pos,3],x = Data0[pos,1], y = Data0[pos,2],type = "scatter3d", mode = "markers",
                        marker = list(color =  Data0[pos,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto= F,
                                      cmin = -max(Data0[pos,4]),
                                      cmax = max(Data0[pos,4]), showscale = T))%>%add_markers(showlegend = FALSE)
          } 
          pos1<-which(Datapvalues$pvalues1 < 0.05)
          Data1<-Data[,c(1:3,5)]
          if (length(pos1)==0) {
            p1<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                        marker = list(color = ~beta1,colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta1)*5,
                                      cmax = max(Data$beta1)*5,showscale = T)) %>%
              add_markers(showlegend = FALSE) %>%
              layout(title = paste("<b>Male <b>"))
          } else {
            p1<-plot_ly(Data1, x = ~x , y = ~y , z = ~z,scene='scene2',
                        marker = list(color = "grey",colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta1),
                                      cmax = max(Data$beta1),showscale = F)) %>%
              add_markers(showlegend = FALSE) %>%
              layout(title = paste("<b>Male <b>"))%>%
              add_trace(z = Data1[pos1,3],x = Data1[pos1,1], y = Data1[pos1,2],type = "scatter3d", mode = "markers",
                        marker = list(color =  Data1[pos1,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto= F,
                                      cmin = -max(Data1[pos1,4]),
                                      cmax = max(Data1[pos1,4]), showscale = F))%>%add_markers(showlegend = FALSE)
          } 
          pos2<-which(Datapvalues$pvalues2 < 0.05)
          Data2<-Data[,c(1:3,6)]
          if (length(pos2)==0) {
            p2<-plot_ly(Data2, x = ~x , y = ~y , z = ~z,scene='scene2',
                        marker = list(color = ~beta2,colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta2),
                                      cmax = max(Data$beta2),showscale = T)) %>%
              add_markers(showlegend = FALSE) %>%
              layout(title = paste("<b>Male <b>"))
          } else {
            p2<-plot_ly(Data2, x = ~x , y = ~y , z = ~z,scene='scene3',
                        marker = list(color = "grey" ,colorbar=list(title='beta - (a.u)',len = 0.55), 
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto = F,
                                      cmin = -max(Data$beta2)*5,
                                      cmax = max(Data$beta2)*5,showscale = F)) %>%
              add_markers(showlegend = FALSE) %>%
              layout(title = paste("<b>Male <b>"))%>%
              add_trace(z = Data2[pos2,3],x = Data2[pos2,1], y = Data2[pos2,2],type = "scatter3d", mode = "markers",
                        marker = list(color =  Data2[pos2,4],colorbar=list(title='beta_pvalues',len = 0.55, x = 1.13, y = 0.4),
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto= F,
                                      cmin = -max(Data2[pos2,4]),
                                      cmax = max(Data2[pos2,4]), showscale = F))%>%add_markers(showlegend = FALSE)
          } 
          
          p <- subplot(p0,  p1) %>%
            layout(title="",scene = list(domain=list(x=c(0,0.5),y=c(0.5,1)),xaxis = ax,yaxis = ax, zaxis = ax1, dragmode="turntable"),
                   scene2 = list(domain=list(x=c(0.5,1),y=c(0.5,1)),xaxis = ax,yaxis = ax, zaxis = ax2, dragmode="turntable"),
                   scene3 = list(domain=list(x=c(0,0.5),y=c(0,0.5)),xaxis = ax,yaxis = ax, zaxis = ax3, dragmode="turntable"),showlegend = FALSE)
          p  
        } 
        
      } 
      
    })
  })
}

