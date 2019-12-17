######################################################
################# IPA for ShinyApp ##########################
######################################################
######################################################
rm(list = ls(all = TRUE))  
#start with an empty environment
#install.packages("shiny")
setwd("Z:/Experiments_of_Maria/20191128_Shinyapp_interactive_phenotypic_applications/")
library(data.table)
library(multtest)
library(mutools3D)

Xcoord <- readRDS("Z:/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/WT/WTedXcoordinate.rds")
Ycoord <- readRDS("Z:/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/WT/WTedYcoordinate.rds")
Zcoord <- readRDS("Z:/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/WT/WTedZcoordinate.rds")
#NUMBER OF CORES TO USE
nofCores = 6
#IMAGING DATA MATRIX
#NCOL = N POINTS ON THE ATLAS
#NROW = N PATIENTS
Yw <- readRDS("Z:/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/WT/WTedLV.rds")
#DATA PRE-PROCESSING
Yw <- scale(Yw)
dim(Yw)
#CLINICAL DATA MATRIX
#NCOL = N COVARIATES UNDER STUDY
#NROW = N PATIENTS
X <- readRDS(paste("Z:/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/WT/WTedmodel.rds"))
dim(X)
X<-X[,-8]
inputClinical <- data.frame(readRDS(paste("Z:/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/WT/WTedLVclinicalData.rds")))
inputClinical<-inputClinical[,-8]

#IMAGING DATA MATRIX
#NCOL = N POINTS ON THE ATLAS
#NROW = N PATIENTS
Ys <- readRDS("Z:/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/S2S/S2SedLV.rds")
#DATA PRE-PROCESSING
Ys <- scale(Ys)
dim(Ys)

#IMAGING DATA MATRIX
#NCOL = N POINTS ON THE ATLAS
#NROW = N PATIENTS
Yc <- readRDS("Z:/Experiments_of_Maria/20191104_UKBB_allbind/All_bind/Curvature/CurvatureedLV.rds")
#DATA PRE-PROCESSING
Yc <- scale(Yc)
dim(Yc)

extractNames <- c("Race", "Sex", "Age", "Smoking", "BSA", "SBP", "DBP")
choice<- c("All","Normotensive","Prehypertensive", "Hypertensive")
whichEE=2
Group=choice[1]
nPermutations=100

endoEpi <- read.table("~/cardiac/Experiments_of_Maria/20191024_ShinyApp/shinyplot/endo_epi.txt")
vert2print <- list(which(endoEpi[,4]==0),which(endoEpi[,4]==1),1:length(endoEpi[,4]))
setwd("~/cardiac/Experiments_of_Maria/20191128_Shinyapp_interactive_phenotypic_applications/Epi_Normotensive")


{
  if (whichEE==1){
    ##READ THE NNLIST ASSOCIATED TO EACH VERTEX
    NNmatrix <- readRDS("Z:/Experiments_of_Maria/20191024_ShinyApp/shinyplot/redendoNNmatrix.rds")
    ##READ AREAS ASSOCIATED TO EACH VERTEX
    A <- readRDS("Z:/Experiments_of_Maria/20191024_ShinyApp/shinyplot/endoLVareas.rds")
    Ywout <- Yw[,vert2print[[1]]]
    Ysout <- Ys[,vert2print[[1]]]
    Ycout <- Yc[,vert2print[[1]]]
    x_Coordinates <- Xcoord[,vert2print[[1]]]
    y_Coordinates <- Ycoord[,vert2print[[1]]]
    z_Coordinates <- Zcoord[,vert2print[[1]]]
    
  } else if (whichEE==2){
    ##READ THE NNLIST ASSOCIATED TO EACH VERTEX
    NNmatrix <- readRDS("Z:/Experiments_of_Maria/20191024_ShinyApp/shinyplot/redepiNNmatrix.rds")
    ##READ AREAS ASSOCIATED TO EACH VERTEX
    A <- readRDS("Z:/Experiments_of_Maria/20191024_ShinyApp/shinyplot/epiLVareas.rds")
    Ywout <- Yw[,vert2print[[2]]]
    Ysout <- Ys[,vert2print[[2]]]
    Ycout <- Yc[,vert2print[[2]]]
    x_Coordinates <- Xcoord[,vert2print[[2]]]
    y_Coordinates <- Ycoord[,vert2print[[2]]]
    z_Coordinates <- Zcoord[,vert2print[[2]]]
    
  } else if (whichEE==3){
    ##READ THE NNLIST ASSOCIATED TO EACH VERTEX
    NNmatrix <- readRDS("Z:/Experiments_of_Maria/20191024_ShinyApp/shinyplot/redNNmatrix.rds")
    ##READ AREAS ASSOCIATED TO EACH VERTEX
    A <- readRDS("Z:/Experiments_of_Maria/20191024_ShinyApp/shinyplot/LVarea.rds")
    Ywout <- Yw
    Ysout <- Ys
    Ycout <- Yc
    x_Coordinates <- Xcoord[,vert2print[[3]]]
    y_Coordinates <- Ycoord[,vert2print[[3]]]
    z_Coordinates <- Zcoord[,vert2print[[3]]]
  }
  
  if (Group == choice[1]){
    
    X_coordinates <- colMeans(x_Coordinates)
    Y_coordinates <- colMeans(y_Coordinates)
    Z_coordinates <- colMeans(z_Coordinates)
    mesh_Coordinates <- data.frame(X_coordinates, Y_coordinates, Z_coordinates)
    colnames(mesh_Coordinates) <- c("x", "y", "z")
    Ywfinal <- Ywout
    Xout <- X
    Ysfinal <- Ysout
    Ycfinal <- Ycout
    inputClinicalout <- inputClinical
    
  } else if (Group == choice[2]){
    normotensive<-which((inputClinical$SBP<120))
    
    Xco <- x_Coordinates[normotensive,]
    Yco <- y_Coordinates[normotensive,]
    Zco <- z_Coordinates[normotensive,]
    X_coordinates <- colMeans(Xco)
    Y_coordinates <- colMeans(Yco)
    Z_coordinates <- colMeans(Zco)
    mesh_Coordinates <- data.frame(X_coordinates, Y_coordinates, Z_coordinates)
    colnames(mesh_Coordinates) <- c("x", "y", "z")
    Ywfinal <- Ywout[normotensive,]
    Xout <- X[normotensive,]
    Ysfinal <- Ysout[normotensive,]
    Ycfinal <- Ycout[normotensive,]
    inputClinicalout <- inputClinical[normotensive,]
  } else if (Group == choice[3]){
    prehpertesive<-which((inputClinical$SBP>=120) & (inputClinical$SBP<=139))
    
    Xco <- x_Coordinates[prehpertesive,]
    Yco <- y_Coordinates[prehpertesive,]
    Zco <- z_Coordinates[prehpertesive,]
    X_coordinates <- colMeans(Xco)
    Y_coordinates <- colMeans(Yco)
    Z_coordinates <- colMeans(Zco)
    mesh_Coordinates <- data.frame(X_coordinates, Y_coordinates, Z_coordinates)
    colnames(mesh_Coordinates) <- c("x", "y", "z")
    Ywfinal <- Ywout[prehpertesive,]
    Xout <- X[prehpertesive,]
    Ysfinal <- Ysout[prehpertesive,]
    Ycfinal <- Ycout[prehpertesive,]
    inputClinicalout <- inputClinical[prehpertesive,]
  } else if (Group == choice[4]){
    hypertesive<-which((inputClinical$SBP>=140))
    
    Xco <- x_Coordinates[hypertesive,]
    Yco <- y_Coordinates[hypertesive,]
    Zco <- z_Coordinates[hypertesive,]
    X_coordinates <- colMeans(Xco)
    Y_coordinates <- colMeans(Yco)
    Z_coordinates <- colMeans(Zco)
    mesh_Coordinates <- data.frame(X_coordinates, Y_coordinates, Z_coordinates)
    colnames(mesh_Coordinates) <- c("x", "y", "z")
    Ywfinal <- Ywout[hypertesive,]
    Xout <- X[hypertesive,]
    Ysfinal <- Ysout[hypertesive,]
    Ycfinal <- Ycout[hypertesive,]
    inputClinicalout <- inputClinical[hypertesive,]
  }
  
  for (extract in  2:8){
    iSNP <- extract-1
    iEx <-1
    resultw <- murHC4m(X,Ywfinal,extract)
    results <- murHC4m(X,Ysfinal,extract)
    resultc <- murHC4m(X,Ycfinal,extract)
    
    #extract only the snp betas
    
    #MULTIPLE TESTING CORRECTION
    correctedw <- mt.rawp2adjp(resultw[,3+(iEx-1)*3], proc=c("BH"), na.rm = FALSE)
    pvalueADJ5tsbh <- array(dim = length(resultw[,3+(iEx-1)*3]))
    BHpvaluesw <- correctedw$adjp[order(correctedw$index),][,2]
    correcteds <- mt.rawp2adjp(results[,3+(iEx-1)*3], proc=c("BH"), na.rm = FALSE)
    pvalueADJ5tsbh <- array(dim = length(results[,3+(iEx-1)*3]))
    BHpvaluess <- correcteds$adjp[order(correcteds$index),][,2]
    correctedc <- mt.rawp2adjp(resultc[,3+(iEx-1)*3], proc=c("BH"), na.rm = FALSE)
    pvalueADJ5tsbh <- array(dim = length(resultc[,3+(iEx-1)*3]))
    BHpvaluesc <- correctedc$adjp[order(correctedc$index),][,2]
    #setwd("~/cardiac/carloExperiments/jamesPaper/results_new_DL_segs_08_19/S2Ses_HCM_SNPs_without_status/")
    meshCoordinates <- cbind(mesh_Coordinates,99999)
    #PRINT OUTPUT
    meshCoordinates[,4] <- resultw[,1]
    write.table(meshCoordinates, paste(extractNames[iSNP],"_beta_WT.txt",sep=""))
    meshCoordinates[,4] <- resultw[,3]
    write.table(meshCoordinates, paste(extractNames[iSNP],"_pvalues_WT.txt",sep=""), col.names = FALSE, row.names = FALSE)
    meshCoordinates[,4] <- BHpvaluesw
    write.table(meshCoordinates, paste(extractNames[iSNP],"_BHpvalues_WT.txt",sep=""), col.names = FALSE, row.names = FALSE)
    
    meshCoordinates[,4] <- results[,1]
    write.table(meshCoordinates, paste(extractNames[iSNP],"_beta_S2S.txt",sep=""))
    meshCoordinates[,4] <- results[,3]
    write.table(meshCoordinates, paste(extractNames[iSNP],"_pvalues_S2S.txt",sep=""), col.names = FALSE, row.names = FALSE)
    meshCoordinates[,4] <- BHpvaluess
    write.table(meshCoordinates, paste(extractNames[iSNP],"_BHpvalues_S2S.txt",sep=""), col.names = FALSE, row.names = FALSE)
    
    meshCoordinates[,4] <- resultc[,1]
    write.table(meshCoordinates, paste(extractNames[iSNP],"_beta_Curvature.txt",sep=""))
    meshCoordinates[,4] <- resultc[,3]
    write.table(meshCoordinates, paste(extractNames[iSNP],"_pvalues_Curvature.txt",sep=""), col.names = FALSE, row.names = FALSE)
    meshCoordinates[,4] <- BHpvaluesc
    write.table(meshCoordinates, paste(extractNames[iSNP],"_BHpvalues_Curvature.txt",sep=""), col.names = FALSE, row.names = FALSE)
    
    signw <- permFL(X,Ywfinal,extract,A,NNmatrix,nPermutations, TRUE, TRUE, nofCores, E = 0.5, H = 2)
    signs <- permFL(X,Ysfinal,extract,A,NNmatrix,nPermutations, TRUE, TRUE, nofCores, E = 0.5, H = 2)
    signc <- permFL(X,Ycfinal,extract,A,NNmatrix,nPermutations, TRUE, TRUE, nofCores, E = 0.5, H = 2)
    
    #MULTIPLE TESTING CORRECTION
    pfdr5TSBHw <- mt.rawp2adjp(signw[,1], proc=c("BH"), na.rm = FALSE)
    pvalueADJ5tsbh <- array(dim = length(signw[,1]))
    BHpvaluesTFCEw <- pfdr5TSBHw$adjp[order(pfdr5TSBHw$index),][,2]
    pfdr5TSBHs <- mt.rawp2adjp(signs[,1], proc=c("BH"), na.rm = FALSE)
    pvalueADJ5tsbh <- array(dim = length(signs[,1+(iEx-1)*2]))
    BHpvaluesTFCEs <- pfdr5TSBHs$adjp[order(pfdr5TSBHs$index),][,2]
    pfdr5TSBHc <- mt.rawp2adjp(signc[,1], proc=c("BH"), na.rm = FALSE)
    pvalueADJ5tsbh <- array(dim = length(signc[,1]))
    BHpvaluesTFCEc <- pfdr5TSBHc$adjp[order(pfdr5TSBHc$index),][,2]
    
    #PRINT OUTPUT
    meshCoordinates[,4] <- signw[,1]
    write.table(meshCoordinates, paste(extractNames[iSNP],"_pvaluesTFCE_WT.txt",sep=""), col.names = FALSE, row.names = FALSE)
    meshCoordinates[,4] <- BHpvaluesTFCEw
    write.table(meshCoordinates, paste(extractNames[iSNP],"_BHpvaluesTFCE_WT.txt",sep=""), col.names = FALSE, row.names = FALSE)
    
    meshCoordinates[,4] <- signs[,1]
    write.table(meshCoordinates, paste(extractNames[iSNP],"_pvaluesTFCE_S2S.txt",sep=""), col.names = FALSE, row.names = FALSE)
    meshCoordinates[,4] <- BHpvaluesTFCEs
    write.table(meshCoordinates, paste(extractNames[iSNP],"_BHpvaluesTFCE_S2S.txt",sep=""), col.names = FALSE, row.names = FALSE)
    
    meshCoordinates[,4] <- signc[,1]
    write.table(meshCoordinates, paste(extractNames[iSNP],"_pvaluesTFCE_Curvature.txt",sep=""), col.names = FALSE, row.names = FALSE)
    meshCoordinates[,4] <- BHpvaluesTFCEc
    write.table(meshCoordinates, paste(extractNames[iSNP],"_BHpvaluesTFCE_Curvature.txt",sep=""), col.names = FALSE, row.names = FALSE)
    
    
    
    ##### Check for control ##########
    #################################
    SelectControls <- function(Xin,Ywfinal,Ysfinal,Ycfinal,extract,extractNames){ 
      
      bgn <-length(extractNames)+1
      fns <- ncol(Xin)
      iE<-0
      iSNP <- extract-1
      start.time <- Sys.time()
      betaw<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ywfinal))
      pvaluesw<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ywfinal))
      BHpvaluesw<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ywfinal))
      pvaluesTFCEw<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ywfinal))
      BHpvaluesTFCEw<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ywfinal))
      
      betas<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ysfinal))
      pvaluess<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ysfinal))
      BHpvaluess<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ysfinal))
      pvaluesTFCEs<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ysfinal))
      BHpvaluesTFCEs<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ysfinal))
      
      betac<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ycfinal))
      pvaluesc<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ycfinal))
      BHpvaluesc<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ycfinal))
      pvaluesTFCEc<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ycfinal))
      BHpvaluesTFCEc<-matrix(0,ncol = fns+1-bgn, nrow = ncol(Ycfinal))
      
      for(ic in bgn:fns){
        iE<-iE+1
        resultw<- murHC4m(Xin[,c(1:length(extractNames),ic)],Ywfinal,extract)
        results<- murHC4m(Xin[,c(1:length(extractNames),ic)],Ysfinal,extract)
        resultc<- murHC4m(Xin[,c(1:length(extractNames),ic)],Ycfinal,extract)
        
        correctedw <- mt.rawp2adjp(resultw[,3], proc=c("BH"), na.rm = FALSE)
        pvalueADJ5tsbh <- array(dim = length(resultw[,3]))
        BH_pvaluesw <- correctedw$adjp[order(correctedw$index),][,2]
        correcteds <- mt.rawp2adjp(results[,3], proc=c("BH"), na.rm = FALSE)
        pvalueADJ5tsbh <- array(dim = length(results[,3]))
        BH_pvaluess <- correcteds$adjp[order(correcteds$index),][,2]
        correctedc <- mt.rawp2adjp(resultc[,3], proc=c("BH"), na.rm = FALSE)
        pvalueADJ5tsbh <- array(dim = length(resultc[,3]))
        BH_pvaluesc <- correctedc$adjp[order(correctedc$index),][,2]
        betaw[,iE]<-resultw[,1]
        pvaluesw[,iE]<-resultw[,3]
        BHpvaluesw[,iE]<-BH_pvaluesw
        betas[,iE]<-results[,1]
        pvaluess[,iE]<-results[,3]
        BHpvaluess[,iE]<-BH_pvaluess
        betac[,iE]<-resultc[,1]
        pvaluesc[,iE]<-resultc[,3]
        BHpvaluesc[,iE]<-BH_pvaluesc
        
        signw <- permFL(Xin[,c(1:length(extractNames),ic)],Ywfinal,extract,A,NNmatrix,nPermutations, TRUE, TRUE, nofCores, E = 0.5, H = 2)
        signs <- permFL(Xin[,c(1:length(extractNames),ic)],Ysfinal,extract,A,NNmatrix,nPermutations, TRUE, TRUE, nofCores, E = 0.5, H = 2)
        signc <- permFL(Xin[,c(1:length(extractNames),ic)],Ycfinal,extract,A,NNmatrix,nPermutations, TRUE, TRUE, nofCores, E = 0.5, H = 2)
        
        #MULTIPLE TESTING CORRECTION
        pfdr5TSBHw <- mt.rawp2adjp(signw[,1+(iEx-1)*2], proc=c("BH"), na.rm = FALSE)
        pvalueADJ5tsbh <- array(dim = length(signw[,1+(iEx-1)*2]))
        BH_pvaluesTFCEw <- pfdr5TSBHw$adjp[order(pfdr5TSBHw$index),][,2]
        pfdr5TSBHs <- mt.rawp2adjp(signs[,1+(iEx-1)*2], proc=c("BH"), na.rm = FALSE)
        pvalueADJ5tsbh <- array(dim = length(signs[,1+(iEx-1)*2]))
        BH_pvaluesTFCEs <- pfdr5TSBHs$adjp[order(pfdr5TSBHs$index),][,2]
        pfdr5TSBHc <- mt.rawp2adjp(signc[,1+(iEx-1)*2], proc=c("BH"), na.rm = FALSE)
        pvalueADJ5tsbh <- array(dim = length(signc[,1+(iEx-1)*2]))
        BH_pvaluesTFCEc <- pfdr5TSBHc$adjp[order(pfdr5TSBHc$index),][,2]
        pvaluesTFCEw[,iE]<-signw[,1]
        BHpvaluesTFCEw[,iE]<-BH_pvaluesTFCEw
        pvaluesTFCEs[,iE]<-signs[,1]
        BHpvaluesTFCEs[,iE]<-BH_pvaluesTFCEs
        pvaluesTFCEc[,iE]<-signc[,1]
        BHpvaluesTFCEc[,iE]<-BH_pvaluesTFCEc
      }
      
      randomMatrix <- matrix(99999, nrow=nrow(mesh_Coordinates), ncol=fns-bgn+1)
      meshCoordinates <- cbind(mesh_Coordinates,randomMatrix)
      
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- betaw
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_beta_WT.txt",sep=""))
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- pvaluesw
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_pvalues_WT.txt",sep=""), col.names = FALSE, row.names = FALSE)
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- BHpvaluesw
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_BHpvalues_WT.txt",sep=""), col.names = FALSE, row.names = FALSE)
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- pvaluesTFCEw
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_pvaluesTFCE_WT.txt",sep=""), col.names = FALSE, row.names = FALSE)
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- BHpvaluesTFCEw
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_BHpvaluesTFCE_WT.txt",sep=""), col.names = FALSE, row.names = FALSE)
      
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- betas
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_beta_S2S.txt",sep=""))
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- pvaluess
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_pvalues_S2S.txt",sep=""), col.names = FALSE, row.names = FALSE)
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- BHpvaluess
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_BHpvalues_S2S.txt",sep=""), col.names = FALSE, row.names = FALSE)
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- pvaluesTFCEs
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_pvaluesTFCE_S2S.txt",sep=""), col.names = FALSE, row.names = FALSE)
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- BHpvaluesTFCEs
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_BHpvaluesTFCE_S2S.txt",sep=""), col.names = FALSE, row.names = FALSE)
      
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- betac
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_beta_Curvature.txt",sep=""))
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- pvaluesc
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_pvalues_Curvature.txt",sep=""), col.names = FALSE, row.names = FALSE)
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- BHpvaluesc
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_BHpvalues_Curvature.txt",sep=""), col.names = FALSE, row.names = FALSE)
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- pvaluesTFCEc
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[v],"_pvaluesTFCE_Curvature.txt",sep=""), col.names = FALSE, row.names = FALSE)
      meshCoordinates[,(3+fns-bgn):(3+fns-bgn+1)] <- BHpvaluesTFCEc
      write.table(meshCoordinates, paste(extractNames[iSNP],"_",extractNames[pos],"_BHpvaluesTFCE_Curvature.txt",sep=""), col.names = FALSE, row.names = FALSE)
      
      #Controls <- list(betaw,pvaluesw,BHpvaluesw,pvaluesTFCEw,BHpvaluesTFCEw,betas,pvaluess,BHpvaluess,pvaluesTFCEs,BHpvaluesTFCEs,betac,pvaluesc,BHpvaluesc,pvaluesTFCEc,BHpvaluesTFCEc)
      #return(Controls) 
    }
    
    ###### create dummie variable for each category ####
    ####################################################
    
    # Race
    inputClinicalin <-as.data.frame.array(Xout[,-1])
    inputClinicalin$race0 <- sapply(inputClinicalin$Race, function (x){ if(x==0) return(1) else return(0)})
    inputClinicalin$race1 <- sapply(inputClinicalin$Race, function (x){ if(x==1) return(1) else return(0)})
    inputClinicalin$race2 <- sapply(inputClinicalin$Race, function (x){ if(x==2) return(1) else return(0)})
    inputClinicalin$race3 <- sapply(inputClinicalin$Race, function (x){ if(x==3) return(1) else return(0)})
    inputClinicalin$race4 <- sapply(inputClinicalin$Race, function (x){ if(x==4) return(1) else return(0)})
    inputClinicalin$race5 <- sapply(inputClinicalin$Race, function (x){ if(x==5) return(1) else return(0)})
    Xin <- data.matrix(inputClinicalin)
    Xin <- cbind(1, Xin)
    pos<-which(colnames(Xin)==extractNames[1])
    Xin<-Xin[,-pos]
    Xin<-as.matrix(Xin)
    SelectControls(Xin,Ywfinal,Ysfinal,Ycfinal,extract,extractNames,pos)
    
    # Sex
    inputClinicalin <-as.data.frame.array(Xout[,-1])
    inputClinicalin$sex0 <- sapply(inputClinicalin$Sex, function (x){ if(x==0) return(1) else return(0)})
    inputClinicalin$sex1 <- sapply(inputClinicalin$Sex, function (x){ if(x==1) return(1) else return(0)})
    Xin <- data.matrix(inputClinicalin)
    Xin <- cbind(1, Xin)
    pos<-which(colnames(Xin)==extractNames[2])
    Xin<-Xin[,-pos]
    Xin<-as.matrix(Xin)
    SelectControls(Xin,Ywfinal,Ysfinal,Ycfinal,extract,extractNames,pos)
    
    # Age
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
    pos<-which(colnames(Xin)==extractNames[3])
    Xin<-Xin[,-pos]
    Xin<-as.matrix(Xin)
    
    print(dim(Xin))
    print(dim(Yfinal))
    SelectControls(Xin,Ywfinal,Ysfinal,Ycfinal,extract,extractNames,pos)
    
    # Smoking
    inputClinicalin <-as.data.frame.array(Xout[,-1])
    inputClinicalin$Smoking0 <- sapply(inputClinicalin$Smoking, function (x){ if(x==0) return(1) else return(0)})
    inputClinicalin$Smoking1 <- sapply(inputClinicalin$Smoking, function (x){ if(x==1) return(1) else return(0)})
    inputClinicalin$Smoking2 <- sapply(inputClinicalin$Smoking, function (x){ if(x==2) return(1) else return(0)})
    inputClinicalin$Smoking3 <- sapply(inputClinicalin$Smoking, function (x){ if(x==3) return(1) else return(0)})
    
    Xin <- data.matrix(inputClinicalin)
    Xin <- cbind(1, Xin)
    pos<-which(colnames(Xin)==extractNames[4])
    Xin<-Xin[,-pos]
    Xin<-as.matrix(Xin)
    SelectControls(Xin,Ywfinal,Ysfinal,Ycfinal,extract,extractNames,pos)
    
    # BSA
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
    pos<-which(colnames(Xin)==extractNames[5])
    Xin<-Xin[,-pos]
    Xin<-as.matrix(Xin)
    SelectControls(Xin,Ywfinal,Ysfinal,Ycfinal,extract,extractNames,pos)
    
    # SBP
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
    pos<-which(colnames(Xin)==extractNames[6])
    Xin<-Xin[,-pos]
    Xin<-as.matrix(Xin)
    SelectControls(Xin,Ywfinal,Ysfinal,Ycfinal,extract,extractNames,pos)
    
    # DBP
    inputClinicalin <-as.data.frame.array(inputClinicalout[,-1])
    d1<-(inputClinicalin$DBP<80)
    d2<-((inputClinicalin$DBP>=80) & (inputClinicalin$SBP<=89))
    d3<-(inputClinicalin$DBP>=90)
    
    dbp1 <- ifelse(d1,1,0)
    dbp2 <- ifelse(d2,2,dbp1)
    dbp3 <- ifelse(d3,3,dbp2)
    dbp_value <-sbp3
    Xin <- data.frame(Xout)
    Xin$DBP1 <- sapply(dbp_value, function (x){ if(x==1) return(1) else return(0)})
    Xin$DBP2 <- sapply(dbp_value, function (x){ if(x==2) return(1) else return(0)})
    Xin$DBP3 <- sapply(dbp_value, function (x){ if(x==3) return(1) else return(0)})
    pos<-which(colnames(Xin)==extractNames[7])
    Xin<-Xin[,-pos]
    Xin<-as.matrix(Xin)
    SelectControls(Xin,Ywfinal,Ysfinal,Ycfinal,extract,extractNames,pos)
    
  }
  
}# END
