#################################################################################
#######  SAVE IMAGING AND CLINICAL MATRICES FOR EACH MODEL UNDER STUDY #########
#################################################################################
rm(list = ls(all = TRUE))  
#start with an empty environment
library(doParallel)
library(parallel)
library(foreach)
registerDoParallel(detectCores())
#detect the number of cores available and use them

#################################################################################
############################# USER DEFINITIONS ##################################
#################################################################################
#Before running this code, you need to .csv file containig the clinicalData: this file should 
#contain in the first column the BRU numbers of the subjects and in the other columns the clinical covariates you want to study.

#SET THE PATH OF THE WORKING DIRECTORY
setwd("~/cardiac/Experiments_of_Maria/Mass_univariate_analysis/")

#SET THE PATH OF THE FILE CONTAINING THE CLINICAL DATA OF THE PATIENTS FOR ALL THE MODELS
fileClinicalDataPath <- c("~/cardiac/DL_segmentation/IHD_Richard/IHD_Cohort/IHD_databases/IHDtestbatch.csv")


#SET THE PATH OF THE FOLDERS STORING THE IMAGING DATA
folderImagingDataPath<-c("~/cardiac/DL_segmentation/to_conv_ms_goodIHD/")
# folderImagingDataPath2<-c("~/cardiac/UKBB_40616/4D_Segmented_2.0_to_do_motion_HCM_2_copy_bad/")

endoEpi <- read.table("data/endo_epi.txt")

#DEFINE THE IMAGING PHENOTYPE
phenoType <- c(1:4)

#1=WTed, 2=WTes, 3=S2Sed, 4=S2Ses, 5=Curvatureed, 6=Curvaturees, 7=EC, 8=EL
#1=WT, 2=RWT, 3=S2Sed, 4=S2Ses, 5=E, 6=ER, 7=EC, 8=EL

#DEFINE THE INDEPENDENT VARIABLES OF THE MODEL
#PUT THEM IN THE SAME ORDER AS THEY ARE IN THE FILE
#indVar <- c("ID","Race", "Sex", "Age", "Smoking", "BSA", "SBP2", "SBP21","DBP2", "DBP21","Diabetes")


#################################################################################
#################################################################################
########################## DATA LOADING FUNCTION ################################
#################################################################################
#################################################################################
loadingData <- function(fileClinicalDataPath, folderImagingDataPath, indVar, phenoType){
  
  x <- as.data.frame(read.csv("~/cardiac/DL_segmentation/IHD_Richard/IHD_Cohort/IHD_databases/IHDtestbatch.csv"), header=T)
  indVar = colnames(x)[2:6]
  phenoType <- c(1:4)
  
  fileNames <- c("/lv_myoed_wallthickness.txt",
                 "/lv_myoes_wallthickness.txt",
                 "/lv_myoed_signeddistances.txt",
                 "/lv_myoes_signeddistances.txt",
                 "/lv_myoed_curvature.txt",
                 "/lv_myoes_curvature.txt")
  # "/lv_function_components.txt",
  # "/lv_function_components.txt") 
  
  foldersNames <- list.dirs(folderImagingDataPath,
                            full.names = FALSE,
                            recursive = F)
  clinicalData <- as.data.frame(read.csv(fileClinicalDataPath))
  nPatients <- nrow(clinicalData)
  nPoints <- 46808 #number of point in the atlas 
  
  iDel <- 1
  noK <- c()
  
  for(iP in 1:nPatients){
    filePath <- paste(folderImagingDataPath,
                      foldersNames[grep(clinicalData[iP,1], foldersNames)[1]],
                      fileNames[phenoType],  sep = "") 
    setTxtProgressBar(txtProgressBar(style=3),iP/nPatients)
    if(!file.exists(filePath[1])){ 
      noK[iDel] <- iP
      iDel <- iDel + 1
    }
  }
  
  
  if(length(noK)>0) clinicalData <- clinicalData[-noK,]
  nPatients <- nrow(clinicalData)
  
  Y <- matrix(0, ncol = nPoints, nrow = nPatients)
  Xc <- matrix(0, ncol = nPoints, nrow = nPatients)
  Yc <- matrix(0, ncol = nPoints, nrow = nPatients)
  Zc <- matrix(0, ncol = nPoints, nrow = nPatients)
  print(dim(Y))
  
  if(phenoType<5){ 
    #READ ATLAS DATA
    #Y response matrix
    for(iP in 1:nPatients){
      filePath <- paste(folderImagingDataPath,
                        foldersNames[grep(clinicalData[iP,1], foldersNames)[1]],
                        fileNames[phenoType],  sep = "") 
      setTxtProgressBar(txtProgressBar(style=3),iP/nPatients)
      #Imaging Data Path for the iP patient
      if (file.exists(filePath)){  #if the folder exist
        rDataFrame <- lapply(file.path(filePath), function(x) {
          tryCatch(read.table(x), error=function(e) NULL)
        })
        if(!is.null(rDataFrame[[1]]$V4)){
          Xc[iP,] <- as.vector(rDataFrame[[1]]$V1) #take the 1st row of the mesh 
          Yc[iP,] <- as.vector(rDataFrame[[1]]$V2) #take the 2nd row of the mesh 
          Zc[iP,] <- as.vector(rDataFrame[[1]]$V3) #take the 3rd row of the mesh
          Y[iP,] <- as.vector(rDataFrame[[1]]$V4) #take the 4th row of the mesh
        }
      }
    }
  }
  
  if(phenoType>=5){
    #COMPUTE DISTANCES
    #Y response matrix
    if(phenoType==5) extract = 10
    if(phenoType==6) extract = 9
    if(phenoType==7) extract = 8
    if(phenoType==8) extract = 7
    
    for(iP in 1:nPatients){
      filePath <- paste(folderImagingDataPath,
                        foldersNames[grep(clinicalData[iP,1], foldersNames)[1]],
                        fileNames[phenoType],  sep = "") 
      setTxtProgressBar(txtProgressBar(style=3),iP/nPatients)
      #Imaging Data Path for the iP patient
      if (file.exists(filePath)){  #if the folder exist
        rDataFrame <- lapply(file.path(filePath), function(x) {
          tryCatch(read.table(x), error=function(e) NULL)
        })
        Xc[iP,] <- as.vector(rDataFrame[[1]]$V1) #take the 1st row of the mesh 
        Yc[iP,] <- as.vector(rDataFrame[[1]]$V2) #take the 2nd row of the mesh 
        Zc[iP,] <- as.vector(rDataFrame[[1]]$V3) #take the 3rd row of the mesh
        Y[iP,] <- as.vector(rDataFrame[[1]]$V4) #take the 4th row of the mesh
      }
    }
  }
  
  
  X <- clinicalData[,which(names(clinicalData)%in%indVar)]
  fmlaLM <- paste("Y[,y] ~ ", paste("clinicalData$", names(X), collapse= "+"))
  
  model <- list(X, Y, Xc, Yc, Zc, fmlaLM)
  
  return(model)
}


############################################################################
############################################################################
############################### STORE OUTPUT ###############################
############################################################################
############################################################################
#phenoTypeNames <- c("WTed","WTes","S2Sed", "S2Ses","Curvatureed", "Curvaturees")
phenoTypeNames <- c("WTed","WTes","S2Sed","S2Ses")
nPoints <- 46808 

for(iM in 1:length(phenoType)){
  model <- loadingData(fileClinicalDataPath, folderImagingDataPath, indVar, phenoType[iM])
  
  clinicalData <- model[[1]]
  Y <- model[[2]]
  Xc <- model[[3]]
  Yc <- model[[4]]
  Zc <- model[[5]]
  
  saveRDS(Y, paste("data/", phenoTypeNames[phenoType[iM]],"LV.rds",sep =""))
  saveRDS(clinicalData, paste("data/", phenoTypeNames[phenoType[iM]],"LVclinicalData.rds",sep =""))
  saveRDS(Xc, paste("data/", phenoTypeNames[phenoType[iM]],"Xcoordinate.rds",sep =""))
  saveRDS(Yc, paste("data/", phenoTypeNames[phenoType[iM]],"Ycoordinate.rds",sep =""))
  saveRDS(Zc, paste("data/", phenoTypeNames[phenoType[iM]],"Zcoordinate.rds",sep =""))
  
  
}
