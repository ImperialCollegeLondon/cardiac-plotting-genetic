###########################################################################################
################# Mass Univariate Analysis for ShinyApp ###################################
###########################################################################################
###########################################################################################
rm(list = ls(all = TRUE))  
#start with an empty environment

install.packages("data.table")
install.packages("BiocManager")
BiocManager::install("multtest")
install.packages("devtools")
install_github("UK-Digital-Heart-Project/mutools3D", build_vignettes = TRUE)
install.packages("Rcpp")
install.packages("RcppArmadillo")
install.packages("RcppEigen")
install.packages("float")
install.packages("doParallel")
install.packages("foreach")
install.packages("igraph")
install.packages("plyr")
# install all packages required

setwd("~/cardiac/Experiments_of_Maria/20191128_Shinyapp_interactive_phenotypic_applications/Mass_univariate_analysis/")
library(data.table)
library(multtest)
library(mutools3D)
library(Rcpp) # required in permFL_fast
library(RcppArmadillo) # required in mulitply.cpp
library(RcppEigen) # required in mulitply.cpp
library(float) # required in permFL_fast

func_dir<-"~/cardiac/Experiments_of_Maria/20191128_Shinyapp_interactive_phenotypic_applications/Mass_univariate_analysis/"
#folder directory of the code. Will be used for the functions as: source(paste(dir,"functions/xxx", sep="/")) 
#in the permFL_fast function.

sourceCpp(paste(func_dir,"functions/multiply.cpp",sep=""))
#include a cpp functions for faster multiplication of matrices

# source(paste(func_dir,"functions/murq.R",sep=""))
# include the mur function with QR decomposition (slightly faster!), HC4m = FALSE
#or include the mur function from mutools3D package

#include the functions for TFCE from mutools3D package

source(paste(func_dir,"functions/permFL_fast.R",sep=""))
#include the permFL_fast functions with murq and HC4m = FALSE

#CLINICAL DATA MATRIX
#NCOL = N COVARIATES UNDER STUDY
#NROW = N PATIENTS
# ClinicalData<-fread("~/cardiac/Experiments_of_Maria/20191128_Shinyapp_interactive_phenotypic_applications/data_3d/Old_data/Phenotypes_27k_imp.txt")
# inputClinical<-as.data.frame(ClinicalData)#[pc,])
inputClinical <- readRDS("data_3d/Old_data/WTedLVclinicalData.rds")
 nr1<-which(inputClinical$Race==-1)
 inputClinical<-inputClinical[-nr1,] # remove -1 (Do not know) from Race
 nr3<-which(inputClinical$Race==-3) 
 inputClinical<-inputClinical[-nr3,] # remove -3 (Prefer not to say) from Race
 

inputClinical$Age <- scale(inputClinical$Age)
inputClinical$BSA <- scale(inputClinical$BSA)
inputClinical$SBP <- scale(inputClinical$SBP)
inputClinical$DBP <- scale(inputClinical$DBP)
#inputClinical$Vigorous_activity <- scale(inputClinical$Vigorous_activity)
colnames(inputClinical)
forscale<-c(9:23)
for (iS in 1:15){
  inputClinical[,forscale[iS]]<-scale(inputClinical[,forscale[iS]])
}

# Remove any "Don't know" or "Prefer not to say" from data
#  nd1<-which(inputClinical$Diabetes==-1)
#  inputClinical<-inputClinical[-nd1,]
# nc1<-which(inputClinical$Cancer==-1)
# inputClinical<-inputClinical[-nc1,]
# nc3<-which(inputClinical$Cancer==-3)
# inputClinical<-inputClinical[-nc3,]
# ns3<-which(inputClinical$Smoking==-3)
# inputClinical<-inputClinical[-ns3,]
# na3<-which(inputClinical$Alcohol==-3)
# inputClinical<-inputClinical[-na3,]
# nb1<-which(inputClinical$Disability==-1)
# inputClinical<-inputClinical[-nb1,]
# nb3<-which(inputClinical$Disability==-3)
# inputClinical<-inputClinical[-nb3,]

head(inputClinical)
X <- data.matrix(inputClinical)
X <- cbind(1, X)
X<-X[,c(1,4:9,29)]
dim(X)
head(X)

Xcoord <- readRDS("data_3d/Old_data/WTedXcoordinate.rds")
Ycoord <- readRDS("data_3d/Old_data/WTedYcoordinate.rds")
Zcoord <- readRDS("data_3d/Old_data/WTedZcoordinate.rds")

# dim(Xcoordd)
#NUMBER OF CORES TO USE
nofCores = 48

#IMAGING DATA MATRIX
#NCOL = N POINTS ON THE ATLAS
#NROW = N PATIENTS
Yw <- readRDS("data_3d/Old_data/WTedLV.rds")
#DATA PRE-PROCESSING

{
dim(Yw)
Ywd<-Yw[-nr1,]
Ywd<-Ywd[-nr3,]
Ywd <- scale(Ywd)
dim(Ywd)
}

#IMAGING DATA MATRIX
#NCOL = N POINTS ON THE ATLAS
#NROW = N PATIENTS
Ys <- readRDS("data_3d/Old_data/S2SedLV.rds")
#DATA PRE-PROCESSING

dim(Ys)
{
Ysd<-Ys[-nr1,]
Ysd<-Ysd[-nr3,]
Ysd <- scale(Ysd)
dim(Ysd)
}

nPermutations=1000
whichEE <- 3
#1 endo, 2 epi, 3 full shape
endoEpi <- read.table("~/cardiac/Experiments_of_Maria/20191024_ShinyApp/shinyplot/endo_epi.txt")
vert2print <- list(which(endoEpi[,4]==0),which(endoEpi[,4]==1),1:length(endoEpi[,4]))
Ywd<-Ywd[,vert2print[[whichEE]]]
Ysd<-Ysd[,vert2print[[whichEE]]]


##READ THE NNLIST ASSOCIATED TO EACH VERTEX
NNmatrix <- readRDS("~/cardiac/Experiments_of_Maria/20191024_ShinyApp/shinyplot/redNNmatrix.rds")
##READ AREAS ASSOCIATED TO EACH VERTEX
A <- readRDS("~/cardiac/Experiments_of_Maria/20191024_ShinyApp/shinyplot/LVarea.rds")

## Produce a new mean of meshCoordinates

# x_Coordinates <- Xcoord[,vert2print[[3]]]
# y_Coordinates <- Ycoord[,vert2print[[3]]]
# z_Coordinates <- Zcoord[,vert2print[[3]]]
# 
#   X_coordinates <- colMeans(x_Coordinates)
#   Y_coordinates <- colMeans(y_Coordinates)
#   Z_coordinates <- colMeans(z_Coordinates)
# mesh_Coordinates <- data.frame(X_coordinates, Y_coordinates, Z_coordinates)
# colnames(mesh_Coordinates) <- c("x", "y", "z")
# write.table(mesh_Coordinates,"data_3d/Old_data/meshCoordinates.txt", col.names = FALSE, row.names = FALSE)

## or use the meshCoordinates from the mean of 27k
mesh_Coordinates <- read.table("data_3d/Old_data/meshCoordinates.txt", quote="\"", comment.char="")
setwd("~/cardiac/Experiments_of_Maria/20191128_Shinyapp_interactive_phenotypic_applications/All_3D")

extractNames <- colnames(X)
extract=7
{

  iEx <-1
  resultw <- murq(X,Ywd,extract)
  results <- murq(X,Ysd,extract)
  #extract only the pheno betas
  
  #MULTIPLE TESTING CORRECTION
  correctedw <- mt.rawp2adjp(resultw[,3], proc=c("BH"), na.rm = FALSE)
  pvalueADJ5tsbh <- array(dim = length(resultw[,3]))
  BHpvaluesw <- correctedw$adjp[order(correctedw$index),][,2]
  correcteds <- mt.rawp2adjp(results[,3], proc=c("BH"), na.rm = FALSE)
  pvalueADJ5tsbh <- array(dim = length(results[,3]))
  BHpvaluess <- correcteds$adjp[order(correcteds$index),][,2]
  
  meshCoordinates <- cbind(mesh_Coordinates,99999)
  #PRINT OUTPUT
  meshCoordinates[,4] <- resultw[,1]
  write.table(meshCoordinates, paste(extractNames[extract],"_beta_WT.txt",sep=""), col.names = FALSE, row.names = FALSE)
  meshCoordinates[,4] <- resultw[,3]
  write.table(meshCoordinates, paste(extractNames[extract],"_pvalues_WT.txt",sep=""), col.names = FALSE, row.names = FALSE)
  meshCoordinates[,4] <- BHpvaluesw
  write.table(meshCoordinates, paste(extractNames[extract],"_BHpvalues_WT.txt",sep=""), col.names = FALSE, row.names = FALSE)
  
  meshCoordinates[,4] <- results[,1]
  write.table(meshCoordinates, paste(extractNames[extract],"_beta_S2S.txt",sep=""), col.names = FALSE, row.names = FALSE)
  meshCoordinates[,4] <- results[,3]
  write.table(meshCoordinates, paste(extractNames[extract],"_pvalues_S2S.txt",sep=""), col.names = FALSE, row.names = FALSE)
  meshCoordinates[,4] <- BHpvaluess
  write.table(meshCoordinates, paste(extractNames[extract],"_BHpvalues_S2S.txt",sep=""), col.names = FALSE, row.names = FALSE)
  length(which(BHpvaluesw<0.05))
  length(which(BHpvaluess<0.05))
  
  signifw<-permFL_fast(X, Ywd, extract, A, NNmatrix, nPermutations, E=0.5, H=2, func.dir)
  signifs<-permFL_fast(X, Ysd, extract, A, NNmatrix, nPermutations, E=0.5, H=2, func.dir)

  signw<-signifw$pvalues
  signs<-signifs$pvalues
  pfdr5TSBHw <- mt.rawp2adjp(signw[,1], proc=c("BH"), na.rm = FALSE)
  pvalueADJ5tsbh <- array(dim = length(signw[,1]))
  BHpvaluesTFCEw <- pfdr5TSBHw$adjp[order(pfdr5TSBHw$index),][,2]
  pfdr5TSBHs <- mt.rawp2adjp(signs[,1], proc=c("BH"), na.rm = FALSE)
  pvalueADJ5tsbh <- array(dim = length(signs[,1]))
  BHpvaluesTFCEs <- pfdr5TSBHs$adjp[order(pfdr5TSBHs$index),][,2]
  # 
  # #PRINT OUTPUT
  meshCoordinates[,4] <- signw[,1]
  write.table(meshCoordinates, paste(extractNames[extract],"_pvaluesTFCE_WT.txt",sep=""), col.names = FALSE, row.names = FALSE)
  meshCoordinates[,4] <- BHpvaluesTFCEw
  write.table(meshCoordinates, paste(extractNames[extract],"_BHpvaluesTFCE_WT.txt",sep=""), col.names = FALSE, row.names = FALSE)
  
  meshCoordinates[,4] <- signs[,1]
  write.table(meshCoordinates, paste(extractNames[extract],"_pvaluesTFCE_S2S.txt",sep=""), col.names = FALSE, row.names = FALSE)
  meshCoordinates[,4] <- BHpvaluesTFCEs
  write.table(meshCoordinates, paste(extractNames[extract],"_BHpvaluesTFCE_S2S.txt",sep=""), col.names = FALSE, row.names = FALSE)
  
}
time.taken
length(which(BHpvaluesTFCEw<0.05))
length(which(BHpvaluesTFCEs<0.05))

# END
