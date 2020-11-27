
rm(list = ls(all = TRUE))  
#start with an empty environment

setwd("~/cardiac/DL_segmentation/3D_HCM_2020_Experiments/Mass_univariate_analysis_GenScan/")
library(data.table)
library(multtest)
library(mutools3D)
library(Rcpp) # required in permFL_fast
library(RcppArmadillo) # required in mulitply.cpp
library(RcppEigen) # required in mulitply.cpp
library(float) # required in permFL_fast
func.dir<-"~/cardiac/DL_segmentation/3D_HCM_2020_Experiments/Mass_univariate_analysis_GenScan/"
# folder directory of the code. Will be used for the functions as: source(paste(dir,"functions/TFCE.R", sep="/")) in the permFL function.
source("functions/TFCE.R")
#include the functions for TFCE
source(paste(func_dir,"functions/permFL_fast.R",sep=""))
#include the permZ for TFCE on z-statistics

# Apply TFCE on z-statistics
###TFCE####

# input summary table data
inputClinical1 <- readRDS(paste(func.dir,"data/HVOL/WTedLVclinicaldata.rds", sep=""))
inputClinical2 <- readRDS(paste(func.dir,"data/GENSCAN_2/WTedLVclinicaldata.rds", sep=""))
# HCM summary table
head(summary_table)
dim(summary_table)


# input imaging data
Yw1 <- readRDS(paste(func.dir,"data/HVOL/WTedLV.rds", sep=""))
Ys1 <- readRDS(paste(func.dir,"data/HVOL/S2SedLV.rds", sep=""))
Yw1<-Yw1[x,];
Yw1<-Yw1[pos,];
Ys1<-Ys1[x,];
Ys1<-Ys1[pos,];
Yw2 <- readRDS(paste(func.dir,"data/GENSCAN_2/WTedLV.rds", sep=""))
Ys2 <- readRDS(paste(func.dir,"data/GENSCAN_2/S2SedLV.rds", sep=""))
Yw2<-Yw2[pos_1,];
Ys2<-Ys2[pos_1,];
dim(Yw1);dim(Yw2)
# input mesh coordinates
Xcoord <- readRDS(paste(func.dir,"data/HVOL/WTedXcoordinate.rds", sep=""))
Ycoord <- readRDS(paste(func.dir,"data/HVOL/WTedYcoordinate.rds", sep=""))
Zcoord <- readRDS(paste(func.dir,"data/HVOL/WTedZcoordinate.rds", sep=""))

whichEE <- 3
#1 endo, 2 epi, 3 full shape
endoEpi <- read.table(paste(func.dir,"data/endo_epi.txt", sep=""))
vert2print <- list(which(endoEpi[,4]==0),which(endoEpi[,4]==1),1:length(endoEpi[,4]))
Yw1<-Yw1[,vert2print[[whichEE]]];Yw2<-Yw2[,vert2print[[whichEE]]]
Ys1<-Ys1[,vert2print[[whichEE]]];Ys2<-Ys2[,vert2print[[whichEE]]]

##READ THE NNLIST ASSOCIATED TO EACH VERTEX
NNmatrix <- readRDS(paste(func.dir,"data/redNNmatrix.rds", sep=""))
##READ AREAS ASSOCIATED TO EACH VERTEX
A <- readRDS(paste(func.dir,"data/LVarea.rds", sep=""))

## Produce a new mean of meshCoordinates

x_Coordinates <- Xcoord[,vert2print[[whichEE]]]
y_Coordinates <- Ycoord[,vert2print[[whichEE]]]
z_Coordinates <- Zcoord[,vert2print[[whichEE]]]

X_coordinates <- colMeans(x_Coordinates)
Y_coordinates <- colMeans(y_Coordinates)
Z_coordinates <- colMeans(z_Coordinates)
mesh_Coordinates <- data.frame(X_coordinates, Y_coordinates, Z_coordinates)
colnames(mesh_Coordinates) <- c("x", "y", "z")

nPermutations<-1000

# bind the 2 visits
Ywd<-rbind(Yw2,Yw1);Ysd<-rbind(Ys2,Ys1);
dim(Ywd)
visit<-matrix(1,nrow=(nrow(Yw2)+nrow(Yw1)))
visit[(nrow(Ywd2)+1):nrow(visit),]<-0
(nrow(Ywd2)+1)

# differences
sYw1<-colMeans(Yw1);sYw2<-colMeans(Yw2);
sYs1<-colMeans(Ys1);sYs2<-colMeans(Ys2);

difYw<-sYw2-sYw1
difYs<-sYs2-sYs1
mean(difYw)

# Compute the z-statistics from wilcoxon test
{
  pvals<-matrix(0, nrow=ncol(Ywd), ncol=2)
  zstat<-matrix(0, nrow=ncol(Ywd), ncol=2)
  for (iN in 1: ncol(Ywd)){
    my_data<-as.data.frame(cbind(Ywd[,iN], Ysd[,iN],visit))
    colnames(my_data)<-c("Yw", "Ys","visit")
    resw<-wilcox.test(Yw~visit, data=my_data,paired=T)#exact=F)#
    ress<-wilcox.test(Ys~visit, data=my_data,paired=T)#exact=F)
    pvals[iN,1]<-resw$p.value; pvals[iN,2]<-ress$p.value
    zstat[iN,1]<-qnorm(resw$p.value/2); zstat[iN,1]<-qnorm(ress$p.value/2)
  }
  
  #MULTIPLE TESTING CORRECTION
  correctedw <- mt.rawp2adjp(pvals[,1], proc=c("BH"), na.rm = FALSE)
  BHpvaluesw <- correctedw$adjp[order(correctedw$index),][,2]
  correcteds <- mt.rawp2adjp(pvals[,2], proc=c("BH"), na.rm = FALSE)
  BHpvaluess <- correcteds$adjp[order(correcteds$index),][,2]
  length(which(pvals[,1]<0.05));length(which(pvals[,2]<0.05))
  length(which(BHpvaluesw<0.05));length(which(BHpvaluess<0.05))
  
  meshCoordinates <- cbind(mesh_Coordinates,99999)
  #PRINT OUTPUT
  meshCoordinates[,4] <- difYw
  write.table(meshCoordinates, "HCM_difference_WT.txt", col.names = FALSE, row.names = FALSE)
  meshCoordinates[vert2print[[whichEE]],4] <- pvals[,1]
  write.table(meshCoordinates, "HCM_pvalues_WT.txt", col.names = FALSE, row.names = FALSE)
  meshCoordinates[vert2print[[whichEE]],4] <- BHpvaluesw
  write.table(meshCoordinates, "HCM_BHpvalues_WT.txt", col.names = FALSE, row.names = FALSE)
  
  meshCoordinates[,4] <- difYs
  write.table(meshCoordinates, "HCM_difference_S2S.txt", col.names = FALSE, row.names = FALSE)
  meshCoordinates[vert2print[[whichEE]],4] <- pvals[,2]
  write.table(meshCoordinates, "HCM_pvalues_S2S.txt", col.names = FALSE, row.names = FALSE)
  meshCoordinates[vert2print[[whichEE]],4] <- BHpvaluess
  write.table(meshCoordinates, "HCM_BHpvalues_S2S.txt", col.names = FALSE, row.names = FALSE)
  
  # Compue TFCE of the z-statistics
  signifw<-permZ(Ywd, visit, extract, A, NNmatrix, nPermutations)
  signifs<-permZ(Ysd, visit, extract, A, NNmatrix, nPermutations)
  
  signw<-signifw$pvalues # get p-values
  signs<-signifs$pvalues # get p-values
  
  BHpvaluess <- correcteds$adjp[order(correcteds$index),][,2]
  length(which(pvals[,1]<0.05));length(which(pvals[,2]<0.05))
  length(which(BHpvaluesw<0.05));length(which(BHpvaluess<0.05))
  setwd(paste(func.dir,"output/GENSCAN_2/step2b", sep=""))
  
  meshCoordinates <- cbind(mesh_Coordinates,99999)
  #PRINT OUTPUT
  meshCoordinates[vert2print[[whichEE]],4] <- pvals[,1]
  write.table(meshCoordinates, "HCM_pvalues_WT.txt", col.names = FALSE, row.names = FALSE)
  meshCoordinates[vert2print[[whichEE]],4] <- BHpvaluesw
  write.table(meshCoordinates, "HCM_BHpvalues_WT.txt", col.names = FALSE, row.names = FALSE)
  
  meshCoordinates[vert2print[[whichEE]],4] <- pvals[,2]
  write.table(meshCoordinates, "HCM_pvalues_S2S.txt", col.names = FALSE, row.names = FALSE)
  meshCoordinates[vert2print[[whichEE]],4] <- BHpvaluess
  write.table(meshCoordinates, "HCM_BHpvalues_S2S.txt", col.names = FALSE, row.names = FALSE)
  
}

# END