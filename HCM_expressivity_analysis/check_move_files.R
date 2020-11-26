#################################################
##### PRODUCE, CHECK and MOVE FILES #############
#################################################
rm(list = ls(all = TRUE))

install.packages("matrixStats")

library(matrixStats)

#SET THE PATH OF THE WORKING DIRECTORY
setwd("~/cardiac/UKBB_40616/4D_Segmented_2.0_to_do_motion_HCM_2_copy_bad/")

#SET THE PATH OF THE FOLDERS STORING THE IMAGING DATA
folderImagingDataPath<-c("~/cardiac/UKBB_40616/4D_Segmented_2.0_to_do_motion_HCM_2_copy_bad/")

#DEFINE THE IMAGING PHENOTYPE
phenoType <- c(1:2)

fileNames <- c("/lv_myoed_wallthickness.txt",
               "/lv_myoes_wallthickness.txt",
               "/lv_myoed_signeddistances.txt",
               "/lv_myoes_signeddistances.txt",
               # "/lv_myoed_curvature.txt",
               # "/lv_myoes_curvature.txt"
               )

foldersNames <- list.dirs(folderImagingDataPath,
                          full.names = FALSE,
                          recursive = F)

clinicalData<-foldersNames
nPatients<-length(clinicalData)

# PRODUCE THE Y MATRIX
Y <- matrix(0, ncol = nPoints, nrow = nPatients)

for(iP in 1:nPatients){
  filePath <- paste(folderImagingDataPath,
                    foldersNames[iP],
                    fileNames[phenoType],  sep = "") 
  setTxtProgressBar(txtProgressBar(style=3),iP/nPatients)
  #Imaging Data Path for the iP patient
  if (file.exists(filePath)){  #if the folder exist
    rDataFrame <- lapply(file.path(filePath), function(x) {
      tryCatch(read.table(x), error=function(e) NULL)
    })
    if(!is.null(rDataFrame[[1]]$V4)){
      Y[iP,] <- as.vector(rDataFrame[[1]]$V4) #take the 4th row of the mesh
    }
  }
}


#CHECK FOR SPURIOUS WT WITH MAXIMUM VALUES >20mm #####
Yw_max<-rowMaxs(Y)
p1<-which(Yw_max>=20)
p2<-which(Yw_max>=30)
p3<-which(Yw_max>=45)
id_20<-clinicalData[p1]
id_30<-clinicalData[p2]
id_45<-clinicalData[p3]
n<-max(length(id_20), length(id_30), length(id_45))
length(id_20)<-n
length(id_30)<-n
length(id_45)<-n

#BIND ALL IDS WITH WT>20mm, WT>30mm AND WT>45mm AND SAVE
ids<-cbind(id_20,id_30,id_45)
colnames(ids)<-c("IDs_20","IDs_30","IDs_45")
write.csv(ids,"IDs_20_30_45.csv", col.names = T, row.names = F)
max(Yw_max)


#MOVE THE IDS WITH WT<20mm TO THE DIRECTORY FOR ANALYSIS #####
base.dir<- "~/cardiac/UKBB_40616/4D_Segmented_2.0_to_do_motion_HCM_2_copy_bad"
setwd(base.dir)

#SET THE PATH OF THE DIRECTORY WERE YOU WANT TO CREATE THE NEW FOLDERS
setwd("~/cardiac/UKBB_40616/4D_Segmented_2.0_to_do_motion_HCM/")
l1<-inp[-p1] # l1 ARE ALL THE IDS WITH WT<20mm 
for (i in 1:length(l1)){
  dir.create(l1[i])}

#MOVE THE FILES FROM NE DIRECTORY TO ANOTHER
for (i in 1:length(l1)){
  file <- as.matrix(Sys.glob(file.path(paste(base.dir, l1[i], sep="/"),"*")))
  file.copy(from=file[1:length(file)], 
            to=paste("~/cardiac/UKBB_40616/4D_Segmented_2.0_to_do_motion_HCM",l1[i], sep="/"),
            recursive = TRUE)
  
}

#END
