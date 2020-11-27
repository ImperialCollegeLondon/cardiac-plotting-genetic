# The following is a script used to generate a file neopheno.txt with additional coloumns with strain from tensors resolved into its long, circ, radial strain
#1. First compile a list of codes you want the neopheno file generated
# codes <- c("PHXY0808","PHAB2312") #these are fictitious
#set the directory in :
# path <- "S:/xyz"
#this is the parent directory of the timcode folders.
#I suggest trialling this on a test folder that is a copy first
rm(list = ls(all = TRUE)) 

install.packages("gdata")
install.packages("data.table")
install.packages("FNN")

library(gdata)
library(data.table)
library(FNN)
#this converts cylindrical coordinates to cartesian coordinates
cart2cylc <- function(x.i,y.i,z.i) { #theta in degrees
  r.i <- sqrt(x.i^2+y.i^2)
  t.i <- atan(y.i/x.i)*(180/pi)
  z.i <- z.i
  return(c(r.i,t.i,z.i))
}

#this converts cartesian coordinates to cylindrical coordinates 
cart2sph <- function(x.i,y.i,z.i) { #theta in degrees
  r.i <- sqrt(x.i^2+y.i^2+z.i^2)
  t.i <- acos(z.i/r.i)*(180/pi)
  p.i <- atan(y.i/x.i)*(180/pi)
  return(c(r.i,p.i,t.i))
}

#this converts cartesian coordinates to spherical coordinates 
cylc2cart <- function(r.i,t.i,z.i) { 
  x.i <- r.i * (cos((t.i))) # if t.i is in rad or (cos((t.i*pi)/180)) if t.i is in degrees
  y.i <- r.i * (sin((t.i))) # if t.i is in rad or (sin((t.i*pi)/180)) if t.i is in degrees
  z.i <- z.i
  return(c(x.i,y.i,z.i))
}
 
unitvar<-function(x.i,y.i,z.i) { #theta in degrees
  u.i <- (1/sqrt(3))*(x.i+y.i+z.i)
  v.i <- (1/sqrt(6))*(x.i+y.i-(2*z.i))
  w.i <- (1/sqrt(2))*(x.i-y.i)
  return(c(u.i,v.i,w.i))
}

base.dir<- "~/cardiac/UKBB_40616/UKBB_test/4DSegment2.0_test_motion_final"
setwd(base.dir)
l <- substr(list.dirs(recursive=F),3,11)

ErrDataFrame<-vector(mode = "list",length=length(l))
EllDataFrame<-vector(mode = "list",length=length(l))
EccDataFrame<-vector(mode = "list",length=length(l))
coln<-0
np<-length(nPoints)#50656
Sradial_smooth<- matrix(0, nrow=np, ncol=50)
Scirc_smooth<- matrix(0, nrow=np, ncol=50)
Slong_smooth<- matrix(0, nrow=np, ncol=50)
for (ph in 1:50){coln[ph]<-c(paste("phase",ph, sep="_"))}
colnames(Sradial_smooth)<- as.matrix(coln);colnames(Scirc_smooth)<- as.matrix(coln)
colnames(Slong_smooth)<- as.matrix(coln)


start.time <- Sys.time()
for (i in 1:length(l)){
  file <- as.matrix(Sys.glob(file.path(paste(base.dir, l[i],"motion", sep="/"),"*.txt")))
  files<-as.matrix(file[1:100,])
  p<- paste(base.dir, l[i], sep="/")
  setwd(p)
  
  # Generation of new function components -----------------------------------
  

  for (iF in 1:50){
    ## Step 1 - Call epi and endo, project orthogonally onto the unit variable and bind
    # Epicardium and endocardium meshes at end-diastole
    EDendo<- as.data.frame(fread(file = files[1,], header = FALSE, fill = TRUE))
    colnames(EDendo) <- c("x", "y", "z")
    EDepi<- as.data.frame(fread(file = files[1+50,], header = FALSE, fill = TRUE))
    colnames(EDepi) <- c("x", "y", "z")
    # project meshs orthogonally, perpendicular to the unit variable (1,1,1)
    EDepi.data <- t(apply(EDepi[,c(1,2,3)],MARGIN = 1,function(x) unitvar(x[1],x[2],x[3])))
    EDendo.data <- t(apply(EDendo[,c(1,2,3)],MARGIN = 1,function(x) unitvar(x[1],x[2],x[3])))
    ED.data<-as.data.frame(rbind(EDepi.data,EDendo.data))
    
    # Epicardium and endocardium meshes at end-systole
    ESendo<- as.data.frame(fread(file = files[iF,], header = FALSE, fill = TRUE))
    colnames(ESendo) <- c("x", "y", "z")
    ESepi<- as.data.frame(fread(file = files[iF+50,], header = FALSE, fill = TRUE))
    colnames(ESepi) <- c("x", "y", "z")
    # project meshs orthogonally, perpendicular to the unit variable (1,1,1)
    ESepi.data <- t(apply(ESepi[,c(1,2,3)],MARGIN = 1,function(x) unitvar(x[1],x[2],x[3])))
    ESendo.data <- t(apply(ESendo[,c(1,2,3)],MARGIN = 1,function(x) unitvar(x[1],x[2],x[3])))
    ES.data<-as.data.frame(rbind(ESepi.data,ESendo.data))
    
    ## Step 2 - Transform from cartesian to cylindrical coordinates (Optional) 
    ED.datan <- t(apply(ED.data[,c(1,2,3)],MARGIN = 1,function(x) cart2cylc(x[1],x[2],x[3])))
    ES.datan<- t(apply(ES.data[,c(1,2,3)],MARGIN = 1,function(x) cart2cylc(x[1],x[2],x[3])))
    ED_epi<-ED.datan[1:nrow(EDepi.data),];ED_endo<-(ED.datan[(nrow(EDepi.data)+1):nrow(ED.datan),])
    ES_epi<-ES.datan[1:nrow(ESepi.data),];ES_endo<-ES.datan[(nrow(ESepi.data)+1):nrow(ES.datan),]
    
    ## Step 3 - Compute 1% of all data points in LV and find the 1% knn in ED and ES surface get the knns
    sc_1<-round(1*nrow(ED_epi)/100)
    # find the sc_1 knn in surface for ED and ES
    nPoints <- 1:nrow(ED.datan)
    con_ed_epi<-get.knnx(ED_endo,ED_epi, k=sc_1)[[2]]
    con_es_epi<-get.knnx(ES_endo,ES_epi, k=sc_1)[[2]]
    con_ed_endo<-get.knnx(ED_epi,ED_endo, k=sc_1)[[2]]
    con_es_endo<-get.knnx(ES_epi,ES_endo, k=sc_1)[[2]]
    
    ## Step 4 - Compute radial, circumferential ad longitudinal strain
    Sr_epi<-as.matrix((rowSums(con_es_epi)-rowSums(con_ed_epi))/rowSums(con_ed_epi))
    Sr_endo<-as.matrix((rowSums(con_es_endo)-rowSums(con_ed_endo))/rowSums(con_ed_endo))
    Sr_epi<-as.matrix(0.5*(rowSums(con_es_epi)^2-rowSums(con_ed_epi)^2)/(rowSums(con_ed_epi)^2)) #Ferdian
    Sr_endo<-as.matrix(0.5*(rowSums(con_es_endo)^2-rowSums(con_ed_endo)^2)/(rowSums(con_ed_endo)^2)) #Ferdian
    Sr<-rbind(Sr_epi,Sr_endo)
    nPoints <- 1:nrow(ED.datan)
    Sradial_smooth[,iF]<-ksmooth(nPoints,Sr, "normal", bandwidth = 40, n.points = length(nPoints))$y
    
    con_epi_ed2<-get.knn(ED_epi[,2],k=sc_1)[[2]]
    con_epi_es2<-get.knn(ES_epi[,2],k=sc_1)[[2]]

    edepi2<-rowSums(con_epi_ed2);esepi2<-rowSums(con_epi_es2)
    Sc_epi<-as.matrix((esepi2 - edepi2) / edepi2)
    con_endo_ed2<-get.knn(ED_endo[,2],k=sc_1)[[2]]
    con_endo_es2<-get.knn(ES_endo[,2],k=sc_1)[[2]]
    edendo2<-rowSums(con_endo_ed2);esendo2<-rowSums(con_endo_es2)
    Sc_endo<-as.matrix((esendo2 - edendo2) / edendo2)
    Sc<-rbind(Sc_epi, Sc_endo)
    Scirc_smooth[,iF]<-ksmooth(nPoints,Sc, "normal", bandwidth = 40, n.points = length(nPoints))$y

    con_epi_ed<-get.knn(ED_epi[,3],k=sc_1)[[2]]
    con_epi_es<-get.knn(ES_epi[,3],k=sc_1)[[2]]
    edepi<-rowSums(con_epi_ed);esepi<-rowSums(con_epi_es)
    Sl_epi<-as.matrix((esepi - edepi) / edepi)
    con_endo_ed<-get.knn(ED_endo[,3],k=sc_1)[[2]]
    con_endo_es<-get.knn(ES_endo[,3],k=sc_1)[[2]]
    edendo<-rowSums(con_endo_ed);esendo<-rowSums(con_endo_es)
    Sl_endo<-as.matrix((esendo - edendo) / edendo)

    Sl<-rbind(Sl_epi,Sc_endo)
    Slong_smooth[,iF]<-ksmooth(nPoints,Sl, "normal", bandwidth = 40, n.points = length(nPoints))$y
    setTxtProgressBar(txtProgressBar(style=3),iF/50)
    
  }
  ErrDataFrame[[i]]<-Sradial_smooth
  EllDataFrame[[i]]<-Slong_smooth
  EccDataFrame[[i]]<-Scirc_smooth
  setTxtProgressBar(txtProgressBar(style=3),i/length(l))
  
  }
end.time <- Sys.time()
time.taken<-end.time - start.time
time.taken

# Save
Er<-matrix(0,nrow=10, ncol=50)
El<-matrix(0,nrow=10, ncol=50)
Ec<-matrix(0,nrow=10, ncol=50)

for (iE in 1:10){
  Er[iE,]<-colMeans(ErrDataFrame[[iE]], na.rm=TRUE)
  El[iE,]<-colMeans(EllDataFrame[[iE]], na.rm=TRUE)
  Ec[iE,]<-colMeans(EccDataFrame[[iE]], na.rm=TRUE)
}


setwd("~/cardiac/Experiments_of_Maria/3Dstrain_analysis/")

colnames(Err)<-as.matrix(coln)
rownames(Err)<-l
colnames(Ell)<-as.matrix(coln)
rownames(Ell)<-l
colnames(Ecc)<-as.matrix(coln)
rownames(Ecc)<-l
for (iE in 1:10){
Er[iE,]<-as.data.frame(ksmooth(Phases,Er[iE,], "normal", bandwidth = 4, n.points = 50), type="l")$y
Ec[iE,]<-as.data.frame(ksmooth(Phases,Ec[iE,], "normal", bandwidth = 4, n.points = 50), type="l")$y
El[iE,]<-as.data.frame(ksmooth(Phases,El[iE,], "normal", bandwidth = 4, n.points = 50), type="l")$y
}
write.csv(Ec,"Ecc_global_10.csv",col.names = TRUE, row.names = TRUE)
saveRDS(EccDataFrame,"Ecc_slices_10.rds")

write.table(Sradial_smooth,"Strain_radial.txt", row.names = F, col.names = T)
write.table(Scirc_smooth,"Strain_circ.txt", row.names = F, col.names = T)
write.table(Slong_smooth,"Strain_long.txt", row.names = F, col.names = T)

# END