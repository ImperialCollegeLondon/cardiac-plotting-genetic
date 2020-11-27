# The following is a script used to generate a file neopheno.txt with additional coloumns with strain from tensors resolved into its long, circ, radial strain
#1. First compile a list of codes you want the neopheno file generated
#set the directory in :
#this is the parent directory of the timcode folders.
#I suggest trialling this on a test folder that is a copy first
rm(list = ls(all = TRUE)) 

install.packages("gdata")
install.packages("data.table")
install.packages("FNN")
install.packages("Mass")
install.packages("purrr")
install.packages("dplyr")

library(gdata)
library(data.table)
library(FNN)
library(purrr)
library(Mass)
library(dplyr)

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

#compute strain
etens <- function(m_cyl) { 
  # 25/5/16 modified this function to be able to use solve instead of ginv as I do not think that this is the same function 
  # as an inverse of a matrix. I have therefore modified the input to etens to be a matrix: so m_cyl is a matrix [6,4] such 
  # that the first three rows are the undeformed coordinates of the 4 coloumns (4 neighbours), rows 4:6 are the deformed coordinates - all cylindrical
  
  # This is the previous comment:
  #   m_cyl is a 1x6 vector of cyl coords in r,teta,z fashion along coloumns, first of undeformed and then deformed
  #   a <- matrix(data = c(m_cyl[1],m_cyl[2],m_cyl[3]),nrow = 3,ncol = 1) #undeformed
  #   b <- matrix(data = c(m_cyl[4],m_cyl[5],m_cyl[6]),nrow = 3,ncol = 1) #deformed
  library(MASS)
  b <- m_cyl[1:3,] # undeformed
  a <- m_cyl[4:6,] # deformed
  X <- b %*% t(b)
  X <- ginv(X,tol = 1e-21)
  FF <- a %*% (t(b) %*% X)
  ident <- matrix(data = c(1,0,0,0,1,0,0,0,1),nrow = 3,ncol = 3)
  GG <- ginv(FF,tol = 1e-21) # Moghaddam
  l1l2<-function(mc1,mc2){
    a_n <- mc2 # Moghaddam correction
    b_n <- mc1 # Moghaddam correction
    X_n <- b_n %*% t(b_n) # Moghaddam correction
    X_n <- ginv(X_n,tol = 1e-21) # Moghaddam correction
    FF_n<- a_n %*% (t(b_n) %*% X_n) # Moghaddam correction
    GG_n <- ginv(FF_n,tol = 1e-21) # Moghaddam correction
    lambda_F<-sqrt(abs(eigen(FF_n)$values)) # Moghaddam correction
    lambda_G<-sqrt(abs(eigen(GG_n)$values)) # Moghaddam correction
    return(rbind(lambda_F,lambda_G))
  }
  # # lambda_3<-(1/(eigenMapMatMult(lambda_F[1],lambda_F[2])))-1 #strain at Err
  l1<-l1l2(m_cyl[c(1:2),],m_cyl[c(4:5),]) # Calculate the eigenvalues of the 2D deformation (x,y) in the principal direction
  E_L <- ((t(FF) %*% FF)-ident)*0.5 # Langrangian strain
  E_E <- 0.5*(ident-(ginv(FF %*% t(FF)))) # Eulerian strain
  E_L[1,1]<-((1/((l1[1,1]*l1[1,2])))-(1/((l1[2,1]*l1[2,2]))))*0.5 # Moghaddam correction to correct radial principal strain
  E_L <- t(as.data.frame(as.vector(E_L)))
  E_E <- t(as.data.frame(as.vector(E_E)))
  return(cbind(E_L,E_E))
}
# 
base.dir<- "~/cardiac/UKBB_40616/UKBB_test/4DSegment2.0_test_motion_final"
setwd(base.dir)
l <- substr(list.dirs(recursive=F),3,11)

# create foldres with the ID names 
setwd("~/cardiac/Experiments_of_Maria/3Dstrain_analysis/")
for (i in 1:length(l)){
  dir.create(l[i])}

setwd(base.dir)

start.time <- Sys.time()
for (i in 1:length(l)){
  file <- as.matrix(Sys.glob(file.path(paste(base.dir, l[i],"motion", sep="/"),"*.txt")))
  files<-as.matrix(file[1:100,])
  p<- paste(base.dir, l[i], sep="/")
  setwd(p)
  
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
    colnames(ED.data) <- c("x", "y", "z")
    
    # Epicardium and endocardium meshes at end-systole
    ESendo<- as.data.frame(fread(file = files[iF,], header = FALSE, fill = TRUE))
    colnames(ESendo) <- c("x", "y", "z")
    ESepi<- as.data.frame(fread(file = files[iF+50,], header = FALSE, fill = TRUE))
    colnames(ESepi) <- c("x", "y", "z")
    # project meshs orthogonally, perpendicular to the unit variable (1,1,1)
    ESepi.data <- t(apply(ESepi[,c(1,2,3)],MARGIN = 1,function(x) unitvar(x[1],x[2],x[3])))
    ESendo.data <- t(apply(ESendo[,c(1,2,3)],MARGIN = 1,function(x) unitvar(x[1],x[2],x[3])))
    ES.data<-as.data.frame(rbind(ESepi.data,ESendo.data))
    colnames(ES.data) <- c("x", "y", "z")
    ## Step 2 - Find ~50 knn in epi that match with endo for both ED and ES
    
    con_ed_epi<-get.knnx(EDendo,EDepi, k=50)[[1]]
    con_es_epi<-get.knnx(ESendo,ESepi, k=50)[[1]]
    
    ## Step 3 - Compute middle surface
    
    mid_surf_es<-vector(mode = "list",length=50)
    mid_surf_ed<-vector(mode = "list",length=50)
    for (iEx in 1:50){
      mid_surf_es[[iEx]]<-(ESendo[con_es_epi[,iEx],]+ESepi)/2 
      mid_surf_ed[[iEx]]<-(EDendo[con_ed_epi[,iEx],]+EDepi)/2 
    }
    # combine the elements in the vector into a single value
    mid_es<-reduce(mid_surf_es,`+`)/length(mid_surf_es)
    mid_ed<-reduce(mid_surf_ed,`+`)/length(mid_surf_ed)
    
    ED_all<-as.data.frame(rbind(ED.data,mid_ed)) # combine with epi nad endo
    ES_all<-as.data.frame(rbind(ES.data,mid_es)) # combine with epi nad endo
    
    dir<- paste(l[i],"middle_atlas", sep="/")
    setwd(dir)
    
    ## Step 4 - Transform from cartesian to cylindrical coordinates (Optional) 
    ED.datan <- t(apply(ED_all[,c(1,2,3)],MARGIN = 1,function(x) cart2cylc(x[1],x[2],x[3])))
    ES.datan<- t(apply(ES_all[,c(1,2,3)],MARGIN = 1,function(x) cart2cylc(x[1],x[2],x[3])))
    prelatch <- as.matrix(cbind(ED.datan,ES.datan))
    colnames(prelatch) <- c("EDx","EDy","EDz","ESx","ESy","ESz")
    
    ED_epi<-ED.datan[1:nrow(EDepi),];ED_endo<-(ED.datan[(nrow(EDepi)+1):nrow(ED.data),])
    ES_epi<-ES.datan[1:nrow(ESepi),];ES_endo<-ES.datan[(nrow(ESepi)+1):nrow(ES.data),]
    
    
    mid_edn<-ED.datan[(nrow(ED.data)+1):nrow(ED_all),]
    mid_esn<-ES.datan[(nrow(ES.data)+1):nrow(ES_all),]
    
    ## Step 5 - # Step 5 - Compute 1% of all data points in LV and find the 1% knn in ED and ES 
    #  surface get the knns only for the middle surface
    
    sc_1<-round(1*nrow(ED.datan)/100)
    nPoints <- 1:nrow(mid_edn)
    # find the sc_1 knn in middle surface for ED and ES
    con_ed<-get.knnx(ED.datan[1:nrow(ED.data),], mid_edn,k=sc_1)[[1]]
    con_es<-get.knnx(ES.datan[1:nrow(ES.data),], mid_esn,k=sc_1)[[1]]
    # create difference mxlist
    mxlist<-vector(mode="list",length=nrow(mid_ed))
    for (iN in 1:nrow(mid_ed))
    {
      diff_ed<-matrix(0, ncol=3, nrow=sc_1);diff_es<-matrix(0, ncol=3, nrow=sc_1)
      for (jN in i:sc_1){
        diff_ed[jN,]<-unlist(abs(mid_edn[iN,]-ED.datan[con_es[iN,jN],]))
        diff_es[jN,]<-unlist(abs(mid_esn[iN,]-ES.datan[con_es[iN,jN],]))
      }
      mxlist[iN]<-list(t(as.data.frame(cbind(diff_ed, diff_es))))
    }
    
    ######
    # get only the mid surface with the cartesian coordinates
    attach_ed <- EDepi
    colnames(attach_ed) <- c("EDx","EDy","EDz")
    attach_es <- ESepi
    colnames(attach_es) <- c("ESx","ESy","ESz")
    
    ## Strain
    ## Step 6 - create an mxlist with nPoint=nrow(mid_surf) and 1x6rows-1:sc_2columns for each point
    
    attach_str<-t(sapply(mxlist,etens))
    rownames(attach_str)<-NULL
    colnames(attach_str) <- c("ELRR","ELRT","ELRZ","ELTR","ELTT","ELTZ","ELZR","ELZT","ELZZ","EERR","EERT","EERZ","EETR","EETT","EETZ","EEZR","EEZT","EEZZ")
    
    attach_str_new<-matrix(0,ncol = ncol(attach_str), nrow = nrow(attach_str))
    for (iN in 1:ncol(attach_str)){
      attach_str_new[,iN]<-ksmooth(nPoints,attach_str[,iN], "normal", bandwidth = 100, n.points = length(nPoints))$y
      
    }
    colnames(attach_str_new)<-colnames(attach_str)
    #combine and write
    neopheno <- cbind(prelatch[(nrow(ES.data)+1):nrow(ES_all),],attach_ed,attach_es,attach_str_new)
    write.table(x = neopheno,paste("neopheno_",iF,".txt",sep = ""),row.names = F,col.names = T)
    
  }
  end.time <- Sys.time()
  time.taken<-end.time - start.time
  time.taken
}

# END