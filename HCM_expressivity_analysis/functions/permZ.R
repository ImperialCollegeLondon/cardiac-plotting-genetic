permZ<-function(Y, visit, extract,A, NNmatrix,nPermutations){
  library(doParallel)
  library(parallel)
  library(foreach)
  registerDoParallel(detectCores())
  #detect the number of cores available and use them
  library(igraph)
  nPerm<-nPermutations/10
  permresP<-vector(mode = "list", length=nPerm)
  for (iR in 1:nPerm){
    resP <- foreach(iF=1:10, .combine=rbind)%dopar%{
      #deltah=0.01
      E=0.5
      H=2
      Yper <-  Y[sample(1:nrow(Y)),]
      pvals<-matrix(0, nrow=ncol(Y))
      zstat<-matrix(0, nrow=ncol(Y))
      for (iN in 1: ncol(Y)){
        my_data<-as.data.frame(cbind(Yper[,iN],visit))
        colnames(my_data)<-c("Yper","visit")
        resw<-wilcox.test(Yper~visit, data=my_data,paired=T)#exact=F)#
        pvals[iN,1]<-resw$p.value; 
        zstat[iN,1]<-qnorm(resw$p.value/2); 
      }
      computed <- matrix(0, ncol=ncol(Y), 1)
      
      computed <- TFCE(h=round(zstat[,1],2), A=A, NNmatrix=NNmatrix, E=E, H=H)
      return(computed)
    }
    permresP[[iR]]<-resP
    
  }
  
  library(plyr)
  # res<-rbind.fill(permresP)
  resP<-ldply(permresP,rbind)
  
  closeAllConnections()
  significance <- matrix(0, ncol=2, nrow=ncol(Y))
  
  E=0.5 
  H=2
  pvals<-matrix(0, nrow=ncol(Y))
  zstat<-matrix(0, nrow=ncol(Y))
  for (iN in 1: ncol(Y)){
    my_data<-as.data.frame(cbind(Y[,iN],visit))
    colnames(my_data)<-c("Y","visit")
    resw<-wilcox.test(Y~visit, data=my_data,paired=T)#exact=F)
    pvals[iN,1]<-resw$p.value; 
    zstat[iN,1]<-qnorm(resw$p.value/2); 
  }
  tfceScores <- list()
  #compute the residual matrix of Z
  iEx<-1
  tfceScores[[1]] <- TFCE(h=round(zstat[,1],2), A=A, NNmatrix=NNmatrix, E=E, H=H)
  
  TFCEmatrix <- resP[seq(1,nrow(resP), by=1),]
  
  minimum = sort(apply(TFCEmatrix,1,min))
  if (length(which(minimum<0)>0)) { 
    thrMin = minimum[ceiling(0.05*nrow(TFCEmatrix))]
  } else {
    thrMin = 0
  } 
  
  maximum = sort(apply(TFCEmatrix,1,max))
  if (length(which(maximum>0)>0)) {
    thrMax = maximum[floor(0.95*nrow(TFCEmatrix))]
  }else{
    thrMax = 0
  } 
  # nPermutations<-1000
  for(a in 1:ncol(Y)){
    if(tfceScores[[iEx]][a]>=0){
      significance[a,1+(iEx-1)*2]  <- length(which(TFCEmatrix[,a]> tfceScores[[iEx]][a]))/nPermutations
      if(tfceScores[[iEx]][a] > thrMax) significance[a,2+(iEx-1)*2] = 1
    }
    
    if(tfceScores[[iEx]][a]<0){
      significance[a,1+(iEx-1)*2]  <- length(which(TFCEmatrix[,a]< tfceScores[[iEx]][a]))/nPermutations
      if(tfceScores[[iEx]][a] < thrMin) significance[a,2+(iEx-1)*2] = 1
    }
    
    if(significance[a,1+(iEx-1)*2]==0) significance[a,1+(iEx-1)*2] <- 1/nPermutations #minimum pvalue achievable.
  }
  
  
  length(which(significance[,1]<0.05))
  TFCEresults = list("pvalues" = significance, "TFCEmatrix" = TFCEmatrix, "tfceScores" = tfceScores)
  rm(significance)
  rm(TFCEmatrix)
  rm(tfceScores)
  
  return(TFCEresults)    
}
