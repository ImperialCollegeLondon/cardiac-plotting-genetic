TFCE <- function(h, A, NNmatrix, E=0.5, H=2, deltah=0.01){
  #h - input statistic map - N-dimensional vector, N = number of vertices in the mesh.
  #A - area associated to each vertex of the mesh - N-dimensional vector. 
  #NNmatrix - two columns matrix storing the mesh edges, each row contain the vertex ID of the nodes at their extremes.
  #E, H - TFCE parameters
  #deltah - finite dh of TFCE integral
  
  #check input data
  if(length(h)!=length(A)) stop('Lengths of h and A are different.')
  
  nPoints <- length(h)
  #the number of mesh vertexes 
  
  TFCEscore <- array(0, dim = nPoints)
  #this array will contain the TFCE score for each vertex of the mesh
  
  seqT <- c()
  if(min(h)<0) seqT <- c(seqT,-seq(deltah,abs(min(h)),deltah))
  if(max(h)>0) seqT <- c(seqT,seq(deltah,abs(max(h)),deltah))
  # compute the range of h-statistic to study: if the minimum value of the input statistic is positive the
  # range to examine will be (0, max(h)); if the maximum value of the input statistic is negative the range 
  # under examination will be (min(h), 0).
  
  for(iClusters in 1:length(seqT)){
    
    #for all the the possible thresholds computed the algorithm computes 
    #all the possible clusters and assigns to the points that belongs to them a 
    #a TFCE score weighted for the cluster extend
    
    thr <- seqT[iClusters]
    #cluster threshold
    
    scores <- array(0, dim = nPoints)
    
    if(thr>0) origI <- which(h >= thr)
    if(thr<=0) origI <- which(h <= thr)
    #compute the list of h statistic that could be contained in a cluster
    #with forming threshold thr
    
    if(length(origI)>1){
      
      firstRowok <-  which(NNmatrix[,1] %in% origI)
      #row numbers that that have in the first column a origI value
      
      rows2Keep <- firstRowok[NNmatrix[firstRowok,2] %in% origI]
      #rows that have also a origI value int the second columns
      rm(origI)
      
      if(length(rows2Keep)>1){ 
        
        g = graph_from_edgelist(NNmatrix[rows2Keep,], directed = FALSE)
        ##compute the graph from them
        
        compo <- components(g)
        ##and extract the components
        
        memberships <- compo$membership
        ##for each vertex extract its membership
        
        nCluster <- which(compo$csize>1) 
        # cluster indexes of clusters with dimension > 1
        
        for(i in 1:length(nCluster)){
          clusterIndexes <- which(memberships == nCluster[i])
          ## indexes of the vertexe of the cluster with label nCluster[i]
          TFCEscore[clusterIndexes] <- TFCEscore[clusterIndexes] + (sum(A[clusterIndexes]))^E * abs(thr)^H * deltah
        }
      rm(g)  
      }
    }
  }
  
  return(sign(h) * TFCEscore) # re-assign the correct sign
  
}
