#####################################################################################
############# Functions for data cleaning
M2_removal <- function(trait) {
  
  mad <- mad(trait, na.rm=TRUE)
  subjIDs <- c()
  IDs <- 1:length(trait)
  nums <- max(c(0.0001*sum(!is.na(trait)), 20))
  #calculate number of expected values in 0.5*mad neighbourhood (arbitrary choice!)
  
  count <- 0 # count variable: if for 100 observations in a row sufficient nearby observations are identified: stop
  
  sort.trait <- sort(trait)
  order.trait <- order(trait)
  
  for (i in 1:length(trait)) { ##?? modify??
    bounds <- c(sort.trait[i]-0.5*mad, sort.trait[i]+0.5*mad)
    neigh <- sum((bounds[1] < trait) & (bounds[2] > trait), na.rm=TRUE)
    count <- count + 1
    if (neigh<=nums) {
      subjIDs <- c(subjIDs, IDs[order.trait[i]])
      count <- 0
    }
    if (count > 100) {
      break
    }
  }
  
  sort.traitD <- sort(trait, decreasing=TRUE)
  order.traitD <- order(trait, decreasing=TRUE)
  count <- 0
  
  for (i in 1:length(trait)) { ##?? modify
    bounds <- c(sort.traitD[i]-0.5*mad, sort.traitD[i]+0.5*mad)
    neigh <- sum((bounds[1] < trait) & (bounds[2] > trait), na.rm=TRUE)
    count <- count + 1
    if (neigh<=nums) {
      subjIDs <- c(subjIDs, IDs[order.traitD[i]])
      count <- 0
    }
    if (count > 100) {
      break
    }
  }
  if (length(trait) > 0) {
    trait[which(IDs %in% subjIDs)] <- NA
  }
  
  return(trait)
  
}
