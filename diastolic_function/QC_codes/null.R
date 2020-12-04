null_delete <- function(trait) { 
  trait[which(trait==0)] <- NA
  return(trait)
}
