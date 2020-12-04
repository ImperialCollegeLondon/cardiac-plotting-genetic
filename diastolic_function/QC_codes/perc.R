perc_delete <- function(trait) {
  
  low_quan <- quantile(trait, na.rm=T, probs=0.001)
  up_quan <- quantile(trait, na.rm=T, probs=0.999)
  
  trait[which(trait < low_quan | trait > up_quan)] <- NA
  
  return(trait)
  
}
BSA_p<-perc_delete(BSA)
SBP_p<-perc_delete(SBP)
DBP_p<-perc_delete(DBP)
Sex_p<-perc_delete(Sex)
Smoking_p<-perc_delete(Smoking)
Assc_p<-perc_delete(Assessment_centre_2)
