#################################################################################
rm(list = ls(all = TRUE))  
#start with an empty environment
install.packages(synthpop)

library(synthpop)

#SET THE PATH OF THE WORKING DIRECTORY
setwd("~/cardiac/Experiments_of_Maria/Mass_univariate_analysis/")

ods<-readRDS("data/WTedLVclinicaldata.rds")
ods<-ods[,c(1:7)]

my.seed<-17914709

Sample<- sample_n(ods, size=291, replace=T, prob=0.9)
sds.default <- syn(Sample[,c(1:6)], seed=my.seed) # create synthetic for baseline variables
syn1<-sds.default$syn


compare(sds.default,Sample, var="Age")
compare(sds.default,Sample, var="Sex")
compare(sds.default,Sample, var="Race")
compare(sds.default,Sample, var="BSA")
compare(sds.default,Sample, var="SBP")
compare(sds.default,Sample, var="DBP")

# save