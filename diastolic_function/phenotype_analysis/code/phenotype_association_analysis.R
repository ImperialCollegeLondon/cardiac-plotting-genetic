
install.packages("data.table")
install.packages("glmnet")
install.packages("BeSS")
install.packages("mctest")

library(data.table)
library(BeSS)
library(mctest)
library(glmnet)
# load data for multiple linear regression analysis
multidata <- read.table("multiple_datatable.txt", header = TRUE)

beta_ml<-matrix(0,ncol=30, nrow=11)
mat_pv<-matrix(0,ncol=30, nrow=11)
t_BH<-matrix(0,ncol=30,1)
rsq<-matrix(0,ncol=30,1)
conflist<-vector(mode="list",length=30)

iT<-1
for (iN in 12:41){
  cv<-lapply(colnames(data_na)[12], function(x) lm(formula(paste("`",x,"`","~.", sep="")),data=data_na))
  pval<-summary(cv[[1]])$coefficients[,"Pr(>|t|)"] # get p-values
  p.cor<-p.adjust(pval,method = "BH") # adjust using Benjamini - Hochberg procedure
  names(p.cor)<-NULL
  beta<-as.vector(t(coef(cv[[1]], complete=TRUE)))[-1] # get beta coefficient
  t_BH[iT]<-get_bh_threshold(pval, 0.05) # get BH threshold
  smcv<-summary(cv[[1]])
  rsq[iT]<-smcv$r.squared # rsquared
  conflist[iT]<-list(confint(cv[[1]],level=0.95)[-1,]) # confidence intervals
  
  beta_ml[,iT]<-beta
  mat_pv[,iT]<-p.cor[-1]
  iT<-iT+1
}
colnames(beta_ml)<-colnames(multidata)[12:41]
rownames(beta_ml)<-colnames(multidata)[1:11]

### Multivariate LASSO regresion analysis with stability selection for selecting the non-imaging phenotypes 


data_pheno <- read.table("pheno_datatable.txt", header = TRUE)# load only imaging phenotype data

data_ebic<-as.matrix(data_pheno)

# using GPDAS algorithm to select the optimal k
# PDSRll
fit.seqll <- bess(data_ebic[,-18], data_ebic[,18], method="sequential", epsilon = 0)
# PDSRrr
fit.seqrr <- bess(data_ebic[,-19], data_ebic[,19], method="sequential", epsilon = 0)
# LAVmaxi
fit.seqlav <- bess(data_ebic[,-34], data_ebic[,34], method="sequential", epsilon = 0)

K.opt.ebic.ll <- which.min(fit.seqll$EBIC)
K.opt.ebic.rr <- which.min(fit.seqrr$EBIC)
K.opt.ebic.lav <- which.min(fit.seqlav$EBIC)

K.opt.ebic.ll
K.opt.ebic.rr
K.opt.ebic.lav

# PDSRll
fit.one.ll <- bess.one(data_ebic[,-18], data_ebic[,18], s = K.opt.ebic.ll, family = "gaussian")
bm.one.ll <- fit.one.ll$bestmodel
#PDSRrr
fit.one.rr <- bess.one(data_ebic[,-19], data_ebic[,19], s = K.opt.ebic.rr, family = "gaussian")
bm.one.rr <- fit.one.rr$bestmodel
#LAVmaxi
fit.one.lav <- bess.one(data_ebic[,-34], data_ebic[,34], s = K.opt.ebic.lav, family = "gaussian")
bm.one.lav <- fit.one.lav$bestmodel

pheno_long<-names(bm.one.ll$coefficients)[-1]
pheno_radial<-names(bm.one.rr$coefficients)[-1]
pheno_lav<-names(bm.one.lav$coefficients)[-1]
pheno_all<-cbind(pheno_long,pheno_radial,pheno_lav)

pos_pheno<-match(rownames(pheno_all),colnames(pheno)) # order the position of variables selected


multivar_data <- read.table("multivar_datatable.txt", header = TRUE) # load the whole dataset
multivar_data_train <- read.table("multivar_train_datatable.txt", header = TRUE) # load the training dataset
multivar_data_test <- read.table("multivar_test_datatable.txt", header = TRUE) # load the test dataset
colnames(multivar_data_train)<-colnames(multivar_data)
covar <- read.table("cov.txt", header = TRUE) # the position of all non-imaging covariates
pos_pheno <- read.table("fit.txt", header = TRUE) # the position of imaging covariates
## load data for training and for analysis
## covar: position of the covariates to bind with position of phenotypes for analysis
position_stab<-rbind(covar,pos_pheno)

## Final check for collinearity using the selected variables

model<-lm(`PDSRll (s-1)`~., data=as.data.frame(multivar_data[,position_stab[,1]]))
imcdiag(model,method="VIF", vif=5) # 0 if collinearity is not detected by this test

# Apply LASSO regression


# cv.glmnet to train for the lambda parameter
data.train<-as.matrix(multivar_data_train[,position_stab[,1]])
data.train<-na.omit(data.train)
colnames(data.train)
lambda_min<-matrix(0,ncol = 1, nrow = ncol(data.train))
for (iT in 1:ncol(data.train)){
  if (iT==2){ # for the logistic regression
    cv<-cv.glmnet(data.train[,-iT],data.train[,iT],nfolds = 10, family="binomial", alpha=1)$lambda.min
  } else {
    cv<-cv.glmnet(data.train[,-iT],data.train[,iT],nfolds = 10, alpha=1)$lambda.min
  }
  lambda_min[iT]<-round(cv,5)
}

# glmnet test
data_selected<-as.matrix(multivar_data_test[,position_stab[,1]])
data_selected<-na.omit(data_selected)
beta_gl<-matrix(0,ncol = ncol(data_selected), nrow = ncol(data_selected)-1)
for (iS in 1:ncol(data_selected)){
  if (iS==2){ # for the logistic regression
    cv<-glmnet(data_selected[,-iS],data_selected[,iS], family="binomial", lambda = lambda_min[iS], alpha=1)
    beta<-as.vector(t(coef(cv)))
    beta<-as.matrix(beta[-1])
    
  } else {
    cv<-glmnet(data_selected[,-iS],data_selected[,iS],lambda = lambda_min[iS], alpha = 1)
    beta<-as.vector(t(coef(cv)))
    beta<-as.matrix(beta[-1])
  }
  beta_gl[,iS]<-beta[,1]
  
}

colnames(beta_gl)<-colnames(data_selected)
beta_gl<-as.data.frame(beta_gl)
multivar_beta<-matrix(0,nrow = ncol(beta_gl),ncol=ncol(beta_gl))
multivar_beta<-as.data.frame(multivar_beta)
write.table(multivar_beta, "multivar_beta.txt", row.names = T, col.names = T) # save beta coefficients

# END
