############################################################################
### Classification                                                       ###
######################################                                   ###
### Description:                                                         ###
### This R script contains commands that build and evaluate classifiers. ###
############################################################################

# Load the needed libraries
require2("e1071") # naive bayes
require2("caret") # for creating folds
require2("randomForest") # random forest classification technique
require2("beepr") # useful for signaling
require2("DMwR")

# Prepare the data
set.seed(300) # set seed for reproducibility
projdoc_df <- as.data.frame(projdoc)
names(projdoc_df) <- paste("LSAdim",1:ncol(projdoc_df), sep="_")
projdoc_df$taskOrNot <- labels[as.numeric(rownames(projdoc_df))]
projdoc_df$taskOrNot <- factor(projdoc_df$taskOrNot, levels=c(0,1), labels=c("nontask","task"))
mydtm_matwithLabels$taskOrNot <- as.factor(mydtm_matwithLabels$taskOrNot)

# Prepare the data for validation (10 times 10 fold cross-validation)
fold10by10<-createMultiFolds(projdoc_df$taskOrNot,k=10, times=10)
fold10by10orig<-createMultiFolds(mydtm_matwithLabels$taskOrNot, k=10, times=10)

# F-measure
fmeasurefxn<-function(x,y){
  return(2*((x*y)/(x+y)))
}

# Matrices where the results will be saved.
Fmeasure_lsa<-matrix(0, nrow=100, ncol=3)
baccu_lsa<-matrix(0, nrow=100, ncol=3)
j=1

# Run classification using the LSA reduced feature set
ptm <- proc.time()
for(i in fold10by10){
  train<-projdoc_df[i,]
  last<-ncol(train)
  zerocol<-which(apply(train[,-last], 2, var)!=0)
  train<-train[,c(zerocol, last)]
  test<-projdoc_df[-i,]
  newData<-SMOTE(taskOrNot~., data= train, perc.over=300, perc.under=200, k=10)
  if(table(test$taskOrNot)[2]!=0){
  SVMlsa<-svm(taskOrNot~., data=newData,type="C-classification",kernel="linear", cost=100, scale=TRUE,class.weights=c("task"=10, "nontask"=1))
  NBlsa<-naiveBayes(taskOrNot~., data=newData, laplace=1)
  RFlsa<-randomForest(taskOrNot~., data=newData, ntree=1000,classwt=c("nontask"=.9,"task"=.1), strata=train$taskOrNot)
  cont_SVM<-table(predict(SVMlsa, test), test$taskOrNot)
  cont_NB<-table(predict(NBlsa, test), test$taskOrNot)
  cont_RF<-table(predict(RFlsa, test), test$taskOrNot)
  baccuSVM<-((cont_SVM[2,2]/sum(cont_SVM[,2]))+(cont_SVM[1,1]/sum(cont_SVM[,1])))/2
  baccuNB<-((cont_NB[2,2]/sum(cont_NB[,2]))+(cont_NB[1,1]/sum(cont_NB[,1])))/2
  baccuRF<-((cont_RF[2,2]/sum(cont_RF[,2]))+(cont_RF[1,1]/sum(cont_RF[,1])))/2
  fSVM<-fmeasurefxn(cont_SVM[2,2]/sum(cont_SVM[,2]),cont_SVM[2,2]/sum(cont_SVM[2,]))
  fNB<-fmeasurefxn(cont_NB[2,2]/sum(cont_NB[,2]),cont_NB[2,2]/sum(cont_NB[2,]))
  fRF<-fmeasurefxn(cont_RF[2,2]/sum(cont_RF[,2]),cont_RF[2,2]/sum(cont_RF[2,]))
  baccu_lsa[j,]<-c(baccuSVM,baccuNB, baccuRF)
  Fmeasure_lsa[j,]<-c(fSVM,fNB,fRF)} else {print(i)}
  j=j+1
}
proc.time() - ptm
beep("shotgun")

# Results summary
results_lsa<-matrix(rep(0,6),2,3, dimnames=list(c("Fmeasure","Balanced Accuracy"),c("SVM", "NAIVE BAYES", "RANDOM FOREST")))
results_lsa[1,]<-apply(Fmeasure_lsa,2,mean, na.rm=TRUE)
results_lsa[2,]<-apply(baccu_lsa,2,mean)
print(results_lsa)

# Matrices where the results will be saved.
Fmeasure_orig<-matrix(0, nrow=100, ncol=3)
baccu_orig<-matrix(0, nrow=100, ncol=3)

# Run classification on the original feature set
j=1
for(k in fold10by10orig){
  train<-mydtm_matwithLabels[k,]
  last<-ncol(train)
  zerocol<-which(apply(train[,-last], 2, var)!=0)
  train<-train[,c(zerocol,last)]
  test<-mydtm_matwithLabels[-k,]
  newData<-SMOTE(taskOrNot~., data= train, perc.over=300, perc.under=200, k=10)
  newData<-newData[,c(which(apply(newData[,-ncol(newData)], 2, var)!=0),ncol(newData))]
  if(table(test$taskOrNot)[2]!=0){
  SVMorig<-svm(taskOrNot~., data=newData,type="C-classification",kernel="linear", cost=100, scale=TRUE,class.weights=c("task"=10, "nontask"=1))
  NBorig<-naiveBayes(taskOrNot~., data=newData, laplace=1)
  RForig<-randomForest(taskOrNot~., data=newData, ntree=1000,classwt=c("task"=10,"nontask"=1), strata=train$taskOrNot)
  cont_SVM<-table(predict(SVMorig, test), test$taskOrNot)
  cont_NB<-table(predict(NBorig, test), test$taskOrNot)
  cont_RF<-table(predict(RForig, test), test$taskOrNot)
  baccuSVM<-((cont_SVM[2,2]/sum(cont_SVM[,2]))+(cont_SVM[1,1]/sum(cont_SVM[,1])))/2
  baccuNB<-((cont_NB[2,2]/sum(cont_NB[,2]))+(cont_NB[1,1]/sum(cont_NB[,1])))/2
  baccuRF<-((cont_RF[2,2]/sum(cont_RF[,2]))+(cont_RF[1,1]/sum(cont_RF[,1])))/2
  baccu_orig[j,]<-c(baccuSVM,baccuNB, baccuRF)
  fSVM<-fmeasurefxn(cont_SVM[2,2]/sum(cont_SVM[,2]),cont_SVM[2,2]/sum(cont_SVM[2,]))
  fNB<-fmeasurefxn(cont_NB[2,2]/sum(cont_NB[,2]),cont_NB[2,2]/sum(cont_NB[2,]))
  fRF<-fmeasurefxn(cont_RF[2,2]/sum(cont_RF[,2]),cont_RF[2,2]/sum(cont_RF[2,])) 
  Fmeasure_orig[j,]<-c(fSVM,fNB,fRF)} else {print(k)}
  j=j+1
}
beep("mario")

results_orig<-matrix(rep(0,6),2,3, dimnames=list(c("Fmeasure","Balanced Accuracy"),c("SVM", "NAIVE BAYES", "RANDOM FOREST")))
results_orig[1,]<-apply(Fmeasure_orig,2,mean, na.rm=TRUE)
results_orig[2,]<-apply(baccu_orig,2,mean)
print(results_orig)


# Visualize the results for Balanced Accuracy
baccu_combine<-rbind(baccu_lsa,baccu_orig)
baccu_combine<-cbind(baccu_combine,rep(c("LSA","Original"),each=100))
colnames(baccu_combine)<-c("SVM","Naive Bayes","Random Forest","FS")
baccu_combine<-as.data.frame(baccu_combine)
bamelt<-melt(baccu_combine, id.vars=c("FS"))
bamelt$value<-as.numeric(bamelt$value)

g<-ggplot(bamelt, aes(y=value, x=variable ))
g + geom_boxplot(aes(color=FS)) +facet_wrap(~FS)+xlab("Classifier") + ylab("Balanced Accuracy")

# Visualize the results for F-measure
f_combine<-rbind(Fmeasure_lsa,Fmeasure_orig)
f_combine<-cbind(f_combine,rep(c("LSA","Original"),each=100))
colnames(f_combine)<-c("SVM","Naive Bayes","Random Forest","FS")
f_combine<-as.data.frame(f_combine)

fmelt<-melt(f_combine, id.vars=c("FS"))
fmelt$value<-as.numeric(fmelt$value)

gf<-ggplot(fmelt, aes(y=value, x=variable ))
gf + geom_boxplot(aes(color=FS)) +facet_wrap(~FS)+xlab("Classifier") + ylab("F-measure")
