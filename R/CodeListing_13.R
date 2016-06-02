require2("e1071")
require2("caret")
require2("randomForest")
require2("beepr")

set.seed(300)
projdoc_df<-as.data.frame(projdoc)
names(projdoc_df)<-paste("LSAdim",1:ncol(projdoc_df), sep="_")
projdoc_df$taskOrNot<-labels[as.numeric(rownames(projdoc_df))]
projdoc_df$taskOrNot<-as.factor(projdoc_df$taskOrNot)
mydtm_matwithLabels$taskOrNot<-as.factor(mydtm_matwithLabels$taskOrNot)
fold10by10<-createMultiFolds(projdoc_df$taskOrNot,k=10, times=10)
fold10by10orig<-createMultiFolds(mydtm_matwithLabels$taskOrNot, k=10, times=10)

fmeasurefxn<-function(x,y){
  return(2*((x*y)/(x+y)))
}

Fmeasure_lsa<-matrix(0, nrow=100, ncol=3)
baccu_lsa<-matrix(0, nrow=100, ncol=3)
j=1

ptm <- proc.time()
for(i in fold10by10){
  train<-projdoc_df[i,]
  last<-ncol(train)
  zerocol<-which(colSums(train[,-ncol(train)])>0)
  train<-train[,c(zerocol, last)]
  #train$taskOrNot<-projdoc_df$taskOrNot
  test<-projdoc_df[-i,]
  SVMlsa<-svm(taskOrNot~., data=train,type="C-classification",kernel="radial", cost=100,gamma=30, scale=TRUE,class.weights=c("1"=10, "0"=1))
  NBlsa<-naiveBayes(taskOrNot~., data=train, laplace=1)
  RFlsa<-randomForest(taskOrNot~., data=train, ntree=1000,classwt=c("0"=.9,"1"=.1), strata=train$taskOrNot)
  #obj<-tune(svm,taskOrNot~., data=projdoc_df,type="C-classification",kernel="linear",ranges=list(cost=10^seq(-2,2, 0.1)), class.weights=c("1"=10,"0"=1), tunecontrol=tune.control(sampling="boot"))
  #beep("mario")
  #print(table(predict(SVM, test), test$taskOrNot))
  cont_SVM<-table(predict(SVMlsa, test), test$taskOrNot)
  cont_NB<-table(predict(NBlsa, test), test$taskOrNot)
  cont_RF<-table(predict(RFlsa, test), test$taskOrNot)
  #print(cont_SVM[2,2]/sum(cont_SVM[,2]))
  baccuSVM<-((cont_SVM[2,2]/sum(cont_SVM[,2]))+(cont_SVM[1,1]/sum(cont_SVM[,1])))/2
  baccuNB<-((cont_NB[2,2]/sum(cont_NB[,2]))+(cont_NB[1,1]/sum(cont_NB[,1])))/2
  baccuRF<-((cont_RF[2,2]/sum(cont_RF[,2]))+(cont_RF[1,1]/sum(cont_RF[,1])))/2
  fSVM<-fmeasurefxn(cont_SVM[2,2]/sum(cont_SVM[,2]),cont_SVM[2,2]/sum(cont_SVM[2,]))
  #print(fSVM)
  fNB<-fmeasurefxn(cont_NB[2,2]/sum(cont_NB[,2]),cont_NB[2,2]/sum(cont_NB[2,]))
  fRF<-fmeasurefxn(cont_RF[2,2]/sum(cont_RF[,2]),cont_RF[2,2]/sum(cont_RF[2,]))
  baccu_lsa[j,]<-c(baccuSVM,baccuNB, baccuRF)
  Fmeasure_lsa[j,]<-c(fSVM,fNB,fRF)
  j=j+1
  #print(table(predict(SVM, test), test$taskOrNot))
  
}
proc.time() - ptm
beep("shotgun")
#Fmeasure_lsa<-Fmeasure_lsa[complete.cases(Fmeasure_lsa),]

apply(Fmeasure_lsa,2,max, na.rm=TRUE)
apply(Fmeasure_lsa,2,mean, na.rm=TRUE)
apply(baccu_lsa,2,max)
apply(baccu_lsa,2,mean)

#baccu_orig<-matrix(0, nrow=100, ncol=3)
Fmeasure_orig<-matrix(0, nrow=100, ncol=3)
baccu_orig<-matrix(0, nrow=100, ncol=3)
j=1
for(k in fold10by10orig){
  train<-mydtm_matwithLabels[k,]
  last<-ncol(train)
  zerocol<-which(colSums(train[,-ncol(train)])>0)
  train<-train[,c(zerocol,last)]
  test<-mydtm_matwithLabels[-k,]
  SVMorig<-svm(taskOrNot~., data=train,type="C-classification",kernel="radial", cost=100,gamma=30, scale=TRUE,class.weights=c("1"=10, "0"=1))
  NBorig<-naiveBayes(taskOrNot~., data=train, laplace=1)
  RForig<-randomForest(taskOrNot~., data=train, ntree=1000,classwt=c("1"=10,"0"=1), strata=train$taskOrNot)
  #print(table(predict(SVMorig, test), test$taskOrNot))
  cont_SVM<-table(predict(SVMorig, test), test$taskOrNot)
  cont_NB<-table(predict(NBorig, test), test$taskOrNot)
  cont_RF<-table(predict(RForig, test), test$taskOrNot)
  baccuSVM<-((cont_SVM[2,2]/sum(cont_SVM[,2]))+(cont_SVM[1,1]/sum(cont_SVM[,1])))/2
  baccuNB<-((cont_NB[2,2]/sum(cont_NB[,2]))+(cont_NB[1,1]/sum(cont_NB[,1])))/2
  baccuRF<-((cont_RF[2,2]/sum(cont_RF[,2]))+(cont_RF[1,1]/sum(cont_RF[,1])))/2
  baccu_orig[j,]<-c(baccuSVM,baccuNB, baccuRF)
  fSVM<-fmeasurefxn(cont_SVM[2,2]/sum(cont_SVM[,2]),cont_SVM[2,2]/sum(cont_SVM[2,]))
  #print(fSVM)
  fNB<-fmeasurefxn(cont_NB[2,2]/sum(cont_NB[,2]),cont_NB[2,2]/sum(cont_NB[2,]))
  fRF<-fmeasurefxn(cont_RF[2,2]/sum(cont_RF[,2]),cont_RF[2,2]/sum(cont_RF[2,])) 
  Fmeasure_orig[j,]<-c(fSVM,fNB,fRF)
  j=j+1
}
beep("mario")
#Fmeasure_orig[complete.cases(Fmeasure_orig),]

apply(baccu_orig,2,max)
apply(baccu_orig,2,mean)
