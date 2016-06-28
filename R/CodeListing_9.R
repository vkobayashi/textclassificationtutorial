##########
### Feature Selection
##########
require2("FSelector")


# labels for the documents in the Document by Term matrix
#labels<-readLines("labels.txt")
labelsdtm<-labels[as.numeric(rownames(mydtm_mat))]
# append labels
mydtm_matwithLabels<-mydtm_mat
mydtm_matwithLabels$taskOrNot<-ifelse(labelsdtm == "1", "task", "nontask")

#Information Gain
IG<-information.gain(taskOrNot~., data=mydtm_matwithLabels)
subset(IG, attr_importance>0)
##                attr_importance
## zusammenarbeit      0.01889206
#Gain ratio
GR <- gain.ratio(taskOrNot~., data=mydtm_matwithLabels)
subset(GR, attr_importance>0)
##                attr_importance
## zusammenarbeit       0.4471747
#Symmetric Uncertainty
SU<-symmetrical.uncertainty(taskOrNot~., data=mydtm_matwithLabels)
subset(SU, attr_importance>0)
##                attr_importance
## zusammenarbeit       0.123986
