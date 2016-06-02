##########
### Feature Selection
##########
require2("FSelector")

# labels for the documents in the Document by Term matrix
labelsdtm<-labels[as.numeric(rownames(mydtm_mat))]
# append labels
mydtm_matwithLabels<-mydtm_mat
mydtm_matwithLabels$taskOrNot<-labelsdtm

#Information Gain
IG<-information.gain(taskOrNot~., data=mydtm_matwithLabels)
subset(IG, attr_importance>0)
##                attr_importance
## sicherstellung      0.02421507
#Gain ratio
GR <- gain.ratio(taskOrNot~., data=mydtm_matwithLabels)
subset(GR, attr_importance>0)
##                attr_importance
## sicherstellung       0.4208428
#Symmetric Uncertainty
SU<-symmetrical.uncertainty(taskOrNot~., data=mydtm_matwithLabels)
subset(SU, attr_importance>0)
##                attr_importance
## sicherstellung       0.1260144
