d<-sweep(mydtm_mat,1,apply(mydtm_mat,1, max),FUN="/" )
wd<-sweep(d, 2,log2(nrow(d)/colSums(mydtm_mat)), FUN="*")
# Keyword for each document
wd$Document<-paste("Document",row.names(wd))
topicword<-colnames(wd)[apply(wd[,-ncol(wd)], 1,which.max)]
print(cbind(topicword,Document=wd$Document))
