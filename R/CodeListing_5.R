require2("reshape2")
require2("ggplot2")

mydtm<-DocumentTermMatrix(mycorpus)
Terms(mydtm)[1:50]
dimnames(mydtm)$Docs<-paste("S", 1:nDocs(mydtm),"_D1004", sep="")

# See part of the document-by-term matrix
inspect(mydtm[1:10,90:120])
inspect(mydtm[3,])

#Convert to ordinary matrix
mydtm_mat<-as.data.frame.matrix(mydtm)

# remove rows consisting entirely of 0s
mydtm_mat<-mydtm_mat[rowSums(mydtm_mat)!=0,]

# Display words with frequencies over 3
colSums(mydtm_mat)[colSums(mydtm_mat)>3]


percentDoc<-ceiling(nrow(mydtm_mat)*.05)
names(which(apply(mydtm_mat,2, function(x) sum(x!=0))>=percentDoc))

