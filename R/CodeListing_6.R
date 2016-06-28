############################################################################
### Keyword for each document.                                           ###
######################################                                   ###
### Description:                                                         ###
### This R script contains commands that derive keywords for each        ###
### document.                                                            ###
############################################################################

# Remove German stopwords
mydtm<-DocumentTermMatrix(mycorpus, control=list(stopwords=stopwords("de")))

# Convert to ordinary matrix
mydtm_mat<-as.data.frame.matrix(mydtm)

mydtm_mat<-mydtm_mat[rowSums(mydtm_mat)!=0,]
# Display frequent terms (terms that occur in more than one document)
colSums(mydtm_mat)[colSums(mydtm_mat)>1]

# Compute the tf-idf of each word in each document. In a specific document, the word
# with the highest tf-idf is the keyword in that document.
d<-sweep(mydtm_mat,1,apply(mydtm_mat,1, max),FUN="/" )
wd<-sweep(d, 2,log2(nrow(d)/colSums(mydtm_mat)), FUN="*")

# Keyword for each document (or sentence since a sentence here is our document)
wd$Document<-paste("Document",row.names(wd))
topicword<-colnames(wd)[apply(wd[,-ncol(wd)], 1,which.max)]
print(cbind(topicword,Document=wd$Document))
