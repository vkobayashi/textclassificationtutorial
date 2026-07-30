############################################################################
### Text Transformation: Transform free text to Document-by-Term Matrix. ###
######################################                                   ###
### Description:                                                         ###
### This R script contains commands that transforms free text to         ###
### Document-by-Term matrix. The matrix is suitable for the application  ###
### analytical methods.                                                  ###
############################################################################

# Loads reshape2 package which is a package for data manipulation.
# Loads ggplot2 package which is a package for creating graphics.
require2("reshape2")
require2("ggplot2")

# From corpus of free texts to Document by Term matrix based from the
# vector space model of transforming text.
mydtm<-DocumentTermMatrix(mycorpus)

# Inspect the first 50 terms.
Terms(mydtm)[1:50]

# Assign names to the documents.
dimnames(mydtm)$Docs<-paste("S", 1:nDocs(mydtm),"_sampleNursingJob", sep="")

# See part of the document-by-term matrix
inspect(mydtm[1:10,90:120])
inspect(mydtm[3,])

# Convert to ordinary matrix
mydtm_mat<-as.data.frame.matrix(mydtm)

# Remove rows consisting entirely of 0s
mydtm_mat<-mydtm_mat[rowSums(mydtm_mat)!=0,]

# Display words with frequencies over 3
colSums(mydtm_mat)[colSums(mydtm_mat)>3]
# As expected the most frequent words are the stopwords in the German language

percentDoc<-ceiling(nrow(mydtm_mat)*.1)
names(which(apply(mydtm_mat,2, function(x) sum(x!=0))>=percentDoc))

