############################################################################
### Latent Semantic Analysis (LSA).                                      ###
######################################                                   ###
### Description:                                                         ###
### This R script contains commands than runs latent semantic analysis.  ###
### LSA is useful for detecting words having similar senses.             ###
############################################################################

# Load the lsa library
require2("lsa")
require2("slam")

# Load the sentences
vacancysentences<-read.table("./sentences_from_vacancies/sentenceVacancies.txt", header=FALSE, sep="\t")
mysentences<-vacancysentences


#  Create a corpus
mycorpus<-VCorpus(VectorSource(mysentences[,2]))

# Apply transformation from CodeListing_4.R
mycorpus<-transformCorpus(mycorpus)

# For the transformation create the Document by Term Matrix
mydtm<-DocumentTermMatrix(mycorpus, control = list(removePunctuation=TRUE, removeNumbers=TRUE,stopwords=stopwords("de")))
Terms(mydtm)[1:50]

#summary(col_sums(mydtm))
#term_tfidf<-tapply(mydtm$v/row_sums(mydtm)[mydtm$i], mydtm$j, mean) *
#  log2(nDocs(mydtm)/col_sums(mydtm>0))
#summary(term_tfidf)
#mydtm<- mydtm[, term_tfidf>=1.2]
#mydtm<- mydtm[row_sums(mydtm)>0,]

# See part of the document-by-term matrix
inspect(mydtm[3:6,90:110])

#Convert to ordinary matrix
mydtm_mat<-as.data.frame.matrix(mydtm)
# Check the dimension
dim(mydtm_mat) # should display 425 1079


task_indx=c(6,30,31,32,33,34,35,70,71,104,105,149,150,151,185,186,187,216,217,218,
            219,244,245,246,247,248,331,332,333,393,394)
all_indx= 1:425
nontask_indx= all_indx[-task_indx]
set.seed(123)
selectednontask<-sample(nontask_indx, size=360,replace=FALSE)
training_indx<-c(task_indx,selectednontask )

mydtm_mat<-mydtm_mat[training_indx,]
mydtm_mat<-mydtm_mat[rowSums(mydtm_mat)>0,]
mydtm_mat<-mydtm_mat[,colSums(mydtm_mat)>0]
dim(mydtm_mat) # should dispaly 422 1079

# Perform LSA

# Term By Document Matrix
mytdm_mat<-t(mydtm_mat)
# Apply normalization
mytdm_mat_lsa<-lw_tf(mytdm_mat)*gw_normalisation(mydtm_mat)
# Apply LSA
lsaSpace<-lsa(mytdm_mat_lsa, dims=dimcalc_share(share=0.5))

# Project to the LSA space
projdocterms<-as.textmatrix(lsaSpace)
projdocterms<-projdocterms[rowSums(projdocterms)!=0,]
projdocterms<-projdocterms[,colSums(projdocterms)!=0]
