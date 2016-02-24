##########
### Latent Semantic Indexing
##########

# Load the sentences
vacancysentences<-read.table("./allsentences/all_sentencelines.txt", header=FALSE, sep="\t")
mysentences<-vacancysentences

#  Create a corpus
mycorpus<-VCorpus(VectorSource(mysentences[,2]))
mycorpus<-transformCorpus(mycorpus)

mydtm<-DocumentTermMatrix(mycorpus)
Terms(mydtm)[1:50]

# See part of the document-by-term matrix
inspect(mydtm[3:6,90:110])

#Convert to ordinary matrix
mydtm_mat<-as.data.frame.matrix(mydtm)
mydtm_mat<-mydtm_mat[rowSums(mydtm_mat)!=0,]

# Perform LSA
require2("lsa")

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
