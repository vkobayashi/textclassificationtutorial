#############################################################################
### Text Preprocessing: Apply further text textpreprocessing.             ###
######################################                                    ###
### Description:                                                          ###
### This R script contains commands that extract text from an HTML file.  ###
### The extracted text is written to a .txt file which is stored in       ###
### the computer.                                                         ###
#############################################################################

# Loads tm package. This package provides a framework for text mining
# in R
require2("tm")

# Import text data in R
vacancysentences<-read.table("./sentences_from_sample_vacancy/sentencelines_nursing_vacancy.txt", header=FALSE, sep="\t")
mysentences<-vacancysentences

#  Create a corpus (a corpus is a collection of documents)
mycorpus<-VCorpus(VectorSource(mysentences[,2]))
for(i in 1:length(mycorpus)) meta(mycorpus[[i]], "origin")<-vacancysentences[i,1]

# Inspect the 10th document
mycorpus[[10]]$content
# It should dispaly "Fachliche Anleitung von Pflegehilfskräften, Schülern und Praktikanten"

# A function for applying further text preprocessing and transformation

transformCorpus<-function(corpusname){
  mycorpus<-tm_map(mycorpus, content_transformer(tolower))
  punctRemoveEnd<-content_transformer(function(x) gsub("[[:punct:]]$", "", x))
  punctRemoveparcomm<-content_transformer(function(x) gsub("[(),]", " ", x))
  punctRemoveforwardslash<-content_transformer(function(x) gsub("[[a-zA-Z]?/]", " ", x))
  mycorpus<- tm_map(mycorpus, punctRemoveEnd)
  mycorpus<- tm_map(mycorpus, punctRemoveparcomm)
  mycorpus<- tm_map(mycorpus, punctRemoveforwardslash)
  mycorpus<-tm_map(mycorpus, stripWhitespace)
  mycorpus<-tm_map(mycorpus, content_transformer(str_trim))
  #Filter out empty documents
  mycorpus<-tm_filter(mycorpus, FUN=function(x) !(is.null(content(x)) | nchar(content(x))==0 | is.na(content(x))))
  return(mycorpus)
}

mycorpus<-transformCorpus(mycorpus)

for(j in 1:length(mycorpus)) {meta(mycorpus[[j]],"id")}
# Check the first document again and find out the effect of the applied transformations
mycorpus[[6]]$content
# it should dispaly "aktive mitarbeit und umsetzung der pflegekonzepte und der pflegestandards"

# Display the possible transformations
getTransformations()

