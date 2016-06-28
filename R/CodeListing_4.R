#############################################################################
### Text Preprocessing: Apply further text textpreprocessing.             ###
######################################                                    ###
### Description:                                                          ###
### This R script contains commands that extracts text from an HTML file. ###
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
  # Apply all available transformations from the getTransformations() except stemming, remove numbers,
  # and remove punctuation
  #mytransformations<-list(
                          #removeNumbers, 
                          #removePunctuation, 
  #                        content_transformer(tolower)
  #)
  mycorpus<-tm_map(mycorpus, content_transformer(tolower))
  #mycorpus<-tm_map(corpusname, FUN=tm_reduce, tmFuns=mytransformations)
  #mycorpus<-tm_map(mycorpus,removeWords, stopwords("de"))
  #control_char<-content_transformer(function(x)gsub("[„“]", "",iconv(x,"UTF-8")))
  punctRemoveEnd<-content_transformer(function(x) gsub("[[:punct:]]$", "", x))
  punctRemoveparcomm<-content_transformer(function(x) gsub("[(),]", " ", x))
  punctRemoveforwardslash<-content_transformer(function(x) gsub("[[a-zA-Z]?/]", " ", x))
  #mycorpus<-tm_map(mycorpus, control_char)
  mycorpus<- tm_map(mycorpus, punctRemoveEnd)
  mycorpus<- tm_map(mycorpus, punctRemoveparcomm)
  mycorpus<- tm_map(mycorpus, punctRemoveforwardslash)
  mycorpus<-tm_map(mycorpus, stripWhitespace)
  mycorpus<-tm_map(mycorpus, content_transformer(str_trim))
  #Filter out empty documents
  #mycorpus_idx<-tm_index(mycorpus, FUN=function(x) nchar(content(x))!=0)
  mycorpus<-tm_filter(mycorpus, FUN=function(x) !(is.null(content(x)) | nchar(content(x))==0 | is.na(content(x))))
  #mycorpus<-mycorpus[mycorpus_idx]
  #mycorpus<-tm_map(mycorpus, function (x) PlainTextDocument(x$content,id=meta(x,"id"),origin=meta(x,"origin")))
  return(mycorpus)
}

mycorpus<-transformCorpus(mycorpus)

for(j in 1:length(mycorpus)) {meta(mycorpus[[j]],"id");print(j);print(mycorpus[[j]]$content)}
# Check the first document again and find out the effect of the applied transformations
mycorpus[[6]]$content
# it should dispaly "aktive mitarbeit und umsetzung der pflegekonzepte und der pflegestandards"

# Display the possible transformations
getTransformations()

