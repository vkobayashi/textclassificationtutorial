require2("tm")

vacancysentences<-read.table("./sentence_data10004/sentencelines_data10004.txt", header=FALSE, sep="\t")
mysentences<-vacancysentences

#  Create a corpus
mycorpus<-VCorpus(VectorSource(mysentences[,2]))

# Inspect the first document
mycorpus[[1]]$content

transformCorpus<-function(corpusname){
  # Apply all available transformations from the getTransformations() except stemming
  mytransformations<-list(removeNumbers, 
                          removePunctuation, 
                          content_transformer(tolower)
  )
  mycorpus<-tm_map(corpusname, FUN=tm_reduce, tmFuns=mytransformations)
  mycorpus<-tm_map(mycorpus,removeWords, stopwords("de"))
  control_char<-content_transformer(function(x)gsub("[„“]", "",iconv(x,"UTF-8")))
  mycorpus<-tm_map(mycorpus, control_char)
  mycorpus<-tm_map(mycorpus, stripWhitespace)
  mycorpus<-tm_map(mycorpus, content_transformer(str_trim))
  #Filter out empty documents
  mycorpus<-tm_filter(mycorpus, FUN=function(x) nchar(content(x))!=0)
  return(mycorpus)
}

mycorpus<-transformCorpus(mycorpus)

# Check the first document again and find out the effect of the applied transformations
mycorpus[[1]]$content

# Display the possible transformations
getTransformations()

