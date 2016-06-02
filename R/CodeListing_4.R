require2("tm")

vacancysentences<-read.table("./sentence_data10004/sentencelines_data10004.txt", header=FALSE, sep="\t")
mysentences<-vacancysentences

#  Create a corpus
mycorpus<-VCorpus(VectorSource(mysentences[,2]))
for(i in 1:length(mycorpus)) meta(mycorpus[[i]], "origin")<-vacancysentences[i,1]

# Inspect the first document
mycorpus[[1]]$content

transformCorpus<-function(corpusname){
  # Apply all available transformations from the getTransformations() except stemming
  mytransformations<-list(
                          #removeNumbers, 
                          #removePunctuation, 
                          content_transformer(tolower)
  )
  mycorpus<-tm_map(corpusname, FUN=tm_reduce, tmFuns=mytransformations)
  #mycorpus<-tm_map(mycorpus,removeWords, stopwords("de"))
  #control_char<-content_transformer(function(x)gsub("[„“]", "",iconv(x,"UTF-8")))
  punctRemoveEnd<-content_transformer(function(x) gsub("[[:punct:]]$", " ", x))
  punctRemoveEnd<-content_transformer(function(x) gsub("[()]", " ", x))
  #mycorpus<-tm_map(mycorpus, control_char)
  mycorpus<- tm_map(mycorpus, punctRemoveEnd)
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
mycorpus[[26]]$content

# Display the possible transformations
getTransformations()

