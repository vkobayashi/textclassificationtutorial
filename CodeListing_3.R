require2("openNLP")
require2("NLP")

if("openNLPmodels.de" %in% rownames(installed.packages()) == FALSE) {install.packages("openNLPmodels.de", repos = "http://datacube.wu.ac.at/", type = "source" )}

require2("openNLPmodels.de")

sent_token_annotator<-Maxent_Sent_Token_Annotator(language="de")
word_token_annotator<-Maxent_Word_Token_Annotator(language="de")
pos_tag_annotator<-Maxent_POS_Tag_Annotator(language="de")

sent_tokens<-function(sents){
  sents<-as.String(sents)
  tryCatch({
    a1<-sents[annotate(sents, sent_token_annotator)]
    return(a1)},
    error=function(cond){return(sents)})
}


sent_split<-function (string, pattern, n = Inf) 
{
  if(n==1){
    as.list(string)
  } 
  else {
    locations <- str_locate_all(string, pattern)
    pieces <- function(mat, string) {
      cut <- mat[seq_len(min(n - 1, nrow(mat))), , drop = FALSE]
      keep <- invert_match(cut)
      keep[-1,1]<-keep[-1,1]-1
      str_sub(string, keep[, 1], keep[, 2])
    }
    mapply(pieces, locations, string, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
}

tryCatch({
  sentence<-scan('./parsed/data_10004.txt',character(0), sep='\n', strip.white=TRUE)},
  warning=function(cond){sentence<-scan('./parsed/data_10004.txt',character(0), sep='\n', strip.white=TRUE, fileEncoding="UTF-8")})

sentence<-unlist(sapply(sentence, gsub,pattern="Â[[:space:]]?",replacement="", simplify=TRUE, USE.NAMES=FALSE))
sentence<-unlist(sapply(sentence, gsub,pattern="[[:cntrl:]]",replacement="", simplify=TRUE, USE.NAMES=FALSE))
sentence<-unlist(sapply(sentence, sent_split,pattern="\\.[A-Z]", simplify=TRUE, USE.NAMES=FALSE))
sentence<-unlist(sapply(sentence, sent_split,pattern="\\|", simplify=TRUE, USE.NAMES=FALSE))
sentence<-unlist(sapply(sentence, sent_tokens, simplify=TRUE, USE.NAMES=FALSE))
sentence<-sentence[nchar(sentence)!=0]
sent_notab<-sapply(sentence, FUN=function(x) str_trim(gsub(pattern="\t",' ', x)), simplify=TRUE, USE.NAMES=FALSE)

k<-substring('data_10004.txt', first=1,last=nchar('data_10004.txt')-4)
if(file.exists("./sentence_data10004/sentencelines_data10004.txt")) file.remove("./sentence_data10004/sentencelines_data10004.txt")
## [1] TRUE
write(paste(k,sent_notab,sep="\t"), "./sentence_data10004/sentencelines_data10004.txt", append=TRUE)
