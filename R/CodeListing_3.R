#############################################################################
### Preprocessing Text: Apply sentence segmentation.                      ###
### This is an important task for our study since the sentence is         ###
### treated as the unit of analysis (i.e. document).                      ###
######################################                                    ###
### Description:                                                          ###
### This R script contains commands that extract the individual sentences ###
### from the vacancies.                                                   ###
#############################################################################

# Loads the packages openNLP and NLP.
# The openNLP library in R is based on the Apache OpenNLP (https://opennlp.apache.org/)
require2("openNLP")
require2("NLP")

# Loads the models for German since the vacancies are in German.
if("openNLPmodels.de" %in% rownames(installed.packages()) == FALSE) {install.packages("openNLPmodels.de", repos = "http://datacube.wu.ac.at/", type = "source" )}
library("openNLPmodels.de")

# Appy sentence segmentation
sent_token_annotator<-Maxent_Sent_Token_Annotator(language="de")
word_token_annotator<-Maxent_Word_Token_Annotator(language="de")
pos_tag_annotator<-Maxent_POS_Tag_Annotator(language="de")

# Wrapper function for the default sentence annotator in openNLP for German.
sent_tokens<-function(sents){
  if(!(grepl("^\\(", sents))){
  tryCatch({
    sents<-as.String(sents)
    a1<-sents[annotate(sents, sent_token_annotator)]
    logic_bzw<-unlist(sapply(a1, FUN= function(x) {grepl("^(bzw\\.)",x)}, USE.NAMES=FALSE))
    logic_inkl<-unlist(sapply(a1, FUN= function(x) {grepl("(\\(?inkl\\.)",x)}, USE.NAMES=FALSE))
    if(any(logic_bzw)){
      indx<-which(logic_bzw)
      a1[indx]<-paste(a1[indx:(indx+1)], collapse=" ")
      a1[indx+1]<-NULL
    }
    if(any(logic_inkl)){
      indx<-which(logic_inkl)
      a1[indx-1]<-paste(a1[(indx-1):indx], collapse=" ")
      a1[indx]<-NULL
    }
    return(a1)},
    error=function(cond){return(sents)})
  }
  return(sents)
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
  sentence<-scan('./parsed/sample_nursing_vacancy.txt',character(0), sep='\n', strip.white=TRUE)},
  warning=function(cond){sentence<-scan('./parsed/sample_nursing_vacancy.txt',character(0), sep='\n', strip.white=TRUE, fileEncoding="UTF-8")})

# Additional ways to detect sentences. Some sentences may not be properly identified due to
# bad formating. Apply sentence segmentation to the text in sample_nursing_vacancy.txt
sentence<-unlist(sapply(sentence, gsub,pattern="Â[[:space:]]?",replacement="", simplify=TRUE, USE.NAMES=FALSE))
sentence<-unlist(sapply(sentence, gsub,pattern="[[:cntrl:]]",replacement="", simplify=TRUE, USE.NAMES=FALSE))
#sentence<-unlist(sapply(sentence, sent_split,pattern="\\.[:space:]+[A-Z]", simplify=TRUE, USE.NAMES=FALSE))
sentence<-unlist(sapply(sentence, sent_split,pattern="\\|", simplify=TRUE, USE.NAMES=FALSE))
sentence<-unlist(sapply(sentence, sent_tokens, simplify=TRUE, USE.NAMES=FALSE))
sentence<-sentence[nchar(sentence)!=0]
sent_notab<-sapply(sentence, FUN=function(x) str_trim(gsub(pattern="\t",' ', x)), simplify=TRUE, USE.NAMES=FALSE)

# Write the extracted sentences to a new file where one line correspondes to a sentence.
k<-substring('sample_nursing_vacancy.txt', first=1,last=nchar('sample_nursing_vacancy.txt')-4)
if(dir.exists("sentences_from_sample_vacancy")) {unlink("sentences_from_sample_vacancy", recursive=TRUE)}

# Write the sentences to sentences_from_sample_vacancy.txt where each line corresponds to a sentence.
# Note that some lines does not correspond to an actual sentence, this is due to the way
# the text in vacancies are written.
dir.create("sentences_from_sample_vacancy")
write(paste(k,sent_notab,sep="\t"), "./sentences_from_sample_vacancy/sentencelines_nursing_vacancy.txt", append=TRUE)


sentence_segmentor<-function(folder="", writeFolder="", writeFile=""){
  dir<-file.path(".",folder)    # computer path to the folder containing the html files
  vacancytxtfiles<-list.files(dir) # list the files in the folder where the html files are stored
  vacancytxtfiles<-vacancytxtfiles[grepl("*.txt", vacancytxtfiles)] # choose only the files with.html extension
  if(dir.exists(writeFolder)) {unlink(writeFolder, recursive=TRUE)}
  dir.create(writeFolder)
  for(i in vacancytxtfiles){
  tryCatch({
    sentence<-scan(paste("./",folder,"/",i, sep=""),character(0), sep='\n', strip.white=TRUE)},
    warning=function(cond){sentence<-scan(i,character(0), sep='\n', strip.white=TRUE, fileEncoding="UTF-8")})
  
  # Additional ways to detect sentences. Some sentences may not be properly identified due to
  # bad formating. Apply sentence segmentation to the text in sample_nursing_vacancy.txt
  #print(sentence)
  sentence<-unlist(sapply(sentence, gsub,pattern="Â[[:space:]]?",replacement="", simplify=TRUE, USE.NAMES=FALSE))
  sentence<-unlist(sapply(sentence, gsub,pattern="[[:cntrl:]]",replacement="", simplify=TRUE, USE.NAMES=FALSE))
  #sentence<-unlist(sapply(sentence, sent_split,pattern="\\.[:space:]+[A-Z]", simplify=TRUE, USE.NAMES=FALSE))
  sentence<-unlist(sapply(sentence, sent_split,pattern="\\|", simplify=TRUE, USE.NAMES=FALSE))
  sentence<-unlist(sapply(sentence, sent_tokens, simplify=TRUE, USE.NAMES=FALSE))
  sentence<-sentence[nchar(sentence)!=0]
  sent_notab<-sapply(sentence, FUN=function(x) str_trim(gsub(pattern="\t",' ', x)), simplify=TRUE, USE.NAMES=FALSE)
  
  # Write the extracted sentences to a new file where one line correspondes to a sentence.
  k<-substring(i, first=1,last=nchar(i)-4)
  
  # Write the sentences to sentences_from_sample_vacancy.txt where each line corresponds to a sentence.
  # Note that some lines does not correspond to an actual sentence, this is due to the way
  # the text in vacancies are written.
  filetoWrite=paste("./",writeFolder,"/",writeFile, sep="")
  write(paste(k,sent_notab,sep="\t"),filetoWrite, append=TRUE)
  }
}

sentence_segmentor(folder="parsedvacancies", writeFolder="sentences_from_vacancies", writeFile="sentenceVacancies.txt")
