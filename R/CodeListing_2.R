############################
### Preprocessing more than one html files
############################
### Description:
### This R script performs text content extraction on a bunch of HTML files.
### The extracted contents are written to text files (one for each html file)
### and stored inthe computer.
############################

# Loads the package stringr
require2("stringr")

# The function loads a bunch of html files from a folder and
# applies the preprocessing procedures in CodeListing.R to each
# file. The extracted contents are writtent to text files and
# stored in the folder "parsedvacancies" in the current working
# directory. One text file is written for each html file.

htmlfileparser<-function(folder=""){
  dir<-file.path(".",folder)
  dir.create("parsedvacancies")
  vacancyfiles<-list.files(dir)
  vacancyfiles<-vacancyfiles[grepl("*.html", vacancyfiles)]
  
  for(i in vacancyfiles){
    fileName <- file.path(dir,i)
    htmlfile<-readChar(fileName, file.info(fileName)$size)
    htmlfile<-gsub("</?[Ll][Ii]>","\n", htmlfile)
    htmlfile<-gsub("<br/?>","\n", htmlfile)
    htmlfile<-gsub("</?[Pp]>","\n", htmlfile)
    rawpagehtml<-gsub("</?[Uu][Ll]>","\n", htmlfile)
    rawpagehtml<-htmlTreeParse(rawpagehtml, useInternalNodes=TRUE, encoding="UTF-8", asText=TRUE)
    
    jobdescription<-xpathSApply(rawpagehtml,"//body",xmlValue)
    jobdescription<-gsub("[\r]?[\n][[:space:]]+","\n",jobdescription)
    writeLines(jobdescription,paste("./parsedvacancies/",str_extract(i,".+[\\.]"),"txt", sep=""))
  }
}

if(dir.exists("parsedvacancies")) {unlink("parsedvacancies")}
htmlfileparser(folder="vacancypages")
