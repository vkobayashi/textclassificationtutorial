require2("stringr")

htmlfileparser<-function(folder=""){
  dir<-file.path(".",folder)
  dir.create("parsedvacancies")
  vacancyfiles<-list.files(dir)
  vacancyfiles<-vacancyfiles[grepl("*.html", vacancyfiles)]
  
  for(i in vacancyfiles){
    fileName <- file.path(dir,i)
    htmlfile<-readChar(fileName, file.info(fileName)$size)
    htmlfile<-gsub("</?[Ll][Ii]>","\n", htmlfile)
    htmlfile<-gsub("</?br>","\n", htmlfile)
    htmlfile<-gsub("</?[Pp]>","\n", htmlfile)
    rawpagehtml<-gsub("</?[Uu][Ll]>","\n", htmlfile)
    rawpagehtml<-htmlTreeParse(rawpagehtml, useInternalNodes=TRUE, encoding="UTF-8", asText=TRUE)
    
    jobdescription<-xpathSApply(rawpagehtml,"//body",xmlValue)
    writeLines(jobdescription,paste("./parsedvacancies/",str_extract(i,".+[\\.]"),"txt", sep=""))
  }
}

htmlfileparser(folder="vacancypages")
