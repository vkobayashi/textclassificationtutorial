require2<-function(pakgName){
  if(pakgName %in% rownames(installed.packages()) == FALSE) {install.packages(pakgName)}
  library(pakgName, character.only=TRUE)}

require2("XML")

if(file.exists("data_10004.txt")) file.remove("data_10004.txt")

fileName<-file.path(".","data","data_10004.html")
htmlfile<-readChar(fileName, file.info(fileName)$size)
htmlfile<-gsub("</?[Ll][Ii]>","\n", htmlfile)
htmlfile<-gsub("</?br>","\n", htmlfile)
htmlfile<-gsub("</?[Pp]>","\n", htmlfile)
rawpagehtml<-gsub("</?[Uu][Ll]>","\n", htmlfile)
rawpagehtml<-htmlTreeParse(rawpagehtml, useInternalNodes=TRUE)
jobdescription<-xpathSApply(rawpagehtml,"//div[@class='content']",xmlValue)
dir.create("parsed")
writeLines(jobdescription,"./parsed/data_10004.txt")
