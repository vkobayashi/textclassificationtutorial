############################################################################
### Preparing Text: Parsing more than one html files.                    ###
######################################                                   ###
### Description:                                                         ###
### This R script contains commands that extract the relevant text from  ###
### more than one HTML files. The extracted contents are written to .txt ###
### files (one for each html file) and stored in the computer.           ###
############################################################################

# Loads the package stringr
require2("stringr")

# The function loads a bunch of html files from a folder and
# applies the preprocessing procedures in CodeListing_1.R to each
# file. The extracted contents are writtent to text files and
# stored in the folder "parsedvacancies" in the current working
# directory. One text file is written for each html file.

htmlfileparser<-function(folder="", writefolder=""){
  dir<-file.path(".",folder)    # computer path to the folder containing the html files
  vacancyfiles<-list.files(dir) # list the files in the folder where the html files are stored
  vacancyfiles<-vacancyfiles[grepl("*.html", vacancyfiles)] # choose only the files with.html extension
  if(dir.exists(writefolder)) {unlink(writefolder, recursive=TRUE)}
  dir.create(writefolder) # create a folder called parsedvacancies where the extracted text are stored
  
  # iterate over all html files and apply the preprocessing procedures in CodeListing_1.R
  for(i in vacancyfiles){
    fileName <- file.path(dir,i)
    htmlfile<-readChar(fileName, file.info(fileName)$size)
    htmlfile<-gsub("</?[Ll][Ii]>","\n", htmlfile)
    htmlfile<-gsub("<br[/]?>","\n", htmlfile)
    htmlfile<-gsub("</?[Pp]>","\n", htmlfile)
    htmlfile<-gsub("</?[Uu][Ll]>","\n", htmlfile)
    rawpagehtml<-htmlTreeParse(htmlfile, useInternalNodes=TRUE, encoding="UTF-8", asText=TRUE)
    jobdescription<-xpathSApply(rawpagehtml,"//body",xmlValue)
    jobdescription<-gsub("[\r]?[\n][[:space:]]+","\n",jobdescription)
    foldertoWrite=paste("./",writefolder,"/", sep="")
    writeLines(jobdescription,paste(foldertoWrite,str_extract(i,".+[\\.]"),"txt", sep=""))
  }
}

# Apply the function and providing values for the arguments. Here the html files are in
# vacancypages folder and we write the output to parsedvacancies folder.

htmlfileparser(folder="vacancypages", writefolder="parsedvacancies")
