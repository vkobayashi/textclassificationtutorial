############################
### Preparing Text
############################
### Description:
### This R script performs text content extraction on an HTML file.
### The extracted content is written to a text file and stored in
### the computer.
############################

# This command accomplishes two things. 
# If a package is already installed then it loads the package.
# If a package is not installed then it will first
# install the package and then will load it.
 require2<-function(pakgName){
  if(pakgName %in% rownames(installed.packages()) == FALSE) {install.packages(pakgName)}
  library(pakgName, character.only=TRUE)}

# Loads the package "XML".
require2("XML")

# Checks if the file "data_1004.txt" is present in the working
# directory. If it is present it deletes it, if not then
# it does nothing.
if(file.exists("data_10004.txt")) file.remove("data_10004.txt")

# Get the computer path of the file "data_1004.html". Try opening
# this file to see its contents. You can open it using a
# web browser such as Chrome or Mozilla Firefox.
fileName<-file.path(".","data","data_10004.html")

# Read the content (i.e. text) of the file in "data_1004.html"
# and assigned it to a variable. Here the variable is named
# htmlfile.
htmlfile<-readChar(fileName, file.info(fileName)$size)

# Tip: Check whether the content is properly read.
print(htmlfile)

## Start processing the content of the HTML file.

# Replace the item list tag (e.g.<li> and </li>) with a newline
# character encoded as "\n". This is done using the
# gsub() function. This function accepts a text pattern as one of
# its argument and replaces all text that match that pattern.
# Here the text pattern is "</?[Ll][Ii]>" and it matches <LI>,
# </LI>, <li>, and </li>. The pattern is also called a
# "regular expression". The variable htmlfile now contains the
# processed text.
htmlfile<-gsub("</?[Ll][Ii]>","\n", htmlfile)

# Replace the line break tag with a newline character.
htmlfile<-gsub("<br[/]?>","\n", htmlfile)

# Replace the paragraph tag with a newline character.
htmlfile<-gsub("</?[Pp]>","\n", htmlfile)

# Replace the unordered list tags with a newline character.
rawpagehtml<-gsub("</?[Uu][Ll]>","\n", htmlfile)

# Parse the HTML text using the htmlTreeParse() function.
rawpagehtml<-htmlTreeParse(rawpagehtml, useInternalNodes=TRUE)

# Use xpathSApply() function and extract the relevant content which
# is in the div tag with class "content".
jobdescription<-xpathSApply(rawpagehtml,"//div[@class='content']",xmlValue)
jobdescription<-gsub("[\r]?[\n][[:space:]]+","\n",jobdescription)
# Create a folder in the current working directory where we will
# store the extracted content. The folder is named "parsed"
if(dir.exists("parsed")) {unlink("parsed", recursive=TRUE)}

# Finally wite the content to a text file with the file name
# "data_1004.txt"
dir.create("parsed")
writeLines(jobdescription,"./parsed/data_10004.txt", sep="\n")

# Now go to the folder "parsed" in your current working directory,
# there should be a file named "data_1004.txt" containing the
# extracted content from the html file.
