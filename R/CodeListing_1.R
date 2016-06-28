#####################################################################################
### Preparing Text: Parsing html and extracting the relevant text content         ###
######################################                                            ###
### Description:                                                                  ###
### This R script contains commands that extracts text from an HTML file.         ###
### The extracted text is written to a .txt file which is stored in the computer. ###
#####################################################################################

# This command accomplishes two things. 
# (1) If a package is already installed then it loads the package.
# (2) If a package is not installed then it will first
# install the package and then loads it.
require2<-function(pakgName){
  if(pakgName %in% rownames(installed.packages()) == FALSE) {install.packages(pakgName)}
  library(pakgName, character.only=TRUE)}

# Loads the package "XML".
require2("XML")

# Checks if the file "sample_nursing_vacancy.txt" is present in the current working
# directory. If it is present it deletes it, if not then
# it does nothing.
if(file.exists("sample_nursing_vacancy.txt")) file.remove("sample_nursing_vacancy.txt")

# Get the computer path of the file "sample_nursing_vacancy.html". Try opening
# this file to see its contents. You can open it using a
# web browser such as Chrome or Mozilla Firefox.
fileName<-file.path(".","data","sample_nursing_vacancy.html")

# Read all the content of the file in "sample_nursing_vacancy.html"
# and assigned it to a variable. Here the variable is named
# htmlfile.
htmlfile<-readChar(fileName, file.info(fileName)$size)

# Tip: Check whether the content has been properly read.
cat(htmlfile)

# Display the number of characters in htmlfile.
# You should get 10749
nchar(htmlfile)

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

# Replace the html line break tag with a newline character.
htmlfile<-gsub("<br[/]?>","\n", htmlfile)

# Replace the paragraph tag with a newline character.
htmlfile<-gsub("</?[Pp]>","\n", htmlfile)

# Replace the unordered list tags with a newline character.
htmlfile<-gsub("</?[Uu][Ll]>","\n", htmlfile)

# Parse the HTML text using the htmlTreeParse() function.
# htmlTreeParse returns an object of class XMLDOcument.
# This will enable us to navigate through its contents.
rawpagehtml<-htmlTreeParse(file=htmlfile, useInternalNodes=TRUE)

# Use xpathSApply() function and extract the relevant content which
# is in the div tag with class "content". Note that this might be different
# for other webpages. Depending on where the relevant content is located
# you should provide the appropriate value for the path argument
jobdescription<-xpathSApply(doc=rawpagehtml, path="//div[@class='content']", xmlValue)

# This will remove extra line breaks
jobdescription<-gsub("[\r]?[\n][[:space:]]+", "\n", jobdescription)

# Create a folder in the current working directory where we will
# store the extracted content. The folder is named "parsed"
if(dir.exists("parsed")) {unlink("parsed", recursive=TRUE)}

# Finally wite the content to a .txt file with the file name
# "sample_nursing_vacancy.txt"
dir.create("parsed")
writeLines(jobdescription,"./parsed/sample_nursing_vacancy.txt", sep="\n")

# Now go to the folder "parsed" in your current working directory,
# there should be a file named "sample_nursing_vacancy.txt" containing the
# extracted content from the html file.
