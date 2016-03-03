library(XML)
library(stringr)

#########

for(i in 1:51){
  
  weblink=paste("http://www.medi-jobs.de/jobsuche.php?umkreis=30&plzort=&bundesland=deutschland&land=&berufsfeld=3&beruf=104&einsatzgebiet=&jobart=&was=&seite=",i,sep="")
  rawpage<-htmlTreeParse(weblink, useInternal=TRUE)
  all.links<-xpathSApply(rawpage,"//div[@class='ergebnis-titel']/a")
  indv_link<-sapply(all.links, function(alllinks) xmlGetAttr(alllinks, "href"))
  for(j in indv_link){
    weblink2<-paste("http://www.medi-jobs.de",j,sep="")
    rawpage2<-htmlTreeParse(weblink2, useInternal=TRUE)
    jobdesc<-xpathSApply(rawpage2,"//div[@id='anzeige']")
    writeLines(xmlValue(jobdesc[[1]]),paste("./german_vacancies/",gsub("/","_",j),".txt", sep=""))
  }
}

#########
for(i in paste("?page=", seq(1,551), sep="")){
  
  weblink=paste("http://de.gigajob.com/job/Krankenschwester.html",i,sep="")
  rawpage<-htmlTreeParse(weblink, useInternal=TRUE)
  all.links<-xpathSApply(rawpage,"//ul[@class='search-result gjResults bg grayM']//h3/a | //ul[@class='search-result gjResults bg ']//h3/a")
  indv_link<-sapply(all.links, function(alllinks) xmlGetAttr(alllinks, "href"))
  for(j in indv_link){
    weblink2<-paste("http://de.gigajob.com",j, sep="")
    rawpage2<-htmlTreeParse(weblink2, useInternal=TRUE)
    jobdesc<-xpathSApply(rawpage2,"//div[@class='jobtext']")
    writeLines(xmlValue(jobdesc[[1]]),paste("./german_vacancies/",gsub("[:/.]","_",j),".txt", sep=""))
  }
}



