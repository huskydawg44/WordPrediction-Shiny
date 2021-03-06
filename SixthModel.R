# Takes data from Read_Data6.R  sample w/ stopwords
# and processes it out to NgramPreds6.Rdf
setwd("D:/Coursera R/Capstone")
con<-gzfile("NGramstats6.Rdf") #generated by Read_Data6.R
load(con)
close(con)

library(dplyr)
library(stringr)

#Convert lists into Data Frames

#Keep single words occuring more than 2 times.
K1words<-data.frame(Unisum[Unisum>2])
K1words<-mutate(K1words,word=names(Unisum[Unisum>2]))
names(K1words)[1]<-"Num"


# Only keep the first 10 outcomes of K1words for use later ... might need full list for compression.
#K1words<-head(K1words,10)

#Keep BiGrams occuring more than x2 times.
x2<-1
K2words<-data.frame(BGsum[BGsum>x2])
K2words<-mutate(K2words,BiGram=names(BGsum[BGsum>x2]))
names(K2words)[1]<-"Num"
tempPreds<-str_split(K2words$BiGram," ")
part1<-unlist(lapply(tempPreds, function(l) l[[1]]))
part2<-unlist(lapply(tempPreds, function(l) l[[2]]))
K2words<-mutate(K2words,pred=part1,res=part2)

#Keep TriGrams occuring more than x3 times.
x3<-1
K3words<-data.frame(TGsum[TGsum>x3])
K3words<-mutate(K3words,TriGram=names(TGsum[TGsum>x3]))
names(K3words)[1]<-"Num"
tempPreds<-str_split(K3words$TriGram," ")
part1<-unlist(lapply(tempPreds, function(l) l[[1]]))
part2<-unlist(lapply(tempPreds, function(l) l[[2]]))
part3<-unlist(lapply(tempPreds, function(l) l[[3]]))
part12<-paste(part1,part2)
K3words<-mutate(K3words,pred=part12, res=part3)

savelist3<-c("K1words", "K2words", "K3words")
con<-gzfile("NGramPreds6.Rdf")
save(list = savelist3, file=con)
close(con)