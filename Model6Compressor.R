# Compression algorithm for Ngram data frames

library(dplyr)
setwd("D:/Coursera R/Capstone")

## Compress K3words by removing entires that will never be used
## (keep only the top 8 instances of each predictor Ngram)

# 261205 items to compress in K3words
x1<-head(grep(paste("^",8,"$",sep=""),K2words$Num),1)

i<-1
x<-1

while(i<x1){
     locs<-grep(paste("^",K2words$BiGram[i],"$",sep = ""),K3words$pred)
     if(length(locs)>8){
     locs8<-locs[9:length(locs)]
     K3words<-K3words[-locs8,,drop=FALSE]
     }
     i<-i+1
     if(floor(i/1000)==i/1000){
          print(c(x,i))
          print(Sys.time())
     }
     if(floor(i/10000)==i/10000){
          # Save off compressed version of data frames to file
          savelist4<-c("K1words", "K2words", "K3words","i","x")
          con<-gzfile("CompressedPreds6.Rdf")
          save(list = savelist4, file=con)
          close(con)
     }
}

# Keep only the 2 necessary columns
K3words<-select(K3words,pred,res)

## Compress K2words by removing entires that will never be used
## (keep only the top 8 instances of each predictor word)

# 52340 items to compress in K2words
x2<-head(grep(paste("^",8,"$",sep=""),K1words$Num),1)

x<-2
i<-1

while(i<x2){
     locs<-grep(paste("^",K1words$word[i],"$",sep = ""),K2words$pred)
     if(length(locs)>8){
          locs8<-locs[9:length(locs)]
          K2words<-K2words[-locs8,,drop=FALSE]
     }
     i<-i+1
     if(floor(i/2000)==i/2000){
          print(c(x,i))
          print(Sys.time())
     }
     if(floor(i/10000)==i/10000){
          # Save off compressed version of data frames to file
          savelist4<-c("K1words", "K2words", "K3words","i","x")
          con<-gzfile("CompressedPreds6.Rdf")
          save(list = savelist4, file=con)
          close(con)
     }
}

# Keep only the 2 necessary columns
K2words<-select(K2words,pred,res)

## Compress K1words by keeping only the top 8 outcomes (all that will be needed)

# Keep 8 items in K1words
K1words<-head(K1words,8)

#remove the extra column from K1words
K1words<-select(K1words,word)

# Save off compressed version of data frames to file
savelist3<-c("K1words", "K2words", "K3words")
con<-gzfile("CompressedPreds6.Rdf")
save(list = savelist3, file=con)
close(con)