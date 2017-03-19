# Model accuracy checking function

setwd("D:/Coursera R/Capstone")

# Load model
source('D:/Coursera R/Capstone/NgramPredictor6.R')

con<-file("Read6_Test.txt")
Tall<-readLines(con)
close(con)

set.seed(333)


#initialize counting vector to record TRUE FALSE of matches
outComes<-rep(FALSE,length(Tall))

#
for(i in 1:length(Tall)){
temp<-unlist(str_split(Tall[i]," "))
indexes <- which(temp == "")
if(length(indexes) > 0){
     temp <- temp[-indexes]
}

     if(length(temp)<3){
       outComes[i]<-NA
     }else{
          lTemp2<-length(temp)-2
          myloc<-sample(1:lTemp2,1)
          mypreds<-paste(temp[myloc],temp[myloc+1])
          myres<-NgramPred(mypreds)
          actRes<-temp[myloc+2]
          Cres<-length(unique(c(myres,actRes)))
          if(Cres==9){
               outComes[i]<-FALSE
          }else{
               outComes[i]<-TRUE
          }
     }
}

myTab<-table(outComes)

savelist<-"myTab"
con<-gzfile("m6analyzed.Rdf")
save(list = savelist, file=con)
close(con)

# Results for Model 6:
# 9889 True; 10757 False
# 47.9% accuracy rate, on 20,646 samples, when comparing the actual next word to the list of 8 predicted words
