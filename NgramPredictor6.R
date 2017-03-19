# Ngram predictor function
# Master function is called NgramPred(text) which takes a text string as input
#    and returns a list of possible outcomes.

setwd("D:/Coursera R/Capstone")
con<-gzfile("NGramPreds6.Rdf") #created by SixthModel.R
load(con)
close(con)


#load custom cleaning functions
source('D:/Coursera R/Capstone/Text_Cleaners2.R')

testinput<-"hello, what did he"

BGPredF<-function(myinput){
     locs<-grep(myinput,K3words$pred)
     if(length(locs)==0){
          return(NULL)
     }else{
          return(head(K3words$res[locs],8))
     }
}

SGPredF<-function(myinput){
     locs<-grep(myinput,K2words$pred)
     if(length(locs)==0){
          return(NULL)
     }else{
          return(head(K2words$res[locs],8))
     }
}


loop2F<-function(p1,p2){
     tmp<-BGPredF(p1)
     if(is.null(tmp)){
          tmp2<-SGPredF(p2)
          return(tmp2)
     }else{
          if(length(tmp)<8){
          tmp2<-SGPredF(p2)
          tmp3<-head(unique(c(tmp,tmp2)),8)
          return(tmp3)
          }else{
          return(tmp)
          }
     }
}

#accepts any string of words and predicts the next word
NgramPred<- function(myinput){
     cleaned<-Clean_String(myinput)
     split_in<-unlist(str_split(cleaned," "))
     indexes <- which(split_in == "")
     if(length(indexes) > 0){
          split_in <- split_in[-indexes]
     }
     len<-length(split_in)
     #Insert catch for character only input
     if(len==0){
          return(NULL)
     }
     
     #Extract the cleaned input into predictor Ngrams
          if(len==1){
               clean_pred<-paste("^",split_in,"$",sep="")
          }
          if(len==2){
               clean_pred<-paste("^",paste(split_in[1],split_in[2]),"$",sep="")
               clean_pred2<-paste("^",split_in[2],"$",sep="")
          }
          if(len>2){
               clean_pred<-paste("^",paste(split_in[len-1],split_in[len]),"$",sep="")
               clean_pred2<-paste("^",split_in[len],"$",sep="")
          }

     
     #Use Extracted Ngrams to find the predicted word
          if(len==1){
               myRes<-SGPredF(clean_pred)     
          }
          if(len>1){
               myRes<-loop2F(clean_pred,clean_pred2)
          }

     if(is.null(myRes)){
          return(K1words$word[1:8])
     }else{
          if(length(myRes)<8){
               myRes2<-c(myRes,K1words$word[1:8])
               return(head(unique(myRes2),8))
          }else{
               return(myRes)
          }
     }
}