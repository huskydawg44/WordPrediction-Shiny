setwd("D:/Coursera R/Capstone")

library(tm)
library(textreg)
library(stringi)
library(stringr)

#load custom cleaning functions
source('D:/Coursera R/Capstone/Text_Cleaners2.R')

con<-file("en_US/en_US.twitter.txt")
en_Twitter<-readLines(con)
close(con)

con<-file("en_US/en_US.news.txt")
en_News<-readLines(con)
close(con)

con<-file("en_US/en_US.blogs.txt")
en_Blogs<-readLines(con)
close(con)

# sampling function
my_sampler<-function(inlist,my_pct1,my_pct2){
     sample(inlist,(length(inlist)/100*(my_pct1+my_pct2)))
     
}

# Sampling
set.seed(125)

# Set % of data to use for training
trainPct<-25

# Set % of additonal records to pull out for validation
testPct<-.5

# Sample based on parameters
blogS<-my_sampler(en_Blogs,trainPct,testPct)
newsS<-my_sampler(en_News,trainPct,testPct)
tweetS<-my_sampler(en_Twitter,trainPct,testPct)

# Count sets and split using parameters
bSplit<-floor(length(blogS)*trainPct/(trainPct+testPct))
blogTest<-blogS[bSplit+1:length(blogS)]
blogS<-blogS[1:bSplit]

nSplit<-floor(length(newsS)*trainPct/(trainPct+testPct))
newsTest<-newsS[nSplit+1:length(newsS)]
newsS<-newsS[1:nSplit]

tSplit<-floor(length(tweetS)*trainPct/(trainPct+testPct))
tweetTest<-tweetS[tSplit+1:length(tweetS)]
tweetS<-tweetS[1:tSplit]

# Remove empty data from Test data set
tweetTest<-sort(tweetTest)
newsTest<-sort(newsTest)
blogTest<-sort(blogTest)

#remove original read for space
rm(en_Twitter)
rm(en_News)
rm(en_Blogs)

# Clean samples
Cblog<-Clean_Text_Block(blogS)
Cnews<-Clean_Text_Block(newsS)
Ctweet<-Clean_Text_Block(tweetS)

# Clean test data
CblogTest<-Clean_Text_Block(blogTest)
CnewsTest<-Clean_Text_Block(newsTest)
CtweetTest<-Clean_Text_Block(tweetTest)


# Combine training data after cleaning
SallC<-c(Cblog,Cnews,Ctweet)

# Combine testing data after cleaning
TallC<-c(CblogTest,CnewsTest,CtweetTest)

#save sampled and processed file for later
con<-file("Read6_Processed.txt")
writeLines(SallC, con = con)
close(con)

#save sampled test data to file for later
con<-file("Read6_Test.txt")
writeLines(TallC, con = con)
close(con)

#TM method
allCorp<-Corpus(VectorSource(SallC))

UniTDM<-TermDocumentMatrix(allCorp)
Unisum<-slam::row_sums(UniTDM)
Unisum<-sort(Unisum,decreasing = TRUE)

# remove TDM for space
rm("UniTDM")

# Create Bigram tokenizer for TDMs
library(tokenizers)
BGToken <-
     function(x){
          unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
     }

TGToken <-
     function(x){
          unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
     }

#Generate Term Document Matrix and sum to find word counts for Ngrams
BGTDM<-TermDocumentMatrix(allCorp,control=list(tokenize =BGToken))
BGsum<-slam::row_sums(BGTDM)
BGsum<-sort(BGsum,decreasing = TRUE)

rm("BGTDM")
gc()
#

TGTDM<-TermDocumentMatrix(allCorp,control=list(stopwords = TRUE,tokenize =TGToken))
TGsum<-slam::row_sums(TGTDM)
TGsum<-sort(TGsum,decreasing = TRUE)
#
rm("TGTDM")
gc()



# Save data to file for use in Reporting (FifthModel.R)
savelist2<-c("BGsum", "Unisum", "TGsum")
con<-gzfile("NGramstats6.Rdf") #sample with stopwords
save(list = savelist2, file=con)
close(con)