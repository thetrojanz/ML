#to extract the tweets of the user
install.packages("twitteR")
library(twitteR)
install.packages("ROAuth")
library(ROAuth)
install.packages("httpuv")
library(httpuv)
install.packages("base64enc")
library(base64enc)
library(readr)
install.packages("tm")
library(tm)


cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
save(cred,file = "twitterAuthenti.Rdata")
load('twitterAuthenti.Rdata')


setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret

tw <- userTimeline('kunalkamra88',n = 1000,includeRts = T)
twdf <- twListToDF(tw)
View(twdf)
str(twdf)
dim(twdf)

setwd("C://Users//user//Desktop//Assignments")
write.csv(twdf,file = "twitt.csv",row.names = F)
getwd()


data1 <- read.csv(file.choose())
View(data1)
data1 <- data1[,1]

data1 <- VCorpus(VectorSource(data1))

#cleaning the data
x <- tm_map(data1,content_transformer(tolower))
x <- tm_map(x,content_transformer(removePunctuation))
x <- tm_map(x,content_transformer(removeNumbers))
x <- tm_map(x,removeWords,stopwords("english"))
#x <- tm_map(x,content_transformer(removeURL))
x <- tm_map(x,content_transformer(stripWhitespace))

#Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x)
View(tdm)


tdm <- as.matrix(tdm)
tdm[1:409, 1:20]

tdm <- as.matrix(tdm)
tdm[410:900, 1:20]
# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 20)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Term phone repeats in all most all documents
#x1 <- tm_map(x1, removeWords, 'phone')
#x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x)
tdm

# wordcloud2 - shapes for word cloud
w

w_small <- subset(w, w >= 10)
w_small

barplot(w_small, las=2, col = rainbow(30))


library(wordcloud2)
w1 <- data.frame(names(w_small), w_small)
colnames(w1) <- c('word', 'freq')
windows()
wordcloud2(w1, size=0.3, shape='circle')

wordcloud2(w1, size=0.3, shape = 'triangle')

##wordcloud2(w1, size=0.3, shape = 'square')
##from the above wordcloud google has been used many times
