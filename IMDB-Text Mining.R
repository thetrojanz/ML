library(rvest)
library(XML)
library(magrittr)
imdb <- NULL
url <- "https://www.imdb.com/title/tt0903747/reviews?ref_=tt_ov_rt"
rev <- NULL

for (i in 1:10) {
murl <- read_html(as.character(paste(url,sep = "i")))
rev <- murl%>%
  html_nodes(".show-more__control")%>%
  #html_nodes("#tn15content div+ p")
  #html_nodes(".review-text")
  html_text()
imdb <- c(imdb,rev)
}
write.table(imdb,file = "imdb.csv",row.names = F)
View(imdb)

imdb1 <- read.csv(file.choose())
View(imdb1)

imdb1 <- VCorpus(VectorSource(imdb1))

library(tm)
x <- tm_map(imdb1,content_transformer(tolower))
x <- tm_map(x,content_transformer(removePunctuation))
x <- tm_map(x,content_transformer(removeNumbers))
x <- tm_map(x,content_transformer(stripWhitespace))
x <- tm_map(x,removeWords,stopwords('english'))

View(x)
tdm <- TermDocumentMatrix(x)
View(tdm)
tdm<- as.matrix(tdm)

w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 40)
w_sub


barplot(w_sub,las=2,col = rainbow(30))

# wordcloud2 - shapes for word cloud
w

w_small <- subset(w, w >= 10)
w_small

barplot(w_small, las=2, col = rainbow(30))

library(wordcloud2)
w1 <- data.frame(names(w_small),w_small)
colnames(w1) <- c('word','freq')

wordcloud2(w1,size = 0.8,shape = 'circle')
wordcloud2(w1,size = 0.8,shape = 'triangle')

#The word movie, film, and bengahazi used 480, 279, 230 times respectively