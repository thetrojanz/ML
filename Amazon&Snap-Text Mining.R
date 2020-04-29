library(rvest)
library(XML)
library(magrittr)

url <- "https://www.amazon.in/product-reviews/B07Q9MJKBV/ref=acr_dp_hist_5?ie=UTF8&filterByStar=five_star&reviewerType=all_reviews#reviews-filter-bar"
ama <- NULL

for (i in 1:20) {
  murl <- read_html(as.character(paste(url,i,sep="")))
  rev <- murl %>%
  html_nodes(".review-text") %>%
    html_text()
  ama <- c(ama,rev)
}

write.table(ama,"noise.csv",row.names = F)
View(ama)
getwd()


data1 <- read.csv(file.choose())
View(data1)

data1 <- VCorpus(VectorSource(data1))

#####cleaning the data
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
tdm[1:100,]

tdm <- as.matrix(tdm)
tdm[101:200,]
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

w_small <- subset(w, w >= 30)
w_small

barplot(w_small, las=2, col = rainbow(30))


library(wordcloud2)
w1 <- data.frame(names(w_small), w_small)
colnames(w1) <- c('word', 'freq')
windows()
wordcloud2(w1, size=0.8, shape='circle')

wordcloud2(w1, size=0.8, shape = 'triangle')

# xxxxxxxxxxxxxxxxxxxxxx SnapDeal xxxxxxxxxxxxxxxxxxxxxxxxxxxx

url1 <- "https://www.snapdeal.com/product/louis-vuitton-lv-black-leather/657710097478"
url2 <- "2&sortBy=HELPFUL"
snap <- NULL

for (i in 1:10) {
  murl1 <- read_html(as.character(paste(url1,url2,sep = as.character(i))))
  revsnap <- murl1 %>%
  html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snap <- c(snap,revsnap)
}

write.table(snap,"snap.csv",row.names = F)
View(snap)



data1 <- read.csv(file.choose())
View(data1)

data1 <- VCorpus(VectorSource(data1))

#####cleaning the data
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
tdm[1:10,]

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 3)
w_sub

barplot(w_sub, col = rainbow(30))

# Term phone repeats in all most all documents
#x1 <- tm_map(x1, removeWords, 'phone')
#x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x)
tdm

# wordcloud2 - shapes for word cloud
w

w_small <- subset(w, w >= 2)
w_small

barplot(w_small, las=2, col = rainbow(30))


library(wordcloud2)
w1 <- data.frame(names(w_small), w_small)
colnames(w1) <- c('word', 'freq')
windows()
wordcloud2(w1, size = 1, shape='circle')

wordcloud2(w1, size = 1, shape = 'triangle')
