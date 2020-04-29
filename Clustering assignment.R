Crime <- read.csv(file.choose())
View(Crime)

# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(Crime[,2:5]) #excluding the City column before normalizing
?dist
d <- dist(normalized_data, method = "euclidean") # distance matrix
?hclust
fit <- hclust(d, method="complete")
?hclust
plot(fit) # display dendrogram
plot(fit, hang=-1)

?cutree
rect.hclust(fit, k=4, border="blue")
?rect.hclust
groups <- cutree(fit, k=4) # cut tree into 5 clusters

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(Crime, membership)

View(final)

write.csv(final, file="Crimefinal.csv",row.names = F)

aggregate(Crime[,-1],by=list(final$membership),mean)
 