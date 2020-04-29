library(readxl)
air <- read_excel(file.choose(),2)
View(air)

air1<- air[,-1]
sum(is.na(air1))
str(air1)

#since the units are different we have to normalize it
normalize_air <- scale(air1)
View(normalize_air)
air2 <- normalize_air

###calculating the distance 
d = dist(air2, method = 'euclidean')
d
###building the model
fit <- hclust(d,method = 'complete')
fit
fit$dist.method

#plot the model
plot(fit)
plot(fit,hang = -1)
group <- cutree(fit,k=10)
class(group)

rect.hclust(fit,k=10,border = 'purple')
mem<- as.matrix(group)

library(data.table)
final <- data.frame(air2,mem)
View(final)

final1 <- setcolorder(final,c("mem"))
View(final1)

#K-means clustering method
#elbow curve & k ~ sqrt(n/2) to decide the k value
#nrow(normalized_data)-1 = number of rows
wss = (nrow(normalize_air)-1)*sum(apply(normalize_air, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:8) wss[i] = sum(kmeans(normalize_air, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Screw-Plot")

library(kselection)
k <- kselection(normalize_air,parallel = TRUE,k_threshold = 0.9,max_centers = 12)
k
#from the above kselection it gave the result like 10 cluster

air_k<- kmeans(air2,10)
air_k$withinss
air_k$betweenss

###k-means cluster plot by using animation
library(animation)
air_k <- kmeans.ani(air2,10)
air_k$cluster
group1 <- as.matrix(air_k$cluster)
View(group1)

library(data.table)
final2 <- data.frame(air2,air_k$cluster)
View(final2)

setcolorder(final2,neworder = c("air_k.cluster"))
View(final2)
View(aggregate(final2, by = list(group1), FUN = mean))
