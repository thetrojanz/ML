library(readr)
crime <- read.csv(file.choose())
View(crime)
str(crime)
sum(is.na(crime))

#Normailze the data
normal_data <- scale(crime[,-1])
View(normal_data)
str(normal_data)

#Model building
model <- dist(normal_data,method = 'euclidean')
model
#hierarchial cluster
mod<- hclust(model,method = 'complete')
mod
#Plot the cluster
plot(mod)
plot(mod,hang = -1)
#cut the cluster
group <- cutree(mod,k=4)
class(group)

rect.hclust(mod,k=4,border = 'red')
mod_cluster <- as.matrix(group)

#append the cluster to the normal_data to know whch data pt belongs to which cluster
library(data.table)
final <- data.frame(normal_data,mod_cluster)
View(final)

setcolorder(final,c('mod_cluster'))
View(final)
#to find the avg of the cluster
View(aggregate(final,list(mod_cluster),FUN = mean))

################k-means method####################
normal_data <- scale(crime[,-1])
View(normal_data)

plot(normal_data)

wss <- (nrow(normal_data)-1*sum(apply(normal_data, 2, var)))
for (i in 2:6) wss[i] = sum(kmeans(normal_data,centers = i)$swithinss)
plot(1:6, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   
# Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Screw-Plot")
#ffinding the cluster by using kselection
install.packages("kselection")
library(kselection)
k <- kselection(normal_data,parallel = TRUE,k_threshold = 0.9,max_centers = 12)
k
#finding the cluster by using doParallel
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores = 7)
k <- kselection(normal_data,parallel = TRUE,k_threshold = 0.9,max_centers = 12)
k
