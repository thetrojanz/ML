install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

#Invoking the arules and arulesViz packages to create the rules and to visualize the created rules
#In association data as to be read in transaction format, and the format should be in basket

data()
data("Groceries")
summary(Groceries)
groc <- Groceries

inspect(groc[1:100])
#Inspect is used to Display Associations and Transactions in Readable Form

class(groc)


#for support = 0.002,confi = 0.5,minlen = 2
groc1 <- apriori(groc,parameter = list(support = 0.002,confidence = 0.5,minlen=2))
#The Apriori Algorithm is an influential algorithm for mining 
#frequent itemsets for boolean association rules.

groc1   #1098 rules

inspect(groc1)
inspect(sort(groc1,by = 'lift'))
inspect(head(sort(groc1,by = 'lift')))

#Lift > 1 indicates a rule that is useful in finding consequent items sets
#Visualize the obtained rules by using plot
plot(groc1)
head(quality(groc1))
plot(groc1,method = 'grouped')


#for support = 0.003,confi = 0.7,minlen = 3
groc1 <- apriori(groc,parameter = list(support = 0.002,confidence = 0.5,minlen=3))
groc1 #1092 rules
#####If i consider the minlen = 3 then i'll get 1092 rules

inspect(groc1)
inspect(sort(groc1,by = 'lift'))
inspect(head(sort(groc1,by = 'lift')))

#Lift > 1 indicates a rule that is useful in finding consequent items sets  
#Visualize the obtained rules by using plot
plot(groc1)
head(quality(groc1))
plot(groc1,method = 'grouped')

#for support = 0.003,confi = 0.7,minlen = 4

groc1 <- apriori(groc,parameter = list(support = 0.002,confidence = 0.5,minlen=4))
groc1 #516 rules
#If i consider the minlen = 4 then i'll get 516 rules
inspect(groc1)
inspect(sort(groc1,by = 'lift'))
inspect(head(sort(groc1,by = 'lift')))

#Lift > 1 indicates a rule that is useful in finding consequent items sets  
#Visualize the obtained rules by using plot
plot(groc1)
head(quality(groc1))
plot(groc1,method = 'grouped')

#for support = 0.003,confi = 0.7,minlen = 5

groc1 <- apriori(groc,parameter = list(support = 0.002,confidence = 0.5,minlen=5))
groc1 #45 rules
#If we consider the minlen = 5 then we will get 45 rules
inspect(groc1)
inspect(sort(groc1,by = 'lift'))
inspect(head(sort(groc1,by = 'lift')))

#Lift > 1 indicates a rule that is useful in finding consequent items sets  
#Visualize the obtained rules by using plot
plot(groc1)
head(quality(groc1))
plot(groc1,method = 'grouped')

