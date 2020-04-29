library(arules)
library(arulesViz)

data()

mov <- read.transactions(file.choose())
summary(mov)

inspect(movi)
class(movi)


##for support = 0.002,confidence = 0.5,minlen = 2
movi <- apriori(mov,parameter = list(support=0.002,confidence = 0.5,minlen=2))
#The Apriori Algorithm is an influential algorithm for mining 
#frequent itemsets for boolean association rules.
movi
#for the above performance measures will get 245 rules

inspect(movi,by ='lift')
inspect(head(sort(movi, by='lift')))

plot(movi)
head(quality(movi))

plot(movi,method = 'group')


#for support = 0.003,confidence = 0.7,minlen = 3
movi <- apriori(mov,parameter = list(support=0.003,confidence = 0.7,minlen=3))
movi  
#for the above performance measures will get 181 rules

inspect(movi,by ='lift')
inspect(head(sort(movi, by='lift')))

plot(movi)
head(quality(movi))

plot(movi,method = 'group')


#for support = 0.03,confidence = 0.75,minlen = 4

movi <- apriori(mov,parameter = list(support=0.03,confidence = 0.75,minlen=4))
movi  
#for the above performance measures will get 100 rules

inspect(movi,by ='lift')
inspect(head(sort(movi, by='lift')))

plot(movi)
head(quality(movi))

plot(mov,method = 'group')


#for support = 0.003,confidence = 0.7,minlen = 1
movi <- apriori(mov,parameter = list(support=0.3,confidence = 0.8,minlen=1))
movi  
#for the above performance measures will get 181 rules

inspect(movi,by ='lift')
inspect(head(sort(movi, by='lift')))

plot(movi)
head(quality(movi))

plot(movi,method = 'group')
