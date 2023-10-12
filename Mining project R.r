zomato <- read.csv("zomato.csv")

summary(zomato)

sum(is.na(zomato))

summary(zomato$rate)


summary(zomato$rate)


summary(zomato$avgCost)


boxplot.stats(zomato$rate)$out


boxplot.stats(zomato$avgCost)$out


boxplot.stats(zomato$numofratings)$out


boxplot(zomato$rate)


boxplot(zomato$numofratings)


boxplot(zomato$avgCost)


plot(zomato$rate,zomato$avgCost)


hist(zomato$avgCost)

onlineOrder <- table(zomato$online_order)
pie(onlineOrder)

summary (zomato$rate)


summary(zomato$numofratings)


summary(zomato$avgCost)


dim(zomato)


zomato =na.omit(zomato)


dim(zomato)


sum(is.na(zomato))


library(outliers)

OutN = outlier(zomato$numofratings, logical =TRUE)


sum(OutN)


Find_outlier = which(OutN ==TRUE, arr.ind = TRUE)


OutN

Find_outlier


zomato= zomato[-Find_outlier,]


OutAv = outlier(zomato$avgCost, logical =TRUE)
sum(OutAv)



Find_outlier = which(OutAv ==TRUE, arr.ind = TRUE)
OutAv


Find_outlier

zomato= zomato[-Find_outlier,]

head(zomato)

zomato$tablebooking=factor(zomato$tablebooking , levels =c("No","Yes"), labels = c(0,1))
zomato$online_order=factor(zomato$online_order , levels =c("No","Yes"), labels = c(0,1))


normalize <- function(x){return ((x-min(x))/(max(x)-min(x)))}

zomato$numofratings<- normalize(zomato$numofratings)
zomato$avgCost<- normalize(zomato$avgCost)

zomato$rate <- ifelse(zomato$rate <= 2, "Bad",
                  ifelse(zomato$rate <=3, "Okay",
                      ifelse(zomato$rate <=4, "Good",
                             ifelse(zomato$rate <=5, "Great",0 ))))


head(zomato)

newDataset <- read.csv("zomato.csv")
head(newDataset)
tb1 <- table(newDataset$online_order, newDataset$numofratings)
tb1
tb2 <- table(newDataset$online_order,newDataset$avgCost)
tb2
tb3 <- table(newDataset$online_order,newDataset$rate)
tb3
chisq.test(tb1)
chisq.test(tb2)
chisq.test(tb3)



