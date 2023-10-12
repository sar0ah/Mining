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

set.seed(7)


library(mlbench)

library(caret)

library(randomForest)

zomato$rate <- as.factor(zomato$rate)


predictors <- zomato[, c(4,5, 6)] 
class_label <- zomato$rate


model <- randomForest(predictors, class_label, importance = TRUE)

importance <- importance(model)

ranked_features <- sort(importance[, "MeanDecreaseGini"], decreasing = TRUE)

print(ranked_features)

barplot(ranked_features, horiz = TRUE, las = 1, main = "Rate Variable Importance Ranking")

if (!require(caret)) {
  install.packages("caret")
}
if (!require(glmnet)) {
  install.packages("glmnet")
}

if (!require(e1071)) {
  install.packages("e1071")
}

library(caret)
library(glmnet)

x<-zomato[,c("rate","numofratings","avgCost")]
y<-zomato$online_order

CV_folds <- 5
train_control <- trainControl(method ="cv" , number = CV_folds)

method <- "glmnet"
model <- train(x, y, method = method, trControl = train_control, metric = "AUC")
selected_features <- varImp(model)
print(selected_features)

#Features selection:
# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
head(zomato)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(zomato[,11:11], zomato[,12], sizes=c(1:11), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
