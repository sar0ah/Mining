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

# ensure the results are repeatable
set.seed(7)

# load the library

if (!require(caret)) {
  install.packages("caret")
}
if (!require(glmnet)) {
  install.packages("glmnet")
}

if (!require(e1071)) {
  install.packages("e1071")
}
if (!require(randomForest)) {
  install.packages("randomForest")
}
library(randomForest)
library(caret)
library(glmnet)
library(e1071)


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


# Load the dataset
head(zomato)

# Convert the class label to a factor
zomato$online_order <- as.factor(zomato$online_order)

# Separate the predictors and the class label
predictors <- zomato[, -12]  # Excluding the class label (diabetes)
class_label <- zomato$`online_order `

# Train a Random Forest model
model <- randomForest(predictors, class_label, importance = TRUE)

# Get the variable importance
importance <- importance(model)

# Rank the features by importance
ranked_features <- sort(importance[, "MeanDecreaseGini"], decreasing = TRUE)

# Print the ranked features
print(ranked_features)
# Rank the features by importance
ranked_features <- sort(importance$MeanDecreaseGini, decreasing = TRUE)

# Print the ranked features
print(ranked_features)




# Load the data
head(zomato)

# Define the control parameters for RFE using random forest selection function
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# Extract the predictor variables from zomato
predictors <- zomato[, -ncol(zomato)]

# Convert the outcome variable to a factor
outcome <- as.factor(zomato$online_order)

# Run the RFE algorithm
results <- rfe(predictors, outcome, sizes = 1:ncol(zomato), rfeControl = control)

# Summarize the results
print(results)

# List the chosen features selected by RFE
predictors(results)

# Plot the results
plot(results, type = c("g", "o"))
