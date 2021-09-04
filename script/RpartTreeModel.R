library(dplyr)
library(ISLR)
library(tidyverse)
library(modelr)
library(rpart)

#Pull all the files for the project into an array
getwd()
files <- list.files(path = "Data/")

#Put extract data
train_data <- read.csv(paste0("data/train.csv"), header = T, sep = ",")
test_data <- read.csv(paste0("data/test.csv"), header = T, sep = ",")

#reduce model to only features planned to be used
features <- c('SalePrice', 'LotArea', 'YearBuilt', 'X1stFlrSF', 'X2ndFlrSF', 'FullBath', 'BedroomAbvGr', 'TotRmsAbvGrd')
train_data<- train_data[, features]

#split data so that 30% is in the test set and 70% is in the training set
splitData <- resample_partition(train_data, c(test = 0.3, train = 0.7))
lapply(splitData, dim)

#fit to the decision tree model
set.seed(1)
dTree.model <- rpart(SalePrice ~ ., data = splitData$train)

#plot our regression tree
plot(dTree.model, uniform=TRUE)
text(dTree.model, cex=0.7)


#a function to get the max mse for a given max depth. 
get_mse <- function(maxDepth, minSplit, target, predictors, training_data, testing_data) {
  #turn the predictors & target into a formula to pass to rpart()
  predictors <- paste(predictors, collapse="+")
  formula <- as.formula(paste(target, "~", predictors, sep = ""))
  
  #build our model
  model <- rpart(formula, data = training_data, control = rpart.control(maxdepth = maxDepth, minsplit = minSplit))
  
  #get mse
  mse <- mse(model, testing_data)
  return(mse)
}

#target & predictors to feed into the formula
target <- "SalePrice"
predictors <- c("LotArea", "YearBuilt", "X1stFlrSF", "X2ndFlrSF", "FullBath", "BedroomAbvGr", "TotRmsAbvGrd")

#get the MSE for maxdepths between 1 & 10
for(i in 1:10) {
  for (j in 5:20) {
    mse <- get_mse(maxDepth = i, minSplit = j, target = target, predictors = predictors, training_data = splitData$train, testing_data = splitData$test)
    print(glue::glue("Maxdepth: ", i, "\t Minsplit: ", j,  "\t MSE: ", mse))
  }
}


#fit to the decision tree model on all the training data
dTree.model <- rpart(SalePrice ~ ., data = train_data, control = rpart.control(maxdepth = 5, minsplit = 8))

#plot our regression tree
plot(dTree.model, uniform=TRUE)
text(dTree.model, cex=0.7)

#make predictions on test_data
test_data$SalePrice <- predict(dTree.model, newdata = test_data)

#Features to print in submission file
submission_columns <- c('Id', 'SalePrice')
submission_data <- test_data[submission_columns]
write.csv(submission_data, file = "output/DTreeModel.csv", row.names = FALSE)


