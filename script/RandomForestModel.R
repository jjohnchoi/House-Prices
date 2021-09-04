library(dplyr)
library(ISLR)
library(tidyverse)
library(randomForest)

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
rForest.model <- randomForest(SalePrice ~ ., data = splitData$train, importance = TRUE)

#get the mse
mse(rForest.model, splitData$test)

#fit the random forest to full model
set.seed(1)
rForest.model <- randomForest(SalePrice ~ ., data = train_data, importance = FALSE)

#make predictions on test_data
test_data$SalePrice <- predict(rForest.model, newdata = test_data)

#Features to print in submission file
submission_columns <- c('Id', 'SalePrice')
submission_data <- test_data[submission_columns]
write.csv(submission_data, file = "output/RandomForestModel.csv", row.names = FALSE)