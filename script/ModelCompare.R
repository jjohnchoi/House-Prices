library(dplyr)
library(ISLR)
require(tree)
library(tidyverse)
library(modelr)
library(rpart)
library(randomForest)

#Compare the MSE between Tested Models
#Pull all the files for the project into an array
getwd()
files <- list.files(path = "Data/")

#Put extract data
train_data <- read.csv(paste0("data/train.csv"), header = T, sep = ",")

#reduce model to only features planned to be used
features <- c('SalePrice', 'LotArea', 'YearBuilt', 'X1stFlrSF', 'X2ndFlrSF', 'FullBath', 'BedroomAbvGr', 'TotRmsAbvGrd')
train_data<- train_data[, features]

#k-fold cross validation
k <- 5

set.seed(1)
n_row <- nrow(train_data)
folds <- sample(1:k, n_row, replace = TRUE)
tree.errors <- as.numeric(k)
dTree.errors <- as.numeric(k)
modDTree.errors <- as.numeric(k)
rForest.errors <- as.numeric(k)

for(i in 1:k) {
  cv_train_data <- train_data[folds != i,]
  cv_test_data <- train_data[folds == i, ]
  
  #test the tree model
  tree.model <- tree(SalePrice ~., data = cv_train_data)
  tree.errors[i] <- mse(tree.model, cv_test_data)
  
  #test the rpart model
  dTree.model <- rpart(SalePrice ~., data = cv_train_data)
  dTree.errors[i] <- mse(dTree.model, cv_test_data)
  
  #test the tuned rpart model
  modDTree.model <- rpart(SalePrice ~ ., data = cv_train_data, control = rpart.control(maxdepth = 5, minsplit = 8))
  modDTree.errors[i] <- mse(modDTree.model, cv_test_data)
  
  #test the randomForest model
  rForest.model <- randomForest(SalePrice ~ ., data = cv_train_data, importance = FALSE)
  rForest.errors[i] <- mse(rForest.model, cv_test_data)
}

#Plot out the rates
mse_rates <- c(mean(tree.errors), mean(dTree.errors), mean(modDTree.errors), mean(rForest.errors))
n <- length(mse_rates)
plot(1:n, mse_rates, main = "5-fold Training Cross-Validation", xlab = "Models", ylab = "Prediction MSE", type = "b", xaxt="n")
axis(1, at=1:n, labels = c("Tree", "Rpart Tree", "Rpart Mod Tree", "Random Forest"), las=1)
