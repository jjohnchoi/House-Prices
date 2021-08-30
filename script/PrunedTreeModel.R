library(dplyr)
library(ISLR)
require(tree)

#Pull all the files for the project into an array
getwd()
files <- list.files(path = "Data/")

#Put extract data
train_data <- read.csv(paste0("data/train.csv"), header = T, sep = ",")
test_data <- read.csv(paste0("data/test.csv"), header = T, sep = ",")

#reduce model to only features planned to be used
features <- c('SalePrice', 'LotArea', 'YearBuilt', 'X1stFlrSF', 'X2ndFlrSF', 'FullBath', 'BedroomAbvGr', 'TotRmsAbvGrd')
train_data<- train_data[, features]

#fit to the regressor tree model
set.seed(1)
tree.model <- tree(SalePrice ~ ., data = train_data)

#review the tree model
summary(tree.model)
plot(tree.model)
text(tree.model, pretty = 0, cex = 0.7)

#Check to see if pruning the tree would yield results
cv.tree.model <- cv.tree(tree.model)
plot(cv.tree.model$size, cv.tree.model$dev, type = 'b')

#plots confirm that 14 nodes produces lowers cv error rate
prune.tree.model <- prune.tree(tree.model, best = 14)
plot(prune.tree.model)
text(prune.tree.model, pretty = 0, cex = 0.7)

#make predictions on train_data
train_data$yhat <- predict(prune.tree.model, newdata = train_data)

#plot predictions on actuals and calculate MSE
plot(train_data$yhat, train_data$SalePrice)
abline(0,1)
mean((train_data$yhat - train_data$SalePrice)^2)

#make predictions on test_data
test_data$SalePrice <- predict(prune.tree.model, newdata = test_data)

#Features to print in submission file
submission_columns <- c('Id', 'SalePrice')
submission_data <- test_data[submission_columns]
write.csv(submission_data, file = "output/PrunedTreeModel.csv", row.names = FALSE)


