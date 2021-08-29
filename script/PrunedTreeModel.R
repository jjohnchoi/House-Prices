library(dplyr)
library(ISLR)
require(tree)

#Pull all the files for the project into an array
getwd()
files <- list.files(path = "Data/")