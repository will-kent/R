library(tidyverse)

# Check working directory set correctly
#getwd()
#setwd("C:\\Users\\will.kent\\source\\repos\\R\\Machine Learning\\Kaggle\\Titanic")

# Read csv's
train <- read.csv(".\\Data\\train.csv")
test <- read.csv(".\\Data\\test.csv")

##################
# Data Decisions #
##################
# Break up the passenger name into Title, Last Name and "Other Names" 
train <- train %>% 
  separate(Name,c("LastName","Remaining"), sep = ", ", extra = "merge") %>% 
  separate(Remaining, c("Title", "OtherNames"), sep = ". ", extra = "merge")