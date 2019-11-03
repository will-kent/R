library(tidyverse)

# Check working directory set correctly
#getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read csv's
train <- read.csv(".\\Data\\train.csv")
test <- read.csv(".\\Data\\test.csv")

# Create test set for union with train set for data wrangling, make survived = -1 for completeness
test_union <- test
test_union$Survived <- -1

all <- union(train, test_union)
all

##################
# Data Wrangling #
##################
## Name
all %>% 
  distinct(Name)
# Last Name, Title, Given Names format

## Age
all %>% 
  filter(!is.na(Age)) %>% 
  group_by(Sex) %>% 
  summarise(mean(Age))
# Males slightly order, on average

all %>% 
  filter(!is.na(Age)) %>% 
  group_by(Pclass) %>% 
  summarise(mean(Age))
# Younger people in lower classes

all %>% 
  filter(!is.na(Age)) %>% 
  group_by(Sex, Survived) %>% 
  summarise(mean(Age))
# On average older females survived yet younger males survived

all %>% 
  filter(is.na(Age))
#94 records where Age = NA

all %>% 
  filter(!is.na(Age)) %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = Age), binwidth = 3) +
  ggtitle("Age distribution of passengers")
# Mainly younger adults

## Ticket
all %>% 
  group_by(Ticket) %>% 
  summarise("total" = n()) %>% 
  arrange(desc(total))
# tickets are duplicated

all %>% 
  filter(Ticket %in% c("1601", "347082", "CA. 2343")) %>% 
  arrange(Ticket)
# Seems to be that families (or at least groups) were put on the same ticket

#### Does ticket prefix mean something?

## Embarked
all %>% 
  group_by(Embarked) %>% 
  summarise("count" = n())
# Some missing embarkation points

all %>% 
  filter(Embarked == "")

all %>% 
  filter(Ticket == "113572")
# No one else on the ticket


##################
# Data Decisions #
##################
# Break up the passenger name into Title, Last Name and "Other Names" 
all <- all %>% 
  separate(Name,c("LastName","Remaining"), sep = ", ", extra = "merge") %>% 
  separate(Remaining, c("Title", "OtherNames"), sep = ". ", extra = "merge")
