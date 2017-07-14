# Libraries and Dependencies ----
library(readr)
library(ggplot2) 
library(dplyr) 
library(mice) 

# Read in data ----
tain_data <- read_csv("~/projects/my_titanic/data/train.csv")
test_data <- read_csv("~/projects/my_titanic/data/test.csv")
gender_data <- read_csv("~/projects/my_titanic/data/gender_submission.csv")

# Create full data set ----
full_data <- bind_rows(tain_data, test_data)

# Engineer Features ----

# Renaming features for convenience
full_data <- full_data %>% 
  rename(sibling_or_spouse = SibSp) %>% 
  rename(parent_or_child = Parch) %>% 
  rename(passenger_class = Pclass) 

engineered_data <- full_data %>% 
  mutate(title = gsub('(.*, )|(\\..*)', '', full_data$Name)) %>% 
  mutate(family_size = sibling_or_spouse + parent_or_child)

for (i in length(colnames(engineered_data))){
  sum(is.na(engineered_data[,i]))
}