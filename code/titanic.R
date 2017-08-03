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
#inner_join(full_data, gender_data, by = "PassengerId")

# Engineer Features ----

# Renaming features for convenience
full_data <- full_data %>% 
  rename(sibling_or_spouse = SibSp) %>% 
  rename(parent_or_child = Parch) %>% 
  rename(passenger_class = Pclass) 

engineered_data <- full_data %>% 
  mutate(title = gsub('(.*, )|(\\..*)', '', full_data$Name)) %>% 
  mutate(family_size = sibling_or_spouse + parent_or_child)

mean_age <- mean(engineered_data$Age, na.rm=TRUE)

features <- c('Survived', 
              'passenger_class',
              'Sex', 
              'Age', 
              'sibling_or_spouse', 
              'parent_or_child', 
              'Fare', 
              'title',
              'family_size')

imputed_engineered_data <- engineered_data %>% 
  mutate(Age = ifelse(is.na(Age) == TRUE, mean_age, Age)) %>% 
#  filter(!is.na(Embarked)) %>% 
  filter(!is.na(Fare)) %>% 
  select(one_of(features))

# Split Data ----

training_data <- imputed_engineered_data %>% 
  filter(!is.na(Survived))

test_data <- imputed_engineered_data %>% 
  filter(is.na(Survived))


# Modelling ----

logitMod <- glm(Survived ~ passenger_class + Sex + Age + sibling_or_spouse + parent_or_child + Fare, data=training_data, family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, test_data))  # predicted scores
# or
predicted <- predict(logitMod, test_data, type="response")  # predicted scores

threshold <- 0.5 #above which they survive, below which they do not

binary_prediction <- data.frame(ifelse(predicted >= 0.5, 1, 0))
names(binary_prediction) <- "prediction"

gender_data <- filter(gender_data, PassengerId != 1044)

data <- cbind(gender_data, binary_prediction)
