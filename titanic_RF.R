#####################
# Random Forests Model (NA's removed as above)
library(randomForest)
setwd("/Users/williammcdonald/Github/Kaggle_Titanic")

titanic_train <- read.csv("~/Github/Kaggle_Titanic/Data/train.csv")
titanic_test <- read.csv("~/Github/Kaggle_Titanic/Data/test.csv")

# Combine test and train to handle NA's in both data sets
titanic_test$Survived <- NA
all_data <- rbind(titanic_train, titanic_test)

# Exploration
summary(all_data)
# 2 blank Embarked records, 263 NA's for Age, multiple missing cabin info, 1 NA for Fare

# find blank Embarked, subsitute "S" 
all_data[all_data$Embarked == '',]
all_data$Embarked[c(62, 830)] <- "S"

# Missing Fare, replace with median 

all_data$Fare[1044] <- median(all_data$Fare, ma.rm = TRUE)

# Ages
# use rpart to predict ages of NA's based on formula Age ~ Pclass + Sex + ...
# Pull out titles and converge onto common core: Sir, Lady, Miss, Master
# Create Title variable
all_data$Name <- as.character(all_data$Name)
all_data$Title <- sapply(all_data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
all_data$Title <- sub(' ', '', all_data$Title)
all_data$Title[all_data$Title %in% c('Miss', 'Mlle', 'Ms')] <- 'Miss'
all_data$Title[all_data$Title %in% c('Mrs', 'Dona', 'Lady', 'Mme', 'the Countess')] <- 'Lady'
all_data$Title[all_data$Title %in% c('Capt','Col' ,'Don', 'Dr', 'Jonkheer','Major', 
                                     'Mr', 'Rev', 'Sir')] <- 'Sir'
# Convert Title to Factors for Random Forests Model 
all_data$Title <- as.factor(all_data$Title)
table(all_data$Title)

# Model Age
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title ,
                       data=all_data[!is.na(all_data$Age),], method="anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

# Split back into Train and Test set
train <- all_data[1:891,]
test <- all_data[892:1309,]

# Set seed for reproducibility
set.seed(111)

# Apply the Random Forest Algorithm

my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
                                  Embarked + Title, data = train, importance = TRUE, ntree = 1000 )
# Make your prediction using the test set
my_prediction <- predict(my_forest, test)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
rf_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(rf_solution, file = "rf_solution.csv", row.names = FALSE)
