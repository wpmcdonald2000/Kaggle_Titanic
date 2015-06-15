# Load Data
titanic_train <- read.csv("~/Github/Kaggle_Titanic/Data/test.csv")
titanic_test <- read.csv("~/Github/Kaggle_Titanic/Data/test.csv")
# Data Exploration
str(titanic_train)


# Name is Factor??, NA's

# Find out how many NA's 
summary(titanic_train$Age)


# Lets fnd out how many in each class
table(titanic_train$Pclass)

# Survival Rate per Class??
prop.table(table(titanic_train$Survived, titanic_train$Pclass),1)
prop.table(table(titanic_train$Survived, titanic_train$Pclass),2)

# Survival by gender?? row-wise/ column-wise
prop.table(table(titanic_train$Survived, titanic_train$Sex), 1)
prop.table(table(titanic_train$Survived, titanic_train$Sex), 2)

# Females alone by class??
fem <- titanic_train[titanic_train$Sex == "female",]
prop.table(table(fem$Survived, fem$Pclass),1)
prop.table(table(fem$Survived, fem$Pclass),2)

# What about age?? Deal with NA's
summary(titanic_train$Age)
m_vals <- titanic_train[!complete.cases(titanic_train),]

# Combine data sets for added factors
# Add Survived column to test data set to na
titanic_test$Survived <- NA
combi <- rbind(titanic_train, titanic_test)

# Create Title variable
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)

# Combine
combi$Title[combi$Title %in% c('Capt','Col' ,'Don', 'Dr', 'Jonkheer','Major', 'Mr', 'Rev', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Mrs', 'Dona', 'Lady', 'Mme', 'the Countess')] <- 'Lady'
combi$Title[combi$Title %in% c('Miss', 'Mlle', 'Ms')] <- 'Miss'

# Assign all NA's to the median or the mean
# titanic$Age[is.na(titanic$Age) == T] <- 29.7 # does not appear to  add to model
# Looking at age as related to Pclass and Sex
aggregate(Age ~ Pclass + Sex, data = combi, FUN = median)
aggregate(Age ~ Pclass + Sex, data = combi, FUN = mean)

# Summary info Age by Title and Pclass
summary(combi$Age[combi$Title == "Master" & combi$Pclass == 1 ]) # Median = 6, same for Pclass = 3
summary(combi$Age[combi$Title == "Master" & combi$Pclass == 2 ]) # Median = 2
combi$Age[is.na(combi$Age == T & combi$Title == 'Master')] <- 6
summary(combi$Age[combi$Title == "Miss" & combi$Pclass == 1]) # Median = 30
summary(combi$Age[combi$Title == "Miss" & combi$Pclass == 2]) # Median = 20, same for Pclass = 3 
summary(combi$Age[combi$Title == "Mr" & combi$Pclass == 1]) # Median = 40
summary(combi$Age[combi$Title == "Mr" & combi$Pclass == 2]) # Median = 30
summary(combi$Age[combi$Title == "Mr" & combi$Pclass == 3]) # Median = 28
summary(combi$Age[combi$Title == "Lady" & combi$Pclass == 1])
summary(combi$Age[combi$Title == "Lady" & combi$Pclass == 2])
summary(combi$Age[combi$Title == "Lady" & combi$Pclass == 3])

# Restructure ages based on Title and Pclass
combi$Age[is.na(combi$Age) == T & combi$Title == "Master" & combi$Pclass == 3] <-  6
# combi$Age[is.na(combi$Age) == T & combi$Title == "Master" & combi$Pclass == 2] <-  2

combi$Age[is.na(combi$Age) == T & combi$Title == "Miss" & combi$Pclass == 1] <-  30
combi$Age[is.na(combi$Age) == T & combi$Title == "Miss" & combi$Pclass > 1] <-  20

combi$Age[is.na(combi$Age) == T & combi$Title == "Sir" & combi$Pclass == 1] <-  40
combi$Age[is.na(combi$Age) == T & combi$Title == "Sir" & combi$Pclass == 2] <-  30
combi$Age[is.na(combi$Age) == T & combi$Title == "Sir" & combi$Pclass == 3] <-  28

combi$Age[is.na(combi$Age) == T & combi$Title == "Lady" & combi$Pclass == 1] <-  45
combi$Age[is.na(combi$Age) == T & combi$Title == "Lady" & combi$Pclass == 2] <-  30
combi$Age[is.na(combi$Age) == T & combi$Title == "Lady" & combi$Pclass == 3] <-  31

# Add Family Size variable
combi$FamSize <- combi$Parch + combi$SibSp

# Split Fares into 4 classes: 30+, 20 - 30, 10 - 20, <10
combi$Fare2 <- '30+'
combi$Fare2[combi$Fare < 30] <- '20-30'
combi$Fare2[combi$Fare < 20] <- '10-20'
combi$Fare2[combi$Fare < 10] <- '<10'

# Addtional Features ???
# 

# Split train and test data set before applying model
train <- combi[1:891,]
test <- combi[892:1309,]

#####################
# Decision Tree model
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Fare2 + Title + FamSize + Embarked, data = train, method = 'class')

# Use fit model on test set
Prediction <- predict(fit, test, type = 'class')

#Create Data.Frame on test set
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

# Write to csv
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

install.packages('party')
library(party)

####################
# Gender Class Model
for (i in 1:891){
        if (titanic$Sex[i] == 'female' && titanic$Pclass[i] < 2.5){
                titanic$predict[i] = 1}
        else {
                if (titanic$Sex[i] == 'female' && titanic$Pclass[i] == 3  && titanic$famsize > .5 && titanic$famsize[i] < 3.5){
                titanic$predict[i] = 1}
        }
}
# run this on the test set
# write to .csv

#####################
# Random Forests Model (NA's removed as above)
library(randomForest)




######################
# GBM Model
library(caret)


