#################
# GBM Model (Caret package)
library(caret)

titanic_train <- read.csv("~/Github/Kaggle_Titanic/Data/tidy_train.csv")
titanic_test <- read.csv("~/Github/Kaggle_Titanic/Data/tidy_test.csv")

# Set seed for reproducibility
set.seed(111)

str(tidy_train)
 
gbmfit <- train(as.factor(Survived) ~ Pclass + Sex +  Age + Fare2 + Parch + SibSp 
                + Title + Embarked, data = titanic_train, method = "gbm", verbose = FALSE )

predictiom <- predict(gbmfit, titanic_test)

gbm_solution <- data.frame(PassengerId = titanic_test$PassengerId, Survived = gbm_prediction)

write.csv(gbm_solution, file = "gbm_solution.csv", row.names = FALSE)


