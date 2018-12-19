setwd("C:/Users/Admin/Documents/R Lessons")
data <- read.csv("C:/Users/Admin/Documents/R Lessons/train.csv")

installed.packages("class")
library(class)

library(randomForest)

data$PassengerId=NULL
data$Ticket=NULL
data$Fare=NULL
data$Cabin=NULL
data$Name=NULL
data$Embarked=NULL

age <- as.data.frame(data$Age)
age <- na.omit(age)
age$Aged <- age$`data$Age`

meanAge <- mean(age[, "Aged"])

data$Age[is.na(data$Age)] <- meanAge     

data$Pclass <- as.factor(data$Pclass)
data$Survived <- as.factor(data$Survived)
data$SibSp <- as.integer(data$SibSp)
data$Parch <- as.integer(data$Parch)

tree <- rpart(Survived ~ . , data=data)
print(tree$cptable)

forest <- randomForest(Survived ~ . , data=data)
print(forest)

fancyRpartPlot(tree)

data_test <- read.csv("C:/Users/Admin/Documents/R Lessons/test.csv")

data_test$PassengerId=NULL
data_test$Ticket=NULL
data_test$Fare=NULL
data_test$Cabin=NULL
data_test$Name=NULL
data_test$Embarked=NULL

data_test$Age[is.na(data_test$Age)] <- meanAge  

data_test$Pclass <- as.factor(data_test$Pclass)
data_test$SibSp <- as.integer(data_test$SibSp)
data_test$Parch <- as.integer(data_test$Parch)

data_test$Survived <- predict(forest,data_test)

#data_test$Survived <- 1-data_test$Survived

#meanSurv <- mean(data_test$Survived)

ggplot(data_test, aes(x=interaction(Pclass,Sex), y=Survived, col=Survived))+
  geom_jitter()

data_clean <- read.csv("C:/Users/Admin/Documents/R Lessons/test.csv")

data_test$PassengerID=data_clean$PassengerId

data_test_final <- data_test

data_test_final$Pclass=NULL
data_test_final$Sex=NULL
data_test_final$Age=NULL
data_test_final$SibSp=NULL
data_test_final$Parch=NULL
data_test_final$Survived=NULL

data_test_final$Survived <- data_test$Survived

summary(data)
summary(data_test_final)

write.csv(data_test_final, "Titanic _Test_Prediction.csv")