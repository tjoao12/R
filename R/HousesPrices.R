install.packages("class")
library(class)
install.packages("randomForest")
library(randomForest)
install.packages("rgl")
library(rgl)
install.packages("rpart")
library(rpart)
install.packages("rattle")
library(rattle)
install.packages("e1071")
library(e1071)

setwd("C:\\Users\\Admin\\R programmes/")
uncleantrain <- read.csv("train.csv")
uncleantest <- read.csv("test.csv")

uncleantest$SalePrice <- 0

uncleandata <- rbind(uncleantrain, uncleantest)

none <- function(table, colname){
  unclean <- table
  column <- colname
  levels <- levels(column)
  levels[length(levels) + 1] <- "None"
  column <- factor(column, levels = levels)
  unclean$new <- column
  unclean$new[is.na(unclean$new)] <- "None"
  column <- unclean$new
}

NAs <- function(table, column){
  trainingdata <- table
  col <- column
  col <- as.factor(col)
  val <- none(trainingdata, col)
  col <- val
  return(col)
}

valuing <- function(table){
  uncleantrain <- table
  
  uncleantrain$GarageType <- NAs(uncleantrain, uncleantrain$GarageType)
  uncleantrain$BsmtFinType1 <- NAs(uncleantrain, uncleantrain$BsmtFinType1)
  uncleantrain$BsmtExposure <- NAs(uncleantrain, uncleantrain$BsmtExposure)
  uncleantrain$BsmtQual <- NAs(uncleantrain, uncleantrain$BsmtQual)
  uncleantrain$Electrical <- NAs(uncleantrain, uncleantrain$Electrical)
  uncleantrain$PoolQC <- NAs(uncleantrain, uncleantrain$PoolQC)
  uncleantrain$FireplaceQu <- NAs(uncleantrain, uncleantrain$FireplaceQu)
  uncleantrain$Fence <- NAs(uncleantrain, uncleantrain$Fence)
  uncleantrain$MSZoning <- NAs(uncleantrain, uncleantrain$MSZoning)
  uncleantrain$Exterior1st <- NAs(uncleantrain, uncleantrain$Exterior1st)
  uncleantrain$Exterior2nd <- NAs(uncleantrain, uncleantrain$Exterior2nd)
  uncleantrain$KitchenQual <- NAs(uncleantrain, uncleantrain$KitchenQual)
  uncleantrain$MiscFeature <- NAs(uncleantrain, uncleantrain$MiscFeature)
  uncleantrain$BsmtCond <- NAs(uncleantrain, uncleantrain$BsmtCond)
  uncleantrain$MasVnrType[is.na(uncleantrain$MasVnrType)] <- "None"
  uncleantrain$GarageFinish <- NAs(uncleantrain, uncleantrain$GarageFinish)
  uncleantrain$GarageQual <- NAs(uncleantrain, uncleantrain$GarageQual)
  uncleantrain$GarageCond <- NAs(uncleantrain, uncleantrain$GarageCond)
  uncleantrain$Functional <- NAs(uncleantrain, uncleantrain$Functional)
  return(uncleantrain)
}

data <- valuing(uncleandata)

data$LandContour <- NULL
data$Street <- NULL
data$Alley <- NULL
data$Utilities <- NULL
data$LandSlope <- NULL
data$BsmtFinType2 <- NULL
data$BsmtFinSF2 <- NULL
data$LowQualFinSF <- NULL
data$SsnPorch <- NULL
data$ScreenPorch <- NULL
data$MiscVal <- NULL
data$Id <- NULL
data$GarageYrBlt <- NULL
data$YrSold <- NULL
data$SaleType <- NULL

data$GarageCars[is.na(data$GarageCars)] <- 0
data$GarageArea[is.na(data$GarageArea)] <- 0
data$MasVnrArea[is.na(data$MasVnrArea)] <- 0
data$LotFrontage[is.na(data$LotFrontage)] <- 0
data$BsmtFinSF1[is.na(data$BsmtFinSF1)] <- 0
data$BsmtUnfSF[is.na(data$BsmtUnfSF)] <- 0
data$TotalBsmtSF[is.na(data$TotalBsmtSF)] <- 0
data$BsmtFullBath[is.na(data$BsmtFullBath)] <- 0
data$BsmtHalfBath[is.na(data$BsmtHalfBath)] <- 0

data$MSSubClass <- as.factor(data$MSSubClass)
data$YearBuilt <- as.integer(data$YearBuilt)
data$YearRemodAdd <- as.integer(data$YearRemodAdd)

FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }
Normalised <- function(data){FeatureScaling(data)}

data$YearBuilt <- Normalised(data$YearBuilt)
data$YearRemodAdd <- Normalised(data$YearRemodAdd)

train <- data [1: 1460,]
test <- data[1461:2919,]
test$SalePrice <- NULL

fit <- svm(SalePrice ~. , data=train)

test$SalePrice <- predict(fit, test)

prediction <- as.data.frame(uncleantest$Id)
prediction$Id <- uncleantest$Id
prediction$SalePrice <- test$SalePrice
prediction$'uncleantest$Id' <- NULL