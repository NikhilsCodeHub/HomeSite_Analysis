# Load data

library(randomForest)
library(rpart)
library(caret)


dtrain <- read.csv("train.csv", header = TRUE, na.strings = c(" ", "", "NA"))


dtest <- read.csv("test.csv", header = TRUE, na.strings = c(" ", "", "NA"))

# cleaning data and Imputing NA values

na.cols <- sapply(dtrain[,1:dim(dtrain)[2]],anyNA)

na.cols


##----- Basic RF Model
##---

rf_fit <- randomForest(formula = dtrain$QuoteConversion_Flag ~ ., data = dtrain, ntrees = 500)

pred <- predict(rf_fit, dtest)




ImputeData <- function (df_train)
{
## Extract the title of the passengers

ptitle_match<-regexpr("Mr\\.|Mrs\\.|Master\\.|Miss\\.|Ms\\.",text =  df_train$Name)

ptitles <- substr(df_train$Name, ptitle_match, ptitle_match+attr(ptitle_match, "match.length")-2)

table(ptitles)
##    Master   Miss     Mr    Mrs     Ms 
## 26     40    182    517    125      1 

df_train <- cbind(df_train, data.frame("titles"=ptitles))

df_train$titles[df_train$titles=="" & df_train$Sex=="male" & df_train$Age>12 & !is.na(df_train$Age)] <- "Mr"

df_train$titles[df_train$titles=="" & df_train$Sex=="male" & df_train$Age<13 & !is.na(df_train$Age)] <- "Master"

# If sex=female and ((age>18 and parch=0 and SibSp=0) or (Age<19) then title=Miss
df_train$titles[df_train$titles=="" & (df_train$Sex=="female" & df_train$Age>18 & !is.na(df_train$Age) & df_train$Parch==0 & df_train$SibSp==0) | (df_train$Age<19)] <- "Miss"

# If sex=female and age>18 and (parch=1 or SibSp=1) then title=Mrs
df_train$titles[df_train$titles=="" & df_train$Sex=="female" & df_train$Age>18 & !is.na(df_train$Age) & (df_train$Parch==1 | df_train$SibSp==1)] <- "Mrs"

df_train$titles[df_train$titles=="Ms"] <- "Miss"

df_train$titles[df_train$titles=="" & df_train$Sex=="male"] <- "Mr"

df_train$titles <- factor(df_train$titles)

## In case Fare is NA in test data.
df_train$Fare[is.na(df_train$Fare)] <- mean(df_train$Fare, na.rm = TRUE)

## df_train<- data.frame(df_train$titles, stringsAsFactors = FALSE)

## Identify family groups 
grouplist <- regexpr(",",text =  df_train$Name)

df_train<- cbind(df_train, data.frame(groupName = substr(df_train$Name, 0,grouplist-1), stringsAsFactors = FALSE))

# As the majority of distribution is 'S'. We assign that to NA.
df_train[is.na(df_train$Embarked),"Embarked"] <- "S"

df_train$Pclass <- factor(df_train$Pclass)

with_age_df_train <- df_train[!is.na(df_train$Age), ]
without_age_df_train <- df_train[is.na(df_train$Age), ]


# Fitting glm model to impute missing Age

fit_age <- train(Age~Pclass+Sex+SibSp+Parch+Fare+Embarked+titles, data = with_age_df_train, method="glm") 

pred_age <- predict(fit_age, without_age_df_train)

df_train[is.na(df_train$Age),"Age"] <- round(pred_age)
## Alternative: df_train[without_age_df_train$PassengerId,"Age"] <- round(pred_age)

df_train$Age<-round(df_train$Age)

return(df_train)
}

