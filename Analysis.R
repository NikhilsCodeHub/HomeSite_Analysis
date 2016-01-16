
###  Load Packages

library(randomForest)
library(caret)
library(dplyr)



dtrain <- read.csv("train.csv", stringsAsFactors = TRUE, na.strings = c(""," ", "NaN", "NA", "Inf"))
dim(dtrain)
## 260753    299

## Field, CoverageField, SalesField, PersonalField, 
## PropertyField, GeographicField
## 

## Identify columns having NA values
na.cols <- sapply(dtrain[,1:dim(dtrain)[2]],anyNA)

na.cols <- data.frame(ColNames=rownames(data.frame(na.cols)),ColNA = na.cols)

na.cols[na.cols$ColNA,]

#                            ColNames ColNA
# PersonalField7       PersonalField7  TRUE
# PersonalField84     PersonalField84  TRUE
# PropertyField3       PropertyField3  TRUE
# PropertyField4       PropertyField4  TRUE
# PropertyField29     PropertyField29  TRUE
# PropertyField32     PropertyField32  TRUE
# PropertyField34     PropertyField34  TRUE
# PropertyField36     PropertyField36  TRUE
# PropertyField38     PropertyField38  TRUE
# GeographicField63 GeographicField63  TRUE

## Lets group Dataset columns based on name : 
## Field, PersonalField, PropertyField, GeographicField, SalesField, CoverageField

df_personal <- select(dtrain, starts_with("PersonalField"))


df_personal$PersonalField84[is.na(df_personal$PersonalField84)] <- "0"
df_personal$PersonalField84 <- factor(df_personal$PersonalField84)
lev<-levels(dtrain$PersonalField84)
lev[length(lev)+1] <- "0"
levels(dtrain$PersonalField84) <-  lev

## Verify 
## table(df_personal$PersonalField84 , useNA = "ifany")


table(df_personal$PersonalField7 , useNA = "ifany")

levels(dtrain$PersonalField7) <- c("N", "Y", "0")
dtrain$PersonalField7[is.na(dtrain$PersonalField7)]<-"0"


## Identify each column as factor or continuous variable

## If Factor then identify if NA values should be factor 0
##  OR be imputed based on other factors.
##
## If columns have more than 50 factors then
##  identify ways to split the data and create new columns and refactor.
##    Eg : XG, XA, YB, YD   split values to :>  X | G, X | A, Y | B  so on..
##    PersonalField16, PersonalField17, PersonalField18, PersonalField19
  

  df_personal$PersonalField16A <- substr(x = df_personal$PersonalField16,start = 1,stop = 1)
  df_personal$PersonalField16B <- substr(x = df_personal$PersonalField16,start = 2,stop = 2)
  
  df_personal$PersonalField17A <- substr(x = df_personal$PersonalField17,start = 1,stop = 1)
  df_personal$PersonalField17B <- substr(x = df_personal$PersonalField17,start = 2,stop = 2)
  
  df_personal$PersonalField18A <- substr(x = df_personal$PersonalField18,start = 1,stop = 1)
  df_personal$PersonalField18B <- substr(x = df_personal$PersonalField18,start = 2,stop = 2)
  
  df_personal$PersonalField19A <- substr(x = df_personal$PersonalField19,start = 1,stop = 1)
  df_personal$PersonalField19B <- substr(x = df_personal$PersonalField19,start = 2,stop = 2)
  
  train_df_personal <- df_personal[!is.na(df_personal$PersonalField7),]
  train_df_personal <- select(train_df_personal, -tempA, -PersonalField16,-PersonalField17, -PersonalField18, -PersonalField19)  

  test_df_personal <- df_personal[is.na(df_personal$PersonalField7),]

  glm_fit <- glm(as.factor(PersonalField7)~., train_df_personal, family=binomial("logit"))
  
  predict(glm_fit, test_df_personal)
  
## unfactor any columns with nlevels > 15


##  Methods to approach this running models
##
##  1. Groupd Similar columns together and run PCH.
##    a. Run RandomForest on the combo
##    b. Run GLM on the compbo
##  2. Run GLM on the entire Dataset
##  3. Run RandomForest on the entire Dataset
##

table(dtrain$GeographicField63, useNA = "ifany")




ntrain <- tbl_df(dtrain)
ntrain <- select(ntrain, -PersonalField84, -PropertyField29, -Original_Quote_Date, -PersonalField7, -GeographicField63)

## unfactor those columns as they have more than 50 factors
ntrain$PersonalField16 <- as.character(ntrain$PersonalField16)
ntrain$PersonalField17 <- as.character(ntrain$PersonalField17)
ntrain$PersonalField18 <- as.character(ntrain$PersonalField18)
ntrain$PersonalField19 <- as.character(ntrain$PersonalField19)
ntrain$PropertyField7 <- as.character(ntrain$PropertyField7)
ntrain$QuoteConversion_Flag <- factor(ntrain$QuoteConversion_Flag)

# ntrain$GeographicField63 <- as.character(ntrain$GeographicField63)
# ntrain$GeographicField63[is.na(ntrain$GeographicField63)] <- ""
# ntrain$GeographicField63 <- factor(ntrain$GeographicField63)


## Starting with RandomForest
set.seed(2016)

glm_fit <- glm(QuoteConversion_Flag~., data=ntrain, family=binomial("logit"))





## Load Test Data

dtest <- read.csv("test.csv", stringsAsFactors = FALSE)


## Clean Test Data

dtest <- select(dtest, -PersonalField84, -PropertyField29, -Original_Quote_Date, -PersonalField7, -GeographicField63)

dtest$PersonalField16 <- as.character(dtest$PersonalField16)
dtest$PersonalField17 <- as.character(dtest$PersonalField17)
dtest$PersonalField18 <- as.character(dtest$PersonalField18)
dtest$PersonalField19 <- as.character(dtest$PersonalField19)
dtest$PropertyField7 <- as.character(dtest$PropertyField7)
dtest$QuoteConversion_Flag <- factor(dtest$QuoteConversion_Flag)


## Predct Test Data

set.seed(2016)
pred_rf <- predict(glm_fit, dtest)


# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(QuoteNumber = dtest$QuoteNumber, QuoteConversion_Flag = pred_rf)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file=paste0("HomeSite_Submission_", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)




