
###  Load Packages

library(rpart)
library(randomForest)
library(caret)
library(dplyr)



dtrain <- read.csv("train.csv", stringsAsFactors = FALSE, na.strings = c(""," ", "NaN", "NA", "Inf"))
dim(dtrain)
## 260753    299

## Field, CoverageField, SalesField, PersonalField, 
## PropertyField, GeographicField
## 

## Identify columns having NA values




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
lev<-levels(df_personal$PersonalField84)
lev[length(lev)+1] <- "0"
levels(df_personal$PersonalField84) <-  lev

## Verify 
## table(df_personal$PersonalField84 , useNA = "ifany")


table(df_personal$PersonalField7 , useNA = "ifany")

# levels(dtrain$PersonalField7) <- c("N", "Y", "0")
# dtrain$PersonalField7[is.na(dtrain$PersonalField7)]<-"0"


## Identify each column as factor or continuous variable

## If Factor then identify if NA values should be factor 0
##  OR be imputed based on other factors.
##
## If columns have more than 50 factors then
##  identify ways to split the data and create new columns and refactor.
##    Eg : XG, XA, YB, YD   split values to :>  X | G, X | A, Y | B  so on..
##    PersonalField16, PersonalField17, PersonalField18, PersonalField19
  

 
  
  ## ---------------------------------------------------------
  ## Imputing for missing values in PeronalField7 using rPart.
  ## ---------------------------------------------------------
  ## 

  



  ## ---------------------------------------------------
  ## Imputing missing values in PropertyField
  ## ---------------------------------------------------
  ## 

  

  
  df_property <- select(dtrain, starts_with("PropertyField"))
  

  ## ---------------------------------------------------
  ## Converting to Factors where possible
  ## ---------------------------------------------------
  ## 
  
  for (col in colnames(dtrain_final)[1:10]){
    
    print(paste("Processing ",col))
    check_factor_levels(col_vector = dtrain_final[,col], level_vector = dfSummary[dfSummary$colNames==col,"levels"])
    
  }
    
  ## List NA columns
  na_prop_cols <- grep("PropertyField", na.cols$ColNames, value = TRUE)
  
  

  df_geo <- select(dtrain, starts_with("GeographicField"))
  table(df_geo$GeographicField63, useNA = "ifany")
  df_geo[(is.na(df_geo$GeographicField63)),"GeographicField63"] <- "N"

  ## ---------------------------------------------------
  ## Combining All Datasets
  ## ---------------------------------------------------  
  
  dtrain_final <- select(dtrain, -starts_with("PropertyField"))
  dtrain_final <- select(dtrain_final, -starts_with("PersonalField"))
  dtrain_final <- select(dtrain_final, -starts_with("GeographicField"), -Original_Quote_Date, -QuoteNumber)
  df_personal <- select(df_personal, -PersonalField16,-PersonalField17, -PersonalField18, -PersonalField19)  
  dtrain_final <- cbind(dtrain_final, df_personal, df_property, df_geo)

  
  ## --------------------------------------------------
  ## Split Training Set into 2 for model validation
  ## --------------------------------------------------

  set.seed(2016)
  rlst <- createDataPartition(dtrain_final$QuoteConversion_Flag, p = 0.75, list = FALSE)
  rtrain <- dtrain_final[rlst,]
  rvalidate <- dtrain_final[-rlst,]
  
  ## --------------------------------------------------
  ## Build A Model Fit
  ## --------------------------------------------------
  
  date()
  set.seed(2016)
  rfit <- train(as.factor(QuoteConversion_Flag)~., data =  rtrain, method="rpart")  
  
  date()
  set.seed(2016)
  rf_fit <- train(as.factor(QuoteConversion_Flag)~., data = rtrain, method="rf")
  
  date()
  set.seed(2016)
  glm_fit <- glm(QuoteConversion_Flag~., data=rtrain, family=binomial("logit")) 
  date()
  
  ## --------------------------------------------------
  ## Prediction
  ## --------------------------------------------------

  rf_pred <- predict(rf_fit, rvalidate)
  
  
  
  
  
  
  
  
  
  
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


glm_fit <- glm(QuoteConversion_Flag~., data=ntrain, family=binomial("logit"))





## Load Test Data

dtest <- read.csv("test.csv", stringsAsFactors = FALSE, na.strings = c(""," ", "NaN", "NA", "Inf"))



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




