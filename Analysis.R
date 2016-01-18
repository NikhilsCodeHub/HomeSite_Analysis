
###  Load Packages

library(rpart)
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

na.cols <- na.cols[na.cols$ColNA,]
na.cols <- tbl_df(na.cols)


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
  

  df_personal$PersonalField16A <- substr(x = df_personal$PersonalField16,start = 1,stop = 1)
  df_personal$PersonalField16B <- substr(x = df_personal$PersonalField16,start = 2,stop = 2)
  
  df_personal$PersonalField17A <- substr(x = df_personal$PersonalField17,start = 1,stop = 1)
  df_personal$PersonalField17B <- substr(x = df_personal$PersonalField17,start = 2,stop = 2)
  
  df_personal$PersonalField18A <- substr(x = df_personal$PersonalField18,start = 1,stop = 1)
  df_personal$PersonalField18B <- substr(x = df_personal$PersonalField18,start = 2,stop = 2)
  
  df_personal$PersonalField19A <- substr(x = df_personal$PersonalField19,start = 1,stop = 1)
  df_personal$PersonalField19B <- substr(x = df_personal$PersonalField19,start = 2,stop = 2)
  
  ## ---------------------------------------------------------
  ## Imputing for missing values in PeronalField7 using rPart.
  ## ---------------------------------------------------------
  ## 
  train_df_personal <- df_personal[!is.na(df_personal$PersonalField7),]
  train_df_personal <- select(train_df_personal, -PersonalField16,-PersonalField17, -PersonalField18, -PersonalField19)  

  test_df_personal <- df_personal[is.na(df_personal$PersonalField7),]

  fit <- train(as.factor(PersonalField7)~., train_df_personal, method="rpart")
  #   fit
  #   
  #   CART 
  #   
  #   260640 samples
  #   86 predictor
  #   2 classes: 'N', 'Y' 
  #   
  #   No pre-processing
  #   Resampling: Bootstrapped (25 reps) 
  #   Summary of sample sizes: 260640, 260640, 260640, 260640, 260640, 260640, ... 
  #   Resampling results across tuning parameters:
  #     
  #     cp          Accuracy   Kappa       Accuracy SD   Kappa SD  
  #   0.00000000  0.9951377  0.05388021  0.0001781390  0.01333200
  #   0.01427439  0.9952974  0.05592193  0.0001675794  0.01175791
  #   0.02854877  0.9952223  0.02534216  0.0001629112  0.02565144
  #   
  #   Accuracy was used to select the optimal model using  the largest value.
  #   The final value used for the model was cp = 0.01427439. 
    
  pred_personalField7 <- predict(fit, test_df_personal)
  
  df_personal[is.na(df_personal$PersonalField7),"PersonalField7"] <- pred_personalField7
  
  ##  pred_presonalField7
  ##  [1] N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N 
  ##  N N N N N N N N
  ##  [54] N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N 
  ##  N N N N N N N N
  ##  [107] N N N N N N N
  ##  Levels: N Y  
  
  
  ## ---------------------------------------------------
  ## Imputing missing values in PropertyField
  ## ---------------------------------------------------
  ## 

  
  #   impute_Col <- function(dFrame, naCols){
  #     if(length(grep("Y|N", levels(df_property[PropertyField3), value = FALSE))==2){
  #       
  #     }
  #   }
  
  
  df_property <- select(dtrain, starts_with("PropertyField"))
  

    
  ## List NA columns
  na_prop_cols <- grep("PropertyField", na.cols$ColNames, value = TRUE)
  
  levels(df_property$PropertyField3) <- c("N", "Y", "0")
  df_property$PropertyField3[is.na(df_property$PropertyField3)]<-"0"
  
  levels(df_property$PropertyField4) <- c("N", "Y", "0")
  df_property$PropertyField4[is.na(df_property$PropertyField4)]<-"0"
  
  levels(df_property$PropertyField32) <- c("N", "Y", "0")
  df_property$PropertyField32[is.na(df_property$PropertyField32)]<-"0"

  levels(df_property$PropertyField34) <- c("N", "Y", "0")
  df_property$PropertyField34[is.na(df_property$PropertyField34)]<-"0"  
  
  levels(df_property$PropertyField36) <- c("N", "Y", "0")
  df_property$PropertyField36[is.na(df_property$PropertyField36)]<-"0"

  levels(df_property$PropertyField38) <- c("N", "Y", "0")
  df_property$PropertyField38[is.na(df_property$PropertyField38)]<-"0"  
  
  levels(df_property$PropertyField29) <- c("0", "1", "2")
  df_property$PropertyField29[is.na(df_property$PropertyField29)]<-"2"   
  
  
  #   > table(dtrain$PropertyField3, useNA = "ifany")
  #   
  #           N      Y   <NA> 
  #     226966  33706     81 
  #   > table(dtrain$PropertyField4, useNA = "ifany")
  #   
  #           N      Y   <NA> 
  #     226223  34467     63 
  #   > table(dtrain$PropertyField29, useNA = "ifany")
  #   
  #         0      1   <NA> 
  #     60056     12 200685 
  #   > table(dtrain$PropertyField32, useNA = "ifany")
  #   
  #         N      Y   <NA> 
  #     69056 191627     70 
  #   > table(dtrain$PropertyField34, useNA = "ifany")
  #   
  #          N      Y   <NA> 
  #     119498 141185     70 
  #   > table(dtrain$PropertyField36, useNA = "ifany")
  #   
  #          N      Y   <NA> 
  #     248302  12338    113 
  #   > table(dtrain$PropertyField38, useNA = "ifany")
  #   
  #          N      Y   <NA> 
  #     254032   5501   1220 
  #   > table(dtrain$PropertyField33, useNA = "ifany")
  #   
  #   E      F      G      H 
  #   47359   9647  80649 123098 
    
  
  ## unfactor any columns with nlevels > 15

  ##  Methods to approach this running models
  ##
  ##  1. Groupd Similar columns together and run PCH.
  ##    a. Run RandomForest on the combo
  ##    b. Run GLM on the compbo
  ##  2. Run GLM on the entire Dataset
  ##  3. Run RandomForest on the entire Dataset
  ##

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




