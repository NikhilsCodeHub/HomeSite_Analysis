###
# OPS Scripts that performs the magic
###

library(rpart)
library(randomForest)
library(caret)
library(dplyr)
library(doMC)
registerDoMC(cores = 4)



##----- Load, Clean and Impute Data
##------
dtrain <- read.csv("train.csv", stringsAsFactors = FALSE, na.strings = c(""," ", "NaN", "NA", "Inf"))
dtest <- read.csv("test.csv", stringsAsFactors = FALSE, na.strings = c(""," ", "NaN", "NA", "Inf"))

dtrain_final <- process_personal_16_17_18_19(dtrain)

dtest_final <- process_personal_16_17_18_19(dtest)

dfSummary <- dataset_summary(dtrain_final, dtest_final, colOutcome = "QuoteConversion_Flag")

dtrain_missing <- tabulate_missing_values(dtrain_final)


dtrain_final[,dtrain_missing$ColNames] <- lapply(dtrain_final[,dtrain_missing$ColNames],impute_missing_values)



dtrain_final <- tbl_df(dtrain_final)

dtrain_final <- select(dtrain_final, -Original_Quote_Date, -QuoteNumber)

# dtrain_final <- data.frame(dtrain_final)
# dfSummary <- data.frame(dfSummary)

dtrain_final <- set_factor_levels(dtrain_final, dfSummary)

dsList <- split_datasets(dtrain_final[1:200,])

lstFits <- lapply(dsList, createModelFits)

dtest_missing <- tabulate_missing_values(dtest_final)

dtest_final[,dtest_missing$ColNames] <- lapply(dtest_final[,dtest_missing$ColNames],impute_missing_values)

dtest_final <- select(dtest_final, -Original_Quote_Date, -QuoteNumber)

dtest_final <- set_factor_levels(dtest_final, dfSummary)

lstPred <- generatePredictions(lstFit = lstFits, newData = dtest_final, ptype = "prob")

##------ Create Data Partition for Validation
##------
lst_train <- createDataPartition(dtrain$Pclass, p = 0.7, list = FALSE)

itrain <- dtrain[lst_train, ]
ivalidate <- dtrain[-lst_train,]


##------ Formula Creation
##------
yterm <- "as.factor(Survived)"

xterm <- list(c("Pclass", "Sex", "Age", "SibSp", "Parch"))
xterm[[2]] <- c("Pclass", "Sex", "SibSp", "Parch")
xterm[[3]] <- c("Pclass", "Sex", "SibSp", "Parch", "Fare")
xterm[[4]] <- c("Pclass", "titles", "SibSp", "Parch", "Fare", "Embarked")
xterm[[5]] <- c("Pclass", "Sex", "titles", "SibSp", "Parch", "Fare", "Embarked")
xterm[[6]] <- c("Pclass", "Sex", "SibSp", "Parch", "titles")

fmla <- createFormula(yterm, xterm)


##------ Model Building
##------

trCtrl <- trainControl(method = "cv", number = 10)
mdls1 <- createModels(fmla, itrain, "rf", ntrees=1000, trControl=trCtrl)

mdls2 <- createModels(fmla, itrain, "rpart")

preds1 <- generatePredictions(mdls1, itrain)
colnames(preds1) <-  paste("c", seq(1:dim(preds1)[2]), sep = "")
preds2 <- generatePredictions(mdls2, itrain)
colnames(preds2) <-  paste("d", seq(1:dim(preds2)[2]), sep = "")
cmbDF <- cbind("Survived" =itrain$Survived, preds1, preds2)

cmbFit <- train(as.factor(Survived)~., data=cmbDF, method="rf", trControl=trCtrl)

## vimp1 <- sapply(mdls1, varImp, simplify = FALSE)
## vimp2 <- sapply(mdls2, varImp, simplify = FALSE)


##------ Validating the combo model on ivalidate dataset
##------

predsv1 <- generatePredictions(mdls1, ivalidate)
predsv2 <- generatePredictions(mdls2, ivalidate)

colnames(predsv1) <-  paste("c", seq(1:dim(predsv1)[2]), sep = "")
colnames(predsv2) <-  paste("d", seq(1:dim(predsv2)[2]), sep = "")


cmbDFv <- cbind(predsv1, predsv2 )

predsV <- predict(cmbFit, cmbDFv)

cnfMx <- confusionMatrix(predsV, ivalidate$Survived)


##------ Process Test Data
##------

dtest <- read.csv("test.csv", header = TRUE)

dtest <- ImputeData(dtest)
na.cols <- sapply(dtest[,1:dim(dtest)[2]],anyNA)
na.cols

predsTst1 <- generatePredictions(mdls1, dtest)
predsTst2 <- generatePredictions(mdls2, dtest)

colnames(predsTst1) <-  paste("c", seq(1:dim(predsTst1)[2]), sep = "")
colnames(predsTst2) <-  paste("d", seq(1:dim(predsTst2)[2]), sep = "")


cmbDFTst <- cbind(predsTst1, predsTst2 )

predsTst <- predict(cmbFit, cmbDFTst)



my_solution <- data.frame(PassengerId = dtest$PassengerId, data.frame(Survived=predsTst))

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file=paste0("Titanic_Submission_", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)



