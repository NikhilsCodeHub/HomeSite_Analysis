### ---------------------------------------------- ###
### ---- Time to process testData Data   ------------  ###
### ---------------------------------------------- ###

## --- Commenting as we are evaluating the model on the split train-test data.  ---- ##

testData<-read.csv("test.csv")  

testData<-tbl_df(testData)
testData$Fare<-as.numeric(testData$Fare)
testData$family_size<-(testData$SibSp + testData$Parch+1)
testData$Age_Imputed<-testData$Age  


#----x----x----x---x-----#

set.seed(101)
excess_factor <- 1.5

df_Mr<-testData[grep("Mr\\.", testData$Name, ignore.case = FALSE),]
df_Master<-testData[grep("Master\\.", testData$Name, ignore.case = FALSE),]
df_Miss<-testData[grep("Miss\\.", testData$Name, ignore.case = FALSE),]
df_Mrs<-testData[grep("Mrs\\.", testData$Name, ignore.case = FALSE),]
df_Dr<-testData[grep("Dr\\.", testData$Name, ignore.case = FALSE),]

df_Mr <- tbl_df(df_Mr)
df_Master <- tbl_df(df_Master)
df_Miss <- tbl_df(df_Miss)
df_Mrs <- tbl_df(df_Mrs)
df_Dr <- tbl_df(df_Dr)  

##### Imputing Mr.
imp<-rnorm(sum(is.na(df_Mr$Age))*excess_factor,mean(df_Mr$Age[!is.na(df_Mr$Age)]),sd(df_Mr$Age[!is.na(df_Mr$Age)]))

imp<-imp[imp>12][1:sum(is.na(df_Mr$Age))]


testData$Age_Imputed[grepl("Mr\\.", testData$Name) & is.na(testData$Age)]<-as.integer(imp)

#### ----- ####


##### Imputing Mrs.
imp<-rnorm(sum(is.na(df_Mrs$Age))*excess_factor,mean(df_Mrs$Age[!is.na(df_Mrs$Age)]),sd(df_Mrs$Age[!is.na(df_Mrs$Age)]))

imp<-as.integer(imp[imp>21][1:sum(is.na(df_Mrs$Age))])


testData$Age_Imputed[grepl("Mrs\\.", testData$Name) & is.na(testData$Age)]<-as.integer(imp)

#### ----- ####

##### Imputing Miss.
imp<-rnorm(sum(is.na(df_Miss$Age))*excess_factor,mean(df_Miss$Age[!is.na(df_Miss$Age)]),sd(df_Miss$Age[!is.na(df_Miss$Age)]))

imp<-as.numeric(imp[imp>0][1:sum(is.na(df_Miss$Age))])
imp<-as.integer(imp[imp>1])

testData$Age_Imputed[grepl("Miss\\.", testData$Name) & is.na(testData$Age)]<-imp
#### ----- ####


##### Imputing Master.
imp<-rnorm(sum(is.na(df_Master$Age))*excess_factor,mean(df_Master$Age[!is.na(df_Master$Age)]),sd(df_Master$Age[!is.na(df_Master$Age)]))

imp<-as.numeric(imp[imp<=12 & imp>0][1:sum(is.na(df_Master$Age))])
imp<-as.integer(imp[imp>1])


testData$Age_Imputed[grepl("Master\\.", testData$Name) & is.na(testData$Age)]<-imp
#### ----- ####

##### Imputing Dr.
imp<-rnorm(sum(is.na(df_Dr$Age))*excess_factor,mean(df_Dr$Age[!is.na(df_Dr$Age)]),sd(df_Dr$Age[!is.na(df_Dr$Age)]))

imp<-as.integer(imp[imp>25][1:sum(is.na(df_Dr$Age))])


testData$Age_Imputed[grepl("Dr\\.", testData$Name) & is.na(testData$Age)]<-(imp)
#### ----- ####


testData[testData$Embarked=="S" & !is.na(testData$Fare) & testData$family_size==1 & grepl("Mr\\.", testData$Name),c("Name", "Age","Age_Imputed", "Fare", "family_size", "SibSp","Parch")]

testData$Fare[testData$Embarked=="S" & is.na(testData$Fare) & testData$family_size==1 & grepl("Mr\\.", testData$Name)] = mean(testData$Fare[testData$Embarked=="S" & !is.na(testData$Fare) & testData$family_size==1 & grepl("Mr\\.", testData$Name)])

testData$Age_Imputed[grepl("Ms\\.", testData$Name) & is.na(testData$Age)]<-11

testData$Pclass <- as.factor(testData$Pclass)

testData$Fare_category <- cut(testData$Fare, seq(0,600, 100), right = FALSE, labels = c(1:6))

testData$Age_Imputed_Cat<- cut(testData$Age_Imputed,breaks = seq(0,100,20), right = FALSE, labels = c(1:5))

testData$Women_Children = ifelse(test= testData$Sex=="female",yes = testData$Parch+1,no = 0)

train.testset$Survived <- as.factor(train.testset$Survived)

## ---- Write out imputed Test Set
##write.csv(x = testData, file = "testData_set_imputed.csv", row.names = FALSE)

##testData$Survived<-0

#===========================#
#===========================#

# Make your prediction using `my_tree_five` and `testData_new`
my_class_prediction <- predict(my_tree, newdata=testData, type="class")
my_prob_prediction <- predict(my_tree, newdata=testData, type="prob")

test_result <- as.data.frame(my_prob_prediction, stringsAsFactors = FALSE)  

df<-data.frame(my_class_prediction, stringsAsFactors = FALSE)
test_result <- cbind(test_result, test.class.predicted = as.numeric(df$my_class_prediction))

test_result$test.class.predicted <- test_result$test.class.predicted -1

table(test_result$`1`,test_result$test.class.predicted)

table(test_result$`1`>0.6,test_result$test.class.predicted)


# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_filedata <- data.frame(PassengerId = testData$PassengerId, Survived = ifelse(test_result$`1`>0.5,1,0))

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_filedata, file=paste0("Titanic_Submission_", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)

confusionMatrix(df$my_prediction , testData$Survived)

## =====================
## Random Forest
## =====================

library(randomForest)

# Set seed for reproducibility
set.seed(101)

# Apply the Random Forest Algorithm
##
## my_forest <- randomForest(as.factor(Survived)~Pclass+Sex+Age_Imputed+SibSp+Parch+Fare+Embarked+family_size, data=train, importance=TRUE, ntree=2000)

my_forest <- randomForest(as.factor(Survived)~Sex+Pclass+Age_Imputed+Fare+SibSp+Parch+family_size+Embarked, data=train.trainset, importance=TRUE, ntree=2000)



# Make your prediction using the testData set
my_prediction <- predict(my_forest, testData)

df<-data.frame(my_prediction)

testData$Survived<-df[,1]
# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = testData$PassengerId, Survived = df[,1])

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file=paste0("Titanic_Submission_", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)



