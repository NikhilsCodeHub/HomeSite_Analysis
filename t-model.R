## Partitioning data and Model building

library(caret)

lst_train <- createDataPartition(dtrain$Survived, p = 0.6, list = FALSE)

itrain <- dtrain[lst_train, ]
ivalidate <- dtrain[-lst_train,]

fit.rf <- train(as.factor(Survived)~Pclass+Age+Sex+SibSp+Parch+Fare+Embarked+titles, data=itrain, method="rf", ntrees = 1000, importance = TRUE)

fit.gbm <- train(as.factor(Survived)~Pclass+Age+Sex+SibSp+Parch+Fare+Embarked+titles, data=itrain, method="gbm", verbose=FALSE)

trControl <- trainControl(method = "repeatedcv", number = 5, repeats = 10)

fit.rpart <- train(as.factor(Survived)~Pclass+Age+Sex+SibSp+Parch+Fare+Embarked+titles, data=itrain, method="rpart", trControl= trControl)

pred.rf <- predict(fit.rf, ivalidate)
pred.gbm <- predict(fit.gbm, ivalidate)
pred.rpart <- predict(fit.rpart, ivalidate)

confusionMatrix(pred.rf, ivalidate$Survived)
confusionMatrix(pred.gbm, ivalidate$Survived)
confusionMatrix(pred.rpart, ivalidate$Survived)

df_pred <- cbind(data.frame(pred.rf=pred.rf), data.frame(pred.gbm=pred.gbm), data.frame(pred.rpart=pred.rpart), data.frame(Survived=ivalidate$Survived))

fit_combo <- train(as.factor(Survived)~., data = df_pred, method="rf", ntrees=1000)

pred_combo <- predict(fit_combo, df_pred)

confusionMatrix(pred_combo, ivalidate$Survived)


