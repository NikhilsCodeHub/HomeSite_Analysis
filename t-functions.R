createFormula <- function(yterm, xterm){
  fmla <- paste(yterm, sapply(xterm, paste,collapse="+"), sep = "~")
  return(sapply(fmla, formula))
}


createModels <- function(fmla, idata, modelNm,...){
  
  ### modelNm in "lm", "rpart", "rf", "gbm", "glm"
  fit <- sapply(fmla, train, data= idata, method=modelNm, simplify = FALSE,...)
  ## simplify = TRUE --- outputs the fit in matrix format.
  ## simplify = FALSE --- outputs the fit in object format.
  ## fit <- train(fmla, data= idata, method="rf", ntrees=1000)
  return(fit)
}


generatePredictions <- function(fit, newData){
  pred <- sapply(fit, predict.train, newdata=newData, type="raw")
  return(as.data.frame(pred))
}

calculateRMSE <- function(predictons, reference){
  pRMSE <- sapply(as.data.frame(predictons)[,1:3], RMSE, m[15:20,1])
}

calcConfusionMx <- function(predictons, reference){
  cnfMx <- sapply(predictons, confusionMatrix, reference, simplify = FALSE)
  return(cnfMx)
}

