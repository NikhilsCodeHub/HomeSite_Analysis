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


process_personal_16_17_18_19 <- function(dfData){
  dfData$PersonalField16A <- substr(x = dfData$PersonalField16,start = 1,stop = 1)
  dfData$PersonalField16B <- substr(x = dfData$PersonalField16,start = 2,stop = 2)
  
  dfData$PersonalField17A <- substr(x = dfData$PersonalField17,start = 1,stop = 1)
  dfData$PersonalField17B <- substr(x = dfData$PersonalField17,start = 2,stop = 2)
  
  dfData$PersonalField18A <- substr(x = dfData$PersonalField18,start = 1,stop = 1)
  dfData$PersonalField18B <- substr(x = dfData$PersonalField18,start = 2,stop = 2)
  
  dfData$PersonalField19A <- substr(x = dfData$PersonalField19,start = 1,stop = 1)
  dfData$PersonalField19B <- substr(x = dfData$PersonalField19,start = 2,stop = 2)
  
  dfData <- dfData[,-c("PersonalPersonalField16", "PersonalPersonalField17", "PersonalPersonalField18", "PersonalPersonalField19")]
  
  return(dfData)
}

check_factor_levels <- function(col_vector){
  if (length(unique(col_vector)) < 27){
    rnames <- rownames(table(col_vector))
    
    if (length(grepl("Y|N",rnames))==length(rnames) & length(rnames) == 2){
    col_vector <- factor(col_vector, levels = c("Y", "N"))
    print("Data is Y|N levels.")
    }
    else if (length(grepl("[A-Z]",rnames))==length(rnames)){
      col_vector <- factor(col_vector, levels = LETTERS)
      print("Data is A-Z levels.")
    }
    
  }
  else {
    
    
  }
  return(col_vector)
}

impute_missing_values <- function(col_vector){
  if (typeof(col_vector) == "integer"){
    col_vector[is.na(col_vector)] <- -10
  }
  if (length(unique(col_vector)) < 27){
    rnames <- rownames(table(col_vector))
    if (length(grepl("Y|N",rnames))==length(rnames) & length(rnames) == 2){
      misng <- length(is.na(col_vector)) 
      ratioYN <- length(c[c=="Y" & !is.na(c)])/length(c[c=="N" & !is.na(c)])
      if(ratioYN<1){
        y_counts <- round(length(col_vector[is.na(col_vector)]) * ratioYN)
        if(y_counts>0){
          col_vector[is.na(col_vector)][1:y_counts] <- "Y"
          col_vector[is.na(col_vector)] <- "N"
        }
        else{
          col_vector[is.na(col_vector)] <- "N"
        }
      }
      else {
        n_counts <- round(length(col_vector[is.na(col_vector)]) / ratioYN)
        if(n_counts>0){
          col_vector[is.na(col_vector)][1:n_counts] <- "N"
          col_vector[is.na(col_vector)] <- "Y"
        }
        else{
          col_vector[is.na(col_vector)] <- "Y"
        }
      }
        
    }
  
  }  
  return(col_vector)
}

