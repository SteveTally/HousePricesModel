library(caret)
library(reshape2)
library(glmnet)

#### Function to split data to  folds based on y value ####

nfolds <- 5
foldindex <- createFolds(vtreatment$crossFrame$SalePrice,nfolds,list = FALSE)



#### Model Parameters ####

#xgboost parameters
XGBparams <- list( colsample_bytree=0.4,
                gamma=0.045,                 
                learning_rate=0.07,
                max_depth=20,
                min_child_weight=1.5,
                reg_alpha=0.65,
                reg_lambda=0.45,
                subsample=0.95)

#glmnet Parameters
alpha <- 1


#### Functions to fit a model on one fold of the data ####

FitFoldXGB <- function(mode = "train", data, fold = NULL, model = NULL, y = NULL){
if (mode == "train"){
  ### code to train an XGBOOST model
  dtrain <- xgb.DMatrix(data[foldindex != fold,], label = y[foldindex != fold], missing = NaN)
  mdl <- xgboost(data = dtrain, nround=100, nthread = 4, metrics=list("rmse"),params = XGBparams, objective = "reg:linear", verbose = 0)
  yhat <- predict(mdl,newdata = data[foldindex == fold,], missing = NaN)
  list(modelFit = mdl, yhat = yhat, y =  y[foldindex == fold], id = which(foldindex == fold), alpha = alpha)
} else {
  ### code to score an arbitrary xgboost model on a new set of data
  predict(model, newdata = data, missing = NaN)
}
}
  


FitFoldGLMNET <- function(mode = "train", data, fold = NULL, model = NULL, y = NULL){
if (mode == "train"){
  glmnetmodel <- cv.glmnet(x = data[foldindex != fold,], y = log(y[foldindex != fold]), type.measure = "mse")
  mdl <- glmnetmodel
  yhat <- exp(predict(glmnetmodel, newx = data[foldindex == fold,], s = "lambda.min", type = "response"))
  list(modelFit = mdl, yhat = yhat, y =  y[foldindex == fold], id = which(foldindex == fold))
} else {
  exp(predict(model,newx = data, s = "lambda.min", type = "response"))
}
}





#### Function to fit model on each fold and return a set of models, validation results and L1 predictions ####
CVFit <- function(fitfunc, data, y){
  lfeature <- NULL
  accuracy <- NULL
  models <- NULL
  for(i in 1:nfolds){
    foldfit <- do.call(fitfunc,list(mode = "train", fold = i, data = data, y = y))
    lfeature <- rbind(lfeature,data.table(l1x = foldfit[["yhat"]],id = foldfit[["id"]]))
    accuracy <- c(accuracy,sqrt(mean((log(foldfit[["yhat"]])-log(foldfit[["y"]]))^2)))
    models[[i]] <- foldfit[["modelFit"]]
  }
  lfeature <- lfeature[order(lfeature$id),]
  list(models = models, lfeature = lfeature, accuracy = accuracy)
}
  

#### Function to score nested models on new data and return average prediction for each new data pont ###
CVScore <- function(Models, data, scoreFunction){
  scores <- matrix(nrow = nrow(data),ncol = 0)
  for(i in 1:nfolds){
    scorefold <- do.call(scoreFunction,list(mode = "test", fold = i, data = data, model = Models[[i]]))
    scores <- cbind(scores,scorefold)
  }
  rowMeans(scores)
}





##### Call CVFit for each model type to create level 1 predictors ####

XGBL1 <- CVFit("FitFoldXGB", data = model_matrix,  y = model_y)
mean(XGBL1[["accuracy"]])


GLMNETL1 <- CVFit("FitFoldGLMNET", data = model_matrix, y = model_y)
mean(GLMNETL1[["accuracy"]])



#### Create add features to level 1 set for training level 2 ####

model_matrix_L2 <- cbind(model_matrix, XGBL1 = XGBL1[["lfeature"]]$l1x)
model_matrix_L2 <- cbind(model_matrix_L2, GLMNETL1 = GLMNETL1[["lfeature"]]$l1x)




#### Train level two model ####

GLMNETL2 <- CVFit("FitFoldGLMNET", data = model_matrix_L2, y = model_y)
mean(XGBL2[["accuracy"]])




#### Test Set Level 1 Predictions ####

GLMNETL1.Test <- CVScore(GLMNETL1$models,test_matrix,"FitFoldGLMNET")
XGBL1.Test <- CVScore(XGBL1$models,test_matrix,"FitFoldXGB")

test_matrix_L2 <- cbind(test_matrix, XGBL1 = XGBL1.Test)
test_matrix_L2 <- cbind(test_matrix_L2, GLMNETL1 = GLMNETL1.Test)


#### Test Set Level 2 Predictions ####


GLMNETL2.Test <- CVScore(GLMNETL2$models,test_matrix_L2,"FitFoldGLMNET")

#### Output Test Predictions for Competition ####
combinedpredictions <- data.frame(Id = tstdata$Id,SalePrice = GLMNETL2.Test)
write.csv(combinedpredictions, file = "submission10.csv", row.names = FALSE)




