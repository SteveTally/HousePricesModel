library(data.table)
library(xgboost)
library(glmnet)
library(mice)

train_mice <- mice(as.matrix(train_matrix))
train_mice <- complete(train_mice,1)

dtrain <- xgb.DMatrix(as.matrix(model_matrix), label = trndata$SalePrice, missing = NaN)

params <- list( colsample_bytree=0.7,
                               gamma=0.045,                 
                               learning_rate=0.07,
                               max_depth=20,
                               min_child_weight=1.5,
                reg_alpha=0.65,
               reg_lambda=0.45,
                subsample=0.95)

model <- xgb.cv(data = dtrain, nround=1000, nthread = 4, nfold = 4,params = params, metrics=list("rmse"), objective = "reg:linear")

modelfinal <- xgboost(data = dtrain, nround=800, nthread = 4, metrics=list("rmse"),params = params, objective = "reg:linear")








glmnetmodel <- cv.glmnet(x = as.matrix(train_mice), y = trndata$SalePrice, type.measure = "mse")
glmnetmodel$lambda.min

coef(glmnetmodel, s = "lambda.min")


categoricalPreserveVars <- c("MSSubClass",
                             "MSZoning",
                             "Alley",
                             "LotShape",
                             "LandContour",
                             "LotConfig",
                             "LandSlope",
                             "Neighborhood",
                             "BldgType",
                             "HouseStyle",
                             "OverallQual",
                             "OverallCond",
                             "RoofStyle",
                             "Exterior1st",
                             "Exterior2nd",
                             "MasVnrType",
                             "ExterQual",
                             "ExterCond",
                             "Foundation",
                             "BsmtExposure",
                             "Heating",
                             "HeatingQC",
                             "Electrical",
                             "KitchenQual",
                             "Functional",
                             "FireplaceQu",
                             "GarageType",
                             "GarageFinish",
                             "GarageQual",
                             "GarageCond",
                             "PavedDrive",
                             "PoolQC",
                             "Fence",
                             "SaleType",
                             "MoSold",
                             "SaleCondition"
)

table(modelframe$KitchenQual)
sum(is.na(modelframe$LowQualFinPct))
hist(log(modelframe$LowQualFinPct))
