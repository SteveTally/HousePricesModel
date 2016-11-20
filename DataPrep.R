library(data.table)
library(xgboost)
library(Matrix)
library(vtreat)
library(mice)
library(Amelia)



# Load Data
trndata <- fread("Data/train.csv")
tstdata <- fread("Data/test.csv")
samplesub <- fread("Data/sample_submission.csv")

# Set Indicators
tstdata[,SalePrice := 0]
trndata[,TrainSet := 1]
tstdata[,TrainSet := 0]


# Combine Train and Test
data <- rbind(trndata,tstdata)



options(na.action='na.pass')
modelframe <- data[,.(MSSubClass = as.factor(MSSubClass),
                      MSZoning,
                      LotFrontage = log(LotFrontage+1),
                      LotArea = log(LotArea),
                      StreetPaved = ifelse(Street == "Pave",1,0),
                      Alley,
                      LotShape = ifelse(LotShape %in% c("IR1","IR2","IR3"),1,0), # 
                      LandContour = ifelse(LandContour == "Gtl",0,1),
                      #Utilities, - excluded due to no variation
                      LotConfig = ifelse(LotConfig == "FR3","FR2",LotConfig), # FR2 &FR3 are similar and low freq.  Group together
                      LandSlope = ifelse(LandSlope %in% c("Mod","Sev"),1,0), #Group low frequency slope vars together
                      Neighborhood,
                      RRProx = ifelse(substr(Condition1,1,2)=="RR" | substr(Condition2,1,2) == "RR",1,0),
                      PosProx = ifelse(substr(Condition1,1,2)=="Po" | substr(Condition2,1,2) == "Po",1,0),
                      ArteryProx = ifelse(substr(Condition1,1,2)=="Ar" | substr(Condition2,1,2) == "Ar",1,0),
                      FeederProx = ifelse(substr(Condition1,1,2)=="Fe" | substr(Condition2,1,2) == "Fe",1,0),
                      #Condition1,
                      #Condition2,
                      BldgType,
                      Stories = ifelse(substr(HouseStyle,1,6) %in% c("1Story","1.5Fin","1.5Unf"),1,ifelse(substr(HouseStyle,1,6) %in% c("2Story","2.5Fin","2.5Unfin","SFoyer","SLvl"),2,0)),
                      PartiallyUnfinished = ifelse(substr(HouseStyle,4,6) == "Unf",1,0),
                      SplitLevel = ifelse(substr(HouseStyle,1,1) == "S",1,0),   
                      #HouseStyle, captured above
                      OverallQual = ifelse(OverallQual %in% c(1,2,3,4),1,ifelse(OverallQual %in% c(5,6,7),OverallQual-3,5)), #Grouping infrequent levels
                      OverallCond = ifelse(OverallCond %in% c(1,2,3,4),1,ifelse(OverallCond %in% c(5,6,7),OverallCond-3,5)), #Grouping infrequent levels
                      YearBuilt = 2010-YearBuilt,
                      YearRemodAdd = 2010-YearBuilt,
                      RemodelInd = ifelse(YearRemodAdd == YearBuilt,0,1),
                      RoofStyle = ifelse(RoofStyle %in% c("Gambrel","Mansard","Shed","Flat"),"Other",RoofStyle),
                      #RoofMatl, not enough deviation
                      ExtStucco = ifelse(substr(Exterior1st,1,3) %in% c("ImS","Stu") | substr(Exterior2nd,1,3) %in% c("ImS","Stu"),1,0),
                      ExtVinyl = ifelse(substr(Exterior1st,1,3) %in% c("Vin") | substr(Exterior2nd,1,3) %in% c("Vin") ,1,0),
                      ExtBrkStn = ifelse(substr(Exterior1st,1,3) %in% c("Brk","Sto") | substr(Exterior2nd,1,3) %in% c("Brk","Sto") ,1,0),
                      ExtSiding = ifelse(substr(Exterior1st,1,3) %in% c("Met","Wd ") | substr(Exterior2nd,1,5) %in% c("Metal","Wd Sd") ,1,0),
                      ExtShingle = ifelse(substr(Exterior1st,1,3) %in% c("Asp","WdS","Asb") | substr(Exterior2nd,1,3) %in% c("AsphS","Wd Sh ","AsbSh") ,1,0),
                      #Exterior1st,
                      #Exterior2nd,
                      MasVnrType,
                      MasVnrArea = log(ifelse(is.na(MasVnrArea),0,MasVnrArea) +1),
                      ExterQual = ifelse(ExterQual == "Fa", "TA",ExterQual),
                      ExterCond = ifelse(ExterCond == "Po", "Fa",ExterCond),
                      Foundation = ifelse(Foundation %in% c("Wood","Stone","Slab"), "Oth", Foundation),
                      BsmtInd <- ifelse(is.na(BsmtCond),0,1),
                      BsmtQual = ifelse(BsmtQual == "Fa", "TA",BsmtQual),
                      BsmtCond,
                      BsmtExposure,
                      #BsmtFinType1,
                      BsmtUnFinPct = BsmtUnfSF / TotalBsmtSF,
                      #BsmtFinSF1 = log(BsmtFinSF1+1),
                      #BsmtFinType2,
                      #BsmtFinSF2 = log(BsmtFinSF2+1),
                      BsmtUnfSF = log(BsmtUnfSF+1),
                      TotalBsmtSF = log(TotalBsmtSF+1),
                      BsmtALQPct = (ifelse(BsmtFinType1 == "ALQ",BsmtFinSF1,0)+ifelse(BsmtFinType2 == "ALQ",BsmtFinSF2,0)) / TotalBsmtSF,
                      BsmtBLQPct = (ifelse(BsmtFinType1 == "BLQ",BsmtFinSF1,0)+ifelse(BsmtFinType2 == "BLQ",BsmtFinSF2,0)) / TotalBsmtSF,
                      BsmtGLQPct = (ifelse(BsmtFinType1 == "GLQ",BsmtFinSF1,0)+ifelse(BsmtFinType2 == "GLQ",BsmtFinSF2,0)) / TotalBsmtSF,
                      BsmtLWQPct = (ifelse(BsmtFinType1 == "LWQ",BsmtFinSF1,0)+ifelse(BsmtFinType2 == "LWQ",BsmtFinSF2,0)) / TotalBsmtSF,
                      BsmtRecPct = (ifelse(BsmtFinType1 == "Rec",BsmtFinSF1,0)+ifelse(BsmtFinType2 == "Rec",BsmtFinSF2,0)) / TotalBsmtSF,
                      BsmtUnfPct = (ifelse(BsmtFinType1 == "Unf",BsmtFinSF1,0)+ifelse(BsmtFinType2 == "Unf",BsmtFinSF2,0)) / TotalBsmtSF,
                      #Heating, - near zero variance
                      #HeatingQC, - near zero variance
                      CentralAirInd = ifelse(CentralAir == "Y",1,0),
                      Electrical = ifelse(Electrical == "SBrkr",1,0), # Group together infrequent levels
                      FirstFlrSF = log(`1stFlrSF`+1),
                      SecondFlrSF = log(`2ndFlrSF`+1),
                      LowQualFinSF = log(LowQualFinSF +1),
                      LowQualFinPct = log((LowQualFinSF / (`1stFlrSF`+`2ndFlrSF`))+1),
                      GrLivArea = log(GrLivArea+1),
                      BsmtFullBath = ifelse(is.na(BsmtFullBath),0,BsmtFullBath),
                      BsmtHalfBath = ifelse(is.na(BsmtHalfBath),0,BsmtHalfBath),
                      FullBath = ifelse(is.na(FullBath),0,FullBath),
                      HalfBath = ifelse(is.na(HalfBath),0,HalfBath),
                      TotalBath = ((BsmtHalfBath+HalfBath)*0.5)+FullBath+BsmtFullBath,
                      PctBathAbvGrd = ((HalfBath*0.5)+FullBath)/(((BsmtHalfBath+HalfBath)*0.5)+FullBath+BsmtFullBath),
                      BedroomAbvGr,
                      KitchenAbvGr,
                      KitchenQual,
                      TotRmsAbvGrd,
                      Functional, #= ifelse(Functional %in% c("Maj1","Maj2","Sev"),1,ifelse(Functional %in% c("Min1","Min2","Mod"),2,3)),
                      Fireplaces,
                      #FireplaceQu, 
                      GarageInd = ifelse(is.na(GarageType),0,1),
                      GarageType,
                      GarageYrBlt = ifelse(is.na(GarageYrBlt),0,GarageYrBlt - YearBuilt), # transform to number of years after house was built
                      GarageFinish,
                      GarageCars,
                      GarageArea = log(GarageArea+1),
                      GarageQual = ifelse(GarageQual %in% c("Gd,Po"),"Fa",ifelse(GarageQual == "Ex","TA",GarageQual)),
                      GarageCond = ifelse(GarageCond %in% c("Gd,Po"),"Fa",ifelse(GarageCond == "Ex","TA",GarageCond)),
                      PavedDrive = ifelse(PavedDrive == "P","N",PavedDrive),
                      WoodDeckSF = log(WoodDeckSF+1),
                      OpenPorchSF = log(OpenPorchSF+1),
                      OpenPorchInd = ifelse(is.na(OpenPorchSF),0,1),
                      EnclosedPorch,
                      EnclosedPorchInd = ifelse(is.na(EnclosedPorch),0,1),
                      ThreeSsnPorch = `3SsnPorch`,
                      ScreenPorch,
                      ScreenPorchInd = ifelse(is.na(ScreenPorch),0,1),
                      PoolArea = log(PoolArea+1),
                      PoolInd = ifelse(is.na(PoolQC),0,1),
                      #PoolQC, - too few here
                      Fence = ifelse(Fence == "MnWw","GdWo",Fence), #colapse infrequent levels
                      FenceInd = ifelse(is.na(Fence),0,1),
                      Shed  = ifelse(MiscFeature == "Shed",1,0),
                      #MiscVal,
                      YrSold = as.factor(YrSold),
                      MoSold = as.factor(MoSold),
                      SaleType = ifelse(substr(SaleType,1,3)=="Con","Con",ifelse(SaleType %in% c("Oth", "CWD"),"WD",SaleType)), #Grouping some rare levels
                      SaleCondition,
                      TrainSet)]

#### Explore Missing Values ####
sort(sapply(data, function(x) { sum(is.na(x)) }), decreasing=TRUE)


#### Impute Missing Values ####
# Don't worry, we are not imputing all the missing values, just the ones that make sense
trainimputed <- mice(modelframe[,-"TrainSet",with = FALSE], m=1, method='cart', printFlag=FALSE)


#### Selectively Apply Imputed Values to Dataset ####
# Most of the missing values are useful but some cause confusion.
# In this step we are applying imputation jist to the columns where it really makes sense
modelframe$LotFrontage <- complete(trainimputed)$LotFrontage
modelframe$LotFrontage <- complete(trainimputed)$LotFrontage
tmp <- data[data$BsmtFinSF1==0,]


#### Prepare Variable Treatments ####
vtreatment <- mkCrossFrameNExperiment(cbind(modelframe[TrainSet ==1,],SalePrice = trndata[,SalePrice]),
                                      colnames(modelframe[,-"TrainSet",with = FALSE]),
                                      "SalePrice",
                                      minFraction = 0.03,
                                      rareCount = 10,
                                      ncross = 5,
                                      smFactor = 0.05,
                                      collarProb = 0.01,
                                      doCollar = TRUE
                                      )

scoreFrame <- vtreatment$treatments$scoreFrame
#prunesig <- 0.003
useVars <- vtreatment$treatments$scoreFrame$varName[scoreFrame$code %in% c('lev','catN','clean','isBad') ] # (scoreFrame$sig <prunesig) condition for prunesig


#### Data Set for Model Buildiing ####
model_matrix <- as.matrix(vtreatment$crossFrame[,useVars])
model_y <- vtreatment$crossFrame$SalePrice


#### Data Set for Competition ####
test_matrix <- as.matrix(vtreat::prepare(vtreatment$treatments,modelframe[TrainSet == 0,-"TrainSet",with = FALSE], pruneSig = NULL,varRestriction = useVars))

  


