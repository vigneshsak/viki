
## Reading files & rbind

training_data = read.csv(file = file.path("train.csv"))
test_data = read.csv(file = file.path("test.csv"))
test_data$SalePrice <- 0
tot_data <- rbind(training_data, test_data)



library(plyr)
library(dplyr)
library(purrr)

## Finding NA's

na.cols <- which(colSums(is.na(tot_data)) > 0)
sort(colSums(sapply(tot_data[na.cols], is.na)), decreasing = TRUE)
paste('There are', length(na.cols), 'columns with missing values')

#imputing selected variables na's to 0
library(tidyr)
library(ggplot2)
library(glmnet)
# LotFrontage : NA most likely means no lot frontage
tot_data$LotFrontage[is.na(tot_data$LotFrontage)] <- 0
tot_data$MasVnrArea[is.na(tot_data$MasVnrArea)] <- 0
tot_data$BsmtFinSF1[is.na(tot_data$BsmtFinSF1)] <- 0
tot_data$BsmtFinSF2[is.na(tot_data$BsmtFinSF2)] <- 0
tot_data$BsmtUnfSF[is.na(tot_data$BsmtUnfSF)] <- 0
tot_data$TotalBsmtSF[is.na(tot_data$TotalBsmtSF)] <- 0
tot_data$BsmtFullBath[is.na(tot_data$BsmtFullBath)] <- 0
tot_data$BsmtHalfBath[is.na(tot_data$BsmtHalfBath)] <- 0
tot_data$GarageCars[is.na(tot_data$GarageCars)] <- 0
tot_data$GarageArea[is.na(tot_data$GarageArea)] <- 0

#considering garage was built on same as house built
tot_data$GarageYrBlt[is.na(tot_data$GarageYrBlt)] <- tot_data$YearBuilt[is.na(tot_data$GarageYrBlt)]

summary(tot_data$GarageYrBlt)
#obvious typo
tot_data$GarageYrBlt[tot_data$GarageYrBlt==2207] <- 2007
library(caret)
library(dummies)
library(stringr)
#imputing na's of selected variables with most common value
tot_data$KitchenQual[is.na(tot_data$KitchenQual)] <- names(sort(-table(tot_data$KitchenQual)))[1]
tot_data$MSZoning[is.na(tot_data$MSZoning)] <- names(sort(-table(tot_data$MSZoning)))[1]
tot_data$SaleType[is.na(tot_data$SaleType)] <- names(sort(-table(tot_data$SaleType)))[1]
tot_data$Exterior1st[is.na(tot_data$Exterior1st)] <- names(sort(-table(tot_data$Exterior1st)))[1]
tot_data$Exterior2nd[is.na(tot_data$Exterior2nd)] <- names(sort(-table(tot_data$Exterior2nd)))[1]
tot_data$Functional[is.na(tot_data$Functional)] <- names(sort(-table(tot_data$Functional)))[1]

# For the rest we change NAs to their actual meaning
#and turning into factor wherever needed
tot_data$Alley = factor(tot_data$Alley, levels=c(levels(tot_data$Alley), "No"))
tot_data$Alley[is.na(tot_data$Alley)] = "No"
# where Bsmt fn- NA for basement features is "no basement"
tot_data$BsmtQual = factor(tot_data$BsmtQual, levels=c(levels(tot_data$BsmtQual), "No"))
tot_data$BsmtQual[is.na(tot_data$BsmtQual)] = "No"
tot_data$BsmtCond = factor(tot_data$BsmtCond, levels=c(levels(tot_data$BsmtCond), "No"))
tot_data$BsmtCond[is.na(tot_data$BsmtCond)] = "No"
tot_data$BsmtExposure[is.na(tot_data$BsmtExposure)] = "No"
tot_data$BsmtFinType1 = factor(tot_data$BsmtFinType1, levels=c(levels(tot_data$BsmtFinType1), "No"))
tot_data$BsmtFinType1[is.na(tot_data$BsmtFinType1)] = "No"
tot_data$BsmtFinType2 = factor(tot_data$BsmtFinType2, levels=c(levels(tot_data$BsmtFinType2), "No"))
tot_data$BsmtFinType2[is.na(tot_data$BsmtFinType2)] = "No"
# Fence NA means "no fence"
tot_data$Fence = factor(tot_data$Fence, levels=c(levels(tot_data$Fence), "No"))
tot_data$Fence[is.na(tot_data$Fence)] = "No"
# FireplaceQu NA means "no fireplace"
tot_data$FireplaceQu = factor(tot_data$FireplaceQu, levels=c(levels(tot_data$FireplaceQu), "No"))
tot_data$FireplaceQu[is.na(tot_data$FireplaceQu)] = "No"
# Garage var NA for garage features is "no garage"
tot_data$GarageType = factor(tot_data$GarageType, levels=c(levels(tot_data$GarageType), "No"))
tot_data$GarageType[is.na(tot_data$GarageType)] = "No"
tot_data$GarageFinish = factor(tot_data$GarageFinish, levels=c(levels(tot_data$GarageFinish), "No"))
tot_data$GarageFinish[is.na(tot_data$GarageFinish)] = "No"
tot_data$GarageQual = factor(tot_data$GarageQual, levels=c(levels(tot_data$GarageQual), "No"))
tot_data$GarageQual[is.na(tot_data$GarageQual)] = "No"
tot_data$GarageCond = factor(tot_data$GarageCond, levels=c(levels(tot_data$GarageCond), "No"))
tot_data$GarageCond[is.na(tot_data$GarageCond)] = "No"

# MasVnrType NA most likely means no veneer
tot_data$MasVnrType = factor(tot_data$MasVnrType, levels=c(levels(tot_data$MasVnrType), "No"))
tot_data$MasVnrType[is.na(tot_data$MasVnrType)] = "No"
# MiscFeature : NA = "no misc feature"
tot_data$MiscFeature = factor(tot_data$MiscFeature, levels=c(levels(tot_data$MiscFeature), "No"))
tot_data$MiscFeature[is.na(tot_data$MiscFeature)] = "No"
# PoolQC  data description says NA means "no pool"
tot_data$PoolQC = factor(tot_data$PoolQC, levels=c(levels(tot_data$PoolQC), "No"))
tot_data$PoolQC[is.na(tot_data$PoolQC)] = "No"
# Electrical  NA means "UNK"
tot_data$Electrical = factor(tot_data$Electrical, levels=c(levels(tot_data$Electrical), "UNK"))
tot_data$Electrical[is.na(tot_data$Electrical)] = "UNK"
# GarageYrBlt -that most houses would build a garage when the house itself was built.
idx <- which(is.na(tot_data$GarageYrBlt))
tot_data[idx, 'GarageYrBlt'] <- tot_data[idx, 'YearBuilt']
# droping unwanted variables

tot_data$Utilities <- NULL
tot_data$Id <- NULL


#checking for na's
na.cols <- which(colSums(is.na(tot_data)) > 0)
paste('There are now', length(na.cols), 'columns with missing values')


#finding outliers
plot(training_data$SalePrice, training_data$GrLivArea)
plot(training_data$SalePrice, training_data$LotArea)
plot(training_data$SalePrice, training_data$X1stFlrSF)
plot(training_data$SalePrice, training_data$X2ndFlrSF)
plot(training_data$SalePrice, training_data$LowQualFinSF)
plot(training_data$SalePrice, training_data$TotalBsmtSF)
plot(training_data$SalePrice, training_data$MiscVal)

#transforming outliers
tot_data$GrLivArea[tot_data$GrLivArea>4000] <- mean(tot_data$GrLivArea)%>%as.numeric
tot_data$LotArea[tot_data$LotArea>35000] <- mean(tot_data$LotArea)%>%as.numeric
tot_data$X1stFlrSF[tot_data$X1stFlrSF>3000] <- mean(tot_data$X1stFlrSF)%>%as.numeric
tot_data$TotalBsmtSF[tot_data$TotalBsmtSF>2900] <- mean(tot_data$TotalBsmtSF)%>%as.numeric

#categorical variables
#ranking them
tot_data$ExterQual<- recode(tot_data$ExterQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=18)
tot_data$ExterCond<- recode(tot_data$ExterCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=6)
tot_data$BsmtQual<- recode(tot_data$BsmtQual,"No"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=18)
tot_data$BsmtCond<- recode(tot_data$BsmtCond,"No"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=6)
tot_data$BsmtExposure<- recode(tot_data$BsmtExposure,"No"=0,"No"=1,"Mn"=2,"Av"=3,"Gd"=6)
tot_data$BsmtFinType1<- recode(tot_data$BsmtFinType1,"No"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
tot_data$BsmtFinType2<- recode(tot_data$BsmtFinType2,"No"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
tot_data$HeatingQC<- recode(tot_data$HeatingQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
tot_data$KitchenQual<- recode(tot_data$KitchenQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=6)
tot_data$Functional<- recode(tot_data$Functional,"None"=0,"Sev"=1,"Maj2"=2,"Maj1"=3,"Mod"=4,"Min2"=5,"Min1"=6,"Typ"=7)
tot_data$FireplaceQu<- recode(tot_data$FireplaceQu,"No"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=6)
tot_data$GarageFinish<- recode(tot_data$GarageFinish,"No"=0,"Unf"=1,"RFn"=2,"Fin"=3)
tot_data$GarageQual<- recode(tot_data$GarageQual,"No"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=18)
tot_data$GarageCond<- recode(tot_data$GarageCond,"No"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=6)
tot_data$PoolQC<- recode(tot_data$PoolQC,"No"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=6)
tot_data$Fence<- recode(tot_data$Fence,"No"=0,"MnWw"=1,"GdWo"=2,"MnPrv"=3,"GdPrv"=6)

#making model to understand good and bad qualities
tot_data['IsExterQualBad'] <- ifelse(tot_data$ExterQual< 3, 1, 0)
tot_data['IsExterCondlBad'] <- ifelse(tot_data$ExterCond< 3, 1, 0)
tot_data['IsBsmtQualBad'] <- ifelse(tot_data$BsmtQual< 3, 1, 0)
tot_data['IsBsmtCondBad'] <- ifelse(tot_data$BsmtCond< 3, 1, 0)
tot_data['IsBsmtExposureBad'] <- ifelse(tot_data$BsmtExposure< 3, 1, 0)
tot_data['IsHeatingQCBad'] <- ifelse(tot_data$HeatingQC< 3, 1, 0)
tot_data['IsKitchenQualBad'] <- ifelse(tot_data$KitchenQual< 3, 1, 0)
tot_data['IsFireplaceQuBad'] <- ifelse(tot_data$FireplaceQu< 3, 1, 0)
tot_data['IsGarageQualBad'] <- ifelse(tot_data$GarageQual< 3, 1, 0)
tot_data['IsGarageCondBad'] <- ifelse(tot_data$GarageCond< 3, 1, 0)
tot_data['IsPoolQCBad'] <- ifelse(tot_data$PoolQC< 3, 1, 0)
tot_data['IsExterQualGood'] <- ifelse(tot_data$ExterQual >= 3, 1, 0)
tot_data['IsExterCondlGood'] <- ifelse(tot_data$ExterCond >= 3, 1, 0)
tot_data['IsBsmtQualGood'] <- ifelse(tot_data$BsmtQual >= 3, 1, 0)
tot_data['IsBsmtCondGood'] <- ifelse(tot_data$BsmtCond >= 3, 1, 0)
tot_data['IsBsmtExposureGood'] <- ifelse(tot_data$BsmtExposure >= 3, 1, 0)
tot_data['IsHeatingQCGood'] <- ifelse(tot_data$HeatingQC >= 3, 1, 0)
tot_data['IsKitchenQualGood'] <- ifelse(tot_data$KitchenQual >= 3, 1, 0)
tot_data['IsFireplaceQuGood'] <- ifelse(tot_data$FireplaceQu >= 3, 1, 0)
tot_data['IsGarageQualGood'] <- ifelse(tot_data$GarageQual >= 3, 1, 0)
tot_data['IsGarageCondGood'] <- ifelse(tot_data$GarageCond >= 3, 1, 0)
tot_data['IsPoolQCGood'] <- ifelse(tot_data$PoolQC >= 3, 1, 0)
library(e1071)
library(xgboost)
#remodeled house var
#has been remodeld
tot_data['HasBeenRemodeled'] <- ifelse(tot_data$YearRemodAdd == tot_data$YearBuilt, 0, 1)
# Has been the house been remodelled after the year it was sold?
tot_data['HasBeenRecentlyRemodeled'] <- ifelse(tot_data$YearRemodAdd == tot_data$YrSold, 0, 1) 
# Has been the house sold the year it was built
tot_data['IsNewHouse'] <-ifelse(tot_data$YearBuilt == tot_data$YrSold, 1, 0) 

# Age
tot_data['Age'] <- as.numeric(2010 - tot_data$YearBuilt)
# Time since last selling
tot_data['TimeSinceLastSelling'] <- as.numeric(2010 - tot_data$YrSold)
# Time since remodeled and sold 
tot_data['TimeSinceRemodeledAndSold'] <- as.numeric(tot_data$YrSold - tot_data$YearRemodAdd)
areas <- c('LotFrontage', 'LotArea', 'MasVnrArea', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF',
           'TotalBsmtSF', 'X1stFlrSF', 'X2ndFlrSF', 'GrLivArea', 'GarageArea', 'WoodDeckSF', 
           'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch', 'LowQualFinSF', 'PoolArea')
# Total surface of the house, combining the area-related features
tot_data['TotalSF'] <- as.numeric(rowSums(tot_data[,areas]))
# Total surface of the house, combining the total inside surfacae
tot_data['TotalInsideSF'] <- as.numeric(tot_data$X1stFlrSF + tot_data$X2ndFlrSF)
# There are more number of sales in April, May, June and July, which may indicate some stationality. we create a new variable indicating that the house has been sold in one of these months
tot_data['IsHotMonth'] = recode(tot_data$MoSold,"1"=0,"2"=0,"3"=0,"4"=1,"5"=1, "6"=1, "7"=1, "8"=0, "9"=0, "10"=0, "11"=0, "12"=0)

#transforming var into 0,1
#considering if its not regular it is meant to be customized or exp
#lotshape whether it has a regular shape or not

tot_data$IsRegLotShape <- ifelse(tot_data$LotShape == 'Reg', 1, 0)
#landcontour

tot_data['IsLandLvl'] <- ifelse(tot_data$LandContour == 'Lvl', 1, 0)

#landslope

tot_data['IsLandSlopeGtl'] <-  ifelse(tot_data$LandSlope == 'Gtl', 1, 0)

#paveddrive

tot_data['HasPavedDrive'] <-  ifelse(tot_data$PavedDrive == 'Y', 1, 0)


#electricl

tot_data['IsElectricalSBrkr'] <- ifelse(tot_data$Electrical == 'SBrkr', 1, 0)

#considering all area related features
area_features <- c('X2ndFlrSF', 'MasVnrArea', 'WoodDeckSF', 'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch', 'WoodDeckSF')
for (area_feature in area_features){
  tot_data[str_c('Has',area_feature)] <- ifelse(tot_data[,area_feature] != 0, 1, 0)
}
#considering neighborhood to be rich or poor

training_data[,c('Neighborhood','SalePrice')] %>%
  group_by(Neighborhood) %>%
  summarise(avg = median(SalePrice, na.rm = TRUE)) %>%
  arrange(avg) %>%
  mutate(sorted = factor(Neighborhood, levels=Neighborhood)) %>%
  ggplot(aes(x=sorted, y=avg)) +
  geom_bar(stat = "identity") + 
  labs(x='Neighborhood', y='Price') +
  ylim(NA, 350000) + 
  theme(axis.text.x = element_text(angle=90)) 
richNeighborhood <- c('Crawfor', 'ClearCr', 'Veenker', 'Somerst', 'Timber', 'StoneBr', 'NridgeHt', 'NoRidge')
tot_data['IsNeighborhoodRich'] <- (tot_data$Neighborhood %in% richNeighborhood) *1
#recoding the neighborhood to their levels
tot_data$NeighborhoodScored <- recode(tot_data$Neighborhood, 'MeadowV' = 0, 'IDOTRR' = 0, 'Sawyer' = 1, 'BrDale' = 1, 'OldTown' = 1, 'Edwards' = 1, 'BrkSide' = 1, 'Blueste' = 2, 'SWISU' = 2, 'NAmes' = 2, 'NPkVill' = 2, 'Mitchel' = 2,'SawyerW' = 3, 'Gilbert' = 3, 'NWAmes' = 3, 'Blmngtn' = 3, 'CollgCr' = 3, 'ClearCr' = 3, 'Crawfor' = 3, 'Veenker' = 4, 'Somerst' = 4, 'Timber' = 4, 'StoneBr' = 5, 'NoRidge' = 6, 'NridgHt' = 6)


#sub variables

tot_data["OverallQual-s2"] <- sapply(tot_data$OverallQual, function(x){x**2})
tot_data["OverallQual-s3"] <- sapply(tot_data$OverallQual, function(x){x**3})
tot_data["OverallQual-Sq"] <- sqrt(tot_data["OverallQual"])
tot_data["TotalSF-2"] <- sapply(tot_data$TotalSF, function(x){x**2})
tot_data["TotalSF-3"] = sapply(tot_data$TotalSF, function(x){x**3})
tot_data["TotalSF-Sq"] = sqrt(tot_data["TotalSF"])
tot_data["GrLivArea-2"] = sapply(tot_data$GrLivArea, function(x){x**2})
tot_data["GrLivArea-3"] = sapply(tot_data$GrLivArea, function(x){x**3})
tot_data["GrLivArea-Sq"] = sqrt(tot_data["GrLivArea"])
tot_data["ExterQual-2"] = sapply(tot_data$ExterQual, function(x){x**2})
tot_data["ExterQual-3"] = sapply(tot_data$ExterQual, function(x){x**3})
tot_data["ExterQual-Sq"] = sqrt(tot_data["ExterQual"])
tot_data["GarageCars-2"] = sapply(tot_data$GarageCars, function(x){x**2})
tot_data["GarageCars-3"] = sapply(tot_data$GarageCars, function(x){x**3})
tot_data["GarageCars-Sq"] = sqrt(tot_data["GarageCars"])
tot_data["KitchenQual-2"] = sapply(tot_data$KitchenQual, function(x){x**2})
tot_data["KitchenQual-3"] = sapply(tot_data$KitchenQual, function(x){x**3})
tot_data["KitchenQual-Sq"] = sqrt(tot_data["KitchenQual"])

#factorizing few numerical features
tot_data$MSSubClass <- as.factor(tot_data$MSSubClass)
tot_data$MoSold <- as.factor(tot_data$MoSold)
tot_data$YrSold <- as.factor(tot_data$YrSold)

#correlation
numericVars <- which(sapply(tot_data, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
tot_data_numVar <- tot_data[, numericVars]
cor_numVar <- cor(tot_data_numVar, use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- as.matrix(names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5))))
cor_numVar <- data.frame(cor_numVar[CorHigh, CorHigh])
install.packages("corrplot")
library(corrplot)
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
corrplot(as.matrix(CorHigh), order = "alphabet")

#plots
ggplot(data=tot_data[!is.na(tot_data$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(tot_data$GrLivArea[!is.na(tot_data$SalePrice)]>4500, rownames(tot_data), '')))
ggplot(data=tot_data[!is.na(tot_data$SalePrice),], aes(x=GarageArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(tot_data$GarageArea[!is.na(tot_data$SalePrice)]>4500, rownames(tot_data), '')))
ggplot(data=tot_data[!is.na(tot_data$SalePrice),], aes(x=GarageCars, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(tot_data$GarageArea[!is.na(tot_data$SalePrice)]>4500, rownames(tot_data), '')))
ggplot(data=tot_data[!is.na(tot_data$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
ggplot(data=tot_data[!is.na(tot_data$SalePrice),], aes(x=Age, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
b1 <- ggplot(data=tot_data, aes(x=BsmtFinSF1)) +
  geom_histogram() + labs(x='Type 1 finished square feet')
b2 <- ggplot(data=tot_data, aes(x=BsmtFinSF2)) +
  geom_histogram()+ labs(x='Type 2 finished square feet')
b3 <- ggplot(data=tot_data, aes(x=BsmtUnfSF)) +
  geom_histogram()+ labs(x='Unfinished square feet')
b4 <- ggplot(data=tot_data, aes(x=as.factor(BsmtFinType1))) +
  geom_histogram(stat='count')+ labs(x='Rating of Type 1 finished area')
b5 <- ggplot(data=tot_data, aes(x=as.factor(BsmtFinType2))) +
  geom_histogram(stat='count')+ labs(x='Rating of Type 2 finished area')
b6 <- ggplot(data=tot_data, aes(x=as.factor(BsmtQual))) +
  geom_histogram(stat='count')+ labs(x='Height of the basement')
b7 <- ggplot(data=tot_data, aes(x=as.factor(BsmtCond))) +
  geom_histogram(stat='count')+ labs(x='Rating of general condition')
b8 <- ggplot(data=tot_data, aes(x=as.factor(BsmtExposure))) +
  geom_histogram(stat='count')+ labs(x='Walkout or garden level walls')

layout <- matrix(c(1,2,3,4,5,9,6,7,8),3,3,byrow=TRUE)
#multiplot( b2, b3,b4, b5, b6, b7, b8, layout=layout)

qqnorm(tot_data$SalePrice)
qqline(tot_data$SalePrice)
#transforming saleprice with log
#tot_data$SalePrice <- log(tot_data$SalePrice)


#bringing num cols which are not factors for skewing
column_types <- sapply(names(tot_data),function(x){class(tot_data[[x]])})
numeric_columns <-names(column_types[column_types != "factor"])
#skew <- sapply(numeric_columns,function(x){skewness(tot_data[[x]],na.rm = T)})
#dkskew <- skew[skew > 0.75]
#for (x in names(skew)) {
 # bc = BoxCoxTrans(tot_data[[x]], lambda = 0.15) 
  #tot_data[[x]] = predict(bc, tot_data[[x]])
#}
str(numeric_columns)


# Train, test splitting

fe_training <- tot_data[1:1460,]
fe_test <- tot_data[1461:2919,]
#colSums(is.na(fe_training ))
#models
#base-liner regression model
model =lm(log(SalePrice)~GarageCars+LotFrontage+LotArea+YearBuilt+OverallQual+MasVnrArea+BsmtFinSF1+TotalBsmtSF,data=fe_training)
summary(model)

lm.pred=predict(model, fe_test )
lr.pred=exp(lm.pred)
#cart
library(rpart)
library(rpart.plot)

class.tree <- rpart(SalePrice~.,
                    data = fe_training,control = rpart.control(cp = 0.01))

plotcp(class.tree)
printcp(class.tree)
rpart.plot(class.tree, 
           box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)
#random forest
library(randomForest)
RF <- randomForest(SalePrice ~MSSubClass+LotFrontage+LotArea+YearBuilt+OverallQual+MasVnrArea+BsmtFinSF1+TotalBsmtSF+GrLivArea+ExterQual+KitchenQual+GarageArea+TotalBsmtSF+BsmtCond+GarageFinish+YearBuilt, data = fe_training, 
                   importance =TRUE,ntree=500,nodesize=7)
options(repr.plot.width=9, repr.plot.height=6)
varImpPlot(RF, type=1)
rf.pred <- predict(RF, newdata=fe_training )
plot(rf.pred, fe_training$SalePrice, main = "Figure 9 Predicted vs. Actual log SalePrice") 
abline(0,1)
#clustering-archived
cluster_distance=dist(tot_data,method="euclidean")
cluster=hclust(cluster_distance,method="ward.D2")
#dendogram
plot(cluster)
rect.hclust(cluster,k=4,border="blue" )
#k-means 
#k_means=kmeans(fe_training,centers = 2)
#str(k_means)
#centroid=k_means$centers
#train_c1=subset(fe_training,k_means$cluster==1)
#train_c2=subset(fe_training,k_means$cluster==2)
#train_c1=fe_training[k_means$cluster==1,]
#train_c2=fe_training[k_means$cluster==2,]
##running model on cluster1
#cluster1_lm=lm(SalePrice~.,train_c1)
#cluster1_rf=randomforest(SalePrice~.,train_c1)
#predict_cluster1=predict(cluster1_rf,fe_test)
##running model on cluster2
#cluster2_lm=lm(SalePrice~.,train_c2)
#cluster2_rf=randomforest(SalePrice~.,train_c2)
#predict_cluster2=predict(cluster2_rf,fe_test)


# Lasso Regression

set.seed(123)
lasso <- cv.glmnet(x = data.matrix(fe_training[, - which(names(fe_training) %in% c('SalePrice'))]), y = fe_training$SalePrice, nfolds = 10)
plot(lasso)


lasso$lambda.min


sqrt(lasso$cvm[lasso$lambda == lasso$lambda.min])

set.seed(46)
lasso <-  cv.glmnet(x = data.matrix(fe_training[, - which(names(fe_training) %in% c('SalePrice'))]), y = fe_training$SalePrice, nfolds = 10)
lasso_pred <- as.numeric(exp(predict(lasso, newx = data.matrix(fe_test[, - which(names(fe_test) %in% c('SalePrice'))]), s = "lambda.min"))-1)

#hist(lasso_pred, main="Histogram of Lasso Predictions", xlab = "Predictions")
lasso_submission <- data.frame(Id = test_data$Id, SalePrice= (lasso_pred))
colnames(lasso_submission) <-c("Id", "SalePrice")
write.csv(lasso_submission, file = "lasso_submission.csv", row.names = FALSE) 

#xgb parameters
xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)
#sparse_matrix <- sparse.model.matrix(SalePrice ~ .-1, data = tot_data)

label_train <- tot_data$SalePrice[!is.na(tot_data$SalePrice)]
tnumericVars <- which(sapply(tot_data$SalePrice, is.numeric))
dnumericVars <- which(sapply(fe_training, is.numeric))
# put our testing & training data into two seperates Dmatrixs objects
#dtrain <- xgb.DMatrix(data = as.matrix(dnumericVars), label= tnumericVars)
#dtest <- xgb.DMatrix(data = as.matrix(fe_test))

default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.05, #default = 0.3
  gamma=0,
  max_depth=3, #default=6
  min_child_weight=4, #default=1
  subsample=1,
  colsample_bytree=1
)

#xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)
#xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 454)
#XGBpred <- predict(xgb_mod, dtest)
#predictions_XGB <- exp(XGBpred) #need to reverse the log to the real values
#head(predictions_XGB)
#xgb_model <- data.frame(Id=test_data$Id, SalePrice = (predictions_XGB))
#write.csv(xgb_model, file = 'xgb_model.csv', row.names = F)
predictions_XGB <- read.csv(file = file.path("xgb_model.csv"))
library(Ckmeans.1d.dp) #required for ggplot clustering
#mat <- xgb.importance (feature_names = colnames(fe_training),model = xgb_mod)
#xgb.ggplot.importance(importance_matrix = mat[1:20], rel_to_first = TRUE)

#sub_avg <- data.frame(Id=fe_test$Id, SalePrice = (predictions_XGB+3*lasso_pred)/4)
#head(sub_avg)

#write.csv(sub_avg, file = 'xgb_model.csv', row.names = F)

#neural networks
require(neuralnet)
#nnet1 <- train(log(SalePrice)~ GarageCars+Age+LotFrontage+LotArea+YearBuilt+OverallQual+MasVnrArea+BsmtFinSF1+TotalBsmtSF,       
              # data = fe_training,     # training set used to build model
              # method = "nnet",     # type of model you want to build
              # trControl = ctrl,    # how you want to learn
              # tuneLength = 15,
              # maxit = 100,
              # metric = "RMSE"     # performance measure
#)
#plot(nnet1)
#nnet1_pred=predict(nnet1,fe_test)
#head(nnet1_pred)
#nnet1_model <- data.frame(Id=test_data$Id, SalePrice = (nnet1_pred))
#write.csv(nnet1_model, file = 'nnet1_model.csv', row.names = F)

install.packages("neuralnet")
df_nnt=data.frame(fe_training)
df_nntt=data.frame(fe_test)
require(neuralnet)
nn=neuralnet(SalePrice~GarageCars+Age+LotFrontage+LotArea+YearBuilt+OverallQual+MasVnrArea+BsmtFinSF1+TotalBsmtSF,data=df_nnt, hidden=3,act.fct = "logistic",
             linear.output = FALSE)
plot(nn)
Predict=compute(nn,df_nntt)
Predict$net.result
prob_nnt <- Predict$net.result
pred_nnt <- ifelse(prob_nnt>0.5, 1, 0)
pred_nnt
nnet2_model <- data.frame(Id=test_data$Id, SalePrice = (pred_nnt))
write.csv(nnet2_model, file = 'nnet2_model.csv', row.names = F)
head(pred_nnt)
sub_avg <- data.frame(Id=test_data$Id, SalePrice = (predictions_XGB+3*lasso_pred+lr.pred)/4)
head(sub_avg)
write.csv(sub_avg, file = 'newfinal.23.csv', row.names = F)
sub_avg2 <- data.frame(Id=test_data$Id, SalePrice = (predictions_XGB+3*lasso_pred+1000*pred_nnt)/4.00)#just for trial& error 
head(sub_avg2)
write.csv(sub_avg2, file = 'newfinal22.csv', row.names = F)

