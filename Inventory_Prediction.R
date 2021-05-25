
### Read the data
dataset = read.csv('SalesKaggle3.csv')
str(dataset)


### Data Removing and Grouping  
library(dplyr)
# Drop order as it is only a sequential counter 
# Drop SKU number as each row representing one product, all have different SKU
dataset<- select(dataset, -Order, -SKU_number) 
str(dataset)


# Change category data and year to factor
dataset$File_Type <- factor(dataset$File_Typ)
dataset$SoldFlag <- factor(dataset$SoldFlag)
dataset$MarketingType <- factor(dataset$MarketingType)
dataset$New_Release_Flag <- factor(dataset$New_Release_Flag)
dataset$ReleaseYear <- factor(dataset$ReleaseYear)
str(dataset)

# Scaling the numeric type data
dataset <- as.data.frame(lapply(dataset, function(x) if(is.numeric(x))
  {scale(x, center=TRUE, scale = TRUE)} else x))
str(dataset)


# Categorized to historical and active type data
# Historical data train the model
# Model predict for active data
sum (is.na(dataset))
colSums(sapply(dataset,is.na))
historical.data <- subset(dataset, File_Type == 'Historical')
sum (is.na(historical.data))
active.data <- subset(dataset, File_Type == 'Active')
sum (is.na(active.data))

# Impute NA in historical data
library(mice)
imputed_data <- mice(historical.data, m=2, maxit = 2, method='pmm', seed = 123)
imp_ds <- complete(imputed_data, 1)
sum (is.na(imp_ds))
historical.data <- imp_ds


### Exploratory Data Analysis

library(ggplot2)

# Visualize how many item is unsold and sold
ggplot(historical.data, aes(SoldFlag, fill=SoldFlag)) + 
  geom_bar() + 
  xlab("Sold status 0-unsold 1-sold") +
  ylab("Total SKU count") +
  ggtitle("Sold Status of Historical Data")
## Majority of the product is unsold.

# Visualize factor affecting sold data
his_sold_data <- subset(historical.data, SoldFlag == '1')
his_unsold_data <- subset(historical.data, SoldFlag == '0')


ggplot(his_sold_data, aes(MarketingType, SoldCount, fill=MarketingType))+ 
  geom_boxplot() + 
  xlab("Marketing Type") +
  ylab("Sold Count") +
  ggtitle("Sold Count of Historical Sold Data  ")
## Majority of the product only sold 1 unit.

ggplot(his_sold_data, aes(ReleaseNumber, fill=MarketingType))+ 
  geom_bar() + 
  xlab("Release Number") +
  ylab("Total SKU Count") +
  ggtitle("Historical Sold Data for Different Release Number ")
## Majority of the product sold for Release number less than 20.
ggplot(his_unsold_data, aes(ReleaseNumber, fill=MarketingType))+ 
  geom_bar() + 
  xlab("Release Number") +
  ylab("Total SKU Count") +
  ggtitle("Historical Unsold Data for Different Release Number ")
## Majority of the unsold product had Release number less than 20.

ggplot(his_sold_data, aes(New_Release_Flag, fill=MarketingType))+ 
  geom_bar() + 
  xlab("Release Flag") +
  ylab("Total SKU Count") +
  ggtitle("Historical Sold Data and Release Flag ")
## Majority of the product sold had New Release in the future.
ggplot(his_unsold_data, aes(New_Release_Flag, fill=MarketingType))+ 
  geom_bar() + 
  xlab("Release Flag") +
  ylab("Total SKU Count") +
  ggtitle("Historical Unsold Data and Release Flag ")
## Majority of the unsold product had New Release in the future

ggplot(his_sold_data, aes(MarketingType, StrengthFactor, fill=MarketingType))+ 
  geom_boxplot() + 
  xlab("Marketing Type") +
  ylab("Strength Factor") +
  ggtitle("Strength Factor of Historical Sold Data  ")
## Majority of the product sold had lower strength factor.
ggplot(his_unsold_data, aes(MarketingType, StrengthFactor, fill=MarketingType))+ 
  geom_boxplot() + 
  xlab("Marketing Type") +
  ylab("Strength Factor") +
  ggtitle("Strength Factor of Historical Unsold Data  ")
## Majority of the unsold product sold had lower strength factor.

ggplot(his_sold_data, aes(MarketingType, PriceReg, fill=MarketingType))+ 
  geom_boxplot() + 
  xlab("Marketing Type") +
  ylab("Registered Price") +
  ggtitle("Registered Price of Historical Sold Data  ")
## Majority of the product sold were priced below 250.
ggplot(his_unsold_data, aes(MarketingType, PriceReg, fill=MarketingType))+ 
  geom_boxplot() + 
  xlab("Marketing Type") +
  ylab("Registered Price") +
  ggtitle("Registered Price of Historical Unsold Data  ")
## Majority of the unsold product were priced below 250.

ggplot(his_sold_data, aes(ReleaseYear, fill=MarketingType))+ 
  geom_bar() + 
  xlab("Release Year") +
  ylab("Total SKU Count") +
  ggtitle("Historical Sold Data for Different Release Year ")
## Majority of the product sold after release year of 2000.
ggplot(his_unsold_data, aes(ReleaseYear, fill=MarketingType))+ 
  geom_bar() + 
  xlab("Release Year") +
  ylab("Total SKU Count") +
  ggtitle("Historical Unsold Data for Different Release Year ")
## Majority of the unsold product had release year of after 2000.

ggplot(his_sold_data, aes(ItemCount, fill=MarketingType))+ 
  geom_bar() + 
  xlab("Item Count") +
  ylab("Total SKU Count") +
  ggtitle("Historical Sold Data for Different Item Count ")
## Majority of the product sold had Item Count of less than 250.
ggplot(his_unsold_data, aes(ItemCount, fill=MarketingType))+ 
  geom_bar() + 
  xlab("Item Count") +
  ylab("Total SKU Count") +
  ggtitle("Historical Unsold Data for Different Item Count ")
## Majority of the unsold product had Item Count of less than 200.

ggplot(his_sold_data, aes(MarketingType, LowUserPrice, fill=MarketingType))+ 
  geom_boxplot() + 
  xlab("Marketing Type") +
  ylab("Low-end User Price") +
  ggtitle("Low-end User Price of Historical Sold Data  ")
## Majority of the product sold had low-end price priced below 200.
ggplot(his_unsold_data, aes(MarketingType, LowUserPrice, fill=MarketingType))+ 
  geom_boxplot() + 
  xlab("Marketing Type") +
  ylab("Low-end User Price") +
  ggtitle("Low-end User Price of Historical Unsold Data  ")
## Majority of the unsold product had low-end price priced below 200.

ggplot(his_sold_data, aes(MarketingType, LowNetPrice, fill=MarketingType))+ 
  geom_boxplot() + 
  xlab("Marketing Type") +
  ylab("Low-end Net Price") +
  ggtitle("Low-end Net Price of Historical Sold Data  ")
## Majority of the product sold had Low-end Net Price below 200.
ggplot(his_unsold_data, aes(MarketingType, LowNetPrice, fill=MarketingType))+ 
  geom_boxplot() + 
  xlab("Marketing Type") +
  ylab("Low-end Net Price") +
  ggtitle("Low-end Net Price of Historical Unsold Data  ")
## Majority of the unsold product had Low-end Net Price below 200.

# Plot for correlation between numeric data
library(corrplot)
historical.data.num <-select_if(historical.data, is.numeric)

historical.data.col <- cor(historical.data.num)
corrplot(historical.data.col, method = "number", 
         title = "Correlation on Historical numeric Variables")



# The dataset is too huge, it is reduced to half to decrease the processing time
library(caTools)
set.seed(123)
split = sample.split(historical.data$SoldFlag, SplitRatio = 0.5)
ReduceData1 = subset(historical.data, split == TRUE)
ReduceData2 = subset(historical.data, split == FALSE)

# Checking if there is any bias in sampling
prop.table(table(ReduceData1$SoldFlag))
prop.table(table(ReduceData2$SoldFlag))

# ReducaData1 will be used
# Group the data into SoldFlag for category and SoldCount for numeric prediction
# File_Type can be removed as all data here are historical type
# ReleaseYear is not important after testing, remove it from dataset
his_SoldFlag <- select(ReduceData1, -SoldCount, -File_Type, -ReleaseYear)
his_SoldCount <- select(ReduceData1, -SoldFlag, -File_Type, -ReleaseYear)

# Splitting the data into train and validation data
set.seed(123)
split = sample.split(his_SoldFlag$SoldFlag, SplitRatio = 0.7)
train_Data = subset(his_SoldFlag, split == TRUE)
validation_Data = subset(his_SoldFlag, split == FALSE)
table(train_Data$SoldFlag)

# Checking if there is any bias in sampling
prop.table(table(train_Data$SoldFlag))
prop.table(table(validation_Data$SoldFlag))


### Building Model 1: Decision Tree

# Train the model
library(rpart)
library(rpart.plot)
library(caret)
library(MLmetrics)
library(ROSE)

tree_model = rpart(SoldFlag~ ., data=train_Data)
prp (tree_model)
summary(tree_model)

# Predict Training data 
Predict_DT_T = predict(tree_model, train_Data, type = "class")
confusionMatrix(Predict_DT_T, train_Data$SoldFlag)


# Predict Validation data
Predict_DT_V = predict(tree_model, validation_Data, type = "class")
confusionMatrix(Predict_DT_V, validation_Data$SoldFlag)
F1_Score(validation_Data$SoldFlag, Predict_DT_V)
AUC(Predict_DT_V, validation_Data$SoldFlag)


# plot ROC
roc.curve(validation_Data$SoldFlag, Predict_DT_V, plotit = T)



## tree with 3 repeated 10-fold cross validation and tuning
treeControl <-  trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 3,
                             search = "random")
tree_model_cv = train(SoldFlag~ ., data=train_Data,
                      method = "rpart",
                      metric = "Accuracy",
                      trControl = treeControl,
                      tuneLength = 15)
tree_model_cv

# Predict Training data 
Predict_DT_cv_T = predict(tree_model_cv, train_Data, type = "raw")
confusionMatrix(Predict_DT_cv_T, train_Data$SoldFlag)


# Predict Validation data
Predict_DT_cv_V = predict(tree_model_cv, validation_Data, type = "raw")
confusionMatrix(Predict_DT_cv_V, validation_Data$SoldFlag)
F1_Score(validation_Data$SoldFlag, Predict_DT_cv_V)

# Plot ROC
roc.curve(validation_Data$SoldFlag, Predict_DT_cv_V, plotit = T,
          add.roc = T, col = "green")





### Building Model 2: Logistic Regression

# Train the model
LR_model = glm(SoldFlag ~.,
                 train_Data,
                 family = binomial)
summary(LR_model)

# Predicting the Training set results
prob_pred_LR_T = predict(LR_model, type = 'response', train_Data )
y_pred_LR_T = as.factor(ifelse(prob_pred_LR_T > 0.5, 1, 0))
confusionMatrix(y_pred_LR_T, train_Data$SoldFlag)


# Predicting the Validation set results
prob_pred_LR_V = predict(LR_model, type = 'response', validation_Data )
y_pred_LR_V = as.factor(ifelse(prob_pred_LR_V > 0.5, 1, 0))
confusionMatrix(y_pred_LR_V, validation_Data$SoldFlag)
F1_Score(validation_Data$SoldFlag, y_pred_LR_V)

# Plot ROC
roc.curve(validation_Data$SoldFlag, y_pred_LR_V, plotit = T)



## LR with 3 repeated 10-fold cross validation
LRControl <-  trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 3)
LR_model_cv = train(SoldFlag~ ., data=train_Data,
                      method = "glm",
                      trControl = LRControl)
summary(LR_model_cv)

# Predicting the Training set results
pred_LR_cv_T = predict(LR_model_cv, type = 'raw', train_Data )
confusionMatrix(pred_LR_cv_T, train_Data$SoldFlag)

# Predicting the Validation set results
pred_LR_cv_V = predict(LR_model_cv, type = 'raw', validation_Data )
confusionMatrix(pred_LR_cv_V, validation_Data$SoldFlag)
F1_Score(validation_Data$SoldFlag, pred_LR_cv_V)

# Plot ROC
roc.curve(validation_Data$SoldFlag, pred_LR_cv_V, plotit = T,
          add.roc = T, col = "blue")


## LR feature reductions, remove LowNetPrice as not significant
LR_train_Data_FR <- select(train_Data, -LowNetPrice)
LR_validation_Data_FR <- select(validation_Data, -LowNetPrice)

LR_FR_model = glm(SoldFlag ~.,
               LR_train_Data_FR,
               family = binomial)
summary(LR_FR_model)

# Predicting the Training set results
prob_pred_LR_FR_T = predict(LR_FR_model, type = 'response', LR_train_Data_FR )
y_pred_LR_FR_T = as.factor(ifelse(prob_pred_LR_FR_T > 0.5, 1, 0))
confusionMatrix(y_pred_LR_FR_T, LR_train_Data_FR$SoldFlag)


# Predicting the Validation set results
prob_pred_LR_FR_V = predict(LR_FR_model, type = 'response', LR_validation_Data_FR )
y_pred_LR_FR_V = as.factor(ifelse(prob_pred_LR_FR_V > 0.5, 1, 0))
confusionMatrix(y_pred_LR_FR_V, LR_validation_Data_FR$SoldFlag)
F1_Score(LR_validation_Data_FR$SoldFlag, y_pred_LR_V)

# Plot ROC
roc.curve(LR_validation_Data_FR$SoldFlag, y_pred_LR_FR_V, plotit = T,
          add.roc = T, col = "green")




### Building Model 3: SVM

# Train the model
library(e1071)
svm_rbf <- svm(SoldFlag~., data = train_Data)
summary(svm_rbf)

# Predicting the Training set results
pred_svm_t = predict (svm_rbf, train_Data)
confusionMatrix(pred_svm_t, train_Data$SoldFlag)

# Predicting the Validation set results
pred_svm_v = predict(svm_rbf, validation_Data )
confusionMatrix(pred_svm_v, validation_Data$SoldFlag)
F1_Score(validation_Data$SoldFlag,pred_svm_v)


# Plot ROC
roc.curve(validation_Data$SoldFlag, pred_svm_v, plotit = T)



## SVM tuning took too much processing time
# further reduce data size for parameter tuning
split = sample.split(train_Data$SoldFlag, SplitRatio = 0.2)
PT_train_data = subset(train_Data, split == TRUE)
PT_validation_Data = subset(train_Data, split == FALSE)

# tune the model
svm_tune = tune(svm, SoldFlag~., data=PT_train_data,
                   ranges = list(epsilon = seq (0, 1, 0.2), cost = 2^(0:2)))

summary (svm_tune)

opt_model = svm_tune$best.model
summary(opt_model)

# Building the best model
svm_best <- svm (SoldFlag~., data = PT_train_data, epsilon = 0, cost = 2)
summary(svm_best)


# Predicting the Training set results
pred_svm_best_t = predict (svm_best, train_Data)
confusionMatrix(pred_svm_best_t, train_Data$SoldFlag)

# Predicting the Validation set results
pred_svm_best_v = predict(svm_best, validation_Data )
confusionMatrix(pred_svm_best_v, validation_Data$SoldFlag)
F1_Score(validation_Data$SoldFlag,pred_svm_best_v)


# Plot ROC
roc.curve(validation_Data$SoldFlag, pred_svm_best_v, plotit = T,
          add.roc = T, col = "green")




### Building Model 4: RF

# Train the model
library(randomForest)
rf_model = randomForest(SoldFlag~.,data =train_Data)
plot(rf_model)
print(rf_model)

# Predicting the Training set results
pred_rf_t <- predict(rf_model, train_Data)
confusionMatrix(pred_rf_t, train_Data$SoldFlag)


# Predicting the Validation set results
pred_rf_v = predict(rf_model, validation_Data )
confusionMatrix(pred_rf_v, validation_Data$SoldFlag)
F1_Score(validation_Data$SoldFlag,pred_rf_v)


# Plot ROC
roc.curve(validation_Data$SoldFlag, pred_rf_v, plotit = T)



## RF with 3 repeated 10-fold cross validation, random search
# using reduce data size from svm to train and tune
# train the model
RFControl <-  trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3, search = "random")
RF_model_cv = train(SoldFlag~ ., data=PT_train_data,
                    method = "rf",
                    metric = "Accuracy",
                    tuneLength=15,
                    trControl = RFControl)
RF_model_cv

# Predicting the Training set results
pred_RF_cv_T = predict(RF_model_cv, type = 'raw', train_Data )
confusionMatrix(pred_RF_cv_T, train_Data$SoldFlag)

# Predicting the Validation set results
pred_RF_cv_V = predict(RF_model_cv, type = 'raw', validation_Data )
confusionMatrix(pred_RF_cv_V, validation_Data$SoldFlag)
F1_Score(validation_Data$SoldFlag, pred_RF_cv_V)

# Plot ROC
roc.curve(validation_Data$SoldFlag, pred_RF_cv_V, plotit = T,
          add.roc = T, col = "green")



### DT showing good result, use C50 DT and resembling
library(C50)
## For raw data without resampling
# train the model
str(train_Data)
DT2Control <-trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3, search = "random")

DT2 <- train(SoldFlag~ ., data=train_Data,
             method = "C5.0",
             metric = "Accuracy",
             tuneLength=15,
             trControl = DT2Control)
DT2 

# Predicting the Training set results
pred_DT2_T <- predict(DT2, train_Data)
confusionMatrix(pred_DT2_T, train_Data$SoldFlag)


# Predicting the Validation set results
pred_DT2_v = predict(DT2, validation_Data )
confusionMatrix(pred_DT2_v, validation_Data$SoldFlag)
F1_Score(validation_Data$SoldFlag,pred_DT2_v)


# Plot ROC
roc.curve(validation_Data$SoldFlag,pred_DT2_v, plotit = T)


## Balanced class by increasing the number of class "1" 
B_data_increase <- ovun.sample(SoldFlag~., data = train_Data, 
                               method = "over", N=40000)$data
table(train_Data$SoldFlag)
table(B_data_increase$SoldFlag)
str(B_data_increase) 

# train the model
DT2_inc <- C5.0(B_data_increase[,-1], B_data_increase$SoldFlag, trials = 9,
                model = rules, winnow = FALSE)

# Predicting the Training set results
pred_DT2_inc_T <- predict(DT2_inc, train_Data)
confusionMatrix(pred_DT2_inc_T, train_Data$SoldFlag)


# Predicting the Validation set results
pred_DT2_inc_v = predict(DT2_inc, validation_Data )
confusionMatrix(pred_DT2_inc_v, validation_Data$SoldFlag)
F1_Score(validation_Data$SoldFlag,pred_DT2_inc_v)


# Plot ROC
roc.curve(validation_Data$SoldFlag,pred_DT2_inc_v, plotit = T,
          add.roc = T, col = "red")



## Balanced class by reducing the number of class "0" 
B_data_reduce <- ovun.sample(SoldFlag~., data = train_Data, 
                               method = "under", N=10000)$data
table(B_data_reduce$SoldFlag)

# train the model
DT2_red <- C5.0(B_data_reduce[,-1], B_data_reduce$SoldFlag, trials = 9,
                model = rules, winnow = FALSE)

# Predicting the Training set results
pred_DT2_red_T <- predict(DT2_red, train_Data)
confusionMatrix(pred_DT2_red_T, train_Data$SoldFlag)


# Predicting the Validation set results
pred_DT2_red_v = predict(DT2_red, validation_Data )
confusionMatrix(pred_DT2_red_v, validation_Data$SoldFlag)
F1_Score(validation_Data$SoldFlag,pred_DT2_red_v)


# Plot ROC
roc.curve(validation_Data$SoldFlag,pred_DT2_red_v, plotit = T, 
          add.roc = T, col = "blue")


## Balanced class by mixing 
B_data_mixed <- ovun.sample(SoldFlag~., data = train_Data, 
                             method = "both", N=25000)$data
table(B_data_mixed$SoldFlag)

# train the model
DT2_mix <- C5.0(B_data_mixed[,-1], B_data_mixed$SoldFlag, trials = 9,
                model = rules, winnow = FALSE)

# Predicting the Training set results
pred_DT2_mix_T <- predict(DT2_mix, train_Data)
confusionMatrix(pred_DT2_mix_T, train_Data$SoldFlag)


# Predicting the Validation set results
pred_DT2_mix_v = predict(DT2_mix, validation_Data )
confusionMatrix(pred_DT2_mix_v, validation_Data$SoldFlag)
F1_Score(validation_Data$SoldFlag,pred_DT2_mix_v)


# Plot ROC
roc.curve(validation_Data$SoldFlag,pred_DT2_mix_v, plotit = T,
          add.roc = T, col = "green")

