# https://datascienceplus.com/credit-risk-modelling-using-machine-learning-a-gentle-introduction/
# Dataset: https://raw.githubusercontent.com/obaidpervaizgill/CreditRiskModelling/master/credit.csv

############################################
# 1. Data load and preparation
############################################

# load data
credits <- read.csv('data/credits.csv')
head(credits)
View(credits)

# variables structure
str(credits)
library(tidyverse)
glimpse(credits)

# variables summary
summary(credits)

# column names
#names(credits)
colnames(credits)

# tabulate dependent variable
table(credits$default)

# missing values
sum(is.na(credits))
#No missing values in the data
#Note : I would have used "mice" package in R to impute missing values if there were any

#Normalizing or standardizing data
#Note : I would have scaled the variables using standardization or minmax normalization, but I havent done this here! 

#Removing correlated features
#Note : I would have removed correlated feature based on an 80 percent correlation rule in the correlation matrix

#spliting data into test and train
library(caTools)
split <- sample.split(credits$default, SplitRatio = 0.7)
#sum(split)
train <- credits[split == T,]; dim(train)
test <- credits[split == F,]; dim(test)

# check proportions across train and test
prop.table(table(train$default))
prop.table(table(test$default))

############################################
# 2. Building logistic regression model
############################################
# The second step in your prototype will be to train an explainable model, such as a logistic regression model so that you can identify and explain the driving variables

# train model
fit_LogReg <- glm(train$default ~ ., data = train[, -17], family = 'binomial') #removing dependent variable
names(train)[17]
summary(fit_LogReg)
# Note: In theory we should rerun the model removing the non-significant features but since I want to demonstrate multiple model usage I would let it slide

# Predicting on test data
pred_LogReg <-  predict(fit_LogReg, newdata = test[, -17])
pred_LogReg <- ifelse(pred_LogReg > 0.5, 1, 0)
# Note: we want our model to be optimally sensitive hence we use 0.5 as the threshold, redudcing the threshold will make the model more sensitive

# accuracy
table(pred_LogReg, test$default)
levels(test$default)
levels(test$default) <- c(0,1) # relevel, default 'yes' = 2, 'no' = 1
accuracy_LogReg <- mean(pred_LogReg == test$default); 
accuracy_LogReg
  
# computing the baseline model for comparison
max(table(test$default)) / nrow(test)
#Note : Our simple logistic regression model beats the baseline model

# assesing the robustness of model
library(ROCR)
rocr_LogReg <- prediction(pred_LogReg, test$default)
auc_LogReg <- as.numeric(performance(rocr, 'auc')@y.values) # AUC - Area Under Curve
auc_LogReg
# Note : Closer to 1 is better, 0.78 here is not bad for a first model
  
############################################
# 3. Building decision tree model
############################################
# The third step in your prototype will be to train an more complicated model to assess if you can improve over your explainable model through additional tuning as well.
library(rpart)
library(rpart.plot)

train <- credits[split == T,]; dim(train)
test <- credits[split == F,]; dim(test)

# train model
fit_DecTree <- rpart(train$default ~ . , data = train[, -17], method = 'class', minbucket = 1) # min bucket is minimum number of observations in a terminal node
summary(fit_DecTree)

# plot decision tree
prp(fit_DecTree)

# predict on test data
pred_DecTree <- predict(fit_DecTree, newdata = test[, -17], type = 'class') # #getting classes rather than probability

# accuracy
table(pred_DecTree, test$default)
accuracy_DecTree <- mean(pred_DecTree == test$default)
accuracy_DecTree

# compute baseline model for comparison
max(table(test$default)) / nrow(test)

# Note: Our decision tree model beats the basline model in terms of accuracy

# access model robustness
library(ROCR)
rocr_DecTree <- prediction(predict(fit_DecTree, newdata = test[, -17], type = 'prob')[,2], test$default) # getting probability and then picking predicted class
auc_DecTree <- as.numeric(performance(rocr_DecTree, 'auc')@y.values) # #out of sample auc
auc_DecTree

############################################
# 4. Tuning a model using decision trees
############################################

library(caret)
# tuning for complexity parameter, this penalizes model complexity and avoids overfitting
tuneGridDecTree <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
#tuneGridDecTree

# create list of parameters to be passed onto the model
params <- trainControl(method = 'cv', number = 10)

fit_tunedDecTree <- train(x = train[,-17], y = train$default,
                      method = 'rpart',
                      trControl = params,
                      tuneGrid = tuneGridDecTree)

pred_tunedDecTree <- predict(fit_tunedDecTree, newdata = test[,-17], type = 'raw')

# accuracy
table(test$default, pred_tunedDecTree)

accuracy_tunedDecTree <- mean(pred_tunedDecTree == test$default)
accuracy_tunedDecTree

############################################
# 5. Building Random Forest model
############################################
# The final step in your prototype will be to train using a highly robust and more black box model to assess if you can improve over your existing approaches, to see if it is worthwhile to pursue this path

library(randomForest)

# training model
fit_RandFor <- randomForest(as.factor(train$default) ~ . , data = train[,-17], nodesize = 25, ntree = 200)
summary(fit_RandFor)

# identify the most important variables based on mean gini decrease
varImpPlot(fit_RandFor)
# Note : Show how each split result in low impurities or increased homogeneity

# predict on test data
pred_RandFor <- predict(fit_RandFor, newdata = test[,-17], )

# accuracy
table(test$default, pred_RandFor)
accuracy_RandFor <- mean(test$default == pred_RandFor)
accuracy_RandFor

# baseline model for comparison
max(table(test$default)) / nrow(test)
# Note: Our random forest model beats the basline model in terms of accuracy

# access model robustness
library(ROCR)
rocr_RandFor <- prediction(predict(fit_DecTree, newdata = test[, -17], type = 'prob')[,2], test$default) # #getting probability and then picking predicted class
auc_RandFor <- as.numeric(performance(rocr_RandFor, 'auc')@y.values) # out of sample auc
auc_RandFor
# Note : Area under the curve higher than for others models
# Note : Accuracy higher than for others models

############################################
# 6. Tuning a model using random forest
############################################
# Note : We can tune it using tuneRF package but repeated cross validation using caret produces much better results

# tuning for mtry, this the number of variables randomly sampled for splits
tuneGridRandFor <- expand.grid(.mtry = c(1:sqrt(ncol(train[,-17]))))
#tuneGridRandFor

# list of parameters to be passed to the model
params <- trainControl(method = 'repeatedcv',
                       number = 5,
                       repeats = 3,
                       # fivefold cross validation repeated 10 times
                       classProbs = T,
                       summaryFunction = twoClassSummary)
fit_tunedRandFor <- train(x = train[,-17], y = train$default,
                          method = 'rf',
                          trControl = params,
                          verbose = T,
                          metric = 'ROC',
                          tunedGrid = data.frame(tuneGridRandFor),
                          importance = T)
pred_tunedRandFor <- predict(fit_tunedRandFor, newdata = test[,-17])

# accuracy
table(test$default, pred_tunedRandFor)
accuracy_tunedRandFor <-  mean(test$default == pred_tunedRandFor)
accuracy_tunedRandFor
# The highest accuracy!

# Conclusion
# Depending on the problem you are trying to solve, you could pick a model that serves your case, simplest is always the better unless the complicated one is significantly better. Also note that while there may be a temptation to jump into models, most improvement in model performance come from data wrangling and creating new features for your models







































