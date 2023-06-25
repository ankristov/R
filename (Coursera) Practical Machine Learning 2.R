library(kernlab)
data(spam)
head(spam)
dim(spam)
ggplot(spam) + geom_density(aes(x = your, color = type)) +
  geom_vline(xintercept = 0.5)
ggplot(spam) + geom_density(aes(x = capitalTotal, color = type))
ggplot(spam) + geom_density(aes(x = charDollar, color = type))
# simplest spam classifier 
prediction <- ifelse(spam$your > 0.5, 'spam_pred', 'nonspam_pred')
table(prediction, spam$type)

# 1 Relative importance of steps
# Often - more data -> better nodel
# Good features are:
# - based on expert application knowledge
# - retain relevant info
# - lead to data compression
# Algorithms matter less than you'd think

# 2 In sample VS out of sample errors
# In sample error - error rate we get on the same data we use to build prediction
# Out of sample error - the error rate you get on a new data.
# Example
library(kernlab)
data("spam")
smallSpam <- spam[sample(dim(spam)[1], size = 10), ]; smallSpam
smallSpam$type <- as.character.factor(smallSpam$type)
spamLabel <- (smallSpam$type == 'spam') * 1 + 1; spamLabel
cbind(smallSpam$type, spamLabel)
str(smallSpam$type)
ggplot(smallSpam) + geom_point(aes(x = seq.int(1, dim(smallSpam)[1]), y = smallSpam$capitalAve, color = smallSpam$type))

# 3 Prediction study design
# a. define error rate
# b. split data into:
#     - training, testing, validation (optional)
# c. on the training set pick features (use cross-validation)
# d. on the training set pick prediction function
# e. if no validation: apply to test set 1 time
# f. if validation: 
#     - apply to test set and refine
#     - apply to validation set 1 time

# 4 Types of errors
# For continous data- root mean squared error: sensitive to outliers.
# Median absolute deviation - alternative, more robust
# For discrete values: 
# - sensitivity (if we want few missed positives),
# - specificity (if we want few negatives called positives)
# - accuracy (if we want weights false positives/negatives equally)

# 5 ROC
# For measuring quality or goodness of predictive algorithm
# On vertical axis - True Positive, on horizontal - False Positive for different cutoffs.
# In general quality of classification algo is estimated by AUC - area under the curve


# 6 Cross validation
# Used for:
# - Picking variables to include in a model
# - Picking the type of prediction function to use
# - Picking the parameters in the prediction fucntion
# - Comparing different predictions
# For k-fold cross validation:
# - larger k = less bias, more variance
# - smaller k = more bias, less variance
# - random sampling must be done without replacement. With replacement 
# is the bootstrap (underestimates of the error, because some values can become in train set several times)
# If we cross-validate to pick predictors we must estimate on independent data

# 7 Data slicing
library(caret)
library(kernlab)
data(spam)
# train/test
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
# k-fold 
set.seed(32323)
folds <- createFolds(y = spam$type, k = 10, list = T, returnTrain = T) # try to change returnTrain to T/F with setting seed every time and compare test/train data sets
folds[[1]][1:100]
sapply(folds, length)
set.seed(32323)
folds <- createFolds(y = spam$type, k = 10, list = T, returnTrain = F)
folds[[1]][1:100]
sapply(folds, length)
# Resampling
set.seed(32323)
folds <- createResample(y = spam$type, times = 10, list = T)
sapply(folds, length)
folds[[1]][1:10] # some values repeat..
# Time slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y = tme, initialWindow = 20, horizon = 10)
names(folds)
folds$train[1:10]
folds$test[1:10]

# 8 Training options
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p=0.75, list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type ~ ., data = training, method = 'glm')
warnings()
args(trainControl)
# It's important to set seed when we fit models or split data because most of these procedures are based on randomisation and when we rerun code we'll get different results without setting seed: in this case everytime the same sequence of random numbers is generated

# 9 Plotting predictors
# It's important to make exploration only on training test. Why??
# Example: Wage data
library(ISLR)
library(ggplot2)
library(caret)
library(dplyr)
data("Wage")
summary(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)
# 9.1 Feature plot
featurePlot(x = training[, c('age', 'education', 'jobclass')],
            y = training$wage,
            plot = 'pairs')
# 9.2 Separate variables plot
ggplot(training) + geom_point(aes(x = age, y = wage))
ggplot(training) + geom_point(aes(x = age, y = wage, color = jobclass))
ggplot(training %>% filter(age > 30 & age < 50)) + geom_point(aes(x = age, y = wage, color = jobclass))
ggplot(training, aes(x = age, y = wage, color = education)) + 
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x)
# 9.3 Cutting continious variables to bins
library(Hmisc)
cutWage <- cut2(training$wage, g = 3)
table(cutWage)
ggplot(training) + geom_boxplot(aes(x = cutWage, y = age, fill = cutWage)) + 
  scale_fill_brewer(palette = 'Set2')
ggplot(training) + geom_boxplot(aes(x = cutWage, y = age, fill = cutWage)) + 
  geom_jitter(aes(x = cutWage, y = age), alpha = 0.3) +
  scale_fill_brewer(palette = 'Set2')
# 9.4 Tables
t1 <- table(cutWage, training$jobclass); t1
prop.table(x = t1, margin = 1)
# 9.5 Density plots
ggplot(training) + geom_density(aes(x = wage, color = education)) 
  
# 10 Preprocessing
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p=0.75, list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
ggplot(training) + geom_histogram(aes(x = capitalAve))
mean(training$capitalAve)
median(training$capitalAve)
sd(training$capitalAve)
# an example where variable is skewed and highly variable and preprocessing maybe required
# 10.1 Standardizing - Standard way of preprocessing
x <- training$capitalAve
trainCapAve_std <- (x - mean(x)) / sd(x)
mean(trainCapAve_std) # mean ~ 0
sd(trainCapAve_std) # sd ~ 1, less variabile
# 10.2 Standardazing with preProcess (caret)
preObj <- preProcess(training[, -58], method = c('center', 'scale'))
preObj$mean
trainCapAve_std <- predict(preObj, training[, -58])$capitalAve
mean(trainCapAve_std);sd(trainCapAve_std)
# after that we can use preprocess object for test set
testCapAve_std <- predict(preObj, testing[, -58])$capitalAve
mean(testCapAve_std); sd(testCapAve_std)
# we can pass preProcess argument to train func directly
set.seed(32343)
modelFit <- train(type ~ . , data = training, 
                  preProcess = c('center', 'scale'), method = 'glm')
modelFit
# An other way of standardizing - BoxCox transformation: takes continuous values and tries to make them look like normal
preObj <- preProcess(training[, -58], method = c('BoxCox'))
trainCapAve_std <- predict(preObj, training[, -58])$capitalAve
ggplot(data = data_frame(trainCapAve_std = trainCapAve_std)) +
  geom_histogram(aes(trainCapAve_std))
par(mfrow = c(1,2)); hist(trainCapAve_std); qqnorm(trainCapAve_std)
# 10.3 Imputing data
# It's common to have missing data in data sets. When we leave them pred algos usually fail: they are built not to handle missing data
set.seed(13343)
# Make some values NA
training$capitalAve2 <- training$capitalAve
summary(training$capitalAve2) # no NAs
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05) == 1
selectNA
training$capitalAve2[selectNA] <- NA
summary(training$capitalAve2)
# Impute with knn 
preObj <- preProcess(training[, -58], method = 'knnImpute')
capitalAve2_knn <- predict(preObj, training[,-58])$capitalAve2
# - Standardize original to compare 
capitalAve_original <- training$capitalAve
capitalAve_original_std <- (capitalAve_original - mean(capitalAve_original))/sd(capitalAve_original)
# - Compare
quantile(capitalAve2_knn - capitalAve_original_std) # new values are very close to original, diffrence to 0, so imputation is good
dif = capitalAve2_knn - capitalAve_original_std
ggplot(data = data_frame(dif = dif)) +
  geom_histogram(aes(x = dif), bins = 150) +
  xlim(c(mean(dif) - sd(dif)/2, mean(dif) + sd(dif)/2))
# compare only values which were imputed 
quantile((capitalAve2_knn - capitalAve_original_std)[selectNA]) 
# compare only values which were NOT imputed 
quantile((capitalAve2_knn - capitalAve_original_std)[!selectNA]) 
# Google for 'preprocessing with caret package'

# 11 Covariate creation
# - from raw data to covariate
# - transforming tidy data to covariates (only on training set !)
# Example
library(ISLR); library(caret); data(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = F)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
# 11.1 First common step - convert qualitative (categorical) variables to dummy or indicator variables
str(Wage)
table(Wage$jobclass)
# manually
Wage %>% mutate(jobclass.1.Industrial = ifelse(jobclass == '1. Industrial', 1, 0),
                jobclass.2.Information = ifelse(jobclass == '2. Information', 1, 0)) %>%
  select(jobclass, jobclass.1.Industrial, jobclass.2.Information)
# with caret
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(object = dummies, newdata = training))
# 11.2 Removing zero covariates: variables with no variation
nsv <- nearZeroVar(training, saveMetrics = T); nsv
# 11.3 Spline bases
# For lenear regression we can add columns with spline basis to create curved model not linear
library(splines)
bsBasis <- bs(training$age, df = 3); bsBasis
#df <- as.data.frame(bsBasis)
#names(df) <- c('b1', 'b2', 'b3')
#ggplot(data = df) + geom_line(aes(x = 1:dim(df)[1], y = b1)) + xlim(c(500,600))
lm1 <- lm(wage ~ bsBasis, data = training)
ggplot() +
  geom_point(data = training, aes(x = age, y = wage)) +
  geom_line(data = data_frame(age = training$age, wage = predict(lm1, newdata = training)), 
            aes(x = age, y = wage), 
            color = 'red')
# after that we have to create bases for test set using model created for train test
predict(bsBasis, age = testing$age) # if here we again used bs() we created new set of vars which are not related to train set what would introduce base
# Google: "feature extraction for images/voice etc."
# Feature creation for images - Deep learning. Good tutorial: www.cs.nyu.edu/~yann/talks/lecun-ranzato-icml2013.pdf

# 12 Preprocessing with Principal Components Analysis (PCA)
# When we have multiple variables correlated with each others
# 12.1 Correlated predictors
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y = spam$type, p=0.75, list = F)
training <- spam[inTrain,]; testing <- spam[-inTrain,]
M <- abs(cor(training[, -58]))
M[40:57, 40:57]
diag(M) <- 0
which(M > 0.8, arr.ind = T)
names(spam)[c(32,34, 40)]
ggplot(spam) + geom_point(aes(x = spam[,32], y = spam[,34])) # it seems that rhese numbers are parts of one telephone number that's why correlated so strong
ggplot(spam) + geom_point(aes(x = spam[,40], y = spam[,34]))
# 12.2 PCA idea
# The idea of PCA - to take these correlated variables and create one
X <- 0.71 * training$num415 + 0.71 * training$num857
Y <- 0.71 * training$num415 - 0.71 * training$num857
ggplot(data.frame(X = X, Y = Y)) + geom_point(aes(X, Y))
# here adding two variables together capture most of their variability 
# So the goal is: 
# - to find a new set of variables that are uncorrelated and explain as much variance as possible
# - find the best matrix created with fewer variables (lower rank) that explains the original data
# SVD: for X (variables in columns, observations - in rows) SVD is X = UDV,
# where U orthogonal (left singular vector), V orthogonal (right singular vector), D diagonal matrix (singlar values)
# PCA: principal components are equal to the right singular values if we first scale (substract mean and devide sd)
# 12.3 PCA with built in R function
smallSpam <- spam[, c(34,32)]
head(smallSpam %>% filter(num415 != 0))
prComp <- prcomp(smallSpam)
prComp$x[0:10,]
ggplot(data.frame(prComp$x)) + geom_point(aes(PC1, PC2)) # pretty similar to the result of line 270
prComp$rotation
# PCA on whole spam data set
typeColor <- (spam$type == 'spam') * 1 + 1
prComp <- prcomp(log10(spam[, -58] + 1))
ggplot(data = data.frame(prComp$x)) + 
  geom_point(aes(x = PC1, y = PC2, color = typeColor)) +
  scale_color_gradient(low = 'black', high = 'red')
# this is the way of reducing size of dataset and still capture a lot of variation in less new variables
# 12.4 PCA with caret
preProc <- preProcess(log10(spam[, -58] + 1), method = 'pca', pcaComp = 2)
spamPC <- predict(preProc, log10(spam[, -58] + 1))
ggplot(data = spamPC) + geom_point(aes(PC1, PC2, color = typeColor)) +
  scale_color_gradient(low = 'black', high = 'red')
# Fitting linear model on new vars: principal components
preProc <- preProcess(log10(training[, -58] + 1), method = 'pca', pcaComp = 2)
trainPCA <- predict(object = preProc, newdata = log10(training[, -58] + 1))
trainPCA$type <- training$type
modelFit <- train(type ~ . , method = 'glm', data = trainPCA)
modelFit
# for test data we have to use the same principal components
testPCA <- predict(object = preProc, newdata = log10(testing[, -58] + 1))
confusionMatrix(testing$type, predict(modelFit, testPCA))
# here we used only 2 principal components (original vars - 57!) and
# still get very high accuracy ~90%
# 12.5 PCA inside train() 
# We can can do PCA when fitting model
modelFit <- train(type ~ ., data = training, preProcess = 'pca', method = 'glm')
confusionMatrix(testing$type, predict(modelFit, newdata = testing))
# PCA
# - PCA most useful for linear models: glm, linear discriminant analysis,
# - makes a bit more difficult to interpret: new vars meaningless, combination of original ones
# - watch for outliers: before - exploratoty analisys, transfrom first to remove influence of outliers (BoxCox, log)

# 13 Prodicting with Regression
# - easy to implement
# - easy to interpret
# - often poor performance on nonlinear data: usually used in combo with another ML algos
# Example
library(caret); data(faithful); set.seed(333)
head(faithful)
cor(faithful)
ggplot(faithful) + geom_point(aes(eruptions, waiting))
inTrain <- createDataPartition(y = faithful$waiting, p = 0.5, list = F)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
lm1 <- lm(eruptions ~ waiting, data = trainFaith)
summary(lm1)
ggplot(trainFaith) + geom_point(aes(waiting, eruptions)) +
  geom_line(data = data.frame(x = trainFaith$waiting, y = lm1$fitted.values), aes(x, y), color = 'red')
# predict at point 'waiting time' = 80
b0 <- lm1$coefficients[1]
b1 <- lm1$coefficients[2]
b0 + b1 * 80 # compare with plot
newdata <- data.frame(waiting = 80)
predict(lm1, newdata = newdata)
# predict on test set
ggplot() + geom_point(data = testFaith, aes(waiting, eruptions)) +
  geom_line(data = data.frame(x = testFaith$waiting, y = predict(lm1, newdata = testFaith)), aes(x, y), color = 'red')
# test/train/predict line on one image
ggplot() + 
  geom_point(data = trainFaith, aes(waiting, eruptions), color = 'blue', alpha = 0.5) +
  geom_point(data = testFaith, aes(waiting, eruptions), color = 'green', alpha = 0.5) +
  geom_line(data = data.frame(x = testFaith$waiting, y = predict(lm1, newdata = testFaith)), aes(x, y), color = 'red') +
  geom_line(data = data.frame(x = trainFaith$waiting, y = lm1$fitted.values), aes(x, y), color = 'orange') +
  scale_color_identity("trt", labels = letters[1:2], breaks =  c('blue', 'green'),
                       guide = "legend") # how to display legend??
# Errors
# RMSE on train set
sqrt(sum((lm1$fitted.values - trainFaith$eruptions)^2)) # train set
# RMSE test set
pred1 <- predict(lm1, newdata = testFaith)
sqrt(sum((pred1 - testFaith$eruptions)^2))
# Prediction intervals
pred1 <- predict(lm1, newdata = testFaith, interval = 'prediction')
ord <- order(testFaith$waiting)
ggplot() + 
  geom_point(data = testFaith, aes(waiting, eruptions), color = 'blue', alpha = 1) +
  geom_line(data = data.frame(pred1), aes(x = testFaith$waiting, y = fit), color = 'grey')  +
  geom_line(data = data.frame(pred1), aes(x = testFaith$waiting, y = lwr), color = 'red')  +
  geom_line(data = data.frame(pred1), aes(x = testFaith$waiting, y = upr), color = 'red') 
# Linear regression in caret
modelFit <- train(eruptions ~ waiting, data = trainFaith, method = 'lm')
summary(modelFit)

# 14 Predicting with Regression Multiple Covariates 
library(ISLR); library(ggplot2); library(caret);
data("Wage"); head(Wage)
Wage <- subset(Wage, select = -c(logwage))
# Wage <- Wage %>% select(-logwage) # dplyr
Wage <- Wage %>% mutate(race = fct_recode(race, 
                                          'white' = '1. White',
                                          'black' = '2. Black',
                                          'asian' = '3. Asian',
                                          'other' = '4. Other'))

summary(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = F)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)
# Little bit practive of what we studied
cor(Wage)
nearZeroVar(Wage, saveMetrics = T)
dummyObj <- dummyVars(wage ~ health + race + maritl + jobclass, data = Wage)
head(predict(dummyObj, newdata = Wage))
# Feature plot
featurePlot(x = training[, c('age', 'education', 'jobclass')],
            y = training$wage,
            plot = 'pairs')
# Plot some variables
ggplot(training) + geom_point(aes(age, wage))
ggplot(training) + geom_point(aes(age, wage, color = jobclass))
ggplot(training) + geom_point(aes(age, wage, color = education))
# Fit linear model
modelFit <- train(wage ~ age + jobclass + education, data = training, method = 'lm')
modelFit$results # try to leave one var and compare RMSE
finalModel <- modelFit$finalModel
print(modelFit)
# diagnostic plot: residuals vs fitted values
ggplot(data = data.frame(residuals = finalModel$residuals, 
                         fitted_values = finalModel$fitted.values),
       aes(fitted_values, residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red', method = 'lm', se = F, formula = y ~ poly(x, 3))
# we can color by variables not used in model to identify potential trends
ggplot(data = data.frame(residuals = finalModel$residuals, 
                         fitted_values = finalModel$fitted.values),
       aes(fitted_values, residuals)) +
  geom_point(aes(color = training$race)) +
  scale_color_brewer(palette = 'Set2')
ggplot(data = data.frame(residuals = finalModel$residuals, 
                         fitted_values = finalModel$fitted.values),
       aes(fitted_values, residuals)) +
  geom_point(aes(color = training$health)) +
  scale_color_brewer(palette = 'Set2') 
ggplot(data = data.frame(residuals = finalModel$residuals, 
                         fitted_values = finalModel$fitted.values),
       aes(fitted_values, residuals)) +
  geom_point(aes(color = training$health_ins)) +
  scale_color_brewer(palette = 'Set2') 
ggplot(data = data.frame(residuals = finalModel$residuals, 
                         fitted_values = finalModel$fitted.values),
       aes(fitted_values, residuals)) +
  geom_point(aes(color = training$health_ins)) +
  scale_color_brewer(palette = 'Set2') 
# another diagnostic plot - residuals by index: if there is dependence on index - bad
ggplot(data = data.frame(residuals = finalModel$residuals, 
                         index = 1:length(finalModel$residuals)),
       aes(index, residuals)) +
  geom_point()
       
# another diagnostic plot predicted vs truth in test set
pred <- predict(modelFit, testing)
ggplot(data = data.frame(pred = pred, true = testing$wage, year = testing$year)) +
  geom_point(aes(true, pred, color = year))
  
# 15 Predicting with trees
# Idea:
# - iteratively split variables into groups
# - evaluate 'homogenity' within each group
# - split again if necessary
# Pros:
# - easy to interpret
# - better performance in nonlinear settings
# Cons:
# - without pruning/cross-validation can lead to overfitting
# - harder to estimate uncertainty
# - results may be variable
# Basic algo:
# - start with all vars in one group
# - find the variable/spli that best separates the outcomes
# - divide data into two groups ('leaves') ob that split ('node')
# - within each split find the best variable/split that separates the outcomes
# - continue until the groups are too small or sufficiently 'pure'
# Measures of impurity in group
# - misclassification: 1 - p, where p - frequency of the most widely presented var in a group
# - Gini-index: 1 - sum(p1^2 + p2^2 + ...); where p1, p2 - frequencies of vars in a group
# - Information gain: -(p1*log2(p1) + p2*log2(p2) + ...)
# Example
library(caret); library(ggplot2)
data(iris) 
head(iris) # we are going to predict Species
# 15.1 Plot variables
ggplot(training) + geom_point(aes(Petal.Width, Sepal.Width, color = Species))
ggplot(training) + geom_point(aes(Sepal.Width, Sepal.Length, color = Species))
ggplot(training) + geom_point(aes(Petal.Length, Sepal.Width, color = Species))
ggplot(training) + geom_point(aes(Petal.Length, Petal.Width, color = Species))
# 15.2 Fit model
set.seed(123)
modFit <- train(Species ~ . , method = 'rpart', data = training)
print(modFit)
print(modFit$finalModel) # look at 4th plot for understanding
# 15.3 Visualization
par(mfrow = c(1,1))
plot(modFit$finalModel, uniform = T, main = 'Classification Tree')
text(modFit$finalModel, use.n = T, all = T, cex = .8)
# More beatiful plot
library(rattle)
fancyRpartPlot(modFit$finalModel, main = 'Iris classification with decision tree')
# 15.4 Prediction
pred <- predict(modFit, newdata = testing); pred
confusionMatrix(pred, testing$Species)
# Notes 
# - classification trees are non-linear models: they use interactions between variables
# - data transformation less important
# - trees can also be used for regression problems

# 16 Bootstrap 
# Basic idea:
# - Resample cases and recalculate predictions
# - Average or majirity vote
# Notes:
# - similar bias
# - reduced variance
# - more useful for non-linear functions
# Example:
library(ElemStatLearn); data(ozone)
head(ozone)
# order just for illustration
ozone <- ozone[order(ozone$ozone),]
# fit loess model on 10 bagged samples
m <- matrix(data = NA, nrow = dim(ozone)[1], ncol = 10)
for (i in 1:10){
  indexes <- sample(1:dim(ozone)[1], replace = T)
  ozone_sample <- ozone[indexes,]
  ozone_sample <- ozone_sample[order(ozone_sample$ozone),]
  fit <- loess(temperature ~ ozone, data = ozone_sample, span = 0.2, na.action = 'na.omit')
  m[,i] <- predict(fit, newdata = data.frame(ozone = 1:dim(ozone)[1]))
}
m
# visualize 
# with ggplot it's little bit tricky..
require(reshape2)
df <- data.frame(m)
df['index'] <- 1:dim(df)[1]
df
df_melted <- melt(df, id = 'index')
head(df_melted)
sum(is.na(m))
ggplot(data = df_melted) + geom_point(data = ozone, aes(ozone, temperature)) +
  geom_line(aes(x = index, y = value, group = variable), color = 'grey') +
  geom_line(data = df, aes(x = 1:dim(df)[1], y = apply(df[, 1:10], 1, mean), color = 'red'))
# Bagging in caret
# Some models perform bagging; in train() condider method 'bagEarth', 'treebag', 'bagFDA'
# Alternatively you can use bag() function
predictors <- data.frame(ozone = ozone$ozone)
temperature = ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))
treebag
ggplot() + geom_point(data = ozone, aes(ozone, temperature)) +
  geom_point(data = data.frame(predictors = predictors$ozone, prediction = predict(treebag$fits[[1]]$fit, predictors)),
             aes(x = predictors, y = y), color = 'red') +
  geom_point(data = data.frame(predictors = predictors$ozone, prediction = predict(treebag, predictors)),
             aes(x = predictors, y = prediction), color = 'blue') 
# look at used functions
ctreeBag$fit
ctreeBag$pred
ctreeBag$aggregate
# Notes
# - bagging is most useful for nonlinear models
# - often used with trees - an extension is random forest
# - several models use bagging in caret train()

# 17 Random Forest
# - Bootstrap samples
# - At each split, bootstrap variables
# - Grow multiple trees and vote
# Pros:
# - Accuracy (most widely used on Kaggle)
# Cons:
# - speed
# - interpretability
# - overfitting (it's hard to understand which trees lead to overfitting, very important to use cross-validation)
# Example
data("iris"); head(iris)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = F)
training <- iris[inTrain,]; testing <- iris[-inTrain,]
library(caret)
modFit <- train(Species ~ . , data = training, method = 'rf', prox = T)
modFit
# we can look at specific tree in our model
library(randomForest)
getTree(modFit$finalModel, k = 2)
modFit$finalModel$proximity
# class centers
irisP <- classCenter(training[, c(3,4)], training$Species, modFit$finalModel$proximity)
irisP <- as.data.frame(irisP);
irisP$Species <- rownames(irisP); irisP
ggplot() + geom_point(data = iris, aes(x = Petal.Width, y = Petal.Length, color = Species)) +
  geom_point(data = irisP, aes(x = Petal.Width, y = Petal.Length, color = Species), shape = 4, size = 5)
# Prediction
pred <- predict(modFit, testing)
testing$predRight <- pred == testing$Species
table(pred, testing$Species) 
# we predict 2 not correctly, let's look at them
ggplot(testing) + geom_point(aes(x = Petal.Width, y = Petal.Length, color = predRight))
# Notes
# - Random forest is usually one of the two top performing algorithms along with boosting in prediction contests
# - Random forest is difficult to interpret but often very accurate
# - Care should be taken to avoid overfitting (see 'rfcv')

# 18 Boosting
# Boosting is usually one of the two top performing algorithms along with random forest in prediction contests
# Basic idea:
# - take lots of possibly weak predictors
# - weight them and add them up
# - get a stronger predictor
# Algorithm:
# - start with a set of classifiers h1, ..., hk (example: all possible trees or regression models or cutoffs)
# - create a classifier that combines classification functions: f(x) = sgn(sum(alpha_i * h_i(x))), alpha_i - weights
#   - goal is to minimize error (on train set)
#   - iterative, select one h at each step
#   - calculate weights (alpha_i) based on errors
#   - upweight missed classifications and select nect h
# Boosting in R:
# - bosting can be used with any subset of classifiers
# - one large subclass is gradient boosting
# - R has multiple boosting libraries (most available in caret). Difference include the choice of basic classification functions and combination rules
#   - gbm - boosting with trees
#   - mboost - model based boosting
#   - ada - statistical boosting based on additive logistic regression
#   - gamBoost - for boosting generalized additive models
# Example:
library(ISLR); data("Wage"); library(caret)
Wage <- Wage %>% select(-logwage)
head(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = F)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
modFit <- train(wage ~ . , method = 'gbm', data = training, verbose = F)
modFit
ggplot(data = data.frame(x = predict(modFit, testing), y = testing$wage)) +
  geom_point(aes(x, y)) # resonably good prediction

# 19 Model Based Prediction
# - Our goal is to build parametric model for conditional distribution P(Y=k|X=x)
# - A typical approach is to apply Bayes theorem P(Y=k|X=x) = P(X=x|Y=k)P(Y=k)/sum_l(P(X=x|Y=l))
# - P(Y=k) is set in advance
# - A common choice for P(X=x|Y=k) is Gaussian distribution f(x) = (1/sigma*sqrt(2pi)) * (exp(-(x-mu)^2/sigma^2))
# - We have to estimate (mu_k, sigma_k) from the data
# - Classify to class with the highest value of P(Y=k|X=x)
# A range of models use this approach
# - Linear discriminant amalysis assumes f(x) is multivariate Gaussian with same covariance
# - Quadratic discriminant analysis assumes f(x) is multivariate Gaussian with different covariances
# - Model based prediction assumes more complicated version for the covariance matrix
# - Naive Bayes assumes independence between features for model building
# Example:
data("iris")
names(iris)
table(iris$Species)
set.seed(656)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = F)
training <- iris[inTrain,]; testing <- iris[-inTrain,]
dim(training); dim(testing)
modLDA <- train(Species ~ . , data = training, method = 'lda')
modNB <- train(Species ~ . , data = training, method = 'nb')
modLDA$finalModel
modNB$finalModel
predictLDA <- predict(modLDA, newdata = testing)
predictNB <- predict(modNB, newdata = testing)
table(predictLDA, predictNB)
table(predictLDA, testing$Species)
table(predictNB, testing$Species)
equalPredictions <- predictLDA == predictNB; equalPredictions
ggplot(data = testing) + geom_point(aes(x = Petal.Width, y = Sepal.Width, color = Species)) +
  geom_point(data = testing[!equalPredictions,], aes(x = Petal.Width, y = Sepal.Width), color = 'red', shape = 4, size = 5) +
  scale_color_brewer(palette = 'Set2')
