

# spam example
library(kernlab)
data(spam)
head(spam)
dim(spam)
names(spam)
ggplot(spam) +
  geom_density(aes(x = you, color = type))
# spam emails have higher frequency of word "you"
# to filter email we can define constant C such that  if freq of "you" more that C - spam
C <- 0.7
ggplot(spam) +
  geom_density(aes(x = you, color = type)) +
  geom_vline(xintercept = C, color = "olivedrab")
prediction <- ifelse(spam$you > C, "spam_pred", "nonspam_pred")
table(prediction, spam$type) / length(spam$type)
# our prediction algorithm is 65% accurate: 0.321 + 0.326 = 0.647

# in sample vs. out of sample error
library(kernlab)
data("spam")
set.seed(3132)
smallSpam <- spam[sample(dim(spam)[1], size = 10),]
spamLabel <- (smallSpam$type == "spam")*1 + 1
spamLabel
plot(smallSpam$capitalAve, col = smallSpam$type, pch = 19, ylim=c(0,8), xlim = c(0,11))
# from this small population we can build wrong rule
# if capitalAve > 2 and capitalAve < 5 then spam
# this is overfitting
rule1 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x < 2] <- "nonspam"
  prediction[x > 5] <- "nonspam"
  prediction[x >= 2 & x<=5] <- "spam"
  return(prediction)
}
table(rule1(smallSpam$capitalAve), smallSpam$type)
# perfect performance on training set but... 
# what we'll receive if apply these rules to the whole dataset?
table(rule1(spam$capitalAve), spam$type)
# accuracy: how many predicted values correspond to real
sum(rule1(spam$capitalAve) == spam$type)
mean(rule1(spam$capitalAve) == spam$type)


# Caret package
# Good tutorials:
# Tutorials: http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf
# https://cran.r-project.org/web/packages/caret/vignettes/caret.html

# Splitting data
install.packages("caret")
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y=spam$type, p = 0.75, list = F)
head(inTrain)
dim(spam)
dim(inTrain)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
dim(testing)
# K-fold cross validation
set.seed(32323)
folds <- createFolds(y = spam$type, k = 10, list = T, returnTrain = T) # return train set
head(folds)
dim(folds)
sapply(folds, length)
folds[[1]][1:10]
foldsTests <- createFolds(y = spam$type, k = 10, list = T, returnTrain = F) # return test set
head(foldsTests)
sapply(foldsTests, length)
# Resampling
folds <- createResample(y = spam$type, times = 10, list = T)
sapply(folds, length)
folds[[1]][1:10]
# Time slices
tme <- 1:1000
folds <- createTimeSlices(y = tme, initialWindow = 20, horizon = 10)
names(folds)
folds$train[[1]]
folds$train[[2]]
folds$train[[3]]
folds$test[[1]]
folds$test[[2]]
folds$test[[5]]
# Training options
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
install.packages("e1071")
modelFit <- train(type ~ ., data = training, method = "glm")
summary(modelFit)
args(trainControl)

# Plotting predictors
# First step in building machine learning algo - understand data
install.packages("ISLR")
library(ISLR)
library(ggplot2)
library(caret)
data("Wage")
summary(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(Wage)
dim(training)
dim(testing)
featurePlot(x = training[, c("age", "education", "jobclass")],
            y = training$wage, plot="pairs")
ggplot(Wage) + geom_point(aes(x = age, y = wage))
ggplot(Wage) + geom_point(aes(x = age, y = wage, color = jobclass))
ggplot(Wage) + geom_point(aes(x = age, y = wage, color = maritl))
ggplot(Wage) + geom_point(aes(x = age, y = wage, color = health))
ggplot(Wage) + geom_point(aes(x = age, y = wage, color = race))
ggplot(Wage) + geom_point(aes(x = age, y = wage, color = education)) +
  geom_smooth(aes(x = age, y = wage, color = education), se = F)
# cut() function for continuous variables
wageCut <- cut(Wage$wage, breaks = c(0, 50.000, 100.000,150.000, 320.000))
table(wageCut)
library(dplyr)
Wage %>% mutate(wageBin = cut(Wage$wage, breaks = 20.000 * (0:16))) %>%
  count(education, wageBin) %>%
  ggplot() + geom_tile(aes(x = education, y = wageBin, fill = n))
Wage %>% mutate(wageBin = cut(Wage$wage, breaks = 20.000 * (0:16))) %>%
  ggplot() + geom_boxplot(aes(x = wageBin, y = age, fill = wageBin))
Wage %>% mutate(wageBin = cut(Wage$wage, breaks = 20.000 * (0:16))) %>%
  ggplot() + geom_boxplot(aes(x = reorder(wageBin, age, median), y = age, fill = wageBin))
Wage %>% mutate(wageBin = cut(Wage$wage, breaks = 20.000 * (0:16))) %>%
  ggplot(aes(x = reorder(wageBin, age, median), y = age)) + 
  geom_boxplot(aes(fill = wageBin)) +
  geom_jitter(color = "black", alpha = 0.1) + 
  coord_flip()

install.packages("Hmisc")
library(Hmisc)
wageCut <- cut2(Wage$wage,g = 3) # cut2() includes the most lef and right intervals, cut() no, and more flexible
table(wageCut)
p1 <- Wage %>% mutate(wageBin = cut2(wage,g = 3)) %>%
  ggplot() + geom_boxplot(aes(x = wageBin, y = age, fill = wageBin)); p1
p2 <- Wage %>% mutate(wageBin = cut2(wage,g = 3)) %>%
  ggplot() + geom_boxplot(aes(x = wageBin, y = age, fill = wageBin)) +
  geom_jitter(aes(x = wageBin, y = age),alpha = 0.2); p2
grid.arrange(p1,p2)
t1 <- table(wageCut, Wage$jobclass)
t1
prop.table(t1,margin = 1) # 1 tells that we want proportion in rows, 2 - in columns
Wage %>% mutate(wageBin = cut2(wage,g = 3)) %>%
  count(wageBin, jobclass) %>%
  ggplot() + geom_tile(aes(y = wageBin, x = jobclass, fill = n))
# density plot
ggplot(data = Wage) +
  geom_density(aes(x = wage, color = education))
ggplot(data = Wage) +
  geom_density(aes(x = wage, y = ..density.., color = education))

# Basic preprocessing
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y=spam$type, p = 0.75, list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
ggplot(training) + geom_histogram(aes(x = capitalAve))
# here is an example of the variable very skewed. we need preprocess it
mean(training$capitalAve)
median(training$capitalAve)
sd(training$capitalAve) # very big
# let's standardize it
trainCapAve <- training$capitalAve
trainCapAveStd <- (trainCapAve - mean(trainCapAve)) / sd(trainCapAve)
mean(trainCapAveStd)
sd(trainCapAveStd)
# important: when preprocessing and visual exploring we have to use training set
# and for standardazing test set - mean and std from trainig!
testCapAve <- testing$capitalAve
testCapAveStd <- (testCapAve - mean(trainCapAve)) / sd(trainCapAve)
mean(testCapAveStd) # not equal 0 because we used mean and std from training set
sd(testCapAveStd) # not equal to 1 because...
# preProcess() function does described procedure for all variables
preObj <- preProcess(training[,-58], method = c("center", "scale"))
preObj
trainCapAveStd1 <- predict(preObj, training[,-58])$capitalAve
ggplot(data = tibble(t = trainCapAveStd1)) + geom_histogram(aes(x = t))
cbind(trainCapAveStd, trainCapAveStd1)
mean(trainCapAveStd1)
sd(trainCapAveStd1)
# after that we can apply preObj to test set
testCapAveStd1 <- predict(preObj, testing[,-58])$capitalAve
cbind(testCapAveStd, testCapAveStd1)[1:5,]
# we can pass also preProcess as parameter to model
set.seed(32343)
modelFit <- train(type ~ ., data = training,
                  preProcess = c("center", "scale"), method = "glm")
modelFit
# Another type of preprocessing - Box-Cox
preObj <- preProcess(training[,-58], method = c("BoxCox"))
trainCapAveStd2 <- predict(preObj, training[,-58])$capitalAve
cbind(training$capitalAve, trainCapAveStd, trainCapAveStd1, trainCapAveStd2)
ggplot(data = tibble(t = trainCapAveStd2)) + geom_histogram(aes(x = t))
qqnorm(y = trainCapAveStd2)

# Dealing with missing values; 
# most machine learning algos fail when there are missing values
# Standardizing - Imputing data
set.seed(13343)
# let's generate some NA values
training$capitalAve_NA <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 3, prob = 0.05) == 1
mean(rbinom(n = 1000, size = 1, p = 0.5) == 1) # check what is 'size'
selectNA[1:50]
training$capitalAve_NA[selectNA] <- NA
training$capitalAve_NA[1:50]
sum(selectNA)
sum(is.na(training$capitalAve_NA))
# Impute and standardize
install.packages("RANN")
library(RANN)
preObj <- preProcess(training[,-58], method = "knnImpute")
capitalAve_NA <- predict(preObj, training[,-58])$capitalAve_NA
capitalAve_NA[1:50]
# standardizing
capitalAve_STD <- training$capitalAve
capitalAve_STD <- (capitalAve_STD - mean(capitalAve_STD)) / sd(capitalAve_STD)
capitalAve_STD[1:50]
# let's look at a difference between values before imputing and after
cbind(training$capitalAve, capitalAve_NA,capitalAve_STD, selectNA)[1:50,]
quantile(capitalAve_NA - capitalAve_STD)
ggplot(data.frame(dif = capitalAve_NA - capitalAve_STD)) +
  geom_density(aes(dif))
# we see that 50% percentile is approximately 0, so most of the values are close to eache other
# let's look at only NA values
cbind(capitalAve_NA[selectNA],capitalAve_TRUE[selectNA])
quantile((capitalAve_NA - capitalAve_TRUE)[selectNA])
# let's look at only not NA values
quantile((capitalAve_NA - capitalAve_TRUE)[!selectNA]) # they are even more closer

# Covariate creation
library(ISLR)
library(caret)
data("Wage")
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
# 1 Basic idea - convert factor variables to indicator variables
table(training$jobclass)
dummies <- dummyVars(~ jobclass, data = training)
dummies
p <- as_tibble(predict(dummies, newdata = training))
head(p)
cbind(training$jobclass, p[,'jobclass.1. Industrial'], p[,'jobclass.2. Information'])
cbind(training$jobclass, p)
# 2 Removing near zero covariates = variables with vary little influence
nsv <- nearZeroVar(training, saveMetrics = T)
nsv
# Spline bases - we create additional variables to fit into linear regression model
# instead of original var
library(splines)
bsBasis <- bs(training$age, df = 3) # df - degree of polynom
bsBasis
# here first column - age (normalized), second - age squared, third - age in cube
lm1 <- lm(wage ~ age, data = training); summary(lm1)
lm2 <- lm(wage ~ bsBasis, data = training); summary(lm2)
lm3 <- lm(wage ~ age + I(age^2) + I(age^3), data = training); summary(lm3)
df <- tibble(age = training$age, 
             y0 = training$wage,
             y1 = predict(lm1, newdata = training),
             y2 = predict(lm2, newdata = training),
             y3 = predict(lm3, newdata = training))
df
ggplot(data = df) +
  geom_point(aes(x = age, y = y0)) +
  geom_point(aes(x = age, y = y1), color = "red") +
  geom_point(aes(x = age, y = y2), color = "green")
# then we can use this spline basis to predict with test set
predict(bsBasis, age = testing)
# Sources
# Google "feature creation for [data type]"
# http://www.cs.nyu.edu/~yann/talks/lecun-ranzato-icml2013.pdf

# Preprocessing with PCA (principal component analysis)
# How to find correlated predictors
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
M <- abs(cor(training[,-58]))
M
diag(M) <- 0
M
which(M > 0.8, arr.ind = T)
cbind(training$num415, training$direct, training$num857)[1:20,]
table(training$num415)
prop.table(table(training$num415))
table(training$num857)
prop.table(table(training$num857))
names(spam)[c(32,34,40)]
plot(spam[,34], spam[,32])
nearZeroVar(training, saveMetrics = T)
# both predictors are highly correlated so it is not necesary to include both in model
a <- c(1,5,7,2,4,5,29)
which(a >= 5)
which(a == 7)
# let's rotate the plot
X <- 0.71 * training$num415 + 0.71 * training$num857
Y <- 0.71 * training$num415 - 0.71 * training$num857
plot(X,Y)
# we see that most variabilty is along X axis so we can ommit Y
# SVD: X = UDV, V is constructed to explain maximum amount of variation, 
# PCA components are equal to right singular vector (V)
spam_subset <- spam[,c(34,32)]
prComp <- prcomp(spam_subset)
plot(prComp$x[,1], prComp$x[,2])
prComp
prComp$x
# Principal component 1 explains most of variation, PCA2 - second most, PCA3 - third...
typeColor <- (spam$type == "spam") * 1 + 1
randomInd <- rbinom(length(typeColor), size = 1, prob = 0.1) == TRUE
tibble(typeColor[randomInd],spam$type[randomInd])
prComp <- prcomp(log10(spam[,-58] + 1))
plot(prComp$x[,1], prComp$x[,2], col = typeColor, xlab = "PC1", ylab = "PC2")
# we see that in principal component 1 space there is a litle separation of spam and ham messages
# We can do PCA  with preProcess()
preProc <- preProcess(log10(spam[,-58]+1), method = "pca", pcaComp = 2)
predictPCA <- predict(preProc, log10(spam[,-58] + 1))
plot(predictPCA[,1], predictPCA[,2], col = typeColor)
# do the same with train/test subsets
preProc <- preProcess(log10(training[,-58] + 1), method = "pca", pcaComp = 2)
trainPredictPCA <- predict(preProc, log10(training[,-58] + 1))
#modelFit <- train(training$type ~ ., method = "glm", data = trainPredictPCA)
modelFit <- train(type ~ ., method = "glm", data = mutate(trainPredictPCA, type = training$type))
testPredictPCA <- predict(preProc, log10(testing[,-58] + 1))
confusionMatrix(testing$type, predict(modelFit, testPredictPCA))
# we used only two PCA components but still received very high accuracy 0.8983!
# We can do PCA inside train()
#modelFit <- train(training$type ~ ., method = "glm", preProcess = "pca",
#                  data = training)
modelFit <- train(type ~ ., method = "glm", preProcess = "pca", 
                  data = training)
confusionMatrix(testing$type, predict(modelFit, testing))

# Predicting with Regression
library(caret)
data("faithful")
set.seed(333)
head(faithful)
# let's eplore a bit (from help)
require(stats); require(graphics)
f.tit <-  "faithful data: Eruptions of Old Faithful"
ne60 <- round(e60 <- 60 * faithful$eruptions); ne60
faithful$better.eruptions <- ne60 / 60; head(faithful)
te <- table(ne60); te
te[te >= 4]                      # (too) many multiples of 5 !
plot(names(te), te, type = "h", main = f.tit, xlab = "Eruption time (sec)")
head(faithful)
plot(faithful[, -3], main = f.tit,
     xlab = "Eruption time (min)",
     ylab = "Waiting time to next eruption (min)")
lines(lowess(faithful$eruptions, faithful$waiting, f = 2/3, iter = 3),
      col = "red")
# train/test
inTrain <- createDataPartition(y = faithful$waiting, p = 0.5, list = F)
faithfulTrain <- faithful[inTrain,]
faithfulTest <- faithful[-inTrain,]
head(faithfulTrain)
# plot
ggplot(faithful) + geom_point(aes(waiting, eruptions)) # clear linear pattern
# linear model
lm1 <- lm(eruptions ~ waiting, data = faithfulTrain)
summary(lm1)
sd(resid(lm1))
ggplot(faithfulTrain) + geom_point(aes(waiting, eruptions)) +
  geom_line(aes(x = faithfulTrain$waiting, y = lm1$fitted.values), color = "red")
# let's predict eruption = b0 + b1*waiting
waitingPredict <- 80
lm1$coefficients[1] + lm1$coefficients[2] * waitingPredict
# another way
predict(lm1, data.frame(waiting = waitingPredict))
predict(lm1, data.frame(waiting = c(34,80, 90)))
# apply model to test data
ggplot(faithfulTest) + geom_point(aes(waiting, eruptions)) +
  geom_line(aes(faithfulTest$waiting, predict(lm1, newdata = faithfulTest)))
# test and train on one plot
ggplot(faithfulTrain) + 
  geom_point(aes(waiting, eruptions), color = "blue") +
  geom_point(data = faithfulTest, aes(waiting, eruptions), color = "green") +
  geom_line(aes(x = faithfulTrain$waiting, y = lm1$fitted.values), color = "red") +
  geom_line(data = faithfulTest, aes(faithfulTest$waiting, predict(lm(eruptions ~ waiting, data = faithfulTest), newdata = faithfulTest)), color = "orange")
# calculate RMSE (root mean square error) for train
sqrt(sum((lm1$fitted.values - faithfulTrain$eruptions)^2))
# calculate RMSE (root mean square error) for test
sqrt(sum((predict(lm1, newdata = faithfulTest) - faithfulTest$eruptions)^2))
# usually test error larger than train
# Prediction intervals
pred1 <- predict(lm1, newdata = faithfulTest, interval="prediction")
ggplot(cbind(faithfulTest,as.data.frame(pred1))) + 
  geom_point(aes(waiting, eruptions)) +
  geom_line(aes(x = waiting, y = fit), color = "black") +
  geom_line(aes(x = waiting, y = lwr), color = "red") +
  geom_line(aes(x = waiting, y = upr), color = "red") 
# The same in caret package
modFit <- train(eruptions ~ waiting, data = faithfulTrain, method = "lm")
summary(modFit$finalModel)
predict(modFit$finalModel, data = faithfulTrain, interval = "prediction")

# Predicting with regression multiple covariates
library(ISLR)
library(caret)  
data("Wage")  
head(Wage)  
Wage <- subset(Wage, select = -c(logwage))  
Wage <- select(Wage, -logwage)
summary(Wage)
# separate to test/train
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = F)
Wage_train <- Wage[inTrain,]
Wage_test <- Wage[-inTrain,]
dim(Wage_test)
dim(Wage_train)
# plot
featurePlot(x = Wage_train[, c("age", "education", "jobclass")],
            y = Wage_train$wage,
            plot = "pairs")
pairs(wage ~ age + education + jobclass, data = Wage_train)
ggplot(Wage_train) + geom_point(aes(age, wage))
ggplot(Wage_train) + geom_point(aes(age, wage, color = jobclass))
# more blue points in top part => this gives an idea that jobclass predictor may explain variability in that part
ggplot(Wage_train) + geom_point(aes(age, wage, color = education))
# the same for education
# Linear model
# Wage_i = b0 + b1*age + b2*I(jobclass_i="Information") + sum_k(y_k*I(education_i = level_k))
modFit <- train(wage ~ age + jobclass + education, method = "lm", data = Wage_train)
fit <- modFit$finalModel
summary(fit)
# diagnostic
plot(fit, 1, pch = 19, cex = 0.5, col = "#00000010")
plot(fit$fitted.values, fit$residuals, cex = 0.5, col = "#00000010")
ggplot(data.frame(fit_val = fit$fitted.values, res = fit$residuals)) +
  geom_point(aes(fit_val, res), alpha = 0.1) + 
  geom_smooth(aes(fit_val, res), color = "red", method = "lm", se = F)
ggplot(tibble(x = fit$fitted.values, y = fit$residuals)) +
  geom_point(aes(x = x, y = y), alpha = 0.2) +
  geom_smooth(aes(x = x, y = y), color = "red", method = "lm", formula = y ~ splines::bs(x, 2), se = F)
# let's further explore
# another technique - plot residuals vs fitted values and color by predictor
ggplot(tibble(x = fit$fitted.values, y = fit$residuals)) +
  geom_point(aes(x = x, y = y, color = Wage_train$race))
# we again see that top part is colored one color almost => race predictor may expalin its variation
# Another technique - plot residuals vs index
plot(fit$residuals, pch = 19)
# here no pattern (??) in lecture evident!
# pattern may talk about missing values: residuals vs index shouldn't show pattern, it's logical
# Another technique - to plot predicted var vs real from test set
# ideally thay also should lie on one line 45 degree
df <- cbind(Wage_test, predict(modFit, Wage_test))
names(df)[11] <- "wage_predicted"
names(df)
ggplot(df) +
  geom_point(aes(x = wage, y = wage_predicted, color = year)) +
  xlim(0, NA) + ylim(0, NA)
# let's compare with model when all variables are used as predictors
fitAll <- train(wage ~ ., data = Wage_train, method = "lm")
wage_predicted <- predict(fitAll, Wage_test)
ggplot(cbind(Wage_test, wage_predicted)) +
  geom_point(aes(x = wage, y = wage_predicted)) +
  xlim(0, NA) + ylim(0, NA)
# we see that result is very similar to model with only three predictors

# Quiz
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data("concrete")
library(caret)
head(concrete)
head(mixtures)
inTrain <- createDataPartition(mixtures$CompressiveStrength, p = 0.75, list = F)
training <- mixtures[inTrain,]
testing <- mixtures[-inTrain,]
# Make a plot of the outcome (CompressiveStrength) versus the index of the samples
ggplot(mixtures) +
  geom_point(aes(x = as.integer(rownames(mixtures)), y = CompressiveStrength))
# Color by each of the variables in the data set 
# (you may find the cut2() function in the Hmisc package useful for turning 
# continuous covariates into factors). What do you notice in these plots?
library(Hmisc)
ggplot(mutate(mixtures, AgeBin = cut2(Age, g = 2))) +
  geom_point(aes(x = as.integer(rownames(mixtures)), 
                 y = CompressiveStrength, 
                 color = AgeBin))
table(cut2(mixtures$Age, g = 2))
# Make a histogram and confirm the SuperPlasticizer variable is skewed. 
# Normally you might use the log transform to try to make the data more 
# symmetric. Why would that be a poor choice for this variable?
ggplot(mixtures) +
  geom_histogram(aes(x = Superplasticizer))
which(mixtures$Superplasticizer <= 0)
sum(mixtures$Superplasticizer <= 0)
ggplot(mixtures) +
  geom_histogram(aes(x = Superplasticizer)) +
  coord_cartesian(xlim = )
sum(mixtures$Superplasticizer <= 0)
range(mixtures$Superplasticizer <= 0)
sum(mixtures$Superplasticizer == 0)
# Answer: a lot of zeros

# 2. Find all the predictor variables in the training set that begin with IL. 
# Perform principal components on these variables with the preProcess() 
# function from the caret package. Calculate the number of principal 
# components needed to capture 80% of the variance. How many are there?
data(AlzheimerDisease)
adData <-  data.frame(diagnosis,predictors)
names(adData)
head(diagnosis)
head(predictors)
head(adData)
library(dplyr)
adData_sub <- select(adData, starts_with("IL"), diagnosis) # doesn't work ??
adData_sub <- subset(adData, select = c(diagnosis,I_309, ICAM_1, IGF_BP_2,
                                        IL_11, IL_13, IL_16, IL_17E, IL_1alpha,
                                        IL_3, IL_4, IL_5, IL_6, IL_6_Receptor,
                                        IL_7, IL_8,IP_10_Inducible_Protein_10,
                                        IgA))
head(adData_sub)
# let's first define correlated variables
adData_sub_cor <- abs(cor(adData_sub[,-1]))
adData_sub_cor
diag(adData_sub_cor) <- 0
which(adData_sub_cor > 0.6, arr.ind = T)
ggplot(adData_sub, aes(x = IL_3, y = IL_16)) + 
  geom_point()
cor(adData_sub$IL_3,adData_sub$IL_16)
ggplot(adData_sub, aes(x = IL_5, y = IL_16)) + 
  geom_point()
cor(adData_sub$IL_5,adData_sub$IL_16)
ggplot(adData_sub, aes(x = IL_3, y = IL_5)) + 
  geom_point()
cor(adData_sub$IL_3,adData_sub$IL_5)
ggplot(adData_sub, aes(x = IL_3, y = IL_7)) + 
  geom_point()
cor(adData_sub$IL_3,adData_sub$IL_7)
# PCA
pca <- prcomp(adData_sub[,-1])
pca$x
d <- (adData_sub$diagnosis == "Impaired") * 1 + 1
ggplot(data.frame(pca$x)) + 
  geom_point(aes(PC1, PC2, color = d)) # components are not correlated
# PCA with preProcess()
pca <- preProcess(adData_sub[,-1], method = "pca", pcaComp = 11)
adData_pca <- predict(pca, adData_sub[,-1])
plot(adData_pca[,1], adData_pca[,9], col = d, pch = 19)
# let's use PCA for train/test
inTrain <- createDataPartition(adData_sub$diagnosis, p = 0.8, list = F)
training <- adData_sub[inTrain,]
testing <- adData_sub[-inTrain,]
pca <- preProcess(training[,-13], method = "pca", pcaComp = 11)
training_PCA <- predict(pca, training[,-1])
modelFit <- train(diagnosis ~ ., method = "glm", data = mutate(training_PCA, diagnosis = training$diagnosis))
confusionMatrix(training$diagnosis, predict(modelFit, training_PCA))
# accuracy on train set 0.7266
testing_PCA <- predict(pca, testing[,-1])
confusionMatrix(testing$diagnosis, predict(modelFit, testing_PCA))
# accuracy on test set 0.7273
# pca inside train()
modelFit <- train(diagnosis ~ ., method = "glm", preProcess = "pca", data = training)
confusionMatrix(testing$diagnosis, predict(modelFit, testing))

# Decision trees
data("iris")
library(ggplot2)
names(iris)
head(iris)
table(iris$Species)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing) 
ggplot(training) +
  geom_point(aes(x = Petal.Width, y = Sepal.Width, color = Species))
ggplot(training) +
  geom_point(aes(x = Petal.Length, y = Sepal.Length, color = Species))
ggplot(training) +
  geom_point(aes(x = Petal.Length, y = Petal.Width, color = Species))
ggplot(training) +
  geom_boxplot(aes(y = Petal.Length, fill = Species))
library(caret)              
modFit <- train(Species ~ . , method = "rpart", data=training)                
print(modFit$finalModel)                
plot(modFit$finalModel, uniform = T, main = "Classification Tree") 
text(modFit$finalModel, use.n = T, all = T, cex = 0.8)
install.packages("rattle")
library(rattle)
fancyRpartPlot(modFit$finalModel)
predict(modFit, newdata = testing)
table(predict(modFit, newdata = testing))
table(testing$Species)
confusionMatrix(testing$Species, predict(modFit, newdata = testing))

# Bagging
install.packages("ElemStatLearn")
library(ElemStatLearn)
data("ozone")
head(ozone)
ozone <- ozone[order(ozone$ozone),]
head(ozone)
dim(ozone)
# arrange(ozone, ozone)
# demonstrate what is bagging
ll <- matrix(NA, nrow = 10, ncol = 155)
range(ozone$ozone) # why ncol = 155
for (i in 1:10){
  ss <- sample(1:dim(ozone)[1], replace = T)
  ozone0 <- ozone[ss,]
  ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temperature ~ ozone, data = ozone0, span = 0.2)
  ll[i,] <- predict(loess0, newdata = data.frame(ozone = 1:155))
}
plot(ozone$ozone, ozone$temperature, pch = 19, cex = 0.5)
for(i in 1:10){lines(1:155, ll[i,], col = "grey", lwd=2)}
lines(1:155, apply(ll,2,mean), col = "red", lwd=2)
# this is a concept of bagging: we take mean of many sample; result has lower variability but the same bias
# bagging in caret 
# http://www.insider-r.org/packages/cran/caret/docs/nbBag
install.packages("party")
library(party)
predictors <- data.frame(ozone = ozone$ozone)
outcome <- ozone$temperature
treebag <- bag(predictors, outcome, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))
plot(ozone$ozone, ozone$temperature, col="lightgrey", pch=19)
points(ozone$ozone, predict(treebag$fits[[1]]$fit, predictors), pch=19,col="red")
points(ozone$ozone, predict(treebag$fits[[2]]$fit, predictors), pch=19,col="green")
points(ozone$ozone, predict(treebag, predictors), pch=19,col="blue")
# let's see components of model
ctreeBag$fit # builds tree model
ctreeBag$pred # predicts on the base of tree model
fit <- ctreeBag$fit(predictors,outcome)
fit@data@get("response")[,1]
levels(fit@data@get("response")[,1])
unlist(treeresponse(fit,predictors))
ctreeBag$aggregate

# Random forest
# http://www.robots.ox.ac.uk/~az/lectures/ml/
data("iris")
library(ggplot2)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
library(caret)
install.packages("randomForest")
library(randomForest)
modFit <- train(Species ~ . , data = training, method = "rf", prox = T)
modFit
# let's look at specific tree in random forest
getTree(modFit$finalModel, k = 2)
# let's plot class centers
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP); head(irisP)
irisP$Species <- rownames(irisP); head(irisP)
qplot(Petal.Width, Petal.Length, col = Species, data = training) +
  geom_point(aes(x = Petal.Width, y = Petal.Length, col = Species), size = 5, shape = 4, data = irisP)
pred <- predict(modFit, testing)
testing$predRight <- (pred == testing$Species); head(testing)
table(pred, testing$Species)
# let's look wich points were predicted wrong
ggplot(testing) +
  geom_point(aes(Petal.Width, Petal.Length, color = predRight)) +
  title(main = "newdata Predictions")
# wrong prediction for points on the border

# Boosting
# http://webee.technion.ac.il/people/rmeir/BoostingTutorial.pdf
# http://www.cc.gatech.edu/~thad/6601-gradAI-fall2013/boosting.pdf
# http://www.netflixprize.com/assets/GrandPrize2009_BPC_BigChaos.pdf
# https://kaggle2.blob.core.windows.net/wiki-files/327/09ccf652-8c1c-4a3d-b979-ce2369c985e4/Willem%20Mestrom%20-%20Milestone%201%20Description%20V2%202.pdf
library(ISLR)
data("Wage")
head(Wage)
names(Wage)
dim(Wage)
library(caret)
Wage <- select(Wage, -logwage) # doesn't work ??
Wage <- Wage[, -10]; head(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = F)
training <- Wage[inTrain,]; dim(training)
testing <- Wage[-inTrain,]
install.packages("gbm")
library(gbm)
modFit <- train(wage ~ . , method = "gbm", data = training, verbose = F)
print(modFit)
ggplot(testing) +
  geom_point(aes(predict(modFit, testing), wage))

# Model based prediction
data(iris)
names(iris)
table(iris$Species)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
modLDA <- train(Species ~ . , data = training, method = "lda")
installed.packages(klaR)
library(klaR)
modNB <- train(Species ~ . , data = training, method = "nb")
predictLDA <- predict(modLDA, testing)
predictNB <- predict(modNB, testing)
table(predictLDA, predictNB)
equalPredictions <- (predictLDA == predictNB); equalPredictions
ggplot(testing) +
  geom_point(aes(Petal.Width, Sepal.Width, color = equalPredictions))

# Quiz
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
library(caret)
data("segmentationOriginal")
head(segmentationOriginal)
names(segmentationOriginal)
# Subset the data to a training set and testing set based on the Case variable in the data set.
training <- filter(segmentationOriginal, Case == "Train")
testing <- filter(segmentationOriginal, Case == "Test")
set.seed(125)
# Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings
table(segmentationOriginal$Class)
modFit <- train(Class ~ . , data = training, method = "rpart")
summary(modFit)
print(modFit)
plot(modFit$finalModel, uniform = F, main = "Classification Tree")
text(modFit$finalModel,use.n = T, all = T,cex=0.8)
install.packages("rattle")
library(rattle)
fancyRpartPlot(modFit$finalModel)
# In the final model what would be the final model prediction for cases with the following variable values 
# let's create data vector for prediction: TotalIntench2 = 23000, FiberWidthCh1 = 10, PerimStatusCh1=2
# the following 'short' vector doesn't work
predictData <- data.frame(TotalIntench2 = 23000, FiberWidthCh1 = 10, PerimStatusCh1=2)
p <- matrix(data = rep(NA, dim(testing)[2]), nrow = 1, ncol = dim(testing)[2]); p
p <- as.data.frame(p); p
colnames(p) <-  names(testing); p
p$TotalIntench2 <-  23000
p$FiberWidthCh1 <-  10
p$PerimStatusCh1 <- 2
predict(modFit, p)
str(training)
apply(training, 2, typeof)
# i don't know how to create vector for prediction??

#These data contain information on 572 different Italian olive oils from multiple 
# regions in Italy. Fit a classification tree where Area is the outcome variable. 
# Then predict the value of area for the following data frame using the tree 
# command with all defaults
# What is the resulting prediction? Is the resulting prediction strange? Why or why not?
install.packages("pgmm")
library(pgmm)
data(olive)
head(olive)
olive <- olive[,-1]
head(olive)
dim(olive)
modFit <- train(Area ~ . , data = olive, method = "rpart")
summary(modFit)
modFit
library(rattle)
fancyRpartPlot(modFit$finalModel)
predict(modFit, newdata = as.data.frame(t(colMeans(olive))))
table(olive$Area)

# Load the South Africa Heart Disease Data and create training and test sets with the following code
library(ElemStatLearn)
data("SAheart")
head(SAheart)
str(SAheart)
ggplot(SAheart) +
  geom_boxplot(aes(y = ldl, fill = as.factor(chd)))
ggplot(SAheart) +
  geom_histogram(aes(x = ldl, fill = as.factor(chd)))
ggplot(SAheart) +
  geom_density(aes(x = ldl, color = as.factor(chd)))
#Then set the seed to 13234 and fit a logistic regression model (method="glm", 
# be sure to specify family="binomial") with Coronary Heart Disease (chd) as 
# the outcome and age at onset, current alcohol consumption, obesity levels, 
# cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol as 
# predictors. Calculate the misclassification rate for your model using this 
# function and a prediction on the "response" scale
set.seed(8484)
#SAheart$chd <- as.factor(SAheart$chd)
inTrain <- sample(1:dim(SAheart)[1], size = dim(SAheart)[1]/2, replace = F)
training <- SAheart[inTrain,]
testing <- SAheart[-inTrain,]
modFit <- train(as.factor(chd) ~ age + alcohol + obesity + tobacco + typea + ldl,
                data = training, method = "glm", family = "binomial")
modFit$finalModel
predict(modFit, newdata = training)
# confusiobMatrix works with chd as factor
confusionMatrix(as.factor(training$chd), predict(modFit, newdata = training))
confusionMatrix(as.factor(testing$chd), predict(modFit, newdata = testing))
missClass <- function(values, prediction){ sum( (prediction > 0.5) * 1 != values ) / length(values) }
missClass(testing$chd, predict(modFit, newdata = testing))

library(ElemStatLearn)
data("vowel.train")
data("vowel.test")
head(vowel.train)
dim(vowel.train)
vowel.train$y <- as.factor(vowel.train)
vowel.test$y <- as.factor(vowel.test)
set.seed(33833)

# Reqularized regression
library(ElemStatLearn)
data("prostate")
str(prostate)
head(prostate)
m <- cor( prostate[,1:8] )
diag(m) <- 0
which(m > 0.6, arr.ind = T )
m[ m>0.6 ]
pairs( prostate[,1:9], col="violet" )
lm(lpsa ~ ., data = prostate)
small <-  prostate[1:5,]
lm(lpsa ~ . , data = small)

# Combining models
library(ISLR)
data(Wage)
library(caret)
library(dplyr)
#Wage <- select(Wage, -logwage)
Wage <- subset(Wage, select = -c(logwage))
inBuild <- createDataPartition(y = Wage$wage, p = 0.7, list = F)
validation <- Wage[-inBuild,]
buildData <- Wage[inBuild,]
inTrain <- createDataPartition(y = buildData$wage, p = 0.7, list = F)
training <- buildData[inTrain,]
testing <- buildData[-inTrain,]
dim(training)
dim(testing)
dim(validation)                
# let's fit two different prediction models to the same data set
mod1 <- train(wage ~ ., data = training, method = "glm")
mod2 <- train(wage ~ ., data = training, method = "rf", trControl = trainControl(method = "cv"), number = 3)
plot(mod1$finalModel)
plot(mod2$finalModel)
pred1 <- predict(mod1, testing); cbind(testing, pred1)
pred2 <- predict(mod2, testing)
g <- ggplot(training) +
  geom_point(aes(x = age, y = wage), alpha = 0.2) +
  geom_smooth(aes(x = age, y = wage), se = F, color = "black") +
  geom_point(data = data.frame(age = testing$age, pred1), aes(age, pred1), color = "red") +
  geom_smooth(data = data.frame(age = testing$age, pred1), aes(age, pred1), color = "red", se = F) +
  geom_point(data = data.frame(age = testing$age, pred2), aes(age, pred2), color = "blue") 
  geom_smooth(data = data.frame(age = testing$age, pred2), aes(age, pred2), color = "blue", se = F) 
g
# why for the same age we have a lot of different predictons? Because we predict on all variables, not only age
ggplot(data = data.frame(pred1, pred2)) +
  geom_point(aes(pred1, pred2, color = testing$wage)) +
  coord_cartesian(xlim = c(0,250), ylim = c(0,250))
# we see that predictions are close to each other but not perfectly correlate and
# also with real result (colored testing wage)
# to get better result we need to fit a model which combines the predictors (models)
predDF <- data.frame(pred1, pred2, wage = testing$wage) 
combModFit <- train(wage ~ . , data = predDF, method = "gam")
combPred <- predict(combModFit, predDF)
g + 
  #geom_point(data = data.frame(combPred, age = testing$age),aes(age, combPred), color = "green") +
  geom_smooth(data = data.frame(combPred, age = testing$age),aes(age, combPred), color = "green", se = F) +
  ylim(c(50,150))
# testing error
sqrt( sum( (pred1 - testing$wage)^2 ) ) 
sqrt( sum( (pred2 - testing$wage)^2 ) ) 
sqrt( sum( (combPred - testing$wage)^2 ) ) # minimal error
# predict on validation data set
pred1V <- predict(mod1, validation)
pred2V <- predict(mod2, validation)
predDFV <- data.frame(pred1 = pred1V, pred2 = pred2V)
#predDFV <- data.frame(pred1 = pred1V, pred2 = pred2V, wage = validation$wage) # equal to without wage
combPredV <- predict(combModFit, predDFV)
# validation error
sqrt( sum( (pred1V - validation$wage)^2 ) ) 
sqrt( sum( (pred2V - validation$wage)^2 ) ) 
sqrt( sum( (combPredV - validation$wage)^2 ) ) # minimal error should be... but

# Forecasting
install.packages("quantmod")
library("quantmod")
from.dat <- as.Date("01/01/08", format = "%m/%d/%y"); from.dat
to.dat <- as.Date("12/31/13", format = "%m/%d/%y"); to.dat
getSymbols('F',src = "yahoo", from = from.dat, to = to.dat)
head(F)
F <- as.data.frame(F); head(F)
str(F)
mF <- to.monthly(F)
head(mF)
# let's remember how to do that in with dplyr
library(lubridate)
F <- F %>% mutate(Date = as.POSIXct(rownames(F))); head(F)
F <- F %>% group_by(month = month(Date), year = year(Date)) %>%
  summarise(F.Open = mean(F.Open), F.High = mean(F.High),
            F.Low = mean(F.Low), F.Close = mean(F.Close), 
            F.Volume = mean(F.Volume), F.Adjusted = mean(F.Adjusted)) %>%
  arrange(year)
F
# 
yahooOpen <- Op(mF)
ts1 <- ts(yahooOpen, frequency = 12)
plot(ts1, xlab="Year+1", ylab = "Yahoo")
plot(decompose(ts1), xlab="Year+1")
# train/test
ts1Train <- window(ts1, start = 1, end = 5)
ts1Test <- window(ts1,start = 5, end = (7-0.01))
plot(ts1Train, col = "black", xlim = c(0,7))
lines(ts1Test, col = "blue")
lines(mat(ts1Train, order=3), col = "red")
# exponential smoothing
ets1 <- ets(ts1Train, model="MMM")
fcast <- forecast(ets1)
plot(fcast)
lines(ts1Test, col = 'red')
accuracy(fcast, ts1Test)
# Rob Hyndman Forecasting: principles and practice (free book)

# Unsupervised prediction
data(iris)
inTrain <- createDataPartition(y=iris$Species, p = 0.7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
# clustering
kMeans1 <- kmeans(subset(training, select=-c(Species)), centers = 3)
training$clusters <- as.factor(kMeans1$cluster); head(training)
ggplot(training) +
  geom_point(aes(Petal.Width, Petal.Length, color = clusters))
table(training$Species, training$clusters)
# build prediction model
modFit <- train(clusters ~ . , subset(training, select=-c(Species)), 
                method="rpart")
table(predict(modFit, training), training$Species)
# apply on test
testClusterPred <- predict(modFit, testing)
table(predict(modFit, testing), testing$Species)

pairs(SAheart, col = as.factor(SAheart$chd))






