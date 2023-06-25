# Sweave: main web site http://www.statistik.lmu.de/~leisch/Sweave
# knitr: main web site http://yihui.name/knitr/

# http://search.r-project.org/library/kernlab/html/spam.html
install.packages("kernlab")
library(kernlab)
data("spam")
head(spam)
str(spam[,1:5])
# Perform the subsetting
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5)
trainIndicator[1:20]
table(trainIndicator)
trainSpam <- spam[trainIndicator == 1,]
testSpam <- spam[trainIndicator == 0,]
names(trainSpam)
table(trainSpam$type)
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)
plot(log10(trainSpam[,1:4] + 1))
hClust = hclust(dist(t(trainSpam[,1:57])))
plot(hClust)
hClustUpdated = hclust(dist(t(log10(trainSpam[,1:20] +1))))
plot(hClustUpdated)
# statistical prediction and modeling
trainSpam$numType <-  as.numeric(trainSpam$type) - 1
costFunction <-  function(x,y) {sum(x != (y > 0.5))}
cvError <- rep(NA,55) 
library(boot)
for (i in 1:55) {
  lmFormula <-  reformulate(names(trainSpam)[i], response = "numType")
  glmFit <- glm(lmFormula, family = "binomial", data = trainSpam)
  cvError[i] <- cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}
names(trainSpam)[which.min(cvError)]
# Use the best model from the group
predictionModel <- glm(numType ~ charDollar, family = "binomial", data = trainSpam)
# Get prediction on the thest set
predictionTest <- predict(predictionModel, testSpam)
predictedSpam <- rep("nonspam", dim(testSpam)[1])
# Classify as spam those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] <- "spam"
# Classification table
table(predictedSpam, testSpam$type)
# Error rate
(61 + 458) / (1346 + 458 + 61 + 449)







