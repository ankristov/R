# Задача с собеседования в EPAM: написать код для перестановки цифр в числе
ReverseN <- function(n) {
  v <- c()
  i <- 1
  while (TRUE) {
    n_digit <- n %% 10
    n <- n %/% 10
    v[i] <- n_digit
    i <- i + 1
    if (n == 0) {break}
  }
  v
}
VectorToN <- function(v) {
  l <- length(v)
  N <- 0
  tens <- 1
  for (i in 1:l) {
    N <- N + v[l+1-i] * tens
    tens <- tens * 10
  }
  N
}
N <- 3491
v <- ReverseN(N)
v
VectorToN(v)


# R^2 in linear regression
# R^2 = 0.64 :: using TV budget we reduced variance in sales by 64%. 
# That is very strong predictor. In medicine R^2 = 5% considered fine

# Linear regression
library(MASS)
library(ISLR)
head(Boston)
plot(medv ~ lstat, Boston, pch = 19)
plot(Boston$lstat, Boston$medv, pch = 19)
ggplot(Boston) + 
  geom_point(aes(lstat,medv))
fit1 <- lm(medv~lstat, data = Boston)
fit1
summary(fit1)
pred1 <- predict(fit1, newdata = Boston)
pred1
ggplot() + 
  geom_point(data = Boston, aes(lstat,medv)) +
  geom_point(data = data.frame(lstat = Boston$lstat, 
                               pred1 = pred1), 
             aes(x = lstat, y = pred1), color = "green") +
  geom_smooth(data = data.frame(lstat = Boston$lstat, 
                                pred1 = pred1),
              aes(x = lstat, y = pred1), color = "red", se = FALSE)
names(fit1)
confint(fit1)
predict(fit1, data.frame(lstat = c(5,10,15)))
predict(fit1, data.frame(lstat = c(5,10,15)), interval = "confidence")
# Multiple linear regression
fit2 <- lm(medv ~ lstat + age, data = Boston)
summary(fit2)
pred2 <- predict(fit2, newdata = Boston)
ggplot() + 
  geom_point(data = Boston, aes(lstat,medv)) +
  geom_point(data = data.frame(lstat = Boston$lstat, pred2 = pred2), 
             aes(x = lstat, y = pred2), 
             color = "green") +
  geom_smooth(data = data.frame(lstat = Boston$lstat, pred2 = pred2),
              aes(x = lstat, y = pred2), 
              color = "red", se = FALSE)

fit3 <- lm(medv ~ ., Boston)
summary(fit3)
pred3 <- predict(fit3, newdata = Boston)
ggplot() + 
  geom_point(data = Boston, aes(lstat,medv)) +
  geom_point(data = data.frame(lstat = Boston$lstat, pred3 = pred3), 
             aes(x = lstat, y = pred3), color = "green") +
  geom_abline(slope = fit3$coefficients[14], intercept = fit3$coefficients[1], color = 'magenta') +
  geom_smooth(data = data.frame(lstat = Boston$lstat, 
                                pred3 = pred3),
              aes(x = lstat, y = pred3), color = "red", se = FALSE)

par(mfrow = c(2,2))
plot(fit3)
fit4 <- update(fit3, ~ . -age-indus)
summary(fit4)
# Nonlinear terms and Interactions
fit5 <- lm(medv ~ lstat*age, Boston)
summary(fit5)
pred5 <- predict(fit5, newdata = Boston)
ggplot() + 
  geom_point(data = Boston, aes(lstat,medv)) +
  geom_point(data = data.frame(lstat = Boston$lstat, 
                               pred5 = pred5), 
             aes(x = lstat, y = pred5), color = "green") +
  geom_smooth(data = data.frame(lstat = Boston$lstat, pred5 = pred5),
              aes(x = lstat, y = pred5), color = "red", se = FALSE)
fit6 <- lm(medv ~ lstat + I(lstat^2), Boston)
summary(fit6)
pred6 <- predict(fit6, newdata = Boston)
ggplot() + 
  geom_point(data = Boston, aes(lstat,medv)) +
  geom_point(data = data.frame(lstat = Boston$lstat, pred6 = pred6), 
             aes(x = lstat, y = pred6), color = "green") +
  geom_smooth(data = data.frame(lstat = Boston$lstat, pred6 = pred6),
              aes(x = lstat, y = pred6), color = "red", se = FALSE)
fit7 <- lm(medv ~ . + I(lstat^2), Boston)
summary(fit7)
pred7 <- predict(fit7)
ggplot() + 
  geom_point(data = Boston, aes(lstat,medv)) +
  geom_point(data = data.frame(lstat = Boston$lstat, pred7 = pred7), 
             aes(x = lstat, y = pred7), color = "green") +
  geom_smooth(data = data.frame(lstat = Boston$lstat, pred7 = pred7),
              aes(x = lstat, y = pred7), color = "red", se = FALSE)
fit8 <- lm(medv ~ poly(lstat,4), Boston)
pred8 <- fitted(fit8)
ggplot() + 
  geom_point(data = Boston, aes(lstat,medv)) +
  geom_point(data = data.frame(lstat = Boston$lstat, pred8 = pred8), 
             aes(x = lstat, y = pred8), color = "green") +
  geom_smooth(data = data.frame(lstat = Boston$lstat, pred8 = pred8),
              aes(x = lstat, y = pred8), color = "red", se = FALSE)

par(mfrow = c(1,1))
plot(1:25, 1:25, pch = 1:25, cex = 2)
# Qualitative predictors
View(Carseats)
names(Carseats)
summary(Carseats)
fit1 <- lm(Sales ~ . + Income:Advertising + Age:Price, Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc) # shows how R creates dummy variable from qualitative predictor

# Multivariable logistic regression
# How to define the most important factors in heart desease?

data("SAheart")
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/thefactmachine/logisticRegression/master/SAHeart.csv")
SAheart <- read.csv(text = x,  header = TRUE, sep = ",")
head(SAheart)
summary(SAheart)
str(SAheart)
pairs(subset(SAheart,select = -chd), col = as.factor(SAheart$chd))
# in this case we are not trying to predict the risk of heart desease but
# understand the role of risk factors
fit <- glm(chd ~ . , data = SAheart, family = binomial)
summary(fit)
# correlation between variables
SAheart1 <- SAheart %>% mutate(famhist = as.numeric(famhist))
M <- abs(cor(SAheart1))
diag(M) <- 0
which(M > 0.6, arr.ind = T)
# high correlation between adiposity & age, adiposity & obesity
# perhaps it explains low significance of coefficients for obesity and alcohol in model
# Bayes classificator
# according to Bayes theoreme for classification variables (predictors) are 
# normally distributed, let's check
ggplot(SAheart) +
  geom_histogram(aes(x = age))
ggplot(SAheart) +
  geom_histogram(aes(x = typea, y = ..density..)) + # distribution close to normal!
  geom_line(data = data.frame(x = seq(from = min(SAheart$typea), to = max(SAheart$typea), by = 1),
                              y = dnorm(x = seq(from = min(SAheart$typea), to = max(SAheart$typea), by = 1), 
                                        mean = mean(SAheart$typea), 
                                        sd = sd(SAheart$typea))), 
            color = "red", aes(x = x,y = y))
# now let's plot densities for each subclass chd and multiply by pi to build classificator
SAheart_sub1 <- filter(SAheart, chd == 1)
SAheart_sub2 <- filter(SAheart, chd == 0)
pi1 <- dim(SAheart_sub1)[1]/dim(SAheart)[1]; pi1 # p(A)
pi2 <- dim(SAheart_sub2)[1]/dim(SAheart)[1]; pi2 # p(B)
x_min <- min( min(SAheart_sub1$typea), min(SAheart_sub2$typea))
x_max <- max( max(SAheart_sub1$typea), min(SAheart_sub2$typea))
x1 <- seq(from = x_min, to = x_max, by = 1)
x2 <- seq(from = x_min, to = x_max, by = 1)
d1 <- dnorm(x = x1, mean = mean(SAheart_sub1$typea), sd = sd(SAheart_sub1$typea))
d2 <- dnorm(x = x2, mean = mean(SAheart_sub2$typea), sd = sd(SAheart_sub2$typea))
df1 <- data.frame(x1 = x1, d1 = d1)
df2 <- data.frame(x1 = x2, d1 = d2)
ggplot() +
  geom_line(data = df1, aes(x = x1, y = pi1*d1), color = "red") +
  geom_line(data = df2, aes(x = x2, y = pi2*d2), color = "blue")

ggplot(SAheart) +
  geom_histogram(aes(x = ldl)) # close to normal!

# iris dataset
pairs(iris, col = iris$Species)
head(iris)
M <- abs(cor(select(iris, -Species) )); M
diag(M) <- 0; M
which(M > 0.6, arr.ind = T)

# Logistic Regression
require(ISLR)
names(Smarket)
summary(Smarket)
head(Smarket)
?Smarket
pairs(Smarket, col = Smarket$Direction)
# Logistic regression
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket, family = binomial)
summary(glm.fit) # it's seems that all coefficients are not significant
# Null deviance - deviance only for the mean (beta0?)
# Residual deviance - deviance when all predictors are included
glm.probs <- predict(glm.fit, type = "response") # default prediction gives values of log odds; type = "response" gives real probabilities
glm.probs[1:5]
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred, Smarket$Direction)
mean(glm.pred == Smarket$Direction)
# Make training and test sets
library(caret)
inTrain <- createDataPartition(Smarket$Direction, p = 0.7, list = F)
train <- Smarket[inTrain,]
test <- Smarket[-inTrain,]
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = train, family = binomial)
glm.probs <- predict(glm.fit, newdata = test, type = "response")
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred, test$Direction)
mean(glm.pred == test$Direction)
# smaller model
glm.fit <- glm(Direction ~ Lag1 + Lag2,
               data = train, family = binomial)
summary(glm.fit) # no coefficient became significant...
glm.probs <- predict(glm.fit, newdata = test, type = "response")
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred, test$Direction)
mean(glm.pred == test$Direction)

# Linear Discriminant Analysis
library(ISLR)
library(MASS)
library(dplyr)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = Year<2005)
lda.fit
# let's compair means for Lag2 where Direction == 'Down'
mean(select( filter(Smarket, Direction == "Down"), Lag2 )[1,], na.rm = T) # don't work
mean(filter(Smarket, Direction == "Down")$Lag2, na.rm = T)

ggplot(Smarket) + 
  geom_point(aes(x = Lag1, Lag2, color = Direction))
ggplot(Smarket) + 
  geom_density(aes(x = Lag2, color = Direction)) 
ggplot(Smarket) + 
  geom_boxplot(aes(y = Lag2, color = Direction)) 
# means are too close what is not suprising for stock market (in opposite case it would very easy to predict trends)
# why is not equal??
plot(lda.fit)
# let's try to plot ourselves
par(mfrow = c(2,1))
hist(x = select( filter(Smarket, Direction == "Down"), Lag2)[,1],xlab = "Down")
hist(x = filter(Smarket, Direction == "Down")$Lag2,xlab = "Down")
hist(x = select( filter(Smarket, Direction == "Up"), Lag2)[,1], xlab = "Up")
hist(x = filter(Smarket, Direction == "Up")$Lag2, xlab = "Up")
Smarket.2005 <- filter(Smarket, Year == 2005)
lda.pred <- predict(lda.fit, newdata = Smarket.2005)
lda.pred$x[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:50,]
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class == Smarket.2005$Direction)

# K-Nearest Neighbors
library(class)
?knn
Xlag <- cbind(Smarket$Lag1, Smarket$Lag2)
train <- Smarket$Year<2005
knn.pred <- knn(train = Xlag[train,], test = Xlag[!train,], 
                cl = Smarket$Direction[train], k = 1)
knn.pred
table(knn.pred, Smarket$Direction[!train])
mean(knn.pred == Smarket$Direction[!train]) # model is useless, 0.5 probability - not better than flipping a coin
# let's try different k
knn_accuracy <- c()
for (k in 1:500) {
  knn.pred <- knn(train = Xlag[train,], test = Xlag[!train,], 
                  cl = Smarket$Direction[train], k = k)
  knn_accuracy[k] = mean(knn.pred == Smarket$Direction[!train])
  #if (k%%50 == 0) {print(k)}
}
ggplot(data.frame(index = 1:length(knn_accuracy), accuracy = knn_accuracy)) +
  geom_point(aes(index, accuracy)) +
  geom_smooth(aes(index, accuracy), color = "red")
ggplot(data.frame(index = 1:length(knn_accuracy), accuracy = knn_accuracy)) +
  geom_line(aes(index, accuracy)) 

# Cross-validation
library(ISLR)
library(boot)
?cv.glm
ggplot(Auto) +
  geom_point(aes(horsepower, mpg))
# LOOCV (K = n)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
influence(glm.fit)
cv.glm(Auto, glm.fit)$delta
# let's write a simple function to use formula (5.2)
loocv <- function(fit){
  h <- lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}
loocv(glm.fit)
cv.error <- rep(0,5)
degree <- 1:5
for (d in degree) {
  glm.fit <- glm(mpg ~ poly(horsepower,d), data = Auto)
  cv.error[d] = loocv(glm.fit)
}
par(mfrow = c(1,1))
plot(degree, cv.error,type = "b")
# 10-fold cross-validation
cv.error10 <- rep(0,5)
for (d in degree) {
  glm.fit <- glm(mpg ~ poly(horsepower, d), data = Auto)
  cv.error10[d] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
lines(degree, cv.error10, type='b', col = 'red')

# bootstrap
alpha <- function(x,y){
  vx <- var(x)
  vy <- var(y)
  cxy <- cov(x,y)
  (vx-cxy)/(vx+vy-2*cxy)
}
head(Portfolio)
?Portfolio
plot(Portfolio$X, Portfolio$Y)
alpha(Portfolio$X, Portfolio$Y)
# what is the standard error of alpha?
alpha.fn <- function(data,index){
  with(data[index,], alpha(X,Y))
}
alpha.fn(Portfolio, 1:100)
set.seed(1)
alpha.fn(Portfolio,sample(1:100, 100, replace = T))
boot.out <- boot(Portfolio, alpha.fn, R=1000)
boot.out
plot(boot.out)

# Regression model selection
library(ISLR)
summary(Hitters)
#There are some missing values here so before we proceed we will remove them:
with(Hitters, sum(is.na(Salary)))
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
with(Hitters, sum(is.na(Salary)))
# Best subset regression
# We will now use the package 'leaps' to evaluate all the best-subset models.
install.packages("leaps")
library(leaps)
regfit.full <- regsubsets(Salary ~ ., data = Hitters)
regfit.full
summary(regfit.full)
# It gives by default best subsets up to size 8; let's increase that to 19, i.e. all the variables in Hitters
regfit.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
names(reg.summary)
plot.new()
plot(reg.summary$cp, xlab = "Number of variables", ylab = "Cp")
m <- which.min(reg.summary$cp); m
points(m, reg.summary$cp[m], pch = 20, col = "red")
which(reg.summary$cp < 20)
# There is a plot method for the 'regsubset' object
par(mfrow = c(1,1))
plot(regfit.full, scale = "Cp")
coef(regfit.full, m) # cooefficients for model 10 (with the best Cp)

# Forward stepwise selection
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
plot(regfit.fwd, scale = "Cp")
which.min(summary(regfit.fwd)$cp)
plot(summary(regfit.fwd)$cp)

# Model selection using a validation set
dim(Hitters)
train <- sample(x = seq(dim(Hitters)[1]), 
                size = 180, 
                replace = F)
regfit.fwd <- regsubsets(Salary ~ . , data = Hitters[train,],
                         method = "forward", nvmax = 19)
summary(regfit.fwd)
names(regfit.fwd)
coef(regfit.fwd, 10)
# Now we will make predictions on test set. There is no predict method for regsubsets
val.errors <- rep(NA, 19)
x.test <- model.matrix(Salary ~ . , data = Hitters[-train,])
x.test
for (i in 1:19) {
  coefi <- coef(regfit.fwd, id=i)
  pred = x.test[, names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[-train] - pred)^2)
}
val.errors
plot(sqrt(val.errors), ylab = "Root MSE", ylim = c(250,450), pch = 19, type = "b")
points(sqrt(regfit.fwd$rss[-1]/180), col = "blue", pch = 19, type = "b")
legend(x = "topright", legend = c("Training", "Validation"), 
       col = c("blue", "black"), pch = 19)
# as we expect, the training error goes down monotonically as the model gets bigger
# but not so for the validation error
# Will write methode for prediction for future use
predict.regsubsets <- function(object,newdata,id,...){
  f <- as.formula(object$call[[2]])
  m <- model.matrix(f, newdata)
  c <- coef(object, id = id)
  m[, names(c)]%*%c
}
pred <- predict.regsubsets(regfit.fwd, Hitters[-train,], 5)
sqrt(mean((Hitters$Salary[-train] - pred)^2))

# Model selection by Cross-Validation
# We will do 10-fold cross-validation
folds <- sample(rep(1:10,length = nrow(Hitters)))
table(folds)
cv.error <- matrix(NA, 10, 19)
for (k in 1:10) {
  best.fit <- regsubsets(Salary ~ . , data = Hitters[folds != k,], nvmax = 19,
                         method = "forward")
  for (i in 1:19){
    pred <- predict(best.fit, newdata = Hitters[folds == k,], id = i)
    cv.error[k,i] = mean((Hitters$Salary[folds == k] - pred)^2)
  }
}
head(cv.error)
rmse.cv <- sqrt(apply(cv.error, 2, mean))
plot(rmse.cv, pch = 19, type = "b")

# Ridge regression and the Lasso
install.packages("glmnet")
library(glmnet)
x <- model.matrix(Salary ~ .-1, data = Hitters)
head(x)
y <- Hitters$Salary
# first we will fit a ridge regression model by calling glmnet with alpha = 0
# ridge regression keeps all coefficients in but 
fit.ridge = glmnet(x,y,alpha = 0)
plot(fit.ridge, xva = "lambda", label = T)
cv.ridge <- cv.glmnet(x,y,alpha=0)
plot(cv.ridge)
# Lasso model, alpha = 1
fit.lasso <- glmnet(x,y, alpha = 1)
plot(fit.lasso, xvar = "lambda", label = T)
plot(fit.lasso, xvar = "dev", label = T)
cv.lasso <- cv.glmnet(x,y, alpha = 1)
cv.lasso
plot(cv.lasso)
coef(cv.lasso) # coefficients which correspond to the best model
# let's use earlier train/test division to select the best labmda for the lasso
lasso.tr <- glmnet(x[train,], y[train])
lasso.tr
pred <- predict(lasso.tr, x[-train,])
pred
dim(pred)
rmse <- sqrt(apply((y[-train] - pred)^2, 2, mean)); rmse
plot(log(lasso.tr$lambda), rmse, type="b", xlab = "log(lambda)")
lam.best <- lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr, s = lam.best)

# Nonlinear models
library(ISLR)
data("Wage")
# Polynomials
# First we'll use polynomials and focus on a single predictor age
fit <- lm(wage ~ poly(age,4), data = Wage)
summary(fit)
# here we are not interested in coefficients but polynomial curve 
ageLims <- range(Wage$age); ageLims
age.grid <- seq(ageLims[1], ageLims[2], by = 1)
preds <- predict(fit, newdata = list(age = age.grid), se = T)
se.bands <- cbind(preds$fit + 2*preds$se, preds$fit - 2*preds$se)
plot(Wage$age, Wage$wage, col = "darkgrey")
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, col="blue", lty = 2)
# There are other more direct ways of doing this in R
fita <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
summary(fita)
# coefficients are different but the fit is the same
plot(fitted(fit), fitted(fita))
# here we use orthogonal polynomials and so we can separately test for each coefficient
# in ordinar linear regression it's not correct because of correlation between predictors
# so 4th polynom not significant
# But this only works with linear regression and one predictor. In general we would use
# annova() function
fita <- lm(wage~education, data=Wage)
summary(fita)
fitb <- lm(wage~education+age, data=Wage)
summary(fitb)
fitc <- lm(wage~education+poly(age,2), data=Wage)
summary(fitc)
fitd <- lm(wage~education+poly(age,3), data=Wage)
summary(fitc)
# all models are nested:: contain previous
anova(fita, fitb,fitc,fitd)
# it also tells us that age and age^2 significant, age^3 less

# Polynimial logistic regression
# Now we fit logistic regression model to a binary response variable: we code
# the big earners (>250K) as 1, else 0
fit <- glm( I(wage > 250) ~ poly(age,3), data = Wage, family = 'binomial')
summary(fit)
preds <- predict(fit, list(age = age.grid), se = T)
preds
preds$fit
se.bands <- preds$fit + cbind(fit=0, lower=-2*preds$se, upper=2*preds$se)
se.bands[1:5,]
# the result is on log scale; we are usually interested in probabilities 
prob.bands <- exp(se.bands)/(1+exp(se.bands))
prob.bands[1:5,]
matplot(age.grid,prob.bands, col = "blue",lwd=c(2,1,1), 
        lty = c(1,2,2), type = "l", ylim = c(0,0.1))
points(jitter(Wage$age), I(Wage$wage>250)/10, pch="|", cex=0.5)

# Splines
# bs() by default gives cubic spline
library(splines)
fit <- lm(wage~bs(age, knots=c(25,40,60)), data = Wage)
plot(Wage$age,Wage$wage, col = "darkgrey")
lines(age.grid, predict(fit, list(age = age.grid)), col = "darkgreen", lwd = 2)
abline(v=c(25,40,60), lty=2, col="darkgreen")
# The smoothing splines does not require knot selection but it 
# does have a smoothing parameter - effective degree of freedom
fit <- smooth.spline(Wage$age, Wage$wage, df=16)
summary(fit)
lines(fit,col="red", lwd=3)
lines(predict(fit, newdata=list(age = age.grid))$x, predict(fit, newdata=list(age = age.grid))$y, col = "yellow")
# Or we can use LOO cross-validation to select the smoothing parameter for us automatically
fit <- smooth.spline(Wage$age, Wage$wage, cv = T)
lines(fit,col="purple", lwd = 2)
fit

# Generalized additive models
# So far we have focused on fitting models with mostly single nonlinear term.
# The 'gam' package makes it easier to work with multiple nonlinear terms. In addition
# it knows how to plot these functions and their standard errors
install.packages("gam")
library(gam)
gam1 <- gam(wage ~ s(age,df=4) + s(year, df = 4) + education, data = Wage)
# s - is special term known to gam which means smoothing spline
par(mfrow = c(1,3))
plot(gam1, se=T)
# Logistic regression with splines
gam2 <- gam( I(wage > 250) ~ s(age, df=4) + s(year, df=4) + education, 
             data = Wage, family = 'binomial')
plot(gam2) # on graphs - contributions of predictors to log of oods
# We can do some tests. Let's see if we need nonlinear term for year
gam2a <- gam( I(wage > 250) ~ s(age, df=4) + year + education, 
              data = Wage, family=binomial)
anova(gam2a, gam2, test="Chisq")
# p-valuse for second model very high (we fail to reject null hipothesys) 
# what mean that we really don't need non-linear term for year
# We can test further to see whether we need year at all
gam2b <- gam( I(wage > 250) ~ s(age,df=4) + education,
              data = Wage, family = binomial)
anova(gam2b,gam2a, gam2, test="Chisq") # p-value for gam2a model also too big => we don't need even lenear term year in formula
# let's look if age term is significant
gam2c <- gam( I(wage > 250) ~ education,
              data = Wage, family = binomial)
summary(gam2c)
anova(gam2c,gam2b, test = "Chisq") # age term significant (!) p-value significant
anova(gam2c, test = "Chisq")
anova(gam2b, test = "Chisq")
anova(gam2a, test = "Chisq")
# One nice feature of the 'gam' package is that it knows how to plot the function nicely,
# even for models fit by 'lm' and 'glm'
par(mfrow=c(1,3))
lm1 <- lm(wage ~ ns(age, df=4) + ns(year, df=4) + education, data=Wage)
# here ns() means 'natural spline'
plot.Gam(lm1, se=T)

# Decision trees
# We'll have a look at Carseats data. We first create a binary response 
# variable High for high sales
library(ISLR)
install.packages("tree")
library(tree)
data("Carseats")
head(Carseats)
par(mfrow = c(1,1))
hist(Carseats$Sales)
ggplot(Carseats) + geom_histogram(aes(x = Sales))

ggplot(Carseats) + geom_histogram(aes(x = Sales, fill = ShelveLoc))
ggplot(Carseats) + geom_freqpoly(aes(x = Sales, color = ShelveLoc))
ggplot(Carseats) + geom_bar(aes(x = Sales), binwidth = 0.5)
ggplot(Carseats) + geom_col(aes(x = ShelveLoc, y = Sales))
ggplot(Carseats) + geom_col(aes(x = Age, y = Sales))

ggplot(Carseats) + geom_histogram(aes(x = ShelveLoc), stat = 'count') # count number of cases for each ShelveLoc
ggplot(Carseats) + geom_col(aes(x = ShelveLoc, y = Sales)) # count total Sales for each ShelveLoc
Carseats %>% group_by(ShelveLoc) %>% # check
  summarise(n_ShelveLoc = n(),
            sales_ShelvesLoc = sum(Sales))


High <- ifelse(Carseats$Sales <= 8, "No", "Yes")
table(High)
Carseats <- data.frame(Carseats, High)
tree.carseats <- tree(High ~ . -Sales, data = Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0, cex = 0.5)
tree.carseats$frame
# detailed summary of the tree
tree.carseats
# let's create a training and test set
set.seed(1011)
train <- sample(1:nrow(Carseats), size = as.integer(nrow(Carseats) * 0.7))
tree.carseats <- tree(High ~ . -Sales, data = Carseats, subset = train)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.pred <- predict(tree.carseats, Carseats[-train,], type = "class")
t <- table(Carseats$High[-train], tree.pred); t
(t[1] + t[4]) / (t[1] + t[2] + t[3] + t[4]) # accuracy
# This tree was grown to full depth and might be too variable. We now use CV to prune it
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
cv.carseats
plot(cv.carseats)
prune.carseats = prune.misclass(tree.carseats, best = 13)
plot(prune.carseats)
text(prune.carseats, cex = 0.5)
# now let's evaluate this pruned tree on test data
tree.pred <- predict(prune.carseats, newdata = Carseats[-train,], type = "class")
cbind(predict(prune.carseats, newdata = Carseats[-train,]),
      predict(prune.carseats, newdata = Carseats[-train,], type = "class"))
t <- table(Carseats$High[-train], tree.pred)
(t[1] + t[4]) / (t[1] + t[2] + t[3] + t[4]) # accuracy not much better but the pruned tree more interpretable

# Random forest
# Random forest build lots of bushy trees and then average them to reduce the variance
library(randomForest)
library(MASS)
?Boston
dim(Boston)
train <- sample(1:nrow(Boston), 300)
# let's fit a random forest and see how well in performs. We'll use response medv - 
# median housing value
rf.boston <- randomForest(medv ~ ., data = Boston, subset = train)
names(rf.boston)
rf.boston$mse
rf.boston
# Here 'Mean of squared residuals' and '% Variance axplained' are based on OOB or out-of-bag
# estimates, a very clever way in random forest to get honest error estimates. The
# model reports that 'No. of variables tried at each split' is mtry=4, which is the number of 
# variables randomly choosen at each split. Since total number of variables =13 we could 
# try all of them
oob.err <- double(13)
test.err <- double(13)
for (mtry in 1:13){
  fit <- randomForest(medv ~ . , data = Boston, subset = train, mtry = mtry, ntree = 400)
  oob.err[mtry] <- fit$mse[400]
  pred <- predict(fit, newdata = Boston[-train,])
  test.err[mtry] <- mean( (Boston$medv[-train] - pred)^2 )
  cat(mtry, " ")
}
matplot(1:13, cbind(test.err, oob.err), pch=19, col=c("red", "blue"), type="b",
        ylab = "Mean squared error")
legend(x = "topright", legend = c("OOB", "Test"), col = c("red", "blue"), pch=19)
# we see that mtry=4 is the best according to two types of errors
# Although the test error curve drops below the OOB curve these are estimates based
# on data and so have own standard errors which are typically quite large. Points at the end correspond
# to bagging

# Boosting
# Random forest reduces variance, boosting - bias
# Boosting builds lots of smaller trees. Unlike random forests, each new tree in boosting
# tries to patch up the deficiencies of the current ensemble
library(gbm)
boost.boston <- gbm(medv ~ ., data = Boston[train,], distribution = 'gaussian',
                    n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.boston)
plot(boost.boston, i = "lstat")
plot(boost.boston, i = "rm")
# Lets make a prediction on a test set. With boosting the number of trees is a
# tuning parameter and if we have too many we can overfit. So we should use cross
# validation to select the number of trees. This your exercise. Instead we will 
# compute the test error as a function of the number of trees.
n.trees <- seq(from=100, to=10000, by=100)
predmat <- predict(boost.boston, newdata = Boston[-train,], n.trees = n.trees)
dim(predmat)
berr <- with(Boston[-train,], apply( (predmat-medv)^2, 2, mean))
length(berr)
plot(n.trees, berr, pch=19,ylab="Mean squared error", xlab="Number of trees", main="Boosting test error")
abline(h=min(test.err), col="red")

# from the chapter about logistic regression in Introduction to Statistical learning
head(Default)
Default %>% mutate(balance.bins = cut(balance, breaks = 10)) %>%
  arrange(balance.bins) %>% count(balance.bins)
df <- Default %>% filter(student == "Yes") %>% mutate(balance.bins = cut(balance, breaks = 10)) %>%
  group_by(balance.bins) %>% count(default)
df
Default %>% count(student, wt = balance)
ggplot(df) + geom_point(aes(x = balance.bins, y = n, color = default))
ggplot() + 
  geom_density(data = filter(Default, student == "Yes", default == "Yes"), aes(x=balance), color = "red") +
  geom_density(data = filter(Default, student == "No", default=="Yes"), aes(x=balance), color = "blue") 
mean(filter(Default, student == "Yes")$balance)
mean(filter(Default, student == "No")$balance)

# SVM
# Lets generate some data and make them a little separated
set.seed(10111)
x <- matrix(rnorm(40), 20,2)
y <- rep(c(-1,1), c(10,10))
plot(x, col=y+3,pch=19)
x[y==1,] <- x[y==1,] + 1
plot(x, col=y+3,pch=19)
# Now we'll load package 'e1071' which contains the 'svm' function. Then we compute 
# the fit. Notice that we have to specify a 'cost' parameter, which is tuning parameter
library(e1071)
dat <- data.frame(x,y=as.factor(y))
svmfit <- svm(y ~ . , data = dat, kernel = 'linear', cost=10, scale=F)
# scale = FALSE means 'not standardize variables'
# cost - tuning parameter; to choose best one we need to make cross validation
print(svmfit)
# here 'Number of Support Vectors:  6':: support vectors are points which are close 
# to the boundary or on a wrong side of the boundary
plot(svmfit, data = dat)
# This plot function bad: x2 on horizontal axis, colors, etc. Let's make our own.
# The first thing we'll do - a grid of values for x1 and x2. We'll write function
# (for reuse) which use 'expand.grid' and produces n*n points on a lattice 
# covering the domain of 'x'. After we'll make prediction at each point on the lattice
# The support points (points on the margin or on the wrong side of margin)
# are indexed in the '$index' component of the fit
make.grid <- function(x, n=75){
  grange <- apply(x,2,range)
  x1 <- seq(from=grange[1,1], to=grange[2,1], length=n)
  x2 <- seq(from=grange[1,2], to=grange[2,2], length=n)
  expand.grid(X1=x1, X2=x2)
}
xgrid <- make.grid(x)
dim(xgrid)
xgrid[1:100,]
ygrid <- predict(svmfit,xgrid)
plot(xgrid,col=c('red','blue')[as.numeric(ygrid)],pch=20,cex=0.2)
points(x,col=y+3,pch=19)
points(x[svmfit$index,],pch=5,cex=2)
# The 'svm' function is not very friendly in that we have to do some work 
# to get back the linear coefficients, as described in the text. Probably
# the reason is that this only makes sense for linear kernels, and the function
# is more general. Let's extract these coefficients (ch.12 ESL)
beta <- drop(t(svmfit$coefs) %*% x[svmfit$index,])
beta0 <- svmfit$rho
plot(xgrid, col=c('red','blue')[as.numeric(ygrid)], pch=20,cex=0.2)
points(x,col=y+3,pch=19)
points(x[svmfit$index,], pch=5,cex=2)
abline(beta0/beta[2], -beta[1]/beta[2])
abline((beta0-1)/beta[2], -beta[1]/beta[2], lty=2)
abline((beta0+1)/beta[2], -beta[1]/beta[2], lty=2)

# Nonlinear SVM
load(url("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/ESL.mixture.rda"))
names(ESL.mixture)
head(ESL.mixture)
rm(x,y)
# These data are also two dimensional. Lets plot them and fix a nonlinear SVM
# using radial kernel
plot(ESL.mixture$x, col=ESL.mixture$y+1)
dat <- data.frame(y=factor(ESL.mixture$y), x = ESL.mixture$x); head(dat)
names(dat) <- c('y','X1','X2'); head(dat)
fit <- svm(y ~ . , data = dat, scale=F, kernel="radial", cost=5)
# Now we are going to create a grid as before and make predictons on the grid. 
# But these data have grid points for each variable included in data frame
ESL.mixture$px1
ESL.mixture$px2
range(ESL.mixture$x[,1])
range(ESL.mixture$x[,2])
xgrid <- expand.grid(X1=ESL.mixture$px1, X2=ESL.mixture$px2)
ygrid <- predict(fit, xgrid)
plot(xgrid, col=as.numeric(ygrid), pch=20,cex=0.2)
points(ESL.mixture$x, col=ESL.mixture$y+1,pch=19)
# We can go further and have the predict function produce the actual function estimates at 
# each of our grid points. We can include the actual decision boundary on the plot
# by using contour function. In the dataframe is also 'prob', which is the true 
# probability of class 1 for these data at the grid points. If we plot its 0.5
# contour that will give us Bayes Decision Boundary, which is the best one could
# ever do
func <- predict(fit, xgrid, decision.values = T); func
func <- attributes(func)$decision; func
xgrid <- expand.grid(X1=ESL.mixture$px1, X2=ESL.mixture$px2)
ygrid <- predict(fit,xgrid)
plot(xgrid, col=as.numeric(ygrid), pch=20,cex=0.2)
points(ESL.mixture$x, col=ESL.mixture$y+1,pch=19)
contour(ESL.mixture$px1, ESL.mixture$px2, matrix(func,69,99), levels=0, add=T)
contour(ESL.mixture$px1, ESL.mixture$px2, matrix(func,69,99), levels=0.5, add=T, col="blue",lwd=2)
# the second contour - true decision boundary or Bayes decision boundary (on the level 0.5)

# PCA
# For PCA it's important to scale variable if they are in different units 
head(USArrests)
head(scale(USArrests))
mean(USArrests$Murder)
sd(USArrests$Murder)
(USArrests$Murder - mean(USArrests$Murder))/sd(USArrests$Murder)
dimnames(USArrests)
apply(USArrests, 2, mean)
apply(USArrests,2,var)
apply(USArrests,2,sd)
# PCA is about variance, so mean does not play a role. In PCA we are looking for
# linear combination which maximases variance. If we have a single variable like Assault here
# which dominates in variance it will pretty much eat up principal components.
# But variance for Assault is mostly due to different units. So we have to stardardize variables
# when perform PCA
pca.out <- prcomp(USArrests, scale = T)
pca.out
# Note that standard deviation in summary is a std of principal components.
# It always decreases.  
# PC1 component is pretty much loaded on crimes; it seems that it's a measure of
# crimeness. The sign does not matter, because variance does not have sign.
# PC2 is more heavily loaded on UrbanPop
biplot(pca.out, scale = 0, cex=0.5)

# K-means clustering
# K-means works in any dimension, but us most fun to demostrate in two.
# Let's generate data
set.seed(101)
x <- matrix(rnorm(100*2), 100, 2)
xmean <- matrix(rnorm(8,sd=4), 4, 2)
which <- sample(1:4,100,replace = T)
x <- x + xmean[which,]
plot(x,col=which,pch=19)
# We know the true clusters IDs, but wont tell that to the 'kmeans' algorithm
km.out <- kmeans(x,4,nstart = 15)
km.out
# betweem_ss/total_SS - this like R^2 for clustering: percent of 
# variance explained by cluster means and it's pretty high so also done a good job
plot(x,col=km.out$cluster, cex=2,pch=1,lwd=2)
points(x,col=which,pch=19)
points(x,col=c(4,3,2,1)[which],pch=19)

# Hierarchial clustering
hc.complete <- hclust(dist(x), method = "complete")
plot(hc.complete, cex=0.5, xlab = "Hierarchial clustering", hang = -1)
hc.single <- hclust(dist(x), method = "single")
plot(hc.single, cex=0.5, xlab = "Hierarchial clustering",hang = -1)
hc.average <- hclust(dist(x), method = "average")
plot(hc.average, cex=0.5, xlab = "Hierarchial clustering",hang = -1)
# Lets compare this withthe actual clusterin the data
hc.cut <- cutree(hc.complete, 4)
table(hc.cut,which)
table(hc.cut,km.out$cluster) # comparison with k-means
# It's good idea to color points from each cluster to one color (figure out how)
# here we just point group membership
plot(hc.complete, labels = which,  cex=0.5)

hc <- hclust(dist(USArrests), method = "complete", )
plot(hc, cex=0.5, hang = -1)

hc <- hclust(dist(USArrests)^2, "cen")
memb <- cutree(hc, k = 10)
cent <- NULL
for(k in 1:10){
  cent <- rbind(cent, colMeans(USArrests[memb == k, , drop = FALSE]))
}
hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
opar <- par(mfrow = c(1, 2))
plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
plot(hc1, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
par(opar)

##################################################################
########## Book Introduction to statistical learning #############
##################################################################
Advertising <- read.csv("/Users/Andrew/Desktop/Coursera/data/Advertizing.csv")
head(Advertising)
pairs(Advertising)
ggplot(Advertising) +
  geom_point(aes(TV, Sales), color="red") +
  geom_smooth(aes(TV, Sales), se=F, method = "lm")
fit <- lm(Sales ~ TV, data = Advertising)
ggplot(Advertising) +
  geom_point(aes(TV, Sales), color="red") +
  geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2], col="green")
ggplot(Advertising) +
  geom_point(aes(TV, Sales), color="red")  
M <- cor(Advertising)
diag(M) <- 0
which(M >0.5, arr.ind = T)
cor(Advertising$Newspaper, Advertising$Sales)

x <- seq(-pi,pi,length=50)
y <- x
f <- outer(x,y,function(x,y) {cos(y)/(1+x^2)})
dim(f)
par(mfrow=c(1,1))
contour(x,y,f)
contour(x,y,f,nlevels = 45, add = T)
fa <- (f-t(f))/2
contour(x,y,fa,nlevels = 30)
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, data = Auto)
plot(Auto$horsepower, Auto$mpg)
identify(Auto$horsepower, Auto$mpg, Auto$name)

# Nonlinear regression model
head(Auto)
ggplot(Auto) + geom_point(aes(mpg, horsepower))
fit1 <- lm(mpg ~ horsepower,
           data = Auto)
fit2 <- lm(mpg ~ horsepower + I(horsepower^2),
           data = Auto)
fit3 <- lm(mpg ~ horsepower + I(horsepower^2) + I(horsepower^3) + I(horsepower^4),
           data = Auto)
names(fit1)
ggplot(Auto) + geom_point(aes(horsepower,mpg), color = "grey") +
  geom_line(aes(horsepower,fit1$fitted.values), color = "orange") +
  geom_line(aes(horsepower,fit2$fitted.values), color = "lightblue") +
  geom_line(aes(horsepower,fit3$fitted.values), color = "green") 
# residuals vs predictors
ggplot(data = data.frame(p = Auto$horsepower, r = fit1$residuals)) +
  geom_point(aes(x = p, y = r))
ggplot(data = data.frame(p = Auto$horsepower, r = fit2$residuals)) +
  geom_point(aes(x = p, y = r))
ggplot(data = data.frame(p = Auto$horsepower, r = fit3$residuals)) +
  geom_point(aes(x = p, y = r))
# residuals vs fittedvalues
ggplot(data = data.frame(f = fit1$fitted.values, r = fit1$residuals)) +
  geom_point(aes(x = f, y = r)) +
  geom_smooth(aes(x = f, y = r), se = F)
ggplot(data = data.frame(f = fit2$fitted.values, r = fit2$residuals)) +
  geom_point(aes(x = f, y = r))+
  geom_smooth(aes(x = f, y = r), se = F)
ggplot(data = data.frame(f = fit3$fitted.values, r = fit3$residuals)) +
  geom_point(aes(x = f, y = r))+
  geom_smooth(aes(x = f, y = r), se = F)

# Lab: Simple linear regression
library(MASS)
names(Boston)
head(Boston)
lm.fit <- lm(medv ~ lstat, data = Boston)
summary(lm.fit)
# confidence interval for coefficients
confint(lm.fit) 
# confidence interval for predictions
predict(lm.fit, newdata = data.frame(lstat = c(5,10,15)),
        interval = "confidence")
plot(Boston$lstat, Boston$medv)
abline(lm.fit, col='red')
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red") 
plot(Boston$lstat,Boston$medv,col="red")
plot(Boston$lstat,Boston$medv,pch=20)
plot(Boston$lstat,Boston$medv,pch="+")
plot(1:20,1:20,pch=1:20)
# diagnostic plot
par(mfrow=c(2,2))
plot(lm.fit)
# alternative way
plot(predict(lm.fit), residuals(lm.fit)) 
plot(predict(lm.fit), rstudent(lm.fit)) # function rstudent() will return the studentized residuals
lm.fit2 <- lm(medv ~ lstat+I(lstat^2), data = Boston)
summary(lm.fit2)
# We use the anova() function to further quantify the extent to which 
# the quadratic fit is superior to the linear fit.
anova(lm.fit, lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)

# In this exercise you will create some simulated data and will fit simple 
# linear regression models to it
set.seed (1)
x <- rnorm(100)
eps <- rnorm(100,0,1)
y <- -1 + 0.5*x + eps
fit1 <- lm(y ~ x)
summary(fit1)
confint(fit1)
plot(x,y)
abline(fit1, col='red', lty = 1)
abline(a = fit1$coefficients[1], b = confint(fit)[2])
abline(a = fit1$coefficients[1], b = confint(fit)[4])
# Now fit a polynomial regression model that predicts y using x and x2
fit5 <- lm(y ~ poly(x,5))
summary(fit5)
fit15 <- lm(y ~ poly(x,15))
summary(fit15)
library(ggplot2)
ggplot(data = data.frame(x = x, y = y, f1 = fit1$fitted.values,
                         f5 = fit5$fitted.values,
                         f15 = fit15$fitted.values)) +
  geom_point(aes(x,y)) + 
  geom_line(aes(x,f1, color = 'Poly1')) +
  geom_line(aes(x,f5, color = 'Poly5')) +
  geom_line(aes(x,f15, color = 'Poly15')) +
  scale_color_manual(name = 'Legend', 
                     values = c(Poly1 = 'red', Poly5 = 'green', Poly15 = 'blue')) +
  theme(legend.position = 'right', legend.direction = "vertical")

# R^2 the best for fit15, but F-statistics - the worst

# This problem focuses on the collinearity problem.
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
plot(1:length(x1), x1)
plot(1:length(x2), x2)
# What is the correlation between x1 and x2? Create a scatterplot displaying 
# the relationship between the variables
plot(x1,x2)
cor(x1,x2)
# Using this data, fit a least squares regression to predict y using x1 and x2. 
# Describe the results obtained
fit <- lm(y~x1+x2)
summary(fit) # both coefficients are not significant
# let's make 3d plot
install.packages("plotly")
library(plotly)
grid_lines <- 25
x1.pred <- seq(min(x1), max(x1), length.out = grid_lines)
x2.pred <- seq(min(x2), max(x2), length.out = grid_lines)
x1x2 <- expand.grid(x1 = x1.pred,x2 = x2.pred)
z.pred <- matrix(predict(fit, newdata = x1x2), nrow = grid_lines, ncol = grid_lines)
plot_ly(x = df$x1, y = df$x2, z = df$y, type = "surface") 
install.packages("plot3D")
library(plot3D)
scatter3D(x1,x2,y)
scatter3D(x1,x2,y, pch=18,bty = "u", col.panel = "steelblue", expand=0.4,
          col.grid = "darkblue")
scatter3D(x1,x2,y, pch=18,bty = "u", col.panel = "steelblue", expand=0.4,
          col.grid = "darkblue", colkey = list(side=1, length=0.5))
scatter3D(x1,x2,y, pch=18,bty = "u", col.panel = "steelblue", expand=0.4,
          col.grid = "darkblue", colkey = list(side=1, length=0.5),
          theta = 30, phi = 30)
scatter3D(x1,x2,y, pch=18,bty = "u", col.panel = "steelblue", expand=0.4,
          col.grid = "darkblue", colkey = list(side=1, length=0.5),
          theta = 30, phi = 30,
          main = "y(x1, x2)", xlab = "x1", ylab = "x2", zlab = "y")
scatter3D(x1,x2,y, pch=18,bty = "g", col.panel = "steelblue", expand=0.4,
          col.grid = "darkblue", colkey = list(side=1, length=0.5),
          theta = 30, phi = 20,
          main = "y(x1, x2)", xlab = "x1", ylab = "x2", zlab = "y",
          type="h", ticktype = "detailed", pch=19, cex=1)
# regression surface
par(mfrow=c(1,1))
scatter3D(x1,x2,y, pch=18,bty = "g", col.panel = "steelblue", expand=0.4,
          col.grid = "darkblue", colkey = list(side=4, length=0.5),
          theta = 10, phi = 20,
          main = "y(x1, x2)", xlab = "x1", ylab = "x2", zlab = "y",
          type="h", ticktype = "detailed", pch=19, cex=1,
          surf = list(x = x1.pred, y = x2.pred, z = z.pred,
                      facets = NA, fit = predict(fit)))
summary(lm(y ~ x1 + x2))
# Now fit a least squares regression to predict y using only x1 and x2. 
# Comment on your results. Can you reject the null hypothesis H0 :β1 =0?
summary(lm(y ~ x1)) # now slope for x1 is significant
summary(lm(y ~ x2)) # now slope for x2 is also significant
# outlier
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)
fit <- lm(y ~ x1 + x2)
summary(fit)
scatter3D(x = 0.1, y = 0.8, z = 6, add = TRUE, colkey = FALSE, 
          pch = 18, cex = 3, col = "black")
par(mfrow = c(2,2))
plot(fit)
plot(x1,y)
plot(x2,y)

# This problem involves the Boston data set, which we saw in the lab for 
# this chapter. We will now try to predict per capita crime rate using the 
# other variables in this data set. In other words, per capita crime rate is 
# the response, and the other variables are the predictors.
head(Boston)
fit1 <- lm(crim ~ zn, data = Boston)
summary(fit1)
par(mfrow=c(1,1))
plot(Boston$zn, Boston$crim)
table(Boston$zn)
fit2 <- lm(crim ~ indus, data = Boston)
summary(fit2)
plot(Boston$indus, Boston$crim)
fit3 <- lm(crim ~ chas, data = Boston)
summary(fit3) # non significant
plot(Boston$chas, Boston$crim)
fit4 <- lm(crim ~ nox, data = Boston)
summary(fit4)
plot(Boston$nox, Boston$crim)
fit5 <- lm(crim ~ rm, data = Boston)
summary(fit5)
plot(Boston$rm, Boston$crim)
fit6 <- lm(crim ~ age, data = Boston)
summary(fit6)
plot(Boston$age, Boston$crim)
fit7 <- lm(crim ~ dis, data = Boston)
summary(fit7)
plot(Boston$dis, Boston$crim)
fit8 <- lm(crim ~ rad, data = Boston)
summary(fit8)
plot(Boston$rad, Boston$crim)
fit9 <- lm(crim ~ tax, data = Boston)
summary(fit9)
plot(Boston$tax, Boston$crim)
fit10 <- lm(crim ~ ptratio, data = Boston)
summary(fit10)
plot(Boston$ptratio, Boston$crim)
fit11 <- lm(crim ~ black, data = Boston)
summary(fit11)
plot(Boston$black, Boston$crim)
fit12 <- lm(crim ~ lstat, data = Boston)
summary(fit12)
plot(Boston$lstat, Boston$crim)
fit13 <- lm(crim ~ medv, data = Boston)
summary(fit13)
plot(Boston$medv, Boston$crim)
# Fit a multiple regression model to predict the response using all of 
# the predictors. Describe your results. For which predictors can we reject 
# the null hypothesis H0 : βj = 0?
fit.all <- lm(crim ~ ., data = Boston)
summary(fit.all)
# for 13
plot(Boston$medv, Boston$crim, col = 'grey', pch=19)
abline(fit13, col = 'red')
abline(a = fit.all$coefficients[1], fit.all$coefficients[14], col = 'green')
# for 14
plot(Boston$lstat, Boston$crim, col = 'grey', pch=21)
abline(fit12, col = 'red')
abline(a = fit.all$coefficients[1], fit.all$coefficients[12], col = 'green')
plot(1:28,1:28, pch=1:28)
plot(2,21, pch=21)
# Create a plot displaying the univariate regression coefficients from (a) 
# on the x-axis, and the multiple regression coefficients from (b) on the y-axis
univariate_coef <- c(fit1$coefficients[2], fit2$coefficients[2],
                     fit3$coefficients[2], fit4$coefficients[2],
                     fit5$coefficients[2], fit6$coefficients[2],
                     fit7$coefficients[2], fit8$coefficients[2],
                     fit9$coefficients[2], fit10$coefficients[2],
                     fit11$coefficients[2], fit12$coefficients[2],
                     fit13$coefficients[2])
plot(univariate_coef, fit.all$coefficients[2:14])

# Lab: Logistic regression, LDA, QDA, KNN
library(ISLR)
names(Smarket)
head(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
plot(Smarket$Lag1, Smarket$Year)
plot(Smarket$Year,Smarket$Lag1)
cor(Smarket)
cor(Smarket[,-9]) # only year and volume correlated
plot(Smarket$Year,Smarket$Volume)
plot(Smarket$Volume)
Smarket %>% group_by(Year) %>% summarise(VolumeMeanPerYear = mean(Volume)) %>%
  ggplot() + geom_line(aes(Year, VolumeMeanPerYear), color = "red")
# Logistic regression
# we will fit a logistic regression model in order to predict Direction 
# using Lag1 through Lag5 and Volume
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket, family = binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
contrasts(Smarket$Direction)
glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10]
# The type="response" option tells R to output probabilities of the form 
# P(Y = 1|X), as opposed to other information such as the logit.
# We know that these values correspond to the probability of the market going 
# up, rather than down, because the contrasts() function indicates that R 
# has created a dummy variable with a 1 for Up.
contrasts(Smarket$Direction)
# glm.pred <- rep("Down", 1250)
# glm.pred[glm.probs > 0.5] <- "Up"
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
glm.pred[1:10]
# confusion matrix
table(glm.pred, Smarket$Direction)
(507+145)/1250
mean(glm.pred == Smarket$Direction) # the fraction of days for which the prediction was correct
# At first glance, it appears that the logistic regression model is working 
# a little better than random guessing. However, this result is misleading 
# because we trained and tested the model on the same set of 1, 250 observations.
# Let's creat test and train datasets
## inTrain <- sample(1:nrow(Smarket), as.integer(0.6 * nrow(Smarket)))
## train <- Smarket[inTrain,]
## test <- Smarket[-inTrain,]
train <- Smarket$Year < 2005
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Smarket$Direction[!train]
glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data=Smarket, family=binomial,subset = train)
glm.probs <- predict(glm.fit, newdata = Smarket.2005, type = 'response')
glm.pred <- ifelse(glm.probs>0.5, "Up", "Down")
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) # on test data accracy worse
mean(glm.pred != Direction.2005) # test error higher than train, more that 50% what is worse than random guessing!
# We recall that the logistic regression model had very underwhelming 
# p- values associated with all of the predictors, and that the smallest 
# p-value, though not very small, corresponded to Lag1. Perhaps by removing 
# the variables that appear not to be helpful in predicting Direction, we can 
# obtain a more effective model.
glm.fit <- glm(Direction ~ Lag1+Lag2,
               data=Smarket, family=binomial,subset = train)
glm.probs <- predict(glm.fit, newdata = Smarket.2005, type = 'response')
glm.pred <- ifelse(glm.probs>0.5, "Up", "Down")
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
106/(106+76)
# The confusion matrix suggests that on days when logistic regression predicts
# that the market will decline, it is only correct 50 % of the time. However, 
# on days when it predicts an increase in the market, it has a 58% accuracy rate.
# Let's predict Direction on a day when Lag1=1.2, Lag2=1.1 and on a day when thay equal 1.5 and -0.8
predict(glm.fit, newdata = data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)), type = 'response')

# LDA
# Now we will perform LDA on the Smarket data
library(MASS)
library(ggplot2)
library(dplyr)
data("Smarket")
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)
ggplot(Smarket) + geom_point(aes(Lag1, Lag2, color = Direction)) # no visual separation
# prediction with LDA
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
cbind(lda.pred$class, lda.pred$posterior,lda.pred$x)
# applying a 50% threshold
sum(lda.pred$posterior[,1] >= 0.5)
sum(lda.pred$posterior[,1] < 0.5)
# if we need to use a posterior probability threshold other than 50%
sum(lda.pred$posterior[,1] > 0.9)
# let's separate two classes a bit
Smarket1 <- Smarket
Smarket1$Lag2[Smarket1$Direction == "Up"] <- Smarket1$Lag2[Smarket1$Direction == "Up"] + 2
ggplot(Smarket1) + geom_point(aes(Lag1, Lag2, color = Direction)) # no visual separation
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket1, subset = train)
lda.fit
lda.class <- predict(lda.fit, newdata = Smarket1[!train,])$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

# QDA
library(MASS)
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit
qda.class <- predict(qda.fit, newdata = Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)
# 60% level of accuracy is quite impressive for stock market data, which is 
# known to be quite hard to model accurately. This suggests that the quadratic 
# orm assumed by QDA may capture the true relationship more accurately than 
# the linear forms assumed by LDA and logistic regression. However, we 
# recommend evaluating this method’s performance on a larger test set 
# before betting that this approach will consistently beat the market!

# KNN
library(class)
train.X <- cbind(Smarket$Lag1, Smarket$Lag2)[train,]
test.X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train,]
train.Y <- Smarket$Direction[train]
set.seed(1)
# k = 1
knn.pred <- knn(train = train.X, test = test.X, cl = train.Y, k=1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)
(83+43)/252 # accuracy
# k = 3
knn.pred <- knn(train = train.X, test = test.X, cl = train.Y, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)
# It appears that for this data, QDA provides the best results of the methods 
# that we have examined so far.

# An Application to Caravan Insurance Data
library(ISLR)
data("Caravan")
dim(Caravan)
head(Caravan)
summary(Caravan$Purchase)
# Because the KNN classifier predicts the class of a given test observation by 
# identifying the observations that are nearest to it, the scale of the variables matters.
standardized.X <- scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
sd(standardized.X[,1])
# train/test
test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Caravan$Purchase[-test]
test.Y <- Caravan$Purchase[test]
set.seed(1)
# k = 1
knn.pred <- knn(train = train.X, test = test.X, cl = train.Y, k=1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
# The KNN error rate on the 1,000 test observations is just under 12%. 
# At first glance, this may appear to be fairly good. However, since only 6% 
# of customers purchased insurance, we could get the error rate down to 6 % by 
# always predicting No regardless of the values of the predictors!
#  So the overall error rate is not of interest. Instead, the fraction of individuals 
# that are correctly predicted to buy insurance is of interest.
table(test.Y, knn.pred)
9/(68+9) # accuracy knn with k = 1
# Among 77 such customers, 9, or 11.7 %, actually do purchase insurance.
# This is double the rate that one would obtain from random guessing.
# k = 3
knn.pred <- knn(train = train.X, test = test.X, cl = train.Y, k = 3)
table(knn.pred, test.Y)
5/26 # accuracy knn with k = 3
knn.pred <- knn(train = train.X, test = test.X, cl = train.Y, k = 5)
table(knn.pred, test.Y)
4/(4+11) # accuracy knn with k = 5
# let's fit logistic regression 
glm.fit <- glm(Purchase ~ ., data = Caravan, family = binomial, subset = -test)
glm.probs <- predict(glm.fit, newdata = Caravan[test,], type = "response")
glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")
table(glm.pred, test.Y)
glm.pred <- ifelse(glm.probs > 0.25, "Yes", "No")
table(glm.pred, test.Y)
11/(22+11) # accuracy higher than knn with k=5

# Suppose we collect data for a group of students in a statistics class with 
# variables X1 = hours studied, X2 = undergrad GPA, and Y = receive an A. We 
# fit a logistic regression and produce estimated coefficient, β0 = −6, β1 = 0.05, β2 = 1.
# log(p/(1-p)) = Beta0 + Beta1*X1 + Beta2*X2
# p/(1-p) = exp(Beta0 + Beta1*X1 + Beta2*X2)
# p = exp(Beta0 + Beta1*X1 + Beta2*X2) / (1 + exp(Beta0 + Beta1*X1 + Beta2*X2))
# Estimate the probability that a student who studies for 40 h and has 
# an undergrad GPA of 3.5 gets an A in the class
p <- exp(-6 + 0.05*40 + 1*3.5) / (1 + exp(-6 + 0.05*40 + 1*3.5))
# How many hours would the student in part (a) need to study to have 
# a 50 % chance of getting an A in the class?
X1 <- (log(0.5/(1-0.5)) - (-6) - 1*3.5) / 0.05 # 50 hourse

# This question should be answered using the Weekly data set (ISLR package). 
# This data is similar in nature to the Smarket data from this chapter’s lab, 
# except that it contains 1,089 weekly returns for 21 years, from the beginning
# of 1990 to the end of 2010
data("Weekly")
head(Weekly)
pairs(Weekly)
ggplot(Weekly) + geom_point(aes(Year, Lag1))
ggplot(Weekly) + geom_point(aes(Year, Volume)) + 
  geom_smooth(aes(Year, Volume), se = F, color="red")
glm.fit <- glm(Direction ~ . -Year -Today -Direction, data = Weekly, family = binomial)
summary(glm.fit)
plot(glm.fit$fitted.values, glm.fit$residuals)
# Compute the confusion matrix and overall fraction of correct predictions
glm.pred <-  ifelse(predict(glm.fit, type = 'response') > 0.5, "Up", "Down") 
table(Weekly$Direction, glm.pred)
mean(Weekly$Direction == glm.pred)
mean(glm.pred == "Up")
mean(glm.pred == "Down")
cbind(glm.pred,glm.pred == "Down")
sum(glm.pred == "Down")
# Now fit the logistic regression model using a training data period from 1990 
# to 2008, with Lag2 as the only predictor. Compute the confusion matrix and 
# the overall fraction of correct predictions for the held out data (that is, 
# the data from 2009 and 2010)
# Log regression
train <- Weekly$Year <= 2008
glm.fit <- glm(Direction ~ Lag2, data = Weekly, subset = train, family = binomial)
summary(glm.fit)
glm.pred <- ifelse( predict(glm.fit, newdata = Weekly[!train,], type = 'response') > 0.5, "Up", "Down")
table(glm.pred, Weekly$Direction[!train])
mean(glm.pred == Weekly$Direction[!train])
9/(9+34) # accuracy "Down"
56/(56+5) # accuracy "Up"
# LDA
ggplot(Weekly) + geom_density(aes(Lag2, color=Direction))
lda.fit <- lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.fit
names(lda.fit)
lda.pred <- predict(lda.fit, newdata = Weekly[!train,])
lda.pred$class
table(lda.pred$class, Weekly$Direction[!train])
mean(lda.pred$class == Weekly$Direction[!train])
9/(9+34) # accuracy "Down"
56/(56+5) # accuracy "Up"
# QDA
qda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.fit
qda.pred <- predict(qda.fit, newdata = Weekly[!train,])
lda.pred$class
table(qda.pred$class, Weekly$Direction[!train])
mean(qda.pred$class == Weekly$Direction[!train])
0/(0+43) # accuracy "Down"
61/(61+0) # accuracy "Up"
# KNN, k = 1
knn.pred <- knn(train = cbind(Weekly$Lag2[train]), test = cbind(Weekly$Lag2[!train]), 
                cl = Weekly$Direction[train], k = 1)
table(knn.pred, Weekly$Direction[!train])
mean(knn.pred == Weekly$Direction[!train])
21/(21+22) # accuracy "Down"
32/(32+29) # accuracy "Up"
# KNN, k = 3
knn.pred <- knn(train = cbind(Weekly$Lag2[train]), test = cbind(Weekly$Lag2[!train]), 
                cl = Weekly$Direction[train], k = 3)
table(knn.pred, Weekly$Direction[!train])
mean(knn.pred == Weekly$Direction[!train])
16/(16+27) # accuracy "Down"
42/(42+19) # accuracy "Up"

# In this problem, you will develop a model to predict whether a given car gets 
# high or low gas mileage based on the Auto data set.
head(Auto)
Auto <- Auto %>% mutate(mpg01 = as.factor(ifelse(mpg > median(mpg), 1, 0)))
Auto %>% select(mpg, mpg01)
median(Auto$mpg)
ggplot(Auto) + 
  geom_density(aes(mpg)) +
  geom_density(aes(mpg, color = mpg01)) +
  geom_vline(xintercept = median(Auto$mpg), color = "grey")
ggplot(Auto) + geom_boxplot(aes(x = mpg01, y = mpg, color = mpg01))
pairs(Auto)
# mpg01 vs displacement + horsepower
ggplot(Auto) + geom_point(aes(displacement, horsepower, color = mpg01))
# mpg01 vs weight + acceleration
ggplot(Auto) + geom_point(aes(weight, acceleration, color = mpg01))
# split train/test
inTrain <- rbinom(as.integer(nrow(Auto)), size = 1, prob = 0.75) == 1
train <- Auto[inTrain,]
test <- Auto[!inTrain,]
# lda
lda.fit <- lda(mpg01 ~ displacement + horsepower + weight + acceleration,
               data = train)
lda.fit
lda.pred <- predict(lda.fit, newdata = test)
table(lda.pred$class, test$mpg01)
mean(lda.pred$class == test$mpg01)
# qda
qda.fit <- qda(mpg01 ~ displacement + horsepower + weight + acceleration,
               data = train)
qda.fit
qda.pred <- predict(qda.fit, newdata = test)
table(qda.pred$class, test$mpg01)
mean(qda.pred$class == test$mpg01)
# logistic regression
glm.fit <- glm(mpg01 ~ displacement + horsepower + weight + acceleration,
               data = train, family = binomial)
glm.fit
glm.probs <- predict(glm.fit, newdata = test, type = 'response')
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred, test$mpg01)
mean(glm.pred == test$mpg01)
# knn, k = 1
knn.pred <- knn(train = select(train, displacement, weight), 
                test = select(test, displacement, weight),
                cl = train$mpg01, k = 1)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)
# knn, k = 3
knn.pred <- knn(train = select(train, displacement, weight), 
                test = select(test, displacement, weight),
                cl = train$mpg01, k = 3)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)
# knn, k = 7
knn.pred <- knn(train = select(train, displacement, weight), 
                test = select(test, displacement, weight),
                cl = train$mpg01, k = 7)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)
# knn, k = 10
knn.pred <- knn(train = select(train, displacement, weight), 
                test = select(test, displacement, weight),
                cl = train$mpg01, k = 10)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)
# knn, k = 15
knn.pred <- knn(train = select(train, displacement, weight), 
                test = select(test, displacement, weight),
                cl = train$mpg01, k = 15)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)

Power <- function(n,p) {print(n^p)}
Power(3,8)
Power3 <- function(n,p) {
  result <- n^p
  return(result)
}
Power3(3,8)
Power3
plot(1:10, Power3(1:10,2), xlab = 'x', ylab = 'f(x)')
plot(1:10, Power3(1:10,2), xlab = 'x', ylab = 'f(x)', log = 'xy')
title(main = "Plot f(x) = x^2")

# We illustrate the validation set approach on the Auto data set
# Validation set approach
# Let's show that test error on every test set dfferent (ISLR,p.177)
mse <- c()
for (i in 1:10) {
  fit <- lm(mpg ~ poly(horsepower, i), data = Auto)
  mse[i] <- summary(fit)$sigma
}
ggplot(data = data.frame(i = 1:length(mse), mse = mse)) +
  geom_point(aes(x = i, y = mse), color = 'red', size = 4) +
  geom_line(aes(x=i, y=mse), color = 'red') +
  xlab(label = "Degree of polynomial") +
  ylab(label = "Mean squared error") +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10), 
                   labels = c('1','2','3','4','5','6','7','8','9','10'),
                   breaks = c(1,2,3,4,5,6,7,8,9,10))
# Now let's split 10 times data on test/train and illustrate test error
p <- ggplot() +
  xlab(label = "Degree of polynomial") +
  ylab(label = "Mean squared error") +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10), 
                   labels = c('1','2','3','4','5','6','7','8','9','10'),
                   breaks = c(1,2,3,4,5,6,7,8,9,10))
mse_matrix <- matrix()
library(RColorBrewer)
colors <- brewer.pal(10, name = "Set3")
for (i in 1:10) {
  mse <- c()
  inTrain <- rbinom(nrow(Auto),size = 1,prob = 0.75) == 1
  train <- Auto[inTrain,]
  test <- Auto[!inTrain,]
  for (j in 1:10) {
    fit <- lm(mpg ~ poly(horsepower, j), data = train)
    res <- predict(fit, newdata = test) - test$mpg
    mse[j] <- sum((res)^2)/length(res)
  }
  #mse <- cbind(mse,mse_v)
  p <- p + 
    geom_point(data = data.frame(i = 1:length(mse),mse = mse),
               aes(x = i, y = mse), color = colors[i], size = 4) +
    geom_line(data = data.frame(i = 1:length(mse),mse = mse), 
              aes(x=i, y=mse), color = colors[i])
}
p
# All ten curves indicate that the model with a quadratic term has a dramatically 
# smaller validation set MSE than the model with only a linear term. Furthermore, 
# all ten curves indicate that there is not much benefit in including cubic 
# or higher-order polynomial terms in the model
# But it is worth noting that each of the ten curves results in a different 
# test MSE estimate for each of the ten regression models considered. And 
# there is no consensus among the curves as to which model results in the 
# smallest validation set MSE

# K-fold cross validation
# let's illustrate
# Now let's split 10 times data on test/train and illustrate test error
library(RColorBrewer)
colors <- brewer.pal(10, name = "Set3")
mse_mean <- c()
mse <- matrix(nrow = 10,ncol = 10)
for (n_poly in 1:10) {
  # divide data to 10 folds
  Auto <- mutate(Auto, bins = cut(1:nrow(Auto), breaks = 10))
  levels(Auto$bins) <- c(1,2,3,4,5,6,7,8,9,10)
  err <- c()
  # 10 fold cross validation
  for (j in 1:10){
    inTrain <- Auto$bins != j
    train <- Auto[inTrain,]
    test <- Auto[!inTrain,]
    glm.fit <- glm(mpg ~ poly(horsepower, n_poly), data = train)
    glm.pred <- predict(glm.fit, newdata = test)
    err[j] <- mean( (test$mpg - glm.pred)^2 )
  }
  mse[,n_poly] <- err
  mse_mean[n_poly] <- mean(err)
}
ggplot(data = data.frame(i = 1:length(mse), mse = mse)) +
  geom_point(aes(i,mse), color = colors[4], size = 4) + geom_line(aes(i,mse), color = colors[1]) +
  xlab(label = "Degree of polynomial") +
  ylab(label = "Mean squared error") +
  ggtitle('10 fold Cross Validaation for 1-10 polinomial regression') +
  theme(plot.title = element_text(hjust = 0.5, size = 16,margin = margin(20,0,20,0))) +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10), 
                   labels = c('1','2','3','4','5','6','7','8','9','10'),
                   breaks = c(1,2,3,4,5,6,7,8,9,10))
#####
mse_mean <- c()
mse <- matrix(nrow = 10,ncol = 10)
for (n_fold in 1:10) {
  # divide data to 10 folds
  Auto <- mutate(Auto, bins = cut(1:nrow(Auto), breaks = 10))
  levels(Auto$bins) <- c(1,2,3,4,5,6,7,8,9,10)
  inTrain <- Auto$bins != n_fold
  train <- Auto[inTrain,]
  test <- Auto[!inTrain,]
  err <- c()
  # 10 fold cross validation
  for (n_poly in 1:10){
    glm.fit <- glm(mpg ~ poly(horsepower, n_poly), data = train)
    glm.pred <- predict(glm.fit, newdata = test)
    err[n_poly] <- mean( (test$mpg - glm.pred)^2 )
  }
  mse[,n_fold] <- err
}
mse_mean <- apply(mse, 1, mean)
p <- ggplot() +
  xlab(label = "Degree of polynomial") +
  ylab(label = "Mean squared error") +
  ggtitle('Cross Validation for 1-10 polinomial regression') +
  theme(plot.title = element_text(hjust = 0.5, size = 16,margin = margin(20,0,20,0))) +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10), 
                   labels = c('1','2','3','4','5','6','7','8','9','10'),
                   breaks = c(1,2,3,4,5,6,7,8,9,10))
# MSE for every fold and every polynomial degree regression
for (n_fold in 1:10) {
  p <- p + 
    geom_point(data = data.frame(i = 1:10, mse = mse[,n_fold]), aes(i,mse), color = colors[9], size = 4) + 
    geom_line(data = data.frame(i = 1:10, mse = mse[,n_fold]), aes(i,mse), color = colors[9])
}
p
# mean of 10 folds
p <- p +
  geom_point(data = data.frame(i = 1:length(mse_mean), mse_mean = mse_mean),
             aes(i,mse_mean), color = colors[1], size = 4) + 
  geom_line(data = data.frame(i = 1:length(mse_mean), mse_mean = mse_mean),
            aes(i,mse_mean), color = colors[1])
p
# let's compare with LOOCV from built in cv function cv.glm()
cv.error <- rep(0,5)
library(boot)
for (n_poly in 1:10){
  glm.fit <- glm(mpg ~ poly(horsepower, n_poly), data = Auto)
  cv.error[n_poly] <- cv.glm(data = Auto, glmfit = glm.fit)$delta[1]
}
cv.error <- unlist(cv.error)
p <- p + 
  geom_point(data = data.frame(i = 1:length(cv.error), err = cv.error),
             aes(i,err), color = colors[4], size = 4) +
  geom_line(data = data.frame(i = 1:length(cv.error), err = cv.error),
            aes(i,err), color = colors[4]) 
#scale_color_manual(name = 'Legend', 
#                   values = c('CVglm' = colors[10])) +
#theme(legend.position = 'right', legend.direction = "vertical")
p
# let's compare with K-fold cross validation from built in cv function cv.glm()
cv.error.10 <- rep(0,10)
library(boot)
for (n_poly in 1:10){
  glm.fit <- glm(mpg ~ poly(horsepower, n_poly), data = Auto)
  cv.error.10[n_poly] <- cv.glm(data = Auto, glmfit = glm.fit, K=10)$delta[1]
}
cv.error.10 <- unlist(cv.error.10)
cv.error.10
p <- p + 
  geom_point(data = data.frame(i = 1:length(cv.error.10), err = cv.error.10),
             aes(i,err), color = colors[3], size = 4) +
  geom_line(data = data.frame(i = 1:length(cv.error.10), err = cv.error.10),
            aes(i,err), color = colors[3]) 
p

# Bootstrap
head(Portfolio)
alpha.fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  return ( (var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2*cov(X,Y)) )
}
alpha.fn(Portfolio, 1:100)
alpha.fn(Portfolio, sample(100,100, replace = T))
alpha.fn(Portfolio, sample(100,100, replace = T))
boot(data = Portfolio, statistic = alpha.fn, R = 1000)

# Example: estimate the accuracy of linear regression model (coefficients beta0 and beta1)
boot.fn <- function(data, index) {
  return( coef(lm(mpg ~ horsepower, data = data, subset = index)) )
}
boot.fn(Auto, 1:nrow(Auto))
boot.fn(Auto, sample(nrow(Auto), nrow(Auto), replace = T))
boot.fn(Auto, sample(nrow(Auto), nrow(Auto), replace = T))
boot(data = Auto, statistic = boot.fn, R = 1000)
# here SE(beta0) = 0.84, SE(beta1) = 0.0073, let's compare with result lm()
summary(lm(mpg ~ horsepower, data = Auto))$coef
# let's fit quadratic model
boot.fn <- function(data,index){
  return( coef(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index)) )
}
boot.fn(Auto, 1:nrow(Auto))
boot.fn(Auto, sample(nrow(Auto), nrow(Auto), replace = T))
boot(data = Auto, statistic = boot.fn, R = 1000)
summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef

# We will now estimate the test error of this logistic regression model using the 
# validation set approach
# Fit a logistic regression model that uses income and balance to predict default
head(Default)
glm.fit <- glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)
inTrain <- rbinom(n = nrow(Default), size = 1, prob = 0.75) == 1
train <- Default[inTrain,]
test <- Default[!inTrain,]
# inTrain <- sample(nrow(Default), as.integer(nrow(Default) * 0.75), replace = F)
# train <- Default[inTrain,]
# test <- Default[-inTrain,]
# Fit a multiple logistic regression model using only the training observations
glm.fit <- glm(default ~ income + balance, data = train, family = binomial)
glm.prob <- predict(glm.fit, newdata = test, type = 'response')
glm.pred <- ifelse(glm.prob > 0.5, "Yes", "No")
table(glm.pred, test$default)
mean(glm.pred == test$default)
# Now consider a logistic regression model that predicts the prob- ability of 
# default using income, balance, and a dummy variable for student. Estimate the test 
# error for this model using the val- idation set approach. Comment on whether or 
# not including a dummy variable for student leads to a reduction in the test error rate.
glm.fit <- glm(default ~ income + balance + student, data = train, family = binomial)
glm.prob <- predict(glm.fit, newdata = test, type = 'response')
glm.pred <- ifelse(glm.prob > 0.5, "Yes", "No")
table(glm.pred, test$default)
mean(glm.pred == test$default)
# We continue to consider the use of a logistic regression model to predict the 
# probability of default using income and balance on the Default data set. In particular, 
# we will now compute estimates for the standard errors of the income and balance 
# logistic regression co- efficients
glm.fit <- glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)
boot.fn <- function(data,index) {
  return( coef(glm(default ~ income + balance, data = Default, subset = index, family = binomial)) )
}
boot.fn <- function(data,index) {
  return( 5 )
}
boot.fn(Default, 1:nrow(Default))
boot(data = Default, statistic = boot.fn, R = 1000)

# We can use cv.glm() to compute LOOCV test error. Alterna- tively, one could compute 
# those quantities using just the glm() and predict.glm() functions, and a for loop
# Fit a logistic regression model that predicts Direction using Lag1 and Lag2
glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(glm.fit)
# Fit a logistic regression model that predicts Direction using Lag1 and Lag2 using 
# all but the first observation
glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly[2:nrow(Weekly),], family = binomial)
summary(glm.fit)
# Use the last model to predict the direction of the first observation
predict(glm.fit, newdata = Weekly[1:5,], type = 'response') # not correct classification
# Write a for loop from i=1 to i=n, where n is the number of observations in the data set.
# Fit a logistic regression model using all but the ith obser- vation to predict 
# Direction using Lag1 and Lag2. Compute the posterior probability of the market 
# moving up for the ith observation. Determine whether or not an error was made in 
# predicting the direction for the ith observation. If an error was made, then 
# indicate this as a 1, and otherwise indicate it as a 0
err.sum <- 0
for (i in 1:nrow(Weekly)){
  indexes <- 1:nrow(Weekly)
  glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly[indexes != i,], family = binomial)
  glm.prob <- predict(glm.fit, newdata = Weekly[i,], type = 'response') # not correct classification
  glm.pred <- ifelse(glm.prob > 0.5, "Up","Down")
  if (glm.pred != Weekly[i,]$Direction) {
    err.sum <- err.sum + 1
  } 
}
err.sum
err.sum/nrow(Weekly)

# We will now perform cross-validation on a simulated data set
# let's define custom loocv function to check buil in cv.glm()
loocv.my <- function(f, data){
  err.sum <- c()
  indexes <- 1:nrow(data)
  for (i in 1:nrow(data)){
    glm.fit <- glm(f, data = data[indexes != i,])
    glm.pred <- predict(glm.fit, newdata = data[i,]) # not correct classification
    err.sum[i] <- (glm.pred - data[i,]$y)^2
  }
  mean(err.sum)
}
# simulated data
y <- rnorm(100)
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)
df <- data.frame(x,y)
ggplot(data = df) +
  geom_point(aes(x,y))
# model Y = β0 + β1X + ε
glm.fit <- glm(y ~ x, data = df) 
summary(glm.fit1)
cv.glm(data = df, glmfit = glm.fit)$delta
ggplot(data = mutate(df, p = predict(glm.fit,newdata = df))) +
  geom_point(aes(x,y)) +
  geom_line(aes(x, p), color = "red")
loocv.my(y ~ x, df)
# Y = β0 + β1X + β2X2 + ε
glm.fit <- glm(y ~ x + I(x^2), data = df) 
summary(glm.fit)
cv.glm(data = df, glmfit = glm.fit)$delta
ggplot(data = mutate(df, p = predict(glm.fit,newdata = df))) +
  geom_point(aes(x,y)) +
  geom_line(aes(x, p), color = "red")
loocv.my(y ~ x + I(x^2), df)
# Y = β0 +β1X +β2X2 +β3X3 +ε
glm.fit <- glm(y ~ x + I(x^2) + I(x^3), data = df) 
summary(glm.fit)
cv.glm(data = df, glmfit = glm.fit)$delta
ggplot(data = mutate(df, p = predict(glm.fit,newdata = df))) +
  geom_point(aes(x,y)) +
  geom_line(aes(x, p), color = "red")
loocv.my(y ~ x + I(x^2) + I(x^3), df)
# Y = β0 +β1X +β2X2 +β3X3 +β4X4 +ε
glm.fit <- glm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = df) 
summary(glm.fit1)
cv.glm(data = df, glmfit = glm.fit)$delta
ggplot(data = mutate(df, p = predict(glm.fit,newdata = df))) +
  geom_point(aes(x,y)) +
  geom_line(aes(x, p), color = "red")
loocv.my(y ~ x + I(x^2) + I(x^3) + I(x^4), df)

# We will now consider the Boston housing data set, from the MASS library. Based on 
# this data set, provide an estimate for the population mean of medv. Call this estimate μˆ
mean(Boston$medv)
# Provide an estimate of the standard error of μˆ. Interpret this result. Hint: We 
# can compute the standard error of the sample mean by dividing the sample standard 
# deviation by the square root of the number of observations.
sd(Boston$medv)/nrow(Boston)
# Now estimate the standard error of μˆusing the bootstrap
boot.fn <- function(data,index) {
  return(mean(data$medv[index]))
}
boot.fn(Boston, 1:nrow(Boston))
boot(data = Boston, statistic = boot.fn, R = 1000)
# Based on your bootstrap estimate from (c), provide a 95 % con- fidence interval 
# for the mean of medv. Compare it to the results obtained using t.test(Boston$medv).
# Hint: You can approximate a 95 % confidence interval using the formula [μˆ− 2SE(μˆ), μˆ+ 2SE(μˆ)]
c(mean(Boston$medv) - 2*0.3994031, (mean(Boston$medv) + 2*0.3994031))
t.test(Boston$medv)

# Lab 1: Subset Selection Methods
# Best Subset Selection
# Salary variable is missing for some of the players. The is.na() function can be 
# used to identify the missing observations
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
head(Hitters)
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
library(leaps) # regsubsets() 
regfit.full <- regsubsets(Salary ~ . , Hitters)
summary(regfit.full)
regfit.full <- regsubsets(Salary ~ . , Hitters, nvmax = 19)
reg.summary <-  summary(regfit.full)
names(reg.summary)
reg.summary$rsq
reg.summary$adjr2
pairs(~ CRBI + Hits + AtBat + PutOuts, data = Hitters)
# plot RSS vs model variables quantity
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of variables", ylab = "RSS",
     type="l") # type="l" option tells R to connect the plotted points with lines
m <- which.min(reg.summary$rss)
points(m, reg.summary$rss[m], col = "red", cex=2,pch=20)
# plot Adjusted R^2 vs model variables quantity
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab="Adjusted RSq", type = "l")
m <- which.max(reg.summary$adjr2)
points(m, reg.summary$adjr2[m], col = "red", cex=2,pch=20)
# plot Cp and BIC
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
m <- which.min(reg.summary$cp)
points(m, reg.summary$cp[m], col='red',cex=2,pch=20)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
m <- which.min(reg.summary$bic)
points(m, reg.summary$bic[m], col='red',cex=2,pch=20)
# regsubsets() function has a built-in plot() command
par(mfrow = c(2,2))
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
# the model with the lowest BIC is the six-variable model
# to see coefficients
coef(regfit.full, 6)
coef(regfit.full, 6)[3]

# Forward and Backward Stepwise Selection
regfit.fwd <- regsubsets(Salary ~ . , data = Hitters, nvmax = 19,
                         method = "forward")
regfit.fwd.summary <- summary(regfit.fwd)
names(summary(regfit.fwd))
plot(regfit.fwd.summary$adjr2, type='l')
regfit.bwd <- regsubsets(Salary ~ . , data = Hitters, nvmax = 19,
                         method = "backward")
summary(regfit.bwd)

# Choosing Among Models Using the Validation Set Approach and Cross-Validation
# train/test
set.seed(1)
train <- sample(x = c(TRUE, FALSE), size = nrow(Hitters), replace = TRUE)
test <- !train
regfit.best = regsubsets(Salary ~ . , data = Hitters[train,], nvmax = 19)
# compute the validation set error for the best model of each model size, first make a model matrix
test.mat <- model.matrix(Salary ~ . , data = Hitters[test,])
# Now we run a loop, and for each size i, we extract the coefficients from 
# regfit.best for the best model of that size, multiply them into the 
# appropriate columns of the test model matrix to form the predictions, and 
# compute the test MSE
d <- dim(test.mat)[2] - 1 # number of variables
val.errors <- rep(NA,d)
for (i in 1:d){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean( (Hitters$Salary[test] - pred)^2 )
}
val.errors
which.min(val.errors)
coef(regfit.best,7)
# there is no predict function for regsubsets(). Let's create it
predict.regsubsets <- function(model, newdata, id,...) {
  f <- as.formula(model$call[[2]])
  mat <- model.matrix(f, newdata)
  coefi <- coef(model, id = id)
  xvars <- names(coefi)
  mat[,xvars] %*% coefi
}
# Finally, we perform best subset selection on the full data set, and select the best 
# 10-variable model; best 10-variable model on the full data set may 
# differ from the corresponding model on the training set
regfit.best <- regsubsets(Salary ~ . , data = Hitters, nvmax = 19)
coef(regfit.best,7)
# In fact, we see that the best ten-variable model on the full data set has 
# a different set of variables than the best ten-variable model on the training set

# We now try to choose among the models of different sizes using crossvalidation
k <- 10
set.seed(1)
folds <- sample(x = 1:10, size = nrow(Hitters), replace = T)
table(folds)
cv.error <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))
for (j in 1:k){
  best.fit <- regsubsets(Salary ~ . , data = Hitters[folds != j,], nvmax = 19)
  for(i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j,], id = i)
    cv.error[j,i] <- mean( (Hitters$Salary[folds == j] - pred)^2 )
  }
}
mean.cv.errors <- apply(cv.error,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')
which.min(mean.cv.errors)
# We see that cross-validation selects an 10-variable model. We now perform 
# best subset selection on the full data set in order to obtain the 10-variable model
reg.best <- regsubsets(Salary ~ . , data = Hitters, nvmax = 19)
coef(reg.best, 10)

# Lab 2: Ridge Regression and the Lasso
x <- model.matrix(Salary ~ . , Hitters)[,-1]
y <- Hitters$Salary
library(glmnet)
grid <- 10^seq(10, -2, length = 100) # By default the glmnet() function performs ridge regression for an automatically selected range of λ values.
ridge.mod <- glmnet(x,y,alpha = 0, lambda = grid)
# by default glmnet() standardizes the variables so that thay are on the same scale
dim(coef(ridge.mod))
coef(ridge.mod) # 20x100 matrix
names(ridge.mod)
ridge.mod$lambda
plot(ridge.mod)
# We expect the coefficient estimates to be much smaller when a large value of λ is used
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
ridge.mod$lambda[60]
coef(ridge.mod)[-1,60]
# predict for lambda=50
predict(ridge.mod, s = 50, type = "coefficients")[1:20,]
# test/train
set.seed(1)
train <- sample(x = 1:nrow(x), size = nrow(x)/2)
test <- (-train)
y.test <- y[test]
# fit a ridge regression model on the training set, and evaluate its MSE on the test set, using λ = 4
ridge.mod <- glmnet(x = x[train,], y = y[train], alpha = 0, lambda = grid,
                    thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test,])
cbind(ridge.pred, y[test])
mean( (ridge.pred - y.test)^2 )
# if we had instead simply fit a model with just an intercept, we would have 
# predicted each test observation using the mean of the training observations
mean( (mean(y[train]) - y.test)^2 )
# We could also get the same result by fitting a ridge regression model with a very large value of λ
ridge.pred <- predict(ridge.mod, s=1e10, newx = x[test,])
mean( (ridge.pred - y.test)^2 )
# fitting a ridge regression model with λ = 4 leads to a much lower test MSE than fitting a model with just an intercept
# We now check whether there is any benefit to performing ridge regression with λ = 4 instead of just performing least squares regression (lambda = 0)
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,], exact = T) # exact = T for lambda=0, otherwise - interpolation over the grid of λ values
# error; to fix:
ridge.pred <- predict(ridge.mod, x = x[train,], y = y[train],
                      s = 0, newx = x[test,], exact = T) # exact = T for lambda=0, otherwise - interpolation over the grid of λ values
mean( (ridge.pred-y.test)^2)
# compare with linear regression lm()
lm(y ~ x, subset = train)
predict(ridge.mod, x = x[train,], y = y[train],
        s = 0, type = 'coefficients', exact = T)
# instead of arbitrarily choosing λ = 4, it would be better to use cross-validation to choose the tuning parameter λ
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
# What is the test MSE associated with this value of λ
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean( (ridge.pred - y.test)^2 ) # This represents a further improvement over the test MSE that we got using λ = 4
# Finally, we refit our ridge regression model on the full data set, using 
# the value of λ chosen by cross-validation, and examine the coefficient estimates
out <- glmnet(x = x, y = y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]
# As expected, none of the coefficients are zero—ridge regression does not perform variable selection!

# The Lasso
lasso.mod <- glmnet(x = x[train,], y = y[train], alpha=1, lambda = grid)
plot(lasso.mod)
# cross-validation
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
log(bestlam)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean( (lasso.pred - y.test)^2 )
# This is substantially lower than the test set MSE of the null model and of least squares, and very similar to the test MSE of ridge regression with λ chosen by cross-validation
# However, the lasso has a substantial advantage over ridge regression in that the resulting coefficient estimates are sparse
out <- glmnet(x,y,alpha = 1, lambda = grid)
lasso.coef <- predict(out, type="coefficients", s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef != 0]

# Lab 3: PCR and PLS Regression
# Principal Components Regression
Hitters <- na.omit(Hitters)
install.packages('pls')
library(pls)
set.seed(2)
pcr.fit <- pcr(Salary ~ . , data=Hitters, scale = T, validation='CV')
pcr.fit
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
# now perform PCR on the training data and evaluate its test set performance
set.seed(1)
pcr.fit <- pcr(Salary ~ . , data = Hitters, subset = train,
               scale = T, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
# Now we see that the lowest cross-validation error occurs when M = 5
# compute the test MSE
pcr.pred <- predict(pcr.fit, x[test,], ncomp = 7)
mean( (pcr.pred - y.test)^2 )
# the result not differ much from lasso and ridge
# and PCR model more difficult to interpret
# Finally, we fit PCR on the full data set, using M = 7, the number of components identified by cross-validation
pcr.fit <- pcr(y ~ x, scale=T, ncomp=7)
summary(pcr.fit)

# PLS - Partial Least Squares
set.seed(1)
library(pls)
pls.fit <- plsr(Salary ~ . , data = Hitters, subset = train, scale = T,
                validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = 'MSEP')
# The lowest cross-validation error occurs when only M = 2 partial least squares directions are used
# now evaluate the corresponding test set MSE
pls.pred <- predict(pls.fit, x[test,], ncomp = 2)
mean( (pls.pred - y.test)^2 )
# test MSE is comparable to the test MSE obtained using ridge regression, the lasso, and PCR
# Finally, we perform PLS using the full data set, using M = 2, the number of components identified by cross-validation
pls.fit <- plsr(Salary ~ . , data = Hitters, scale=T, ncomp=2)
summary(pls.fit)

# In this exercise, we will generate simulated data, and will then use this 
# data to perform best subset selection
X <- rnorm(100)
eps <- rnorm(100, mean = 0, sd = 0.01)
# Generate a response vector Y = β0 +β1X +β2X2 +β3X3 +ε
Y <- 5 + 2*X + (-2)*X^2 + 2.5*X^3 + eps
ggplot(data = data.frame(X = X, Y = Y)) + 
  geom_point(aes(1:length(Y), Y)) +
  geom_line(aes(1:length(Y), 2*X), color = "red") +
  geom_line(aes(1:length(Y), -2*X^2), color = "green") +
  geom_line(aes(1:length(Y), 2.5*X^3), color = "blue")
ggplot(data = data.frame(X = X, Y = Y)) +
  geom_point(aes(X,Y)) +
  geom_line(aes(X, 2*X), color = "red") +
  geom_line(aes(X, -2*X^2), color = "green") +
  geom_line(aes(X, 2.5*X^3), color = "blue") +
  geom_line(aes(X, 5 + 2*X + (-2)*X^2 + 2.5*X^3), color = "magenta")
# Use the regsubsets() function to perform best subset selection in order 
# to choose the best model containing the predictors X, X2, . . . , X10
df <- data.frame(Y = Y, X1 = X, X2 = X^2, X3 = X^3, X4 = X^4, X5 = X^5, 
                 X6 = X^6, X7 = X^7, X8 = X^8, X9 = X^9, X10 = X^10)
regsub.fit <- regsubsets(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data = df)
regsub.fit.summary <- summary(regsub.fit)
regsub.fit.summary
names(regsub.fit.summary)
plot(regsub.fit.summary$rss, type='b')
plot(regsub.fit.summary$rsq, type='b')
m <- which.max(regsub.fit.summary$rsq)
points(m,regsub.fit.summary$rsq[m], col='red', pch=19)
plot(regsub.fit.summary$adjr2, type='b')
m <- which.max(regsub.fit.summary$adjr2)
points(m,regsub.fit.summary$adjr2[m], col='red', pch=19)
plot(regsub.fit.summary$cp, type='b')
m <- which.min(regsub.fit.summary$cp)
points(m,regsub.fit.summary$cp[m], col='red', pch=19)
plot(regsub.fit.summary$bic, type='b', ylab = "BIC", xlab = "Number of variables")
m <- which.min(regsub.fit.summary$bic)
points(m,regsub.fit.summary$bic[m], col='red', pch=19)
plot(regsub.fit, scale='adjr2')
plot(regsub.fit, scale='r2')
plot(regsub.fit, scale='Cp')
plot(regsub.fit, scale='bic')
# best model - with 3 variables, coefficients
coef(regsub.fit, id = 3)
# plot  estimated function y = f(x) and real one 
mat <- model.matrix(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data = df)
coef <- coef(regsub.fit, id = 3)
Y.pred <- mat[, names(coef)] %*% coef
df <- data.frame(Y = Y, Y.pred = Y.pred, X = X) 
ggplot(df) +
  geom_point(aes(X,Y), color = 'grey') +
  geom_line(aes(X,Y.pred), color = 'red') +
  geom_line(aes(X, 5 + 2*X + (-2)*X^2 + 2.5*X^3), color = 'blue')
# Do the same using forward and backward stepwise selection. 
# How the result compare with best subset selection?
fwd.fit <- regsubsets(Y ~ . , data = df, method = "forward",
                      nvmax = 10)
fwd.summary <- summary(fwd.fit)
fwd.summary
plot(fwd.summary$rsq, type = 'b')
plot(fwd.summary$rss, type = 'b')
plot(fwd.summary$adjr2, type = 'b')
plot(fwd.summary$cp, type = 'b')
plot(fwd.summary$bic, type = 'b')
# backward
bwd.fit <- regsubsets(Y ~ . , data = df, method = "backward",
                      nvmax = 10)
bwd.summary <- summary(bwd.fit)
bwd.summary
plot(bwd.summary$rsq, type = 'b')
plot(bwd.summary$rss, type = 'b')
plot(bwd.summary$adjr2, type = 'b')
plot(bwd.summary$cp, type = 'b')
plot(fwd.summary$bic, type = 'b')
# let's choose the best model with cross validation
k <- 10
n <- 10 # number of varibales
folds <- sample(x = 1:k, size = length(X), replace = T)
cv.error <- matrix(NA, nrow = k, ncol = n, dimnames = list(NULL, paste(1:10)))
for (i in 1:k){
  regsub.fit <- regsubsets(Y ~ . , data = df[folds != i,], nvmax = 10) # on train set
  mat <- model.matrix(Y ~ . , data = df[folds == i,]) # test 
  for (j in 1:n){
    coef <- coef(regsub.fit, j)
    regsub.pred <- mat[, names(coef)] %*% coef
    cv.error[i,j] <- mean( (df$Y[folds == i] - regsub.pred)^2 )
  }
}
mean.cv.errors <- apply(cv.error, 2, mean)
mean(cv.error[,1])
plot(mean.cv.errors, type = 'b')
m <- which.min(mean.cv.errors)
m
points(m, mean.cv.errors[m], col='red')
# Now fit a lasso model to the simulated data, again using X,X2, . . . , X10 
# as predictors. Use cross-validation to select the optimal value of λ. 
# Create plots of the cross-validation error as a function of λ
grid <- 10^seq(from = 10, to = -4, length = 100)
x <- model.matrix(Y ~ ., data = df)[,-1]
y <- df$Y
lasso.fit <- glmnet(x = x, 
                    y = y,
                    alpha = 1,
                    lambda = grid)
lasso.fit
plot(lasso.fit)
cv.out <- cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestLambda <- cv.out$lambda.min
log(bestLambda)
lasso.pred <- predict(lasso.fit, newx = x, s = bestLambda)
mean( (lasso.pred - y)^2 )
predict(lasso.fit, type = 'coefficients', s = bestLambda) # correct selection of first three coef
# compare with null model and least square estimate
lasso.pred <- predict(lasso.fit, newx = x, s = 1e10)
mean( (lasso.pred - y)^2 )
predict(lasso.fit, newx = x, s = 1e10, type = 'coefficients')
mean( (mean(y) - lasso.pred)^2 )
lasso.pred <- predict(lasso.fit, newx = x, s = 0)
mean( (lasso.pred - y)^2 )
predict(lasso.fit, newx = x, s = 0, type = 'coefficients')
summary(lm(y~x))
# train/test
train <- sample(1:nrow(x), size = nrow(x)/2)
test <- (-train)
lasso.fit <- glmnet(x = x[train,], y = y[train], alpha = 1)
lasso.fit
lasso.pred <- predict(lasso.fit, newx = x[-train,], s = 3)
mean( (lasso.pred - y[-train])^2 )
mean( (mean(y[train]) - y[-train])^2 ) # null model
lasso.pred <- predict(lasso.fit, newx = x[-train,], s = 1e10) # null model
mean( (lasso.pred - y[-train])^2 )
lasso.pred <- predict(lasso.fit, newx = x[-train,], s = 0) # least square estimate
mean( (lasso.pred - y[-train])^2 )
lasso.pred <- predict(lasso.fit, x = x[train,], y = y[train],
                      newx = x[-train,], s = bestLambda, exact = T)
mean( (lasso.pred - y[-train])^2 )
# Now generate a response vector Y according to the model Y = β0 + β7X7 + ε,
# and perform best subset selection and the lasso
X <- rnorm(100)
eps <- rnorm(100, mean = 0, sd = 0.1)
# Generate a response vector Y = β0 +β1X +β2X2 +β3X3 +ε
Y <- 3 + 2*X^7 + eps
df <- data.frame(Y = Y, X1 = X, X2 = X^2, X3 = X^3, X4 = X^4, X5 = X^5, 
                 X6 = X^6, X7 = X^7, X8 = X^8, X9 = X^9, X10 = X^10)
ggplot(df) + geom_point(aes(X,Y)) 

regsub.fit <- regsubsets(Y ~ ., data = df, nvmax = 10)
regsub.fit
regsub.summary <-  summary(regsub.fit)
regsub.summary
plot(regsub.summary$bic, type='b')
plot(regsub.summary$cp, type='b')
plot(regsub.summary$adjr2, type='b')
# cross-validation
k = 10
m = dim(df)[2] - 1 # number of variables
folds <- sample(1:k, size = nrow(df), replace = T)
table(folds)
cv.error <- matrix(NA, nrow=k, ncol=m, dimnames = list(NULL, as.character(1:m)))
for(i in 1:k){
  regsub.fit <- regsubsets(Y ~ . , data = df[folds != i,], nvmax = m)
  mat <- model.matrix(Y ~ . , data = df[folds == i,])
  for (j in 1:m){
    coef <- coef(regsub.fit, j)
    regsub.pred <- mat[, names(coef)] %*% coef
    cv.error[i,j] <- mean( (regsub.pred - df$Y[folds == i])^2)
  }
}
mean.cv.error <- apply(cv.error, 2, mean)
plot(mean.cv.error) 
# choose model with 1 varibale: fit on full data, then coef for 1 var
regsub.fit <- regsubsets(Y ~ . , data = df, nvmax = 10)
coef(regsub.fit,1) # coef correct according to the Y = f(X)
coef(regsub.fit,7) # all others coef exept X7 close to 0
# lasso
x <- model.matrix(Y ~ ., data = df)[,-1]
y <- df$Y
lasso.fit <- glmnet(x = x, y = y, alpha = 1)
lasso.fit
plot(lasso.fit)
cv.out <- cv.glmnet(x = x, y = y, alpha = 1)
plot(cv.out)
bestLambda <- cv.out$lambda.min
log(bestLambda)
predict(lasso.fit, s = bestLambda, type = 'coefficients') # perfect coefficient selection!

# In this exercise, we will predict the number of applications received using the other variables in the College data set
head(College)
dim(College)
# Split the data set into a training set and a test set
inTrain <- sample(x = c(T, F), size = nrow(College), replace = T) 
table(inTrain)
train <- College[inTrain,]
test <- College[!inTrain,]
# Fit a linear model using least squares on the training set, and
# report the test error obtained
lm.fit <- lm(Apps ~ . , data = train)
summary(lm.fit )
lm.pred <- predict(lm.fit, newdata = test)
mean( (lm.pred - College$Apps[!inTrain])^2 )
pairs(~ Apps+Enroll + Accept + Top10perc + Top25perc+F.Undergrad+
        P.Undergrad+Room.Board+Outstate, data = College)
# Fit a ridge regression model on the training set, with λ chosen
# by cross-validation. Report the test error obtained
x <- model.matrix(Apps ~ . , data = College)[,-1]
y <- College$Apps
grid <- 10^seq(10, -2, length = 1000)
ridge.fit <- glmnet(x = x[inTrain,], y = y[inTrain], alpha = 0,thresh = 1e-12,
                    lambda = grid)
range(ridge.fit$lambda)
plot(ridge.fit)
ridge.fit$lambda
ridge.fit$lambda[50]
coef(ridge.fit)[,50]
ridge.fit$lambda[70]
coef(ridge.fit)[,70]
predict(ridge.fit, x = x[inTrain,], y = y[inTrain],
        s = 70, type = 'coefficients', exact = T)
ridge.pred <- predict(ridge.fit, s = 70, newx = x[!inTrain,])
mean((ridge.pred - y[!inTrain])^2)
# choose lambda by cross validation
cv.out <- cv.glmnet(x = x, y = y, alpha = 0)
plot(cv.out)
best.lambda <-  cv.out$lambda.min
log(best.lambda)
ridge.pred <- predict(ridge.fit, s = 4, newx = x[!inTrain,])
mean((ridge.pred - y[!inTrain])^2)
ridge.pred <- predict(ridge.fit, s = 0, newx = x[!inTrain,])
mean((ridge.pred - y[!inTrain])^2)
ridge.pred <- predict(ridge.fit, s = 1e10, newx = x[!inTrain,])
mean((ridge.pred - y[!inTrain])^2)
mean((mean(y[inTrain]) - y[!inTrain])^2)
ridge.pred <- predict(ridge.fit, s = best.lambda, newx = x[!inTrain,])
mean((ridge.pred - y[!inTrain])^2) # best MSE
# Fit a lasso model on the training set, with λ chosen by cross- validation. 
# Report the test error obtained, along with the number of non-zero coefficient estimates
lasso.fit <- glmnet(x = x[inTrain,], y = y[inTrain], alpha = 1)
dim(coef(lasso.fit))
length(lasso.fit$lambda)
coef(lasso.fit)
plot(lasso.fit)
cv.out <- cv.glmnet(x = x[inTrain,], y = y[inTrain], alpha = 1)
plot(cv.out)
best.lambda <- cv.out$lambda.min
log(best.lambda)
lasso.pred <- predict(lasso.fit, newx = x[!inTrain,], s = best.lambda)
mean((lasso.pred - y[!inTrain])^2)
predict(lasso.fit, type = 'coefficients', s = best.lambda)
# Fit a PCR model on the training set, with M chosen by crossvalidation. 
# Report the test error obtained, along with the value of M selected by cross-validation
df <- na.omit(df)
library(pls)
pcr.fit <- pcr(Apps ~ . , data = College, scale = T, validation = "CV")
pcr.fit
summary(pcr.fit)
validationplot(pcr.fit, val.type = 'MSEP')
# train/test
pcr.fit <- pcr( ~ . , data = College, subset = inTrain,
                scale = T, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
pcr.pred <- predict(pcr.fit, newdata = College[!inTrain,], ncomp = 5)
mean((pcr.pred - College$Apps[!inTrain])^2)
# Fit a PLS model on the training set, with M chosen by cross- validation. 
# Report the test error obtained, along with the value of M selected by cross-validation
pls.fit <- plsr(Apps ~ . , data = College[inTrain,], scale = T, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = 'MSEP')
pls.pred <- predict(pls.fit, newdata = College[!inTrain,], ncomp = 5)
mean((pls.pred - College$Apps[!inTrain])^2)

# 11. We will now try to predict per capita crime rate in the Boston data set
# Try out some of the regression methods explored in this chapter, such as 
# best subset selection, the lasso, ridge regression, and PCR. Present and 
# discuss results for the approaches that you consider
head(Boston)
inTrain <- sample(1:nrow(Boston), size = as.integer(0.7 * nrow(Boston)), replace = F)
train <- Boston[inTrain,]
test <- Boston[-inTrain,]
# best subset selection
m <- dim(Boston)[2] - 1
regsub.fit <- regsubsets(crim ~ . , data = Boston, nvmax = m)
regsub.summary <- summary(regsub.fit)
plot(regsub.summary$bic, type = 'b') # here 2 or 3 variables model are the best
# cross-validation
k <- 10
folds <- sample(x = 1:k, size = nrow(train), replace = T)
cv.error <- matrix(NA, nrow = k, ncol = m)
for(i in 1:k){
  regsub.fit <- regsubsets(crim ~ . , data = train[folds != i,], nvmax = m)
  mat <- model.matrix(crim ~ . , data = train[folds == i,])
  for(j in 1:m){
    coef <- coef(regsub.fit, id = j)
    regsub.pred <- mat[, names(coef)] %*% coef
    cv.error[i,j] <- mean((regsub.pred - train$crim[folds == i])^2)
  }
}
mean.cv.error <- apply(cv.error, 2, mean)
plot(mean.cv.error, type = 'b') # 2 and 9 variables model look good
# fit model on full data set
regsub.fit <- regsubsets(crim ~ ., data = Boston, nvmax = m)
coef <- coef(regsub.fit, id = 9) # test id = 2 and 9 
mat <- model.matrix(crim ~ ., data = test) # MSE on test set
regsub.pred <- mat[, names(coef)] %*% coef
mean((regsub.pred - test$crim)^2) # for id=2 MSE - 48.98543, for id=9 MSE - 47.40087
# lasso
x <- model.matrix(crim ~ . , data = Boston)[,-1]
y <- Boston$crim
lasso.fit <- glmnet(x = x[inTrain,], y = y[inTrain], alpha = 1)
plot(lasso.fit)
cv.out <- cv.glmnet(x = x[inTrain,], y = y[inTrain], alpha = 1)
plot(cv.out)
best.lambda <- cv.out$lambda.min
lasso.pred <- predict(lasso.fit, newx = x[-inTrain,], s = best.lambda)
mean((lasso.pred - y[-inTrain])^2) # 49.58264
predict(lasso.fit, type='coefficients', s = best.lambda) # no zero coefficients
# ridge
ridge.fit <- glmnet(x = x[inTrain,], y = y[inTrain], alpha = 0)
plot(ridge.fit)
cv.out <- cv.glmnet(x = x[inTrain,], y = y[inTrain], alpha = 0)
plot(cv.out)
best.lambda <- cv.out$lambda.min
log(best.lambda)
ridge.pred <- predict(ridge.fit, newx = x[-inTrain,], s = best.lambda)
mean((ridge.pred - y[-inTrain])^2) # 49.7196
# PCR
pcr.fit <- pcr(crim ~ . , data = Boston[inTrain,], validation = 'CV', scale = T)
summary(pcr.fit)
plot(pcr.fit)
validationplot(pcr.fit) # choose 8 components
pcr.pred <- predict(pcr.fit, newdata = Boston[-inTrain,], ncomp = 8)
mean((pcr.pred - Boston$crim[-inTrain])^2) # 51.96168
# least square fit
lm.fit <- lm(crim ~ . , data = Boston[inTrain,])
lm.pred <- predict(lm.fit, newdata = Boston[-inTrain,])
mean((lm.pred - Boston$crim[-inTrain])^2) # 49.6167

# 7.8 Lab: Non-linear Modeling
library(ISLR)
data("Wage")
head(Wage)
ggplot(Wage) + geom_point(aes(age,wage)) # non linear relationship
fit <- lm(wage ~ poly(age,4), data = Wage) # for degree 7 5-,6-,7- components has very high p-value
ggplot(Wage) + 
  geom_point(aes(age,wage), color = 'grey') +
  geom_line(aes(age, fit$fitted.values), color = 'red', size = 2)
coef(summary(fit))
head(poly(Wage$age,4)) # function returns a matrix whose columns are a basis of orthogonal polynomials
head(poly(Wage$age,4, raw=T)) # to obtain age, age^2, age^3 and age^4 directly
fit2 <- lm(wage ~ poly(age,4, raw = T), data = Wage) # for degree 7 5-,6-,7- components has very high p-value
coef(summary(fit2))
fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage) # for degree 7 5-,6-,7- components has very high p-value
coef(summary(fit2a))
# predict
age.grid <- seq(from = range(Wage$age)[1], to = range(Wage$age)[2], by = 1)
fit.pred <- predict(fit, newdata = list(age = age.grid), se = T)
se.bands <- cbind(fit.pred$fit + 2*fit.pred$se.fit, fit.pred$fit - 2*fit.pred$se.fit)
# plot 
par(mfrow=c(1,1), mar=c(4.5,4.5,1,1), oma = c(0,0,4,0))
plot(Wage$age, Wage$wage,xlim=range(Wage$age),cex=0.5, color='darkgrey')
title("Degree-4 Polynomial", outer = T)
lines(age.grid, fit.pred$fit, lwd=2,col='blue')
matlines(age.grid, se.bands, lwd = 1, col = 'blue', lty=3)
# In performing a polynomial regression we must decide on the degree of the 
# polynomial to use. One way to do this is by using hypothesis tests
# We use the anova() function, which performs an analysis of variance (ANOVA,
# using an F-test) in order to test the null hypothesis that a model M1 is 
# sufficient to explain the data against the alternative hypothesis that a 
# more complex model M2 is required
# In order to use the anova() function, M1 and M2 must be nested models: the 
# predictors in M1 must be a subset of the predictors in M2. In this case, 
# we fit five different models and sequentially compare the simpler model to the more complex model.
fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age,2), data = Wage)
fit.3 <- lm(wage ~ poly(age,3), data = Wage)
fit.4 <- lm(wage ~ poly(age,4), data = Wage)
fit.5 <- lm(wage ~ poly(age,5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5) # The p-value comparing the linear Model 1 to the quadratic Model 2 is essentially zero (<10−15), indicating that a linear fit is not sufficient. Hence, either a cubic or a quartic polynomial appear to provide a reasonable fit to the data, but lower- or higher-order models are not justified.
# instead of using the anova() function, we could have obtained these p-values more succinctly by exploiting the fact that poly() creates orthogonal polynomials
coef(summary(fit.5))
# Notice that the p-values are the same, and in fact the square of the t-statistics are equal to the F-statistics from the anova() function; for example
(-11.9830341)^2
# anova() method works whether or not we used orthogonal polynomials; it also works when we have other terms in the model as well
fit.1 <- lm(wage ~ education + age, data = Wage)
fit.2 <- lm(wage ~ education + poly(age,2), data = Wage)
fit.3 <- lm(wage ~ education+ poly(age,3), data = Wage)
anova(fit.1, fit.2, fit.3)
# As an alternative to using hypothesis tests and ANOVA, we could choose the polynomial degree using cross-validation
# Next we consider the task of predicting whether an individual earns more than $250,000 per year
fit <- glm( I(wage > 250) ~ poly(age,4), data = Wage, family = binomial)
fit.pred <- predict(fit, newdata = data.frame(age = age.grid), se = T)
# values are logit, to receive probability  Pr(Y=1|X) = exp(f(X))/1+exp(f(X))
Pr <- exp(fit.pred$fit) / (1 + exp(fit.pred$fit))
Pr.bands.logit <- cbind(fit.pred$fit - 2*fit.pred$se.fit, fit.pred$fit + 2*fit.pred$se.fit)
Pr.bands <- exp(Pr.bands.logit)/(1 + exp(Pr.bands.logit))
# we could have directly computed the probabilities by selecting the type="response" option in the predict() function
Pr.all <- predict(fit, newdata = data.frame(age = age.grid), type = 'response', se = T)
# plot
plot(Wage$age, I(Wage$wage>250), xlim=range(Wage$age), type = 'n', ylim = c(0,.2))
points(jitter(Wage$age), I((Wage$wage>250)/5), cex=.5,pch="|")
lines(age.grid, Pr, lwd=2,col='blue')
matlines(age.grid, Pr.bands, lwd = 1,col = 'blue',lty=3)

# Step function
table(cut(Wage$age,3))
# table(cut(Wage$age,breaks = c(range(Wage$age)[1],20,30,40,50,60,70, range(Wage$age)[2])))
fit <- lm(wage ~ cut(age,4), data = Wage)
summary(fit)
coef(summary(fit))
# The function cut() returns an ordered categorical variable; the lm() function 
# then creates a set of dummy variables for use in the re- gression. The 
# age<33.5 category is left out, so the intercept coefficient of $94,160 can 
# be interpreted as the average salary for those under 33.5 years of age, 
# and the other coefficients can be interpreted as the average addi- tional 
# salary for those in the other age groups
fit.pred <- predict(fit, newdata = data.frame(age = age.grid))
cbind(age.grid, newdata, fit.pred)
plot(Wage$age, Wage$wage, col='darkgrey')
lines(age.grid, fit.pred, col='red', lwd=4)

# Splines
# The bs() function generates the entire matrix of bs() basis functions for 
# splines with the specified set of knots. By default, cubic splines are produced
library(splines)
fit <- lm(wage ~ bs(age,knots = c(25,40,60), degree = 3), data=Wage)
pred <- predict(fit, newdata = list(age=age.grid), se=T)
plot(Wage$age, Wage$wage, col='gray')
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit + 2*pred$se.fit, lty='dashed')
lines(age.grid, pred$fit - 2*pred$se.fit, lty='dashed')
summary(fit) 
# Here we have prespecified knots at ages 25, 40, and 60. 
# This produces a spline with six basis functions. (Recall that a cubic spline 
# with three knots has seven degrees of freedom; these degrees of freedom are 
# used up by an intercept, plus six basis functions.)
dim(bs(Wage$age,knots = c(25,40,60)))
dim(bs(Wage$age,df=6))
attr(bs(Wage$age,df=6),'knots')
quantile(Wage$age, probs = c(25,50,75)/100)
attr(bs(Wage$age,df=6),'Boundary.knots')
bs(Wage$age,df=6)[,1]
maxy <- max(3.980 * bs(Wage$age,df=6)[,1],
            44.631 * bs(Wage$age,df=6)[,2],
            62.839 * bs(Wage$age,df=6)[,3],
            55.991 * bs(Wage$age,df=6)[,4],
            50.688 * bs(Wage$age,df=6)[,5],
            16.606 * bs(Wage$age,df=6)[,6])
# explore separate splines
colors <- brewer.pal(6,'Set3')
plot(Wage$age, 3.980 * bs(Wage$age,df=6)[,1], col = colors[1], ylim = c(0,maxy), pch=19)
points(Wage$age, 44.631 * bs(Wage$age,df=6)[,2], col = colors[2], pch=19)
points(Wage$age, 62.839 * bs(Wage$age,df=6)[,3], col = colors[3], pch=19)
points(Wage$age, 55.991 * bs(Wage$age,df=6)[,4], col = colors[4], pch=19)
points(Wage$age, 50.688 * bs(Wage$age,df=6)[,5], col = colors[5], pch=19)
points(Wage$age, 16.606 * bs(Wage$age,df=6)[,6], col = colors[6], pch=19)
plot(bs(Wage$age,df=6)[,1])
# not clear how separate splines looks

# Natural spline
fit2 <- lm(wage ~ ns(age,df=4), data=Wage)
pred2 <- predict(fit2, newdata = data.frame(age=age.grid), se=T)
lines(age.grid, pred2$fit, col='red', lwd=2) # comtinue plot line 2460

# Smothing spline
fit3 <- smooth.spline(Wage$age, Wage$wage, df=16) # we specified df=16. The function then determines which value of λ leads to 16 degrees of freedom
fit4 <- smooth.spline(Wage$age, Wage$wage, cv=T) # we select the smoothness level by cross- validation; this results in a value of λ that yields 6.8degrees of freedom
fit4$df
plot(Wage$age, Wage$wage, xlim = range(Wage$age), cex=0.5, col='darkgrey')
title("Smoothing Spline")
lines(fit3, col='red', lwd=2)
lines(fit4, col='blue', lwd=2)
legend(x = 'topright', legend=c("16 DF", "6.8 DF"),
       col = c("red", "blue"), lty=1, lwd = 2, cex=0.8)

# Local Regression
fit5 <- loess(wage~age, span = 0.2, data = Wage) 
fit6 <- loess(wage~age, span = 0.5, data = Wage) 
# Here we have performed local linear regression using spans of 0.2 and 0.5: 
# that is, each neighborhood consists of 20 % or 50 % of the observations
plot(Wage$age, Wage$wage, xlim = range(Wage$age), cex=0.5, col='darkgrey')
title("Local Regression")
lines(age.grid, predict(fit5, data.frame(age=age.grid)), col='red', lwd=2)
lines(age.grid, predict(fit6, data.frame(age=age.grid)), col='blue', lwd=2)
legend(x = 'topright', legend=c("Span=0.2", "Span=0.5"),
       col = c("red", "blue"), lty=1, lwd = 2, cex=0.8)


# GAM
gam1 <- lm(wage ~ ns(year,4) + ns(age,5) + education, data = Wage)
summary(gam1)
# In order to fit more general sorts of GAMs, using smoothing splines or other 
# components that cannot be expressed in terms of basis functions and then fit 
# using least squares regression, we will need to use the gam library in R
library(gam)
gam.m3 <- gam(wage ~ s(year,4) + s(age,5) + education, data = Wage) # s() we stands for smoothing spline
par(mfrow=c(1,3))
plot(gam.m3, se = T, col='blue')
plot(gam1, se = T, col='blue')
plot.gam(gam1, se = T, col='red') # even though gam1 is not of class gam but rather of class lm, we can still use plot.gam()
# In these plots, the function of year looks rather linear. We can perform a 
# series of ANOVA tests in order to determine which of these three models is best: 
# a GAM that excludes year (M1), a GAM that uses a linear function of year (M2), or a GAM that uses a spline function of year (M3)
gam.m1 <- gam(wage ~ s(age,5) + education, data=Wage)
gam.m2 <- gam(wage ~ year + s(age,5) + education, data=Wage)
anova(gam.m1, gam.m2, gam.m3, test = 'F')
# We find that there is compelling evidence that a GAM with a linear function
# of year is better than a GAM that does not include year at all 
# (p-value = 0.00014). However, there is no evidence that a non-linear function
# of year is needed (p-value = 0.349). In other words, based on the results 
# of this ANOVA, M2 is preferred.
summary(gam.m3)
# The p-values for year and age correspond to a null hypothesis of a linear 
# relationship versus the alternative of a non-linear relationship. The 
# large p-value for year reinforces our conclusion from the ANOVA test that 
# a lin- ear function is adequate for this term. However, there is very clear 
# evidence that a non-linear term is required for age.
preds <- predict(gam.m2, newdata = Wage)

# GAM with local regression terms
gam.lo <- gam(wage ~ s(year, df=4) + lo(age, span = 0.7) + education, data = Wage)
plot(gam.lo, se=T, col = 'green')
# We can also use the lo() function to create interactions before calling the gam() function
gam.lo.i <- gam(wage ~ lo(year, age, span=0.5) + education, data = Wage) # the first term is an interaction between year and age, fit by a local regression surface
install.packages('akima')
library(akima)
par(mfrow=c(1,1))
plot(gam.lo.i)

# GAM for logistic regression
gam.lr <- gam(I(wage > 250) ~ year + s(age,df=5) + education,
              family = binomial, data=Wage)
summary(gam.lr)
pred.prob <- predict(gam.lr, newdata = Wage, type = 'response')
pred <- ifelse(pred.prob > 0.5, 1, 0)
mean(pred == (Wage$wage > 250)) # 0.9736667
par(mfrow=c(1,3))
plot(gam.lr, se=T, col = 'green')
# It is easy to see that there are no high earners in the <HS category
table(Wage$education, I(Wage$wage>250))
# let's exclude this <HS category; this provides more sensible results
gam.lr <- gam(I(wage > 250) ~ year + s(age,df=5) + education,
              family = binomial, data=Wage,
              subset = (education != "1. < HS Grad"))
summary(gam.lr)
pred.prob <- predict(gam.lr, 
                     newdata = subset(Wage, education != "1. < HS Grad"),
                     type = 'response')
pred <- ifelse(pred.prob > 0.5, 1, 0)
mean(pred == (Wage$wage > 250)) # 0.9736667
plot(gam.lr, se=T, col = 'green')

# Understanding splines 
n <- 500
x <- seq(0, 4*pi, length = n)
y <- sin(x) + rnorm(n, sd=.3)
par(mfrow=c(1,1))
plot(x,y)
knots <- seq(0, 8*pi, length=20)
splineTerms <- sapply(knots, function(knot) { (x>knot) * (x-knot)^1}) # try ^3 spline
dim(splineTerms)
colors <- brewer.pal(10,'Set3')
# plot splines
plot(x, y, col = colors[1], lwd=0.5)
for(i in 2:10){
  lines(x, splineTerms[,i], col=colors[i])
}
xMat <- cbind(1,x,x^2,splineTerms)
colnames(xMat) <- c('Intercept','x','x^2', paste('spline', 1:20))
lm.fit <- lm(y ~ xMat - 1)
yhat <- predict(lm.fit)
plot(x,y,frame = F, pch=21,bg='lightblue', cex=2)
lines(x, yhat, col='red', lwd=8)
summary(lm.fit)
xMat0 <- cbind(1,splineTerms) # only splines
xMat1 <- cbind(1,x,x^2,x^3,splineTerms[,1])
xMat3 <- cbind(1,x,x^2,x^3,splineTerms[,1:3])
xMat5 <- cbind(1,x,x^2,x^3,splineTerms[,1:5])
xMat10 <- cbind(1,x,x^2,x^3,splineTerms[,1:10])
lm.fit0 <- lm(y ~ xMat0 - 1)
lm.fit1 <- lm(y ~ xMat1 - 1)
lm.fit3 <- lm(y ~ xMat3 - 1)
lm.fit5 <- lm(y ~ xMat5 - 1)
lm.fit10 <- lm(y ~ xMat10 - 1)
yhat0 <- predict(lm.fit0)
yhat1 <- predict(lm.fit1)
yhat3 <- predict(lm.fit3)
yhat5 <- predict(lm.fit5)
yhat10 <- predict(lm.fit10)
lines(x, yhat0, lwd=2, col='blue') # try to plot only splines
lines(x, yhat1, lwd=2, col='green')
lines(x, yhat3, lwd=2, col='green')
lines(x, yhat5, lwd=2, col='green')
lines(x, yhat10, lwd=2, col='green')

# Understanding splines
# Suppose we fit a curve with basis functions b1(X) = X, 
# b2(X) = (X − 1)2I(X ≥ 1). (Note that I(X ≥ 1) equals 1 for X ≥ 1 and 0 
# otherwise.) We fit the linear regression model Y = β0 +β1b1(X)+β2b2(X)+ε.
# and obtain coefficient estimates β0 = 1, β1 = 1, β2 = −2. Sketch the
# estimated curve between X = −2 and X = 2
x <- seq(-2,2,0.1)
b1 <- x
b2 <- I(x>=1) * (x-1)^2
y <- 1 + 1*b1 + (-2)*b2
df <- data.frame(x = x, b1 = b1, b2 = b2, y = y)
ggplot(df) + 
  geom_line(aes(x,b1), color =  colors[1], linetype='dashed') +
  geom_line(aes(x,b2), color =  colors[1], linetype='dashed') +
  geom_line(aes(x,y), color =  colors[1], size = 2)

# Perform polynomial regression to predict wage using age. Use cross-validation 
# to select the optimal degree d for the polyno- mial. What degree was 
# chosen, and how does this compare to the results of hypothesis testing 
# using ANOVA? Make a plot of the resulting polynomial fit to the data
glm.fit <- glm(wage ~ poly(age,2), data = Wage)
cv.glm(glmfit = glm.fit, data = Wage)$delta[1]
cv.error <- rep(NA, 5)
for(i in 1:5){
  glm.fit <- glm(wage ~ poly(age,i), data = Wage)
  cv.error[i] <- cv.glm(glmfit = glm.fit, data = Wage)$delta[1]
}
plot(cv.error, type = 'b')
m <- which.min(cv.error)
points(m, cv.error[m], col='red', pch=19) # 4th degree poly is ok
# anova
lm.fit1 <- lm(wage ~ poly(age,1), data = Wage)
lm.fit2 <- lm(wage ~ poly(age,2), data = Wage)
lm.fit3 <- lm(wage ~ poly(age,3), data = Wage)
lm.fit4 <- lm(wage ~ poly(age,4), data = Wage)
lm.fit5 <- lm(wage ~ poly(age,5), data = Wage)
lm.fit6 <- lm(wage ~ poly(age,6), data = Wage)
lm.fit7 <- lm(wage ~ poly(age,7), data = Wage)
lm.fit8 <- lm(wage ~ poly(age,8), data = Wage)
lm.fit9 <- lm(wage ~ poly(age,9), data = Wage)
lm.fit10 <- lm(wage ~ poly(age,10), data = Wage)
anova(lm.fit1,lm.fit2,lm.fit3,lm.fit4,lm.fit5,
      lm.fit6,lm.fit7,lm.fit8,lm.fit9,lm.fit10) # 4th degree poly is ok
# plot the best model
plot(Wage$age, Wage$wage,frame = F, pch=21,bg='lightblue', cex=1)
age.grid <- min(Wage$age):max(Wage$age)
pred <- predict(lm.fit4, newdata = list(age=age.grid))
lines(age.grid, pred, col='red', lwd=4)

# Fit a step function to predict wage using age, and perform crossvalidation 
# to choose the optimal number of cuts. Make a plot of the fit obtained
step.fit <- glm(wage ~ cut(age,4), data = Wage)
pred <- predict(step.fit, newdata = data.frame(age=age.grid))
plot(Wage$age, Wage$wage,frame = F, pch=21,bg='lightblue', cex=1)
lines(age.grid, pred, col='red', lwd=4)
inTrain <- sample(1:nrow(Wage), size = as.integer(0.7*nrow(Wage)), replace = F)
train <- Wage[inTrain,]
test <- Wage[-inTrain,]
cv.glm(step.fit, data=Wage)
# train/test to choose best cut 
error <- rep(NA, 9)
colors <- brewer.pal(10,'Set3')
for(i in 2:10){
  step.fit <- glm(wage ~ cut(age,i), data=train)
  pred <- predict(step.fit, newdata = data.frame(age=test$age))
  points(test$age, pred, col=colors[i], lwd=1, pch=19)
  error[i] <- mean((pred - test$wage)^2)
}
plot(error)
m <- which.min(error)
points(m, error[m], col='red', pch=19) # cut with 9 breaks win
# plot step model with 9 breaks
plot(Wage$age, Wage$wage,frame = F, pch=21,bg='lightblue', cex=1)
step.fit <- glm(wage ~ cut(age,9), data=train)
pred <- predict(step.fit, newdata = data.frame(age=test$age))
points(test$age, pred, col=colors[4], lwd=1, pch=19)

# The Wage data set contains a number of other features not explored in this 
# chapter, such as marital status (maritl), job class (jobclass), and others. 
# Explore the relationships between some of these other predictors and wage, 
# and use non-linear fitting techniques in order to fit flexible models to 
# the data. Create plots of the results obtained, and write a summary of your findings
head(Wage)
table(Wage$maritl)
table(Wage$jobclass)
plot(Wage$year, Wage$wage, pch=21, bg='lightblue',cex=1)
ggplot(Wage) + 
  geom_boxplot(aes(x = year, y = wage, group = year, fill = year))
ggplot(Wage) + 
  geom_boxplot(aes(x = maritl, y = wage, fill = maritl))
ggplot(Wage) + 
  geom_boxplot(aes(x = jobclass, y = wage, fill = jobclass))

# Fit some of the non-linear models investigated in this chapter to the Auto 
# data set. Is there evidence for non-linear relationships in this data set?
head(Auto)
pairs(Auto)
par(mfrow=c(1,1))
plot(Auto$horsepower, Auto$mpg, bg='lightblue',pch=21)
# test/train
inTrain <- sample(1:nrow(Auto), size = as.integer(0.7*nrow(Auto)), replace = F)
train <- Auto[inTrain,]
test <- arrange(Auto[-inTrain,], horsepower) # arrange for plotting
fit1 <- lm(mpg ~ horsepower, data = train)
fit2 <- lm(mpg ~ poly(horsepower,2), data = train)
fit3 <- lm(mpg ~ poly(horsepower,5), data = train)
fit4 <- lm(mpg ~ bs(horsepower,df = 4, degree = 2), data = train)
fit5 <- lm(mpg ~ ns(horsepower,df = 4), data = train)
pred1 <- predict(fit1, newdata = test)
pred2 <- predict(fit2, newdata = test)
pred3 <- predict(fit3, newdata = test)
pred4 <- predict(fit4, newdata = test)
pred5 <- predict(fit5, newdata = test)
# plot
lines(test$horsepower, pred1, col=colors[1], lwd=4)
lines(test$horsepower, pred2, col=colors[6], lwd=4)
lines(test$horsepower, pred3, col=colors[7], lwd=4)
lines(test$horsepower, pred4, col=colors[4], lwd=4)
lines(test$horsepower, pred5, col=colors[5], lwd=4)
# compare errors
mean((pred1-test$mpg)^2)
mean((pred2-test$mpg)^2)
mean((pred3-test$mpg)^2)
mean((pred4-test$mpg)^2)
mean((pred5-test$mpg)^2)
# errors equal for fit2-5, choose simplest fit2

# 9. This question uses the variables dis (the weighted mean of distances to 
# five Boston employment centers) and nox (nitrogen oxides concen- tration in
# parts per 10 million) from the Boston data. We will treat dis as the predictor 
# and nox as the response
# Use the poly() function to fit a cubic polynomial regression to predict nox 
# using dis. Report the regression output, and plot the resulting data and polynomial fits
head(Boston)
plot(Boston$dis, Boston$nox, pch=21, bg='lightblue')
Boston <- arrange(Boston, dis)
fit <- lm(nox ~ poly(dis, degree = 3), data=Boston)
lines(Boston$dis, fit$fitted.values, col='red', lwd=2)
# Plot the polynomial fits for a range of different polynomial degrees (say, 
# from 1 to 10), and report the associated residual sum of squares
error <- rep(NA,10)
for(i in 1:10){
  fit <- lm(nox ~ poly(dis, degree = i), data=Boston)
  lines(Boston$dis, fit$fitted.values, col=colors[i], lwd=2)
  error[i] <- mean((fit$fitted.values - Boston$nox)^2)
}
plot(error, type = 'b')
# cross validation to choose best polynom
folds <- sample(x = 1:10, size = nrow(Boston), replace = T)
cv.error <- matrix(NA,nrow = 10, ncol = 10, dimnames = list(NULL,as.character(1:10)))
# in each row - errors for different polynomial for current configuration test/train folds
for(i in 1:10){
  for(j in 1:10) {
    fit <- lm(nox ~ poly(dis, degree = j), data = Boston[folds != i,])
    pred <- predict(fit, newdata = Boston[folds == i,])
    cv.error[i,j] <- mean((pred - Boston$nox[folds == i])^2)
  }
}
cv.error.mean <- apply(cv.error, 2, mean)
plot(cv.error.mean) # 2,3,4 polynoms win
plot(cv.error[2,])
# polynom 9th gives very big error, let's take a look
fit <- lm(nox ~ poly(dis, degree = 9), data = Boston[folds != 5,])
pred <- predict(fit, newdata = Boston[folds == 5,])
plot(Boston$dis, Boston$nox, pch=21, bg='lightblue')
lines(Boston$dis[folds == 5], pred, col='red', lwd=2)

# Use the bs() function to fit a regression spline to predict nox using dis. 
# Report the output for the fit using four degrees of freedom. How did you 
# choose the knots?
inTrain <- sample(1:nrow(Boston), size = as.integer(0.7*nrow(Boston)), replace = F)
train <- Boston[inTrain,]
test <- arrange(Boston[-inTrain,], dis) # arrange for plotting
fit1 <- lm(nox ~ bs(dis, df = 4), data = train)
pred1 <- predict(fit1, newdata = test)
attr(x = bs(Boston$dis, df = 4), which = 'knots')
plot(Boston$dis, Boston$nox, pch=21, bg='lightblue')
lines(test$dis, pred1, col='red', lwd=2)
# Now fit a regression spline for a range of degrees of freedom, and plot the
# resulting fits and report the resulting RSS
# df = 5
fit2 <- lm(nox ~ bs(dis, df = 5), data = train)
pred2 <- predict(fit2, newdata = test)
attr(x = bs(Boston$dis, df = 5), which = 'knots')
lines(test$dis, pred2, col=colors[10], lwd=2)
# df = 6
fit3 <- lm(nox ~ bs(dis, df = 6), data = train)
pred3 <- predict(fit3, newdata = test)
attr(x = bs(Boston$dis, df = 6), which = 'knots')
lines(test$dis, pred3, col=colors[2], lwd=2)
# df = 8
fit4 <- lm(nox ~ bs(dis, df = 8), data = train)
pred4 <- predict(fit4, newdata = test)
attr(x = bs(Boston$dis, df = 8), which = 'knots')
lines(test$dis, pred4, col=colors[3], lwd=2)
mean((pred1 - test$nox)^2)
mean((pred2 - test$nox)^2)
mean((pred3 - test$nox)^2)
mean((pred4 - test$nox)^2)
# cross validation to choose best degree of freedom
folds <- sample(x = 1:10, size = nrow(Boston), replace = T)
cv.error <- matrix(NA,nrow = 10, ncol = 10, dimnames = list(NULL,as.character(1:10)))
# in each row - errors for different splines with different gedree of freedom for current configuration test/train folds
for(i in 1:10){
  for(j in 1:10) {
    fit <- lm(nox ~ bs(dis, df = j), data = Boston[folds != i,])
    pred <- predict(fit, newdata = Boston[folds == i,])
    cv.error[i,j] <- mean((pred - Boston$nox[folds == i])^2)
  }
}
cv.error.mean <- apply(cv.error, 2, mean)
plot(cv.error.mean) # 8 degree splines seems the best
which.min(cv.error.mean)

# Split College data into a training set and a test set. Using out-of-state 
# tuition as the response and the other variables as the predictors, perform
# forward stepwise selection on the training set in order to identify a 
# satisfactory model that uses just a subset of the predictors
head(College)
fit <- regsubsets(Outstate ~ . , data = College, nvmax = dim(College)[2]-1,
                  method = 'forward')
plot(fit, scale = "Cp")
plot(summary(fit)$cp)
which.min(summary(fit)$cp)
# 8 variable model seems quit good
# Fit a GAM on the training data, using out-of-state tuition as the response 
# and the features selected in the previous step as the predictors. Plot the 
# results, and explain your findings
coef <- coefficients(fit, id=8)
coefNames <- names(coef)[-1]
coefNames[1] <- 'Private'
College_sub <- College[, c(coefNames, 'Outstate')]
head(College_sub)
inTrain <- sample(x = 1:nrow(College_sub), size = as.integer(0.7 * nrow(College_sub)), replace = F)
train <- College_sub[inTrain,]
test <- College_sub[-inTrain,]
pairs(College_sub)
fit <- gam(Outstate ~ . , data = train)
summary(fit) # all variable significant! forward subset selection did well job!
par(mfrow=c(2,4))
plot(fit, se=T)
pred <- predict(fit, newdata = test)
mean((pred - test$Outstate)^2) # 129574236
# let's look for non-linearity in data
par(mfrow=c(1,1))
plot(College$Private, College$Outstate)
plot(College$Room.Board, College$Outstate)
plot(College$Personal, College$Outstate)
plot(College$PhD, College$Outstate) # nonlinear
plot(College$Terminal, College$Outstate) # nonlinear
plot(College$perc.alumni, College$Outstate) 
plot(College$Expend, College$Outstate) # nonlinear
plot(College$Grad.Rate, College$Outstate)
# now add non-linearity with polynoms
fit <- gam(Outstate ~ Private + Room.Board + Personal +
             poly(PhD,2) + poly(Terminal,2) + perc.alumni +
             poly(Expend,2) + Grad.Rate, data = train)
par(mfrow=c(2,4))
plot(fit)
pred <- predict(fit, newdata = test)
mean((pred - test$Outstate)^2) # 4438875 vs 129574236 (!) 29 times difference (!)
# it's iteresting: when poly(Expend,5) MSE much bigger 10041420
# now add non-linearity with splines
fit <- gam(Outstate ~ Private + Room.Board + Personal +
             ns(PhD,5) + ns(Terminal,5) + perc.alumni +
             ns(Expend,5) + Grad.Rate, data = train)
par(mfrow=c(2,4))
plot(fit)
pred <- predict(fit, newdata = test)
mean((pred - test$Outstate)^2) # 4191888 vs 129574236 (!) 30 times difference (!)

################################################################
# Tree based methods
################################################################
# log
x <- seq(0,2,0.001)
plot(x, log(x), ylim = c(-10,10), type = 'l', lwd=4)
x0 <- 1
abline(h=log(x0), col='lightgray')
abline(v=x0,col='lightgray')
abline(v=0,col='lightgray')
points(x0, log(x0), col='red',pch=19)

# Classification tree
library(tree)
library(ISLR)
head(Carseats)
Carseats$High <- as.factor( ifelse(Carseats$Sales <= 8, "No", "Yes") ) # without as.factor - error
tree.carseats <- tree(High ~ .-Sales, data = Carseats)
summary(tree.carseats)
# Classification trees deviance -2*sum( sum(n_mk * log(p_mk)) )
plot(tree.carseats)
text(tree.carseats, pretty = 0, cex = 0.5)
tree.carseats
# train/test
set.seed(2)
train <- sample(1:nrow(Carseats),200)
Carseats.test <- Carseats[-train,]
High.test <- Carseats$High[-train]
tree.carseats <- tree(High ~ .-Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, newdata = Carseats.test, type = 'class')
table(tree.pred, High.test)
(102+52)/200 # 77% correct predictions
# pruning the tree
# The function cv.tree() performs cross-validation in order to determine the 
# optimal level of tree complexity; cost complexity pruning is used in order 
# to select a sequence of trees for consideration. We use the argument 
# FUN=prune.misclass in order to indicate that we want the classification 
# error rate to guide the cross-validation and pruning process, rather than 
# the default for the cv.tree() function, which is deviance
# The cv.tree() function reports the number of terminal nodes of each tree 
# con- sidered (size) as well as the corresponding error rate and the value 
# of the cost-complexity parameter used (k, which corresponds to α in (8.4)
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats
# Note that, despite the name, dev corresponds to the cross-validation error 
# rate in this instance. The tree with 9 terminal nodes results in the lowest 
# cross-validation error rate, with 50 cross-validation errors
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type='b')
m <- which.min(cv.carseats$dev)
points(cv.carseats$size[m], cv.carseats$dev[m], col='red', pch=19)
plot(cv.carseats$k, cv.carseats$dev, type='b')
points(cv.carseats$k[m], cv.carseats$dev[m], col='red', pch=19)
# We now apply the prune.misclass() function in order to prune the tree to prune.
# obtain the nine-node tree
prune.carseats <- prune.misclass(tree.carseats, best = 9)
prune.carseats
plot(prune.carseats)
text(prune.carseats, pretty = 0, cex=0.5)
# How well does this pruned tree perform on the test data set
tree.pred <- predict(prune.carseats, newdata = Carseats.test, type='class')
table(tree.pred, High.test)
(97+58)/200 # 77.5%
# pruning process produced a more interpretable tree, but it has also improved the classification accuracy
# If we increase the value of best, we obtain a larger pruned tree with lower classification accuracy
prune.carseats <- prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, cex=0.5)
tree.pred <- predict(prune.carseats, newdata = Carseats.test, type = 'class')
table(tree.pred, High.test)
(102+53)/200 # 77.5% 

# Regression tree
# First, we create a training set, and fit the tree to the training data
library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv ~ . , data = Boston, subset = train)
summary(tree.boston)
# only 4 variables were used for growing the tree
# In the context of a regression tree, the deviance is simply the sum of squared errors for the tree
par(mfrow=c(1,1))
plot(tree.boston)
text(tree.boston, cex=0.5, pretty = 0)
# The variable lstat measures the percentage of individuals with lower socioeconomic 
# status. The tree indicates that lower values of lstat cor- respond to more 
# expensive houses. The tree predicts a median house price of $46, 400 for 
# larger homes in suburbs in which residents have high socioe- conomic status (rm>=7.437 and lstat<9.715).
# Now let's see whether pruning the tree will improve performance
cv.boston <- cv.tree(tree.boston)
cv.boston
plot(cv.boston$size, cv.boston$dev, type = 'b')
# unpruned tree perform best
yhat <- predict(tree.boston, newdata = Boston[-train,])
plot(yhat, Boston[-train,'medv'])
abline(a = 0, b = 1)
mean((yhat - Boston[-train,'medv'])^2)
# In other words, the test set MSE associated with the regression tree is 25.05. 
# The square root of the MSE is therefore around 5.005, indicating that this 
# model leads to test predictions that are within around $5, 005 of the true
# median home value for the suburb
prune.boston <- prune.tree(tree.boston, best = 3)
prune.boston
plot(prune.boston)
text(prune.boston, cex=1)

# Bagging and Random Forest
# Recall that bagging is simply a special case of a random forest with m = p
library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv ~ . , data = Boston, subset = train,
                           mtry = 13, importance = T)
bag.boston
# how well bagged model performs on test set
bag.boston.pred <- predict(bag.boston, newdata = Boston[-train,])
plot(bag.boston.pred, Boston[-train,'medv'])
abline(0,1)
mean((bag.boston.pred - Boston[-train,'medv'])^2) # 16.26598
# The test set MSE associated with the bagged regression tree is 16.3, almost half that obtained using an optimally-pruned single tree
# We could change the number of trees grown by randomForest() using the ntree argument
bag.boston <- randomForest(medv ~ . , data = Boston, subset = train,
                           mtry = 13, ntree = 25)
plot(bag.boston)
bag.boston.pred <- predict(bag.boston, newdata = Boston[-train,])
mean((bag.boston.pred - Boston[-train,'medv'])^2) #12.9136
# Growing a random forest proceeds in exactly the same way, except that we use 
# a smaller value of the mtry argument. By default, randomForest() uses p/3 
# variables when building a random forest of regression trees, and √p variables 
# when building a random forest of classification trees
set.seed(1)
rf.boston <- randomForest(medv ~ . , data = Boston, subset = train,
                          mtry = 6, importance = T)
rf.boston
summary(rf.boston)
rf.boston.pred <- predict(rf.boston, newdata = Boston[-train,])
mean((rf.boston.pred - Boston[-train, 'medv'])^2) # 13.2842
plot(rf.boston)
# Using the importance() function, we can view the importance of each importance() variable
importance(rf.boston)
rf.boston$importance
# IncMSE is based upon the mean decrease of accuracy in predictions on the out
# of bag samples when a given variable is excluded from the model. IncNodePurity
# is a measure of the total decrease in node impurity that results from splits 
# over that variable, averaged over all trees
# Plot of variables importance measures
varImpPlot(rf.boston)

# Boosting
# We run gbm() with the option distribution="gaussian" since this is a regression 
# problem; if it were a bi- nary classification problem, we would use distribution="bernoulli"
library(gbm)
library(MASS) # Boston
data(Boston)
set.seed(1)
boost.boston <- gbm(medv ~ . , data = Boston[train,],
                    distribution = 'gaussian', n.trees = 5000,
                    interaction.depth = 4)
# The summary() function produces a relative influence plot and also outputs the relative influence statistics
summary(boost.boston)
# We see that lstat and rm are by far the most important variables. We can also 
# produce partial dependence plots for these two variables. These plots 
# illustrate the marginal effect of the selected variables on the response after 
# integrating out the other variables
par(mfrow=c(2,2))
plot(boost.boston, i='rm')
plot(boost.boston, i='lstat')
# predict
boost.boston.pred <- predict(boost.boston, newdata = Boston[-train,],
                             n.trees = 5000)
mean((boost.boston.pred - Boston$medv[-train])^2) # 19.18304
# The test MSE obtained is 11.8; similar to the test MSE for random forests 
# and superior to that for bagging
# We can perform boosting with a different value of the shrinkage parameter λ
boost.boston <- gbm(medv ~ . , data = Boston[train,],
                    distribution = 'gaussian', n.trees = 5000,
                    interaction.depth = 4, shrinkage = 0.2,
                    verbose = F)
boost.boston.pred <- predict(boost.boston, newdata = Boston[-train,],
                             n.trees = 5000)
mean((boost.boston.pred - Boston$medv[-train])^2) # 20.3696, not lower!

# 8.4 Exercises
# 3. Consider the Gini index, classification error, and cross-entropy in a
# simple classification setting with two classes. Create a single plot
# that displays each of these quantities as a function of pm1 
pm1 <- seq(0,1,0.01)
pm2 <- 1 - pm1
G <- pm1*(1-pm1) + pm2*(1-pm2)
D <- - (pm1*log(pm1) + pm2*log(pm2))
par(mfrow=c(1,1))
plot(pm1, G)
plot(pm1, D)

# 7. In the lab, we applied random forests to the Boston data using mtry=6 and 
# using ntree=25 and ntree=500. Create a plot displaying the test error resulting 
# from random forests on this data set for a more comprehensive range of values 
# for mtry and ntree
inTrain <- sample(x = 1:nrow(Boston), size = 0.7*nrow(Boston))
train <- Boston[inTrain,]
test <- Boston[-inTrain,]
mtryVector <- c(4,8,13)
ntreeVector <- seq(1,500, by = 5)
rf.error <- matrix(NA, nrow = length(mtryVector), ncol = length(ntreeVector))
for(i in 1:length(mtryVector)){
  for(j in 1:length(ntreeVector)){
    rf.boston <- randomForest(medv ~ . , data = train,
                              mtry = mtryVector[i], ntree=ntreeVector[j])
    rf.boston.pred <- predict(rf.boston, newdata = test)
    rf.error[i,j] <- mean((rf.boston.pred - test$medv)^2)
  }
}
library(RColorBrewer)
colors <- brewer.pal(n = 3, name='Set1')
plot(rf.error[1,], type = 'l', col = colors[1], xlab = 'Number of trees',
     ylab = 'Mean squared error')
lines(rf.error[2,], col = colors[2])
lines(rf.error[3,], col = colors[3])
legend('topright', legend = c('mtry = 4', 'mtry = 8', 'mtry = 12'),
       col = c(colors[1], colors[2], colors[3]), pch=19)

# 8. In the lab, a classification tree was applied to the Carseats data set 
# after converting Sales into a qualitative response variable. Now we will 
# seek to predict Sales using regression trees and related approaches, treating 
# the response as a quantitative variable
head(Carseats)
# train/test
inTrain <- sample(1:nrow(Carseats), 0.7*nrow(Carseats))
train <- Carseats[inTrain,]
test <- Carseats[-inTrain,]
# fit regression tree, plot, interpret
library(tree)
tree.carseats <- tree(Sales ~ . , data = train)
tree.carseats
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, cex=0.5)
# test error (unpruned tree )
tree.carseats.pred <- predict(tree.carseats, newdata = test)
mean((tree.carseats.pred - test$Sales)^2) # 4.158795
# cross-validation to choose tree complexity
cv.carseats <- cv.tree(tree.carseats)
cv.carseats
names(cv.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
which.min(cv.carseats$dev) # 6 nodes tree performs well (size - in backward direction)
plot(cv.carseats$k, cv.carseats$dev, type = 'b')
# let's prune at 6 tree MSE
prune.carseats <- prune.tree(tree.carseats, best = 6)
plot(prune.carseats)
text(prune.carseats, cex=0.5)
# test error for 6 node best tree
prune.carseats.pred <- predict(prune.carseats, newdata = test)
mean((prune.carseats.pred - test$Sales)^2) # 4.532505, a bit worse
# does bagging improve test error?
library(randomForest)
bag.carseats <- randomForest(Sales ~ . , data = train, mtry = 10, importance = T)
bag.carseats
bag.carseats.pred <- predict(bag.carseats, newdata = test)
mean((bag.carseats.pred - test$Sales)^2) # 2.03549, improvement two times! over optimally pruned single tree
plot(bag.carseats.pred, test$Sales, xlim = c(0,15))
abline(0,1)
bag.carseats$importance
varImpPlot(bag.carseats)
# does random forest improve test error?
rf.carseats <- randomForest(Sales ~ . , data = train, mtry = 3, importance = T)
rf.carseats
summary(rf.carseats)
plot(rf.carseats)
rf.carseats.pred <- predict(rf.carseats, newdata = test)
mean((rf.carseats.pred - test$Sales)^2) # 2.362902, no improvement over bagging
importance(rf.carseats)
varImpPlot(rf.carseats)

# 9. This problem involves the OJ data set which is part of the ISLR package
library(ISLR)
library(dplyr)
library(ggplot2)
head(OJ)
pairs(OJ)
colors <- brewer.pal(10, 'Set1')
OJ %>% group_by(StoreID) %>% summarise(n = n()) %>%
  ggplot() +
  geom_line(aes(x = StoreID, y = n), color=colors[2]) +
  geom_point(aes(x = StoreID, y = n), color = colors[1], size = 4) +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7),
                   labels = c('store 1','store 2','store 3','store 4',
                              'store 5', 'store 6', 'store 7')) +
  ylab(label = 'Quantity of sellings')
OJ %>% group_by(WeekofPurchase) %>% count() %>%
  ggplot() + geom_line(aes(WeekofPurchase, n), color = colors[3])
ggplot() + geom_boxplot(aes(x = Purchase))
# train/test
dim(OJ)
inTrain <- sample(x = 1:nrow(OJ), size = 0.7*nrow(OJ))
train <- OJ[inTrain,]
test <- OJ[-inTrain,]
# classificatioin tree
tree.OJ <- tree(Purchase ~ . , data = train)
tree.OJ
summary(tree.OJ)
plot(tree.OJ)
text(tree.OJ, cex = 0.7)
# plot of the most important variable LoyalCH
OJ %>% ggplot() + geom_density(aes(LoyalCH, color = Purchase))
# training error
tree.OJ.pred <- predict(tree.OJ, newdata = train, type = 'class')
table(tree.OJ.pred, train$Purchase)
(407 + 217) / 749 # 0.8331108
# test error
tree.OJ.pred <- predict(tree.OJ, newdata = test, type = 'class')
table(tree.OJ.pred, test$Purchase)
(168 + 89) / 321 # 0.8006231
# cross validation to determine the optimal tree size
cv.OJ <- cv.tree(tree.OJ)
cv.OJ
plot(cv.OJ$size, cv.OJ$dev, type='b') # best tree - unpruned, full set of vars
# 5 node tree
prune.OJ <- prune.misclass(tree.OJ, best = 4)
plot(prune.OJ)
text(prune.OJ)
tree.OJ.pred <- predict(prune.OJ, newdata = train, type = 'class')
table(tree.OJ.pred, train$Purchase)
(387 + 237)/749 # 0.8331108
tree.OJ.pred <- predict(prune.OJ, newdata = test, type = 'class')
table(tree.OJ.pred, test$Purchase)
(160 + 104)/321 # 0.8224299

# 10. We now use boosting to predict Salary in the Hitters data set
head(Hitters)
# Remove the observations for whom the salary information is unknown, and then 
# log-transform the salaries
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
sum(is.na(Hitters$Salary))
Hitters$SalaryLog <- log(Hitters$Salary)
head(Hitters)
# train/test
inTrain <- sample(x = 1:nrow(Hitters), size = 0.7*nrow(Hitters))
train <- Hitters[inTrain,]
test <- Hitters[-inTrain,]
# regression tree
boost.Hitters <- gbm(SalaryLog ~ . -Salary, data = train,
                     distribution = 'gaussian', n.trees = 1000,
                     interaction.depth = 4, shrinkage = 0.01)
boost.Hitters.pred <- predict(boost.Hitters, newdata = test)
summary(boost.Hitters)
plot(boost.Hitters, i = 'CAtBat')
plot(boost.Hitters, i = 'CRuns')
boost.Hitters.pred <- predict(boost.Hitters, newdata = test, n.trees = 1000)
mean((boost.Hitters.pred - test$SalaryLog)^2) # 0.1472354
# Perform boosting on the training set with 1,000 trees for a range of values 
# of the shrinkage parameter λ. Plot with different shrinkage values on the 
# x-axis and the corresponding training and test set MSE on the y-axis
lambda <- seq(0.001,1, by=0.01)
error.train <- rep(NA, length(lambda))
error.test <- rep(NA, length(lambda))
for(i in 1:length(lambda)){
  boost.Hitters <- gbm(SalaryLog ~ . -Salary, data = train,
                       distribution = 'gaussian', n.trees = 1000,
                       interaction.depth = 4, shrinkage = lambda[i])
  boost.Hitters.pred <- predict(boost.Hitters, newdata = test, n.trees = 1000)
  error.test[i] <- mean((boost.Hitters.pred - test$SalaryLog)^2)
  boost.Hitters.pred <- predict(boost.Hitters, newdata = train, n.trees = 1000)
  error.train[i] <- mean((boost.Hitters.pred - train$SalaryLog)^2)
}
plot(lambda,error.train, type = 'l', col = colors[1], 
     ylim = c(min(error.train,error.test), max(error.train,error.test)),
     ylab = 'MSE train/test')
points(lambda, error.test, type = 'l', col = colors[2])
# Compare the test MSE of boosting to the test MSE of linear regression
lm.Hitters <- lm(SalaryLog ~ . - Salary, data = train)
lm.Hitters.pred <- predict(lm.Hitters, newdata = test)
mean((lm.Hitters.pred - test$SalaryLog)^2) #0.2848915 vs 0.1472354 for boostong
# Which variables appear to be the most important predictors in the boosted model?
summary(boost.Hitters)
# Which variables appear to be the most important predictors in the linear regression model?
summary(lm.Hitters)
# Now apply bagging to the training set. What is the test set MSE for this approach?
bag.Hitters <- randomForest(SalaryLog ~ . - Salary, data = train,
                            mtry = 19, importance = T)
bag.Hitters
bag.Hitters$importance
bag.Hitters.pred <- predict(bag.Hitters, newdata = test)
mean((bag.Hitters.pred - test$SalaryLog)^2) # 0.1596696 worse than simple tree

# 11. This question uses the Caravan data set
head(Caravan)
?Caravan
summary(Caravan)
plot(Caravan$Purchase)
table(Caravan$Purchase)
dim(Caravan)
# train/test
inTrain <- sample(x = 1:nrow(Caravan), size = 0.7*nrow(Caravan))
train <- Caravan[inTrain,]
test <- Caravan[-inTrain,]
# fit boosting tree, which predictors appear to be the most important?
boost.Caravan <- gbm(ifelse(Purchase == 'Yes', 1,0) ~ . , data = train,
                     distribution = 'bernoulli', n.trees = 1000,
                     shrinkage = 0.01)
boost.Caravan
summary(boost.Caravan)
plot(boost.Caravan, i='PPERSAUT')
# Use the boosting model to predict the response on the test data. Predict 
# that a person will make a purchase if the estimated probability of purchase 
# is greater than 20 %
boost.Caravan.prob <- predict.gbm(boost.Caravan, newdata = test, n.trees = 1000,
                                  type = 'response')
boost.Caravan.pred <- ifelse(boost.Caravan.prob > 0.2, 'Yes', 'No')
table(boost.Caravan.pred, test$Purchase)
(1596 + 10)/1747 # 0.9192902 
# What fraction of the people predicted to make a purchase do in fact make one?
10/44 # only 0.2272727 predicted 'Yes' really made purchase
# for 0.5 level
boost.Caravan.pred <- ifelse(boost.Caravan.prob > 0.5, 'Yes', 'No')
table(boost.Caravan.pred, test$Purchase)
(1638 + 0)/1747 # 0.9376073, but no correct predictions for 'Yes' 
# compare with logistic regression
logreg.Caravan <- glm(Purchase ~ . , data = train, family = 'binomial')
logreg.Caravan.prob <- predict(logreg.Caravan, newdata = test, type = 'response')
logreg.Caravan.pred <- ifelse(logreg.Caravan.prob > 0.5, 'Yes', 'No')
table(logreg.Caravan.pred, test$Purchase)
(1625 + 1) /1747 # 0.9307384
1/114 # 0.00877193, sensitivity very low
# compare with KNN
library(class)
knn.pred <- knn(train = select(train,-Purchase), test = select(test, -Purchase),
                cl = train$Purchase, k = 3)
table(knn.pred, test$Purchase)
(1613 + 2)/1747 # 0.9244419
2/113 # 0.01769912, sensitivity

##################################################
# 9.6 Lab: Support Vector Machines
##################################################
set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
# classes are linear separable? no
par(mfrow=c(1,1))
plot(x, col = (3-y), pch=19)
# fit the support vector classifier. Note that in order for the svm() function
# to perform classification (as opposed to SVM-based regression), we must 
# encode the response as a factor variable
df <- data.frame(x=x, y=as.factor(y))
library(e1071)
svm.fit <- svm(y ~ . , data = df, kernel = 'linear', cost = 1e+10, scale = F)
# cost: A cost argument allows us to specify the cost of a violation to the margin.
# When the cost argument is small, then the mar- gins will be wide and many 
# support vectors will be on the margin or will violate the margin. When the 
# cost argument is large, then the margins will be narrow and there will be
# few support vectors on the margin or violating the margin
plot(svm.fit, df) 
# support vectors are plotted as crosses, which points are support vectors:
svm.fit$index
summary(svm.fit)
# svm() function does not explicitly output the coefficients of the linear 
# decision boundary obtained when the support vector classifier is fit, nor 
# does it output the width of the margin
# The e1071 library includes a built-in function, tune(), to perform crossvalidation
set.seed(1)
tune.out <- tune(svm, y ~ . , data = df, kernel = 'linear',
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out) # best model with cost = 1e+02
# best model
bestModel <- tune.out$best.model
summary(bestModel)
# generate test data
x.test <- matrix(rnorm(20*2), ncol = 2)
y.test <- sample(c(-1,1), 20, replace = T)
x.test[y.test == 1,] <- x.test[y.test == 1,] + 1
df.test <- data.frame(x = x.test, y = y.test)
# predict
y.pred <- predict(bestModel, df.test)
table(predict = y.pred, truth = df.test$y)
# predict for cost = 0.01
svm.fit <- svm(y ~ . , data = df, kernel = 'linear', cost = 0.01,
               scale = F)
y.pred <- predict(svm.fit, df.test)
table(predict = y.pred, truth = df.test$y)
# Now consider a situation in which the two classes are linearly separable
x[y==1, ] <- x[y==1,] + 0.5
plot(x, col=(y+5)/2, pch = 19)
# fit the support vector classifier and plot the resulting hyperplane, using 
# a very large value of cost so that no observations are misclassified
df <- data.frame(x = x, y = as.factor(y))
svm.fit <- svm(y ~ . , data = df, kernel = 'linear', cost = 1e5)
summary(svm.fit)
plot(svm.fit, df)
# No training errors were made and only three support vectors were used
# However, we can see from the figure that the margin is very narrow. It seems
# likely that this model will perform poorly on test data but well on train
# on train
y.pred <- predict(svm.fit, newdata = df)
table(predict = y.pred, true = df$y) # perfect! 100% correctly classified
# on test
y.pred <- predict(svm.fit, newdata = df.test)
table(predict = y.pred, true = df.test$y) # 3 non-correctly
# Let's try smaller value of cost
svm.fit <- svm(y ~ . , data = df, kernel = 'linear', cost = 1)
summary(svm.fit)
plot(svm.fit, df)
# Using cost=1, we misclassify a training observation, but we also obtain a 
# much wider margin and make use of seven support vectors. It seems likely 
# that this model will perform better on test data than the model with cost=1e5
# on train
y.pred <- predict(svm.fit, newdata = df)
table(predict = y.pred, true = df$y) # perfect! 100% correctly classified
# on test
y.pred <- predict(svm.fit, newdata = df.test)
table(predict = y.pred, true = df.test$y)

# 9.6.2 Support Vector Machine
# generate data with nonlinear class boundary
set.seed(1)
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1,150), rep(2,50))
plot(x, col = y)
df <- data.frame(x = x, y = as.factor(y))
# train/test
train <- sample(200,100)
svm.fit <- svm(y ~ . , data = df[train,], 
               kernel = 'radial', gamma = 1, cost = 1)
plot(svm.fit, df[train,])
summary(svm.fit)
names(summary(svm.fit))
summary(svm.fit)$decision.values
svm.fit
# there are a fair number of training errors in this SVM fit. If we increase 
# the value of cost, we can reduce the number of training errors. However, 
# this comes at the price of a more irregular decision boundary that seems to
# be at risk of overfitting the data
svm.fit <- svm(y ~ . , data = df[train,], 
               kernel = 'radial', gamma = 1, cost = 1e5)
plot(svm.fit, df[train,])
# cross-validation using tune() to select the best choice of γ and cost for 
# an SVM with a radial kernel
set.seed(1)
tune.out <- tune(svm, y ~ . , data = df[train,], 
                 kernel = 'radial', 
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)
# predict
svm.pred <- predict(tune.out$best.model, newdata = df[-train,])
table(true = df[-train, "y"], 
      predict = svm.pred)
mean(df[-train, "y"] == svm.pred)
# difference between newdata and newx??
svm.pred <- predict(tune.out$best.model, newx = df[-train,])
table(true = df[-train, "y"], 
      predict = svm.pred)
mean(df[-train, "y"] == svm.pred)

# ROC Curves
install.packages("ROCR")
library(ROCR)
rocplot <- function(pred, truth, ...){
  predObj <- prediction(pred, truth) # from ROCR package
  perf <- performance(predObj, 'tpr', 'fpr') 
  plot(perf, ...)
}
# With svm() we can receive also fitted values which are β0 +βˆX +βˆX +...+βˆX
# for linear case
# To obtain the fitted values for a given SVM model fit decision.values=TRUE
svm.fit <- svm(y ~ . , data = df[train,], kernel = 'radial',
               gamma=2, cost = 1, decision.values = T)
svm.fit$decision.values
fitted <- attributes( predict(svm.fit, df[train,], decision.values = T))$decision.values
# ROC plot
par(mfrow = c(1,1))
rocplot(fitted, df[train, "y"], main = "Training data")
prediction(fitted, df[train,"y"])
# By increasing γ we can produce a more flexible fit and generate further improvements in accuracy
svm.fit.flex <- svm(y ~ . , data = df[train,], kernel = 'radial',
                    gamma = 50, cost = 1, decision.values = T)
fitted <- attributes( predict(svm.fit.flex, df[train,], decision.values = T) )$decision.values
rocplot(fitted, df[train, "y"], add = T, col = 'red')
# now on test data
fitted <- attributes( predict(svm.fit, df[-train,], decision.values = T) )$decision.values
rocplot(fitted, df[train, "y"], main = "Training data")
fitted <- attributes( predict(svm.fit.flex, df[-train,], decision.values = T) )$decision.values
rocplot(fitted, df[train, "y"], add = T, col = 'red')

# 9.6.4 SVM with Multiple Classes
# generate a third class of observations
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0,50))
x[y == 0, 2] <- x[y == 0, 2] + 4
df <- data.frame(x = x, y = as.factor(y))
plot(x, col = y+1, pch=19)
# svm
svm.fit <- svm(y ~ . , data = df, kernel = 'radial', cost = 10, gamma = 1)
plot(svm.fit, df)

# 9.6.5 Application to Gene Expression Data
library(ISLR)
?Khan
names(Khan)
head(Khan)
dim(Khan$xtrain)
Khan$xtrain[1,]
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)
# We will use a support vector approach to predict cancer subtype using gene 
# expression measurements. In this data set, there are a very large number of
# features relative to the number of observations. This suggests that we should 
# use a linear kernel, because the additional flexibility that will result from 
# using a polynomial or radial kernel is unnecessary
df <- data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
svm.fit <- svm(y ~ . , data = df, kernel = 'linear', cost = 10)
summary(svm.fit)
table(svm.fit$fitted, df$y)
# We see that there are no training errors. In fact, this is not surprising, 
# because the large number of variables relative to the number of observations 
# implies that it is easy to find hyperplanes that fully separate the classes.
df.test <- data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
svm.pred.test <- predict(svm.fit, newdata = df.test)
table(svm.pred.test, df.test$y)

# 9.7 Exercises
# (1.a) Sketch the hyperplane 1 + 3X1 − X2 = 0. Indicate the set of points for which 1+3X1 −X2 > 0, as well as the set of points for which 1 + 3X1 − X2 < 0
X1 <- seq(1,100,1)
X2 <- seq(1,100,1)
grid <- expand.grid(X1 = X1, X2 = X2)
ggplot(df) + geom_line(aes(X1, 1 + 3*X1)) +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100))
df <- data.frame(grid, y = (1 + 3*grid$X1 - grid$X2) >= 0)
ggplot(data = df) + 
  geom_point(aes(X1, X2, color = y)) +
  #geom_line(aes(X1, 1 + 3*X1), color = 'red', size = 2) +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100))
# (1.b) On the same plot, sketch the hyperplane −2 + X1 + 2X2 = 0. Indicate the set of points for which −2 + X1 + 2X2 > 0, as well as the set of points for which −2 + X1 + 2X2 < 0.
X1 <- seq(-100,100,1)
X2 <- seq(-100,100,1)
grid <- expand.grid(X1 = X1, X2 = X2)
df <- data.frame(grid, y = (-2 + grid$X1 + 2*grid$X2) >= 0)
ggplot(data = df) + 
  geom_point(aes(X1, X2, color = y)) +
  #geom_line(aes( X1, (2 - X1)/2 ), color = 'magenta', size = 2) +
  coord_cartesian(xlim = c(-100,100), ylim = c(-100,100))

# 2. We now investigate a non-linear decision boundary. Sketch the curve 
# (1+X1)^2 +(2−X2)^2 =4
df <- data.frame(grid, y = ((1+grid$X1)^2 +(2 - grid$X2)^2 - 4) >= 0)
ggplot(data = df) + 
  geom_point(aes(X1, X2, color = y)) +
  #geom_line(aes( X1, (2 - X1)/2 ), color = 'magenta', size = 2) +
  coord_cartesian(xlim = c(-20,20), ylim = c(-20,20))

# 3. Here we explore the maximal margin classifier on a toy data set
df <- tribble(
  ~x1, ~x2, ~y,
  2, 4, 'red',
  2, 2, 'red',
  4, 4, 'red',
  1, 4, 'red',
  2, 1, 'blue',
  4, 3, 'blue',
  4, 1, 'blue'
)
ggplot(df) + geom_point(aes(x1,x2, color = y)) + 
  coord_cartesian(xlim = c(0,5), ylim = c(0,5))
# Sketch optimal hyperplane β0 + β1X1 + β2X2 = 0
# beta0 + beta1*1.5 + beta2*1 = 0
# beta0 + beta1*3.5 + beta2*3 = 0

# 4. Generate a simulated two-class data set with 100 observations and two 
# features in which there is a visible but non-linear separation between the two classes
x1 <- rnorm(200, 5, 1)
x2 <- rnorm(200, 5, 1)
y <- rep(c(1,2), c(100,100))
df <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))
df[100:200,]$x1 <- df[100:200,]$x1 + 4
df[151:200,]$x2 <- df[151:200,]$x2 - 3
df[151:200,]$x1 <- df[151:200,]$x1 - 7
ggplot(df) + geom_point(aes(x1, x2, color = y)) +
  coord_cartesian(xlim = c(-2,12), ylim = c(-2,10))
make.grid <- function(X){
  grange <- apply(X, 2, range)
  x1 <- seq(grange[1,1], grange[2,1], 0.1)
  x2 <- seq(grange[1,2], grange[2,2], 0.1)
  expand.grid(x1 = x1, x2 = x2)
}
grid <- make.grid(select(df, x1,x2))
colors <- brewer.pal(8, 'Set3')
ggplot(df) +
  coord_cartesian(xlim = c(-2,12), ylim = c(-2,10)) +
  geom_point(data = grid, aes(x1,x2), color = 'grey') +
  geom_point(aes(x1, x2, color = colors[y]))
# support vector classifier (linear) 
inTrain <- sample(1:nrow(df), size = 0.75 * nrow(df))
svm.fit <- svm(y ~ . , data = df[inTrain,], kernel = 'linear', cost = 10, scale = F)
plot(svm.fit, df)
svm.pred <- predict(svm.fit, newdata = grid) # on training data
ggplot(df) +
  coord_cartesian(xlim = c(-2,12), ylim = c(-2,10)) +
  geom_point(data = grid, aes(x1,x2), color = colors[svm.pred]) +
  geom_point(data = df, aes(x1, x2, color = y))
table(predict = predict(svm.fit, df[inTrain,]), true = df$y[inTrain])
table(predict = predict(svm.fit, df[-inTrain,]), true = df$y[-inTrain])
mean(predict(svm.fit, df[inTrain,]) == df$y[inTrain])
mean(predict(svm.fit, df[-inTrain,]) == df$y[-inTrain]) # 0.76
# support vector machine with a polynomial kernel
svm.fit <- svm(y ~ . , data = df[inTrain,], kernel = 'polynomial', cost = 10, scale = F)
plot(svm.fit, df)
svm.pred <- predict(svm.fit, newdata = grid) # on training data
ggplot(df) +
  coord_cartesian(xlim = c(-2,12), ylim = c(-2,10)) +
  geom_point(data = grid, aes(x1,x2), color = colors[svm.pred]) +
  geom_point(data = df, aes(x1, x2, color = y))
table(predict = predict(svm.fit, df[inTrain,]), true = df$y[inTrain])
table(predict = predict(svm.fit, df[-inTrain,]), true = df$y[-inTrain])
mean(predict(svm.fit, df[inTrain,]) == df$y[inTrain])
mean(predict(svm.fit, df[-inTrain,]) == df$y[-inTrain]) # 0.82
# support vector machine with a radial kernel
svm.fit <- svm(y ~ . , data = df[inTrain,], kernel = 'radial', cost = 10, scale = F)
plot(svm.fit, df)
svm.pred <- predict(svm.fit, newdata = grid) # on training data
ggplot(df) +
  coord_cartesian(xlim = c(-2,12), ylim = c(-2,10)) +
  geom_point(data = grid, aes(x1,x2), color = colors[svm.pred]) +
  geom_point(data = df, aes(x1, x2, color = y))
table(predict = predict(svm.fit, df[inTrain,]), true = df$y[inTrain])
table(predict = predict(svm.fit, df[-inTrain,]), true = df$y[-inTrain])
mean(predict(svm.fit, df[inTrain,]) == df$y[inTrain])
mean(predict(svm.fit, df[-inTrain,]) == df$y[-inTrain]) # 0.92
# cross-validation for choosing best cost parameter
tune.out <- tune(method = svm, y ~ . , data = df, kernel = 'radial', 
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestModel <- tune.out$best.model
table(predict = predict(bestModel, df[-inTrain,]), true = df$y[-inTrain])
mean(predict(bestModel, df[-inTrain,]) == df$y[-inTrain]) # o.98

# 5. We have seen that we can fit an SVM with a non-linear kernel in order to perform classification using a non-linear decision boundary. We will now see that we can also obtain a non-linear decision boundary by performing logistic regression using non-linear transformations of the features
# a. Generate a data set with n = 500 and p = 2, such that the obser- vations belong to two classes with a quadratic decision boundary between them
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)
df <- data.frame(x1 = x1, x2 = x2, y = y)
grid <- expand.grid(x1 = seq(min(x1), max(x1), by = 0.01), x2 = seq(min(x2), max(x2), by = 0.01))
ggplot() + 
  #geom_point(data = grid, aes(x1,x2, color = (x1^2 - x2^2 > 0))) +
  #geom_line(aes(x1, sqrt(x1^2)), color = 'red', size = 2) +
  geom_point(data = df, aes(x1,x2, color = as.factor(y)))
# c. Fit a logistic regression model to the data, using X1 and X2 as predictors
set.seed(2)
inTrain <- sample(1:nrow(df), size = 0.7 * nrow(df))
glm.fit <- glm(y ~ x1 + x2, data = df[inTrain,], family = 'binomial') # try different variables combinations (x1, x2, x1^2, x2^2),
glm.prob <- predict(glm.fit, newdata = df[inTrain,], type = 'response')
glm.pred <- ifelse( glm.prob > 0.5, 1, 0 )
# p = exp(Beta0 + Beta1*X1 + Beta2*X2) / (1 + exp(Beta0 + Beta1*X1 + Beta2*X2))
range(glm.prob)
ggplot() + 
  #geom_point(data = grid, aes(x1,x2, color = (x1^2 - x2^2 > 0))) +
  #geom_line(aes(x1, sqrt(x1^2)), color = 'red', size = 2) +
  geom_point(data = cbind(df[inTrain,], pred = glm.pred), 
             aes(x1,x2, color = as.factor(pred))) +
  geom_line(data = df[inTrain,], aes(x1, (0 - (glm.fit$coefficients[1]) - (glm.fit$coefficients[2]) * x1) / glm.fit$coefficients[3]),
            color = 'magenta') + # decision boundary
  coord_cartesian(ylim = c(-1,1))
table(glm.pred, df$y[inTrain])
mean(glm.pred == df$y[inTrain])
# Now fit a logistic regression model to the data using non-linear functions 
# of X1 and X2 as predictors (e.g. X12, X1 ×X2, log(X2), and so forth)
glm.fit <- glm(y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1 * x2), 
               data = df[inTrain,], family = 'binomial')
summary(glm.fit) # all coefficients not significant
glm.prob <- predict(glm.fit, newdata = df[inTrain,], type = 'response')
glm.pred <- ifelse( glm.prob > 0.5, 1, 0 )
range(glm.prob)
# in this case decision boundary Beta0 + Beta1*X1 + Beta2*X2 = log(0.5/(1 - 0.5))
ggplot() + 
  #geom_point(data = grid, aes(x1,x2, color = (x1^2 - x2^2 > 0))) +
  #geom_line(aes(x1, sqrt(x1^2)), color = 'red', size = 2) +
  geom_point(data = cbind(df[inTrain,], pred = glm.pred), 
             aes(x1,x2, color = as.factor(pred))) +
  geom_line(data = df[inTrain,], aes(x1, (0 - (glm.fit$coefficients[1]) - (glm.fit$coefficients[2]) * x1) / glm.fit$coefficients[3]),
            color = 'magenta') # decision boundary
table(glm.pred, df$y[inTrain])
mean(glm.pred == df$y[inTrain])
# Support vector clasifier
# Fit a support vector classifier to the data with X1 and X2 as predictors. Obtain a class prediction for each training observa- tion. Plot the observations, colored according to the predicted class labels
# linear
svm.fit <- svm(as.factor(y) ~ . , data = df[inTrain,], kernel = 'linear', cost = 1, scale = F)
plot(svm.fit, df[inTrain,])
grid <- expand.grid(x1 = seq(min(x1), max(x1), by = 0.01), x2 = seq(min(x2), max(x2), by = 0.01))
svm.pred <- predict(svm.fit, newdata = grid) # on training data
svm.pred.test <- predict(svm.fit, df[-inTrain,])
ggplot(df) +
  coord_cartesian(xlim = c(-0.6,0.6), ylim = c(-0.6,0.6)) +
  geom_point(data = grid, aes(x1,x2), color = as.integer(svm.pred) + 1) +
  geom_point(data = df[-inTrain,], aes(x1, x2, color = as.integer(svm.pred.test) + 1))
table(predict = predict(svm.fit, df[inTrain,]), true = df$y[inTrain])
table(predict = predict(svm.fit, df[-inTrain,]), true = df$y[-inTrain])
mean(predict(svm.fit, df[inTrain,]) == df$y[inTrain]) # 0.5342857
mean(predict(svm.fit, df[-inTrain,]) == df$y[-inTrain]) # 0.5533333
# radial
svm.fit <- svm(as.factor(y) ~ . , data = df[inTrain,], kernel = 'radial', cost = 1, scale = F)
plot(svm.fit, df[inTrain,])
grid <- expand.grid(x1 = seq(min(x1), max(x1), by = 0.01), x2 = seq(min(x2), max(x2), by = 0.01))
svm.pred <- predict(svm.fit, newdata = grid) # on training data
svm.pred.test <- predict(svm.fit, df[-inTrain,])
ggplot(df) +
  coord_cartesian(xlim = c(-0.6,0.6), ylim = c(-0.6,0.6)) +
  geom_point(data = grid, aes(x1,x2), color = as.integer(svm.pred) + 1) +
  geom_point(data = df[-inTrain,], aes(x1, x2, color = as.integer(svm.pred.test) + 1))
table(predict = predict(svm.fit, df[inTrain,]), true = df$y[inTrain])
table(predict = predict(svm.fit, df[-inTrain,]), true = df$y[-inTrain])
mean(predict(svm.fit, df[inTrain,]) == df$y[inTrain]) # 0.8485714
mean(predict(svm.fit, df[-inTrain,]) == df$y[-inTrain]) # 0.7866667
ggplot(data = cbind(df[-inTrain,], svm.pred.test)) +
  geom_point(data = grid, aes(x1,x2), color = as.integer(svm.pred) + 1) +
  geom_point(aes(x1, x2, color = as.integer(y) + 1))

# 6. At the end of Section 9.6.1, it is claimed that in the case of data that 
# is just barely linearly separable, a support vector classifier with a small 
# value of cost that misclassifies a couple of training observations may perform 
# better on test data than one with a huge value of cost that does not misclassify 
# any training observations. You will now investigate this claim
# a. Generate two-class data with p = 2 in such a way that the classes are just barely linearly separable
N <- 200
x1 <- rnorm(N, 5, 1)
x2 <- rnorm(N, 5, 1)
y <- rep(c(1,2), c(N/2,N/2))
df <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))
df[(N/2):N,]$x1 <- df[(N/2):N,]$x1 + 3
ggplot(df) + geom_point(aes(x1,x2, color = y))
# b. Compute the cross-validation error rates for support vector classifiers 
# with a range of cost values. How many training er- rors are misclassified 
# for each value of cost considered, and how does this relate to the cross-validation 
# errors obtained?
inTrain <- sample(x = 1:nrow(df), size = 0.7 * nrow(df), replace = F)
tune.out <- tune(method = svm, y ~ . , data = df[inTrain,], kernel = 'linear',
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 20, 50, 80, 100, 200, 500)))
summary(tune.out) # small cost -> huge error
plot(tune.out$performances$cost, tune.out$performances$error, type = 'b')
m <- which.min(tune.out$performances$error)
points(tune.out$performances$cost[m], tune.out$performances$error[m], col = 'red', pch = 19)
bestModel <- tune.out$best.model
bestModel
# training error
svm.pred <- predict(bestModel, newdata = df[inTrain,])
table(predict = svm.pred, true = df$y[inTrain])
mean(svm.pred == df$y[inTrain])
# test error
svm.pred <- predict(bestModel, newdata = df[-inTrain,])
table(predict = svm.pred, true = df$y[-inTrain])
mean(svm.pred == df$y[-inTrain])
# train/test error for range of cost 
costRange <- c(0.001, 0.01, 0.1, 1, 5, 10, 20)
error.train <- rep(NA, length(costRange))
error.test <- rep(NA, length(costRange))
for(i in 1:length(costRange)){
  svm.fit <- svm(y ~ . , data = df[inTrain,], kernel = 'linear',
                 cost = costRange[i], scale = F)
  svm.pred.train <- predict(svm.fit, newdata = df[inTrain,])
  svm.pred.test <- predict(svm.fit, newdata = df[-inTrain,])
  error.train[i] <- mean(svm.pred.train == df$y[inTrain])
  error.test[i] <- mean(svm.pred.test == df$y[-inTrain])
}
ggplot(data.frame(e.test = error.test, e.train = error.train, costs = costRange)) +
  geom_line(aes(costs, e.train), color = 'blue') +
  geom_line(aes(costs, e.test), color = 'red')
# plot classifier with small and large cost
plot(bestModel, df)
grid <- expand.grid(x1 = seq(min(df$x1), max(df$x1), by = 0.1), 
                    x2 = seq(min(df$x2), max(df$x2), by = 0.1))
svm.fit <- svm(y ~ . , data = df[inTrain,], kernel = 'linear',
               cost = 100, scale = F) # change cost: 0.001 and 100
svm.pred.grid <- predict(svm.fit, newdata = grid)
svm.pred.test <- predict(svm.fit, newdata = df[-inTrain,])
ggplot() +
  geom_point(data = data.frame(grid, y = svm.pred.grid),
             aes(grid$x1, grid$x2, color = y), alpha = 0.1) +
  geom_point(data = df[-inTrain,],
             aes(x1,x2, color = svm.pred.test))

# In this problem, you will use support vector approaches in order to predict 
# whether a given car gets high or low gas mileage based on the Auto data set
head(Auto)
Auto <- Auto %>% mutate(mpg_high = as.factor(1 * (mpg > median(mpg))))
Auto %>% select(mpg, mpg_high)
str(Auto)
ggplot(Auto) + geom_boxplot(aes(x = mpg_high, y = mpg))
# b. Fit a support vector classifier to the data with various values of cost, 
# in order to predict whether a car gets high or low gas mileage. Report the 
# cross-validation errors associated with dif- ferent values of this parameter
tune.out <- tune(method = svm, mpg_high ~ . , data = Auto, kernel = 'linear',
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 20, 50, 80, 100, 200, 500)))
summary(tune.out) # small cost -> huge error
bestModel <- tune.out$best.model
bestModel
# c. Now repeat (b), this time using SVMs with radial and polyno- mial basis 
# kernels, with different values of gamma and degree and cost
tune.out <- tune(method = svm, mpg_high ~ ., data = Auto, kernel = 'radial',
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 50, 100),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out) # small cost -> huge error
bestModel <- tune.out$best.model
bestModel
tune.out$performances %>% arrange(cost) %>% 
  ggplot() + geom_line(aes(cost, error, color = as.factor(gamma)))
tune.out$performances %>% 
  ggplot() + geom_line(aes(gamma, error, color = as.factor(cost)))
# number of support vectors vs cost
costRange <- c(0.001, 0.01, 0.1, 1, 10, 50, 100, 500)
nSV <- rep(NA, length(costRange))
for(i in 1:length(costRange)){
  svm.fit <- svm(mpg_high ~ . , data = Auto, kernel = 'linear', cost = costRange[i])
  nSV[i] <- svm.fit$tot.nSV
}
plot(log(costRange), nSV, type = 'b')
# d. Make some plots to back up your assertions in (b) and (c)
plot(svm.fit, Auto, horsepower ~ weight)
plot(bestModel, Auto, horsepower ~ acceleration)
# svm for two variables
svm.fit <- svm(mpg_high ~ horsepower + weight , data = Auto, kernel = 'linear', cost = 100)
plot(svm.fit, Auto, horsepower ~ weight)
svm.fit
?plot.svm

# 8. This problem involves the OJ data set which is part of the ISLR package
library(ISLR)
head(OJ)
dim(OJ)
# a. Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations
inTrain <- sample(1:nrow(OJ), 800)
# b. Fit a support vector classifier to the training data using cost=0.01, with 
# Purchase as the response and the other variables as predictors
svm.fit <- svm(Purchase ~ . , data = OJ[inTrain,], kernel = 'linear', cost = 0.01)
summary(svm.fit)
# c. What are the training and test error rates?
svm.pred.train <- predict(svm.fit, newdata = OJ[inTrain,])
table(predict = svm.pred.train, true = OJ$Purchase[inTrain])
mean(svm.pred.train == OJ$Purchase[inTrain]) # 0.83
svm.pred.test <- predict(svm.fit, newdata = OJ[-inTrain,])
table(predict = svm.pred.test, true = OJ$Purchase[-inTrain])
mean(svm.pred.test == OJ$Purchase[-inTrain]) # 0.8333333
# d. Use the tune() function to select an optimal cost. Consider val-
# ues in the range 0.01 to 10
tune.out <- tune(method = svm, Purchase ~ . , data = OJ, kernel = 'linear',
                 ranges = list(cost = c(0.001,0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestModel <- tune.out$best.model
bestModel
# e. Compute the training and test error rates using this new value for cost
# cost for best model = 10
svm.fit <- svm(Purchase ~ . , data = OJ[inTrain,], kernel = 'linear', cost = 10)
svm.pred.train <- predict(svm.fit, newdata = OJ[inTrain,])
table(predict = svm.pred.train, true = OJ$Purchase[inTrain])
mean(svm.pred.train == OJ$Purchase[inTrain]) # 0.84
svm.pred.test <- predict(svm.fit, newdata = OJ[-inTrain,])
table(predict = svm.pred.test, true = OJ$Purchase[-inTrain])
mean(svm.pred.test == OJ$Purchase[-inTrain]) # 0.8259259
# f. Repeat parts (b) through (e) using a support vector machine
# with a radial kernel. Use the default value for gamma
# kernel = 'radial', cost = 0.01
svm.fit <- svm(Purchase ~ . , data = OJ[inTrain,], kernel = 'radial', cost = 0.01)
summary(svm.fit)
# train/test error
svm.pred.train <- predict(svm.fit, newdata = OJ[inTrain,])
table(predict = svm.pred.train, true = OJ$Purchase[inTrain])
mean(svm.pred.train == OJ$Purchase[inTrain]) # 0.6075
svm.pred.test <- predict(svm.fit, newdata = OJ[-inTrain,])
table(predict = svm.pred.test, true = OJ$Purchase[-inTrain])
mean(svm.pred.test == OJ$Purchase[-inTrain]) # 0.6185185 worse than fr linear (!)
# cross validation to choose best cost
tune.out <- tune(method = svm, Purchase ~ . , data = OJ, kernel = 'radial',
                 ranges = list(cost = c(0.001,0.01, 0.1, 1, 5, 10, 100),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)
bestModel <- tune.out$best.model
bestModel
# train/test errors for best model
svm.pred.train <- predict(bestModel, newdata = OJ[inTrain,])
table(predict = svm.pred.train, true = OJ$Purchase[inTrain])
mean(svm.pred.train == OJ$Purchase[inTrain]) # 0.8625
svm.pred.test <- predict(bestModel, newdata = OJ[-inTrain,])
table(predict = svm.pred.test, true = OJ$Purchase[-inTrain])
mean(svm.pred.test == OJ$Purchase[-inTrain]) # 0.8666667 better than for linear (!)
# with default cost value - result worse than for linear, with cross validated - better

# 10.4 Lab 1: Principal Components Analysis
# In this lab, we perform PCA on the USArrests data set, which is part of the base R package
states <- row.names(USArrests)
states
names(USArrests)
# vars have different means
apply(USArrests, 2, mean)
summary(USArrests)
# vars have different variances
apply(USArrests, 2, var)
apply(USArrests, 2, sd)
ggplot(USArrests) + geom_boxplot(aes(x = 'Assault', y = Assault))
# If we failed to scale the variables before performing PCA, then most of the 
# principal components that we observed would be driven by the Assault variable
pca.out <- prcomp(USArrests, scale = T)
warnings()
# By default, the prcomp() function centers the variables to have mean zero. By 
# using the option scale=TRUE, we scale the variables to have standard deviation one
names(pca.out)
pca.out$center # vars means 
pca.out$scale
# The rotation matrix provides the principal component loadings. When we matrix-
# multiply the X matrix by pr.out$rotation, it gives us the coordinates of the 
# data in the rotated coordinate system
pca.out$rotation
# pca.out$x has as its columns the principal component score vectors
pca.out$x
pca.out$x[,2]
# plot first two principal components
biplot(pca.out, scale = 0)
# change sign
pca.out$rotation <- -pca.out$rotation
pca.out$x <- -pca.out$x
biplot(pca.out, scale = 0)
# standard deviation of each prin- cipal component
pca.out$sdev
# variance explained by each principal component
pca.var <- pca.out$sdev^2
pca.var
# proportion of variance explained
pve <- pca.var / sum(pca.var)
pve
plot(pve, xlab = 'Principal component', ylab = 'Proportion of variance explained',
     ylim = c(0,1), type = 'b')
plot(cumsum(pve), xlab = 'Principal component', ylab = 'Cumulative proportion of variance explained',
     ylim = c(0,1), type = 'b')
x <- c(1,2,8,-3)
cumsum(x)

# 10.5 Lab 2: Clustering
# 10.5.1 K-Means Clustering
set.seed(2)
x <- matrix(rnorm(50*2), ncol = 2, dimnames = list(NULL, c('x1', 'x2')))
x[1:25,1] <- x[1:25,1] + 3
x[1:25,2] <- x[1:25,2] - 4
plot(x[,'x1'], x[,'x2'])
km.out <- kmeans(x, 2, nstart = 20)
warnings()
names(km.out)
km.out$cluster
plot(x, col = km.out$cluster + 1, main = 'K-Means Clustering, K = 2', 
     xlab = '', ylab = '', pch = 20, cex = 2)
# Here the observations can be easily plotted because they are two-dimensional. 
# If there were more than two variables then we could instead perform PCA and 
# plot the first two principal components score vectors
# K = 3
set.seed(3)
km.out <- kmeans(x, 3, nstart = 20)
km.out
plot(x, col = km.out$cluster + 1, main = 'K-Means Clustering, K = 3', 
     xlab = '', ylab = '', pch = 20, cex = 2)
# compare nstart = 1 and = 20
set.seed(3)
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss # total within-cluster sum of squares which we seek to minimize by performing K-means clustering
km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss 
km.out$withinss # individual within-cluster sum-of-squares

# 10.5.2 Hierarchical Clustering
x <- matrix(rnorm(50*2), ncol = 2, dimnames = list(NULL, c('x1', 'x2')))
x[1:25,1] <- x[1:25,1] + 3
x[1:25,2] <- x[1:25,2] - 4
hc.complete <- hclust(d = dist(x), method = 'complete')
hc.average <- hclust(d = dist(x), method = 'average')
hc.single <- hclust(d = dist(x), method = 'single')
par(mfrow = c(1,3))
plot(hc.complete, main = 'Complete Linkage', xlab = '', sub = '', cex = .9)
plot(hc.average, main = 'Average Linkage', xlab = '', sub = '', cex = .9)
plot(hc.single, main = 'Single Linkage', xlab = '', sub = '', cex = .9)
# determine the cluster labels for each observation associated with a given cut of the dendrogram
cutree(tree = hc.complete, k = 2)
cutree(tree = hc.average, k = 2)
cutree(tree = hc.single, k = 2)
par(mfrow = c(1,1))
plot(x, col = cutree(tree = hc.complete, k = 2), 
     main = 'Result of cutting Complete Linkage tree', 
     xlab = '', ylab = '', pch = 20, cex = 2)
cutree(tree = hc.single, k = 4)
plot(x, col = cutree(tree = hc.complete, k = 4), 
     main = 'Result of cutting Single Linkage tree', 
     xlab = '', ylab = '', pch = 20, cex = 2)
# Correlation based distance
# Correlation-based distance can be computed using the as.dist() func- tion, 
# which converts an arbitrary square symmetric matrix into a form that the hclust() 
# function recognizes as a distance matrix. However, this only makes sense for 
# data with at least three features since the absolute corre- lation between 
# any two observations with measurements on two features is always 1. Hence, 
# we will cluster a three-dimensional data set
x <- matrix(rnorm(30*3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(d = dd, method = 'complete'), 
     main = 'Complete Linkage with Correlation-Based Distance', xlab = '', sub = '')

# 10.6 Lab 3: NCI60 Data Example
library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data
nci.labs
length(nci.labs)
table(nci.labs)
dim(nci.data)
nci.data[1,]
# Each cell line is labeled with a cancer type. We do not make use of the cancer types in performing PCA and clustering, as these are unsupervised techniques. But after performing PCA and clustering, we will check to see the extent to which these cancer types agree with the results of these unsupervised techniques
# PCA
pca.out <- prcomp(nci.data, scale = T)
summary(nci.data[,1])
summary(nci.data[,100])
summary(nci.data[,5000])
scale(nci.data[,5000])
cbind(nci.data[,5000], scale(nci.data[,5000]))
sd(nci.data[,5000])
mean(nci.data[,5000])
(-2.400000e-01 -  mean(nci.data[,5000]) ) /  sd(nci.data[,5000])
# plot fist few principal components score vectors
Cols <- function(vec){ # function that assigns a distinct color to each element of a numeric vector
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
} 
Cols(nci.labs)
table(Cols(nci.labs))
par(mfrow = c(1,1))
plot(pca.out$x[,1:2], col = Cols(nci.labs), pch = 19,
     xlab = 'Z1', ylab = 'Z2')
plot(pca.out$x[,c(1,3)], col = Cols(nci.labs), pch = 19,
     xlab = 'Z1', ylab = 'Z3')
# On the whole, cell lines corresponding to a single cancer type do tend to have 
# similar values on the first few principal component score vectors. This indicates 
# that cell lines from the same cancer type tend to have pretty similar gene expression levels.
# Without PCA we coudn't make such visualization (!)
# Summary of the proportion of variance explained (PVE) of the first few principal components
summary(pca.out)
names(summary(pca.out))
plot(pca.out)
pca.out$sdev
pca.var <- pca.out$sdev^2
pve <- 100 * pca.var / sum(pca.var)
plot(pve, xlab = 'Principal components', ylab = 'PVE', type = 'o', col = 'blue')
plot(cumsum(pve), xlab = 'Principal components', ylab = 'Cumulative PVE', type = 'o', col = 'brown3')
# we can obtain elements of pve directly
summary(pca.out)$importance[2,] # PVE
summary(pca.out)$importance[3,] # cumsum PVE
# We see that together, the first seven principal components explain around 40 % of the variance in the data. This is not a huge amount of the variance. However, looking at the scree plot, we see that while each of the first seven principal components explain a substantial amount of variance, there is a marked decrease in the variance explained by further principal components. That is, there is an elbow in the plot after approx- imately the seventh principal component

# 10.6.2 Clustering the Observations of the NCI60 Data
# We now proceed to hierarchically cluster the cell lines in the NCI60 data, 
# with the goal of finding out whether or not the observations cluster into distinct types of cancer
# To begin, we standardize the variables to have mean zero and standard deviation 
# one. As mentioned earlier, this step is optional and should be performed only 
# if we want each gene to be on the same scale
nci.data.std <- scale(nci.data)
d <- dist(nci.data.std)
plot(hclust(d), labels = nci.labs, main = 'Complete Linkage',
     xlab = '', sub = '', ylab = '', cex = 0.5)
plot(hclust(d, method = 'average'), labels = nci.labs, main = 'Average Linkage',
     xlab = '', sub = '', ylab = '', cex = 0.5)
plot(hclust(d, method = 'single'), labels = nci.labs, main = 'Single Linkage',
     xlab = '', sub = '', ylab = '', cex = 0.5)
# we'll continue with complete linkage hierarchial clustering
hc.out <- hclust(dist(nci.data.std), method = 'complete')
hc.out
hc.clusters <- cutree(hc.out, k = 4)
table(hc.clusters)
hc.clusters
table(hc.clusters, nci.labs)
# There are some clear patterns. All the leukemia cell lines fall in cluster 3,
# melanoma - to cluster 1, NSCLC - 1, OVARIAN - 1, RENAL - 1, but some are distributed
# and a lot of diagnosis fall into one cluster
plot(hc.out, labels = nci.labs)
abline(h = 139, col = 'red')
# It is easy to verify that the resulting clusters are the same as the ones we 
# obtained using cutree(hc.out,4)
hc.clusters <- cutree(hc.out, h = 139)
table(hc.clusters, nci.labs)
# Compare with k-means clustering woth k = 4
# We claimed earlier in Section 10.3.2 that K-means clustering and hierarchical
# clustering with the dendrogram cut to obtain the same number of clusters can 
# yield very different results. How do these NCI60 hierarchical clustering results 
# compare to what we get if we perform K-means clustering with K = 4?
set.seed(2)
km.out <- kmeans(nci.data.std, centers = 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)
# we see that clusters are different
# Rather than performing hierarchical clustering on the entire data matrix, we 
# can simply perform hierarchical clustering on the first few principal component 
# score vectors
hc.out <- hclust(dist(pca.out$x[,1:5]))
plot(hc.out, labels = nci.labs, main = 'Hier. clustering on first five score vectors')
table(cutree(hc.out, 4), nci.labs)
# Not surprisingly, these results are different from the ones that we obtained 
# when we performed hierarchical clustering on the full data set
# Sometimes performing clustering on the first few principal component score 
# vectors can give better results than performing clustering on the full data. 
# In this situation, we might view the principal component step as one of denoising 
# the data. We could also perform K-means clustering on the first few principal 
# component score vectors rather than the full data set

# 10.7 Exercises
# Suppose that we have four observations, for which we compute a dissimilarity 
# matrix, given by
dd <- matrix(c(0.0, 0.3, 0.4, 0.7,
               0.3, 0.0, 0.5, 0.8,
               0.4, 0.5, 0.0, 0.45,
               0.7, 0.8, 0.45, 0.0), nrow = 4, ncol = 4)
# On the basis of this dissimilarity matrix, sketch the dendrogram that results 
# from hierarchically clustering these four observations using complete/single/average linkage
d <- as.dist(dd)
hc.out <- hclust(d, method = 'complete')
plot(hc.out, main = 'Complete Linkage', xlab = '', sub = '', ylab = '')
hc.out <- hclust(d, method = 'single')
plot(hc.out, main = 'Single Linkage', xlab = '', sub = '', ylab = '')
hc.out <- hclust(d, method = 'average')
plot(hc.out, main = 'Average Linkage', xlab = '', sub = '', ylab = '')
# investigate dist()
x <- matrix(1:8, nrow = 4)
d <- dist(x)
x
d
# distance 1-4 = sqrt((4-1)^2 + (8-5)^2)
# 3. In this problem, you will perform K-means clustering manually, with K = 2, 
# on a small example with n = 6 observations and p = 2 features
x <- matrix(c(1,4,
              1,3,
              0,4,
              5,1,
              6,2,
              4,0), nrow = 6, byrow = T, dimnames = list(c(1:6), c('X1', 'X2')))
x[,'X1']
x[,1]
x[1:3,]
x <- as.data.frame(x)
# a. plot observations
plot(x$X1, x$X2, pch = 19, xlab = 'X1', ylab = 'X2')
# b. Randomly assign a cluster label to each observation
labels <- sample(x = c(1,2), size = dim(x)[1], replace = T)
plot(x$X1, x$X2, col = c('red', 'blue')[labels],
     pch = 19, xlab = 'X1', ylab = 'X2')
# c. Compute the centroid for each cluster
cX1 <- tapply(x[,'X1'], labels, mean)
cX2 <- tapply(x[,'X2'], labels, mean)
# d. Assign each observation to the centroid to which it is closest, in terms of 
# Euclidean distance. Report the cluster labels for each observation
# distances to centroid 1
dist_c1 <- sqrt( (x$X1 - cX1[1])^2 + (x$X2 - cX2[1])^2 )
dist_c2 <- sqrt( (x$X1 - cX1[2])^2 + (x$X2 - cX2[2])^2 )
dist_c1 < dist_c2

# 7. we mentioned the use of correlation-based distance and Euclidean distance 
# as dissimilarity measures for hierarchical clus- tering. It turns out that 
# these two measures are almost equivalent: if each observation has been centered 
# to have mean zero and standard deviation one, and if we let rij denote the 
# correlation between the ith and jth observations, then the quantity 1 − rij is 
# proportional to the squared Euclidean distance between the ith and jth observations
# On the USArrests data, show that this proportionality holds
head(USArrests)
dist(USArrests)
cor(t(as.matrix(USArrests)))

# 9. Consider the USArrests data. We will now perform hierarchical clustering 
# on the states
head(USArrests)
head(scale(USArrests))
# Hierarchically cluster the states using complete linkage and Eu- clidean distance, after scaling the variables to have standard deviation one
USArrests.std <- scale(USArrests)
hc.out <- hclust(dist(USArrests.std), method = 'complete')
plot(hc.out)
hc.out
hc.clusters <- cutree(hc.out, k = 3)
table(hc.clusters)
hc.clusters
table(hc.clusters)
USArrests %>% mutate(clusters = hc.clusters) %>% mutate(State = rownames(USArrests))
plot(hc.out, hang = -1, cex=0.8)
library("ape")
library(RColorBrewer)
colors = brewer.pal(n = 3,'Set1')
plot(as.phylo(hc.out), type = "unrooted", tip.color = colors[hc.clusters], edge.color = 'grey', label.offset = 0.05, cex = 0.6, no.margin = T)
plot(as.phylo(hc.out), type = "radial", tip.color = colors[hc.clusters], edge.color = 'grey', label.offset = 0.05, cex = 1, no.margin = T)
# Using hierarchical clustering with complete linkage and Euclidean distance, cluster the states (no scaling)
hc.out <- hclust(dist(USArrests), method = 'complete')
hc.clusters <- cutree(hc.out, k = 3)
plot(as.phylo(hc.out), type = "radial", tip.color = colors[hc.clusters], edge.color = 'grey', label.offset = 0.05, cex = 1, no.margin = T)

# 10. In this problem, you will generate simulated data, and then perform PCA and K-means clustering on the data
# a. Generate a simulated data set with 20 observations in each of three classes 
# (i.e. 60 observations total), and 50 variables
x <- matrix(rnorm(3000, mean = 0, sd = 1), nrow = 60)
x[20:40, 5] <- x[20:40, 5] + 3
x[40:60, 5] <- x[40:60, 5] + 7
y <- rep(c(1,2,3), each = 20)
plot(x[,1],x[,5], col = y, pch = 19)
# b. Perform PCA on the 60 observations and plot the first two prin- cipal component score vectors
pca.out <- prcomp(x)
plot(pca.out$x[,1:2], col = y, pch = 19, xlab = 'Z1', ylab = 'Z2')
summary(pca.out)$importance[2,] # PVE
summary(pca.out)$importance[3,] # cumsum PVE
plot(summary(pca.out)$importance[2,], type = 'b')
# c. Perform K-means clustering of the observations with K = 3. How well do the 
# clusters that you obtained in K-means cluster- ing compare to the true class labels?
km.out <- kmeans(x, centers = 3, nstart = 20)
table(predict = km.out$cluster, true = y)
plot(x[,1],x[,5], col = y, pch = 19)
plot(x[,1],x[,5], col = km.out$cluster, pch = 19)
# d. Perform K-means clustering with K = 2. Describe your results
km.out <- kmeans(x, centers = 2, nstart = 20)
table(predict = km.out$cluster, true = y)
plot(x[,1],x[,5], col = km.out$cluster, pch = 19)
# f. Now perform K-means clustering with K = 3 on the first two principal component
# score vectors, rather than on the raw data. That is, perform K-means clustering 
# on the 60 × 2 matrix of which the first column is the first principal component 
# score vector, and the second column is the second principal component score vector. 
km.out <- kmeans(pca.out$x[,1:2], centers = 3, nstart = 3)
table(predict = km.out$cluster, true = y)
plot(x[,1],x[,5], col = y, pch = 19)
plot(x[,1],x[,5], col = km.out$cluster, pch = 19) # much more better
# g. Using the scale() function, perform K-means clustering with K = 3 on the 
# data after scaling each variable to have standard deviation one. How do these
# results compare to those obtained in (b)? Explain
km.out <- kmeans(scale(x), centers = 3, nstart = 20)
table(predict = km.out$cluster, true = y)
plot(x[,1],x[,5], col = y, pch = 19)
plot(x[,1],x[,5], col = km.out$cluster, pch = 19)
apply(x, 2, mean)
apply(scale(x), 2, mean)
x[,1]
x[,1]/sd(x[,1])
(x[,1] - mean(x[,1])) / sd(x[,1]) # this is scale()
scale(x[,1])










