install.packages("UsingR")
library(UsingR)
library(reshape2)
library(RColorBrewer)

data("galton")

cols <- brewer.pal(8, "Set2")
ggplot(galton) + 
  geom_segment(aes(y = galton$child, x = rep(1, dim(galton)[1]), 
                   yend = galton$parent, xend = rep(2, dim(galton)[1])),
               color = cols[1]) +
  geom_point(aes(y = galton$child, x = rep(1, dim(galton)[1])), color = cols[2]) +
  geom_point(aes(y = galton$parent, x = rep(2, dim(galton)[1])), color = cols[2]) +
  labs(x = "Height dynamic child vs. parent", y = "Height") +
  scale_x_continuous(breaks = c(1.00, 2.00), labels = c("1.00" = "children", "2.00" = "parents")) +
  scale_y_continuous(breaks = c(60.0:74.0), labels = as.character(c(60.0:74.0))) +
  theme(axis.text.x = element_text(angle = 90, color = cols[3]),
        axis.text.y = element_text(color = cols[3]))

long <- melt(galton) 
ggplot(long, aes(x = value)) +
  geom_histogram(aes(fill = variable), color = cols[3], binwidth = 1) +
  facet_grid(. ~ variable)

library(manipulate)
myHist <- function(mu) {
  mse <- mean((galton$child - mu)^2)
  sm <- sum((galton$child - mu)^2) / length(galton$child)
  ggplot(galton, aes(x = child)) +
    geom_histogram(fill = "salmon", color = cols[6], binwidth = 1) +
    geom_vline(xintercept = mu, size = 1, color = cols[1]) +
    ggtitle(paste("mu = ", mu, ", MSE = ", round(mse,2), ", Sum = ", round(sm,2), sep = ""))
}
manipulate(myHist(mu), mu = slider(62,74, step = 0.5))

ggplot(galton, aes(x = parent, y = child)) +
  geom_point(color = cols[1])
# Overplotted. let's make points size proportional to quontity of observation 
library(tibble)
freqGalton <- as.data.frame(table(galton$child, galton$parent))
names(freqGalton) <- c("child", "parent", "freq")
head(freqGalton)
str(freqGalton$child)
str(freqGalton)
freqGalton$child <- as.numeric(as.character(freqGalton$child))
freqGalton$parent <- as.numeric(as.character(freqGalton$parent))
str(freqGalton)
library(dplyr)
g <- ggplot(data = filter(freqGalton, freq > 0), aes(x = parent, y = child)) +
  geom_point(color = "grey50", aes(size = freq+10))+
  geom_point(aes(color = freq, size = freq)) +
  scale_color_gradient(low = "lightblue", high = "white") +
  scale_size(range = c(2,10), guide = "none") 
g
# let's plot regression line through mass center and manipulate slope beta
x<- galton$child - mean(galton$child)
y <- galton$parent - mean(galton$parent)
freqGalton <- as.data.frame(table(x,y))
names(freqGalton) <- c("child", "parent", "freq")
freqGalton$child <- as.numeric(as.character(freqGalton$child))
freqGalton$parent <- as.numeric(as.character(freqGalton$parent))
myPlot <- function(beta) {
  freqGalton <- filter(freqGalton, freq > 0)
    ggplot(data = freqGalton, aes(x = parent, y = child)) +
    scale_size(range = c(2,10), guide = "none") +
    geom_point(color = "grey50", aes(size = freq+10))+
    geom_point(aes(color = freq, size = freq)) +
    scale_color_gradient(low = "lightblue", high = "white") +
    geom_abline(intercept = 0, slope = beta, size = 1, color = cols[1]) +
    ggtitle(paste("beta = ", beta, "mse = ", round(mean((freqGalton$child - beta*freqGalton$parent)^2),2)))
}
manipulate(myPlot(beta), beta = slider(0.2,1.2, step = 0.02))
# R regression function
lm(I(child - mean(child)) ~ I(parent - mean(parent)) - 1, data = galton)
lm(child ~ parent, data = galton)

# Covariance
x <- galton$parent
y <- galton$child
n <- length(x)
cov(x,y)
# by definition
covXY_1 <- (1/(n - 1)) * sum((x - mean(x)) * (y - mean(y)))
covXY_1
covXY_2 <- (1/(n-1)) * (sum(x*y) - n*mean(x)*mean(y))
covXY_2
# Correlation
covXY_1 / (sd(x) * sd(y))
cor(x,y)
cor(y,x)

# linear regression coefficients y = beta0 + beta1*x
beta1 <- cor(y,x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x)))
lm(y ~ x)
# reverse x and y
beta1 <- cor(x,y) * sd(x) / sd(y)
beta0 <- mean(x) - beta1 * mean(y)
rbind(c(beta0, beta1), coef(lm(x ~ y)))
lm(y ~ x)
# center x and y
yc <- y - mean(y)
xc <- x - mean(x)
beta1 <- sum(yc * xc) / sum(xc^2)
c(beta1, coef(lm(y ~ x))[2])
lm(yc ~ xc - 1)
# slope of normilized variables is equal to cor
yn <- (y - mean(y)) / sd(y)
xn <- (x - mean(x)) / sd(x)
c(cor(y,x), cor(yn,xn), coef(lm(yn ~ xn))[2])
# let's plot
ggplot(data = filter(freqGalton, freq > 0), aes(x = parent, y = child)) +
  geom_point(color = "grey50", aes(size = freq+10))+
  geom_point(aes(color = freq, size = freq)) +
  scale_color_gradient(low = "lightblue", high = "white") +
  scale_size(range = c(2,10), guide = "none") +
  geom_smooth(method = "lm", formula = y~x, se = F)

# Regression to the mean
library(UsingR)
data("father.son")
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x,y)
ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
  geom_point(size = 6, color = "black", alpha = 0.2) +
  geom_point(size = 4, color = "salmon", alpha = 0.2) +
  xlim(-4,4) + ylim(-4,4) +
  geom_abline(intercept = 0, slope = 1) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_abline(intercept = 0, slope = rho, size = 2) +
  geom_abline(intercept = 0, slope = 1/ rho, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, color = cols[1])
  
# Linerar Regression for Prediction
library(UsingR)
data("diamond")
library(ggplot2)
library(RColorBrewer)
cols = brewer.pal(8, "Set1")
ggplot(diamond, aes(x = carat, y = price)) +
  xlab("Mass (carats") + ylab("Price (SIN $") +
  geom_point(size = 6, color = cols[7], alpha = 0.5) +
  geom_point(size = 5, color = cols[1], alpha = 0.5) +
  geom_smooth(method = "lm", color = cols[2],size = 1) 
  #geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = cols[2])
# fitting linear regression model
fit <- lm(price ~ carat, data = diamond)
fit
coef(fit)
summary(fit)
# Coefficients interpretation: intercept -259.63 is expected price of 0 carat diamond (not prectical); slope estimates an expected 3721.02 dollar increase in price for every carat increase in mass of diamond
# Getting a more interpretable intercept
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)
# Now: intercept $500.0833 is the expected price for the average sized diamond of the data (0.2042 carats) 
fit3 <- lm(price ~ I(carat * 10), data = diamond)
coef(fit3)
# Now: slope 372.1025 - estimate of price change for 1/10 of carat
# Predicting the price of diamond
new_diamonds <- c(0.16,0.27,0.34)
new_diamonds_prices <- coef(fit)[1] + coef(fit)[2] * new_diamonds
predict(fit, newdata = data.frame(carat = new_diamonds))
predict(fit)
ggplot(diamond, aes(x = carat, y = price)) +
  xlab("Mass (carats)") + ylab("Price (SIN $)") +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = cols[5]) +
  geom_hline(yintercept = new_diamonds_prices[1], color = "grey85") +
  geom_vline(xintercept = new_diamonds[1], color = "grey85") +
  geom_hline(yintercept = new_diamonds_prices[2], color = "grey85") +
  geom_vline(xintercept = new_diamonds[2], color = "grey85") +
  geom_hline(yintercept = new_diamonds_prices[3], color = "grey85") +
  geom_vline(xintercept = new_diamonds[3], color = "grey85") +
  geom_point(size = 3, color = cols[8]) +
  geom_point(size = 2, color = cols[2]) +
  geom_point(data = data.frame(x_new = new_diamonds, y_new = new_diamonds_prices), 
             aes(x = x_new, y = y_new), color = cols[1], size = 3) 

# Residuals
y <- diamond$price
x <- diamond$carat
n <- length(y)  
fit <- lm(y ~ x)
e <- resid(fit)
e1 <- y - predict(fit) # without arguments return predictions on observations
max(abs(e - e1))
e2 <- y - (coef(fit)[1] + coef(fit)[2] * x)
max(abs(e - e2))
# sum of residuals = 0
sum(e)
# sum of residuals times variable also 0
sum(e * x)
# Residuals are the signed length of the red lines
plot(diamond$carat, diamond$price,
     xlab = "Mass (carats)",
     ylab = "Price (SIN $)",
     bg = "lightblue",
     col = "black", cex = 1.1, pch = 21, frame = FALSE)
abline(fit, lwd = 2)
for (i in 1:n){
  lines(c(x[i],x[i]), c(y[i],predict(fit)[i]), col = "red", lwd = 2)
}
# let's plot residual versus x
plot(x, e,
     xlab = "Mass (carats)",
     ylab = "Price (SIN $)",
     bg = "lightblue",
     col = "black", cex = 1.1, pch = 21, frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1:n){
  lines(c(x[i],x[i]), c(e[i],0), col = "red", lwd = 2)
}
# let's investigate potential of residuals analysis
# 1 Non-linear data: how residuals can highlight model fit
x <- runif(100,-3,3)
y <- x + sin(x) + rnorm(100,0,0.2)
ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
  geom_point(size = 7, color = "black", alpha = 0.4) +
  geom_point(size = 5, color = "red", alpha = 0.4) +
  geom_smooth(method = "lm", color = "black")
# let's plot residuals vs. x
ggplot(data.frame(x = x, y = resid(lm(y ~ x))),
       aes(x = x, y = y)) +
  geom_hline(yintercept = 0, size = 2) +
  geom_point(size = 7, color = "black", alpha = 0.4) +
  geom_point(size = 5, color = "red", alpha = 0.4) +
  xlab("X") + ylab("Residuals")
# 2 Heteroskedasticity
x <- runif(100, 0, 6)
y <- x + rnorm(100, mean = 0, sd = .001 * x)
ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(size = 7, color = "black", alpha = 0.4) +
  geom_point(size = 5, color = "red", alpha = 0.4) 
# it seems that all data are lying on one line..
# let's show only residuals dynamic
ggplot(data.frame(x = x, y = resid(lm(y ~ x))),
       aes(x = x, y = y)) +
  geom_hline(yintercept = 0, size = 2) +
  geom_point(size = 7, color = "black", alpha = 0.4) +
  geom_point(size = 5, color = "red", alpha = 0.4) +
  xlab("X") + ylab("Residuals")
# residual plot for diamond dataset
diamond$e <- resid(lm(price ~ carat, data = diamond))
ggplot(diamond, aes(x = carat, y = e)) +
  xlab("Mass (carats)") +
  ylab("Residual price (SIN $)") +
  geom_hline(yintercept = 0, size = 2) +
  geom_point(size = 7, color = "black", alpha = 0.5) +
  geom_point(size = 5, color = "blue", alpha = 0.2)
# there is no pattern in a plot, so it's seems like a pretty good fit
# let's demonstarate something about variability what will help demonstrate properties of regression model fit 
e <- c(resid(lm(price ~ 1, data = diamond)),
       resid(lm(price ~ carat, data = diamond)))
fit <- factor(c(rep("Itc", nrow(diamond)),
                rep("Itc, slope", nrow(diamond))))
ggplot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit)) +
  geom_dotplot(binaxis = "y", size = 2, stackdir = "center", binwidth = 20) +
  xlab("Fitting approach") +
  ylab("Residual price") # left: variations in diamond prices around mean; right: variations around regression line

# Residual variance 
# is an estimand of population variance around regression line
y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y ~ x)
summary(fit)
summary(fit)$sigma
# let's check
sqrt( sum(resid(fit)^2) / (n-2) )
# Total variance = Residual Variance + Regression Variance
fit <- lm(child ~ parent, galton)
totalVar <- sum( (galton$child - mean(galton$child))^2 )
residVar <- deviance(fit)
# contribution of variance by regression variance
1 - residVar/totalVar
# this is R^2
summary(fit)$r.squared
# the same as
cor(galton$child, galton$parent)^2

# Inference in regression
# Var(Beta1) depends on how variable our points are around true regression line (residuals variance) and how variable our X are
library(UsingR)
data("diamond")
y <- diamond$price
x <- diamond$carat
n <- length(y)
beta1 <- cor(x,y) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - (beta0 + beta1 * x)
sigma <- sqrt(sum(e^2) / (n-2)) # estimation of residuals std
ssx <- sum((x - mean(x))^2)
sigmaBeta0 <- sqrt( (1/n + mean(x)^2/ssx) * sigma^2 )
sigmaBeta1 <- sqrt( sigma^2 / ssx )
# t statistic for testing hypothesis beta0 == 0 and beta1 == 0
tBeta0 <- (beta0 - 0) / sigmaBeta0
tBeta1 <- (beta1 - 0) / sigmaBeta1
# p-values: probability of this t statistic or larger
pBeta0 <- 2 * pt(abs(tBeta0), df = n-2, lower.tail = F)
pBeta1 <- 2 * pt(abs(tBeta1), df = n-2, lower.tail = F)
coefTable <- rbind(c(beta0, sigmaBeta0, tBeta0, pBeta0),
                   c(beta1, sigmaBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t-value", "P(>|t|)")
rownames(coefTable) <- c("beta0", "beta1")
coefTable
# the same by simple way
fit <- lm(y ~ x)
summary(fit)$coefficients
# let's estimate confidence intervals
sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1,1) * qt(.975, df = fit$df) * sumCoef[1,2]
sumCoef[2,1] + c(-1,1) * qt(.975, df = fit$df) * sumCoef[2,2]
# slope characterises change price for 1 unit of carat; it's too big, let's estimate for 1/10 of carat
( sumCoef[2,1] + c(-1,1) * qt(.975, df = fit$df) * sumCoef[2,2] ) / 10
# wit 95% confidence we estemated that 0.1 carat increase will result in 355.6-388.57 price increase

# let's predict
newX <- data.frame(x = seq(min(x), max(x), length = 25))
p1 <- data.frame(predict(fit, newdata = newX, interval = "confidence"))
p2 <- data.frame(predict(fit, newdata = newX, interval = "prediction"))
p1$interval <- "confidence"
p2$interval <- "prediction"
p1$x <- newX$x
#names(p1)[1] <- "y"
p2$x <- newX$x
#names(p2)[1] <- "y"
dat <- rbind(p1,p2)
names(dat)[1] <- "y"
ggplot(dat, aes(x = x, y = y)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2) +
  geom_line() +
  geom_point(data = data.frame(x = diamond$carat, y = diamond$price), aes(x = x, y = y), size = 2) +
  geom_point(color = cols[3], alpha = 0.5)

# Predictions with linear regression
# There is a distinction between intervals for regression line at the point and the prediction what a y would be at point x0
# Prediction interval at point x0: sigma_hat * sqrt(1 + 1/n + (x0 - X_hat)^2 / sum(Xi - X_hat))
#    prediction std error will be minimized (more accurate) when x0 is close to mean: our prediction of diamond price will be more accurate for diamonds with mass close to mean, or prediction of child height more accurate when parent height close to average
#    sigma_hat - how variable the points are around regression line
#    1/n - std error decreases with number of points n
#    1 + make predicted interval wider


# Examples
# In the mtcars data set, fit a linear regression model of weight (predictor) on mpg (outcome). Get a 95% confidence interval for the expected mpg at the average weight. What is the lower endpoint?
head(mtcars)
fit <- lm(mpg ~ wt, mtcars)
summary(fit)
library(ggplot2)
library(RColorBrewer)
cols <- brewer.pal(8, "Set1")
ggplot(data = mtcars) +
  geom_point(aes(wt, mpg), color = cols[1]) +
  geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2], color = cols[2])
x_new <-  data.frame(x = mean(mtcars$wt))
x_new <- 3.0
data.frame(predict.lm(fit, newdata = data.frame(x = x_new)))

# Multivariable Regression
# Explanation
n <- 100
x <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
y <- 1 + x + x2 + x3 + rnorm(n, sd = .1)
ey <- resid(lm(y ~ x2 + x3))
ex <- resid(lm(x ~ x2 + x3))
sum(ey * ex) / sum(ex^2)
coef(lm(ey ~ ex - 1))
coef(lm(y ~ x + x2 + x3))

ey2 <- resid(lm(y~x+x3))
ex2 <- resid(lm(x2 ~ x + x3))
sum(ey2 * ex2) / sum(ex2^2)
coef(lm(ey2 ~ ex2 - 1))
coef(lm(y ~ x + x2 + x3))

# Multivariable Regression Example 1
require(datasets)
data(swiss)
?swiss
require(GGally)
require(ggplot2)
ggpairs(swiss, lower = list(continuous = "smooth"))
summary(lm(Fertility ~ ., data = swiss))
summary(lm(Fertility ~ ., data = swiss))$coefficients[1]
summary(lm(Fertility ~  Agriculture, data = swiss))
# Note that sign of Agriculture coefficiets changed; in both cases the coef is stastically significant (p-value less that 0.05, reject H0 that beta_agri == 0)
# How can adjustment reverse the sign of an effect? Let's try a simulation
n <- 100
x2 <- 1:n
x1 <- .01 * x2 + runif(n, -0.1, 0.1)
y <- -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef
dat <- data.frame(y = y, x1 = x1, x2 = x2, ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))
ggplot(dat, aes(y = y, x = x1, color = x2)) +
  geom_point(color = "grey50", size = 5) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_point(size = 4)
# let's plot residuals
ggplot(dat, aes(y = ey, x = ex1, color = x2)) +
  geom_point(color = "grey50", size = 5) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_point(size = 4)
# now we see that there is no relationship between residuals and x2 (color points randomly distributed)
# it means that including every variable is right; there are consequences 
# What happens when we include completely unnecessary variable
z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ . + z, data = swiss)
lm(Fertility ~ ., data = swiss)   
lm(Fertility ~ Agriculture, data = swiss) 
lm(Fertility ~ Agriculture + Examination, data = swiss)
lm(Fertility ~ Agriculture + Examination + Education, data = swiss)    
lm(Fertility ~ Agriculture + Examination + Education + Catholic, data = swiss)     
lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, data = swiss)    
# Multivariable Regression Example 2: regression over factor variables
require(datasets)
data("InsectSprays")
ggplot(data = InsectSprays, aes(y = count, x = spray, fill = spray)) +
  geom_violin(color = "black", size = 2) +
  xlab("Type of spray") + ylab("Insect count")
summary(lm(count ~ spray, data = InsectSprays))
#InsectSprays$spray == 'B'
#1 * (InsectSprays$spray == 'B')
summary(lm(count ~
             I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +   # we *1 to trosform boolean to numeric
             I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
             I(1 * (spray == 'F')), 
           data = InsectSprays))$coef 
# What if we include sprayA?
lm(count ~
             I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
             I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
             I(1 * (spray == 'F')) + I(1 * (spray == 'A')), 
           data = InsectSprays)
# What if we omit intercept?
summary(lm(count ~ spray - 1, data = InsectSprays))$coef
# now all coefficiets - means of corresponding values
# and p-values - test of difference from 0 corresponding coef, wether spray C kills any insect
library(dplyr)
summarise(group_by(InsectSprays, spray), mn = mean(count))
# in original model with intercept, intercept itself is mean of A and all others coefitients - comparisons with it
summary(lm(count ~ spray, data = InsectSprays))$coef
# here p-values - tests wether A is different from B, a is different from C, .. wether spray C kills more insects that A. For A, intercept, p-value test against 0
# How to relevel
levels(InsectSprays$spray)
spray2 <- relevel(InsectSprays$spray, "C")
levels(spray2)
summary(lm(count ~ spray2, data = InsectSprays))$coef

# Multivariable Regression Example 3
head(swiss)
# let's remember definition of covariance and correlation
ggpairs(swiss)
cov <- sum( (swiss$Fertility - mean(swiss$Fertility)) *
    (swiss$Agriculture - mean(swiss$Agriculture)) )/(nrow(swiss) - 1)
cov
cov(swiss$Fertility, swiss$Agriculture)
cor(swiss$Fertility, swiss$Agriculture)
cor <- cov/(sd(swiss$Fertility) * sd(swiss$Agriculture))
cor
# let's create binary variable
library(dplyr)
ggplot(swiss) + geom_histogram(aes(x = Catholic), binwidth = 5, color = "white")
swiss <- mutate(swiss, catholicBin = 1 * (Catholic > 50))
g <- ggplot(swiss) + 
  geom_point(aes(x = Agriculture, y = Fertility, color = as.factor(catholicBin))) +
  xlab("% in Agriculture") 
g
fit <- lm(Fertility ~ Agriculture, data = swiss)
summary(fit)
g + geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2])
fit1 <- lm(Fertility ~ Agriculture + factor(catholicBin), data = swiss) # by using factor() we forth R treat variable as categorical, important in case of catholicBin = c(1,2,3,..) in this case R will treat them like numeric
g + 
  geom_abline(intercept = coef(fit1)[1], slope = coef(fit1)[2], color = 'olivedrab') +
  geom_abline(intercept = coef(fit1)[1] + coef(fit1)[3], slope = coef(fit1)[2], color = 'orchid') 
# first line corresponds to protestants, second - catholics
fit2 <- lm(Fertility ~ Agriculture * factor(catholicBin), data = swiss)
summary(fit1)  
g + 
  geom_abline(intercept = coef(fit2)[1], slope = coef(fit2)[2], color = 'olivedrab') +
  geom_abline(intercept = coef(fit2)[1] + coef(fit2)[3],
              slope = coef(fit2)[2] + coef(fit2)[4], color = 'orchid')

# Adjustment Examples
n <- 100
t <- rep(c(0,1), c(n/2,n/2))
x <- c(runif(n/2), runif(n/2))
beta0 <- 0
beta1 <- 2
tau <- 1
sigma <- 0.2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x,y, type = 'n', frame = F)
abline(lm(y ~ x))
abline(h = mean(y[1:(n/2)]))
abline(h = mean(y[(n/2 + 1) : n]))
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2])
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2])
points(x[1:(n/2)], y[1:(n/2)], pch = 21, col = "black", bg = "lightblue")
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon")

# Residuals and Diagnostics
data(swiss)
par(mfrow = c(2,2))
fit <- lm(Fertility ~ . , data = swiss)
plot(fit)

# an example of strong correlation because of only one outlier
n <- 100
x <- c(10, rnorm(n))  
y <- c(10, rnorm(n)) 
plot(x,y, frame = F, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))
cor(x,y)  
cor(x[2:length(x)], y[2:length(y)])  
# let's diagnose outliers
round(dfbeta( lm(y ~ x) )[1:10,2], 3)
round(hatvalues( lm(y ~ x) )[1:10], 3)
# remove outliers  
round(dfbeta( lm(y[2:length(x)] ~ x[2:length(x)]) )[1:10,2], 3)
round(hatvalues( lm(y[2:length(x)] ~ x[2:length(x)]) )[1:10], 3)  
#let's generate another type of outlier - on regression line
x <- c(5, rnorm(100)) 
y <- 0.8 * x + rnorm(100, sd = 0.1)
par(mfrow = c(1,1))
plot(x,y, frame = F, cex = 2, pch = 21, bg = "salmon")
abline(lm(y ~ x))
# outlier diagnose
round(dfbetas(lm(y ~ x))[1:10,2], 3)
round(hatvalues(lm(y ~ x))[1:10], 3) 
# usually we look at residuals plot to understand how well model fits the data
# Example desccribed by Stefanski
# https://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt
dat <- read.table('https://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt')
head(dat)  
pairs(dat)  
fit <- lm(V1 ~ . -1, data = dat)
summary(fit)$coef
# problem: we could not plot residuals vs any axes like in case 1D, because they are estimates from all variables
# usually residuals of multivariable regression are plottef vs predicted y
plot(predict(fit), resid(fit), pch = '.')
  
# Model selection
# exluding important regressor leads to bias
# including not important regressors leads to variance inflation
n <- 100
nosim <- 1000
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
betas <- sapply(1:nosim, function(i){
  y <- x1 + rnorm(n, sd = 0.3)
  c(coef(lm(y ~ x1))[2],
    coef(lm(y ~ x1 + x2))[2],
    coef(lm(y ~ x1 + x2 + x3))[2])
})
betas_sd <- round(apply(betas, 1, sd), 5)
betas_sd
# here variance of beta1 does not changed because of independency of variables
# let's make variable depentent
n <- 100
nosim <- 1000
x1 <- rnorm(n)
x2 <- x1/sqrt(2) + rnorm(n)/sqrt(2)
x3 <- x1*0.95 + rnorm(n) * sqrt(1-0.95^2)
betas <- sapply(1:nosim, function(i){
  y <- x1 + rnorm(n, sd = 0.3)
  c(coef(lm(y ~ x1))[2],
    coef(lm(y ~ x1 + x2))[2],
    coef(lm(y ~ x1 + x2 + x3))[2])
})
betas_sd <- round(apply(betas, 1, sd), 5)
betas_sd
# general rule: if a variable we include as regressor highly correlate to the thing we are interested in we are going to inflate variate more
#so it's important do not include highly correlated variable 
# this is why randomisation of values is highly important: to exclude sudden correlation
# let's demostrate on swiss data VIF (variance inflation)
install.packages("car")
library(car)
fit <- lm(Fertility ~ . , data = swiss)
vif(fit)
sqrt(vif(fit))
# Agricalture = 2.28. What does that mean? Standart error for Agriculture effect doubles compaired to what it would be orthogonal (independend from all others regressors)
# Infant.Mortsality has the lowest VIF, that means that it's unrelated to any other variable

# let's demonstrate the effect of ommitting variable
simbias <- function(seed=8765){
  # The default seed guarantees a nice histogram. This is the only
  # reason that accepting the default, x1c <- simbias(), is required in the lesson. 
  # The effect will be evident with other seeds as well.
  set.seed(seed) 
  temp <- rnorm(100)
  # Point A
  x1 <- (temp + rnorm(100))/sqrt(2)
  x2 <- (temp + rnorm(100))/sqrt(2)
  x3 <- rnorm(100)
  # Function to simulate regression of y on 2 variables.
  f <- function(k){
    # Point B
    y <- x1 + x2 + x3 + .3*rnorm(100)
    # Point C
    c(lm(y ~ x1 + x2)$coef[2],
      lm(y ~ x1 + x3)$coef[2])
  }
  # Point D
  sapply(1:150, f)
}
x1c <- simbias()
# Plot histograms illustrating bias in estimates of a regressor
# coefficient 1) when an uncorrelated regressor is missing and
# 2) when a correlated regressor is missing.
x1hist <- function(x1c){
  p1 <- hist(x1c[1,], plot=FALSE)
  p2 <- hist(x1c[2,], plot=FALSE)
  yrange <- c(0, max(p1$counts, p2$counts))
  plot(p1, col=rgb(0,0,1,1/4), xlim=range(x1c), ylim=yrange, xlab="Estimated coefficient of x1",
       main="Bias Effect of Omitted Regressor")
  plot(p2, col=rgb(1,0,0,1/4), xlim=range(x1c), ylim=yrange, add=TRUE)
  legend(1.1, 40, c("Uncorrelated regressor, x3, omitted", "Correlated regressor, x2, omitted"),
         fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))
}

# Illustrate the effect of bogus regressors on residual squared error.
bogus <- function(){
  temp <- swiss
  # Add 41 columns of random regressors to a copy of the swiss data.
  for(n in 1:41){temp[,paste0("random",n)] <- rnorm(nrow(temp))}
  # Define a function to compute the deviance of Fertility regressed
  # on all regressors up to column n. The function, deviance(model), computes
  # the residual sum of squares of the model given as its argument.
  f <- function(n){deviance(lm(Fertility ~ ., temp[,1:n]))}
  # Apply f to data from n=6, i.e., the legitimate regressors,
  # through n=47, i.e., a full complement of bogus regressors.
  rss <- sapply(6:47, f)
  # Display result.
  plot(0:41, rss, xlab="Number of bogus regressors.", ylab="Residual squared error.",
       main="Residual Squared Error for Swiss Data\nUsing Irrelevant (Bogus) Regressors",
       pch=21, bg='red')
}

# Nested model testing
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
# Analysis of variance (ANOVA) is a useful way to quantify the significance of additional regressors
anova(fit1, fit3,fit5)

#####################################################################
# Logistic Regression
#####################################################################
download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda",
              destfile = "./data/ravensData.rda", method = "curl")
load("./data/ravensData.rda")
head(ravensData)
# let's fit linear model; it's worst thing to do for binary data but usually this is a first quick thing to do
# RWi = b0 + b1*RSi + ei
# RWi - 1 if Ravens win, 0 if not
# RSi - Number of points Ravens scored
# b0 - probability of a Raven win if they score 0 point
# b1 - increase in probability of a win for each additional point
# ei - residual variation due
lmRavens <- lm(ravensData$ravenWinNum ~ ravensData$ravenScore)
summary(lmRavens)$coef
# the slope tells us that probably Ravens will win
library(ggplot2)
ggplot(ravensData) +
  geom_point(aes(x = ravensData$ravenScore, ravensData$ravenWinNum), color = "blue") +
  geom_abline(intercept = lmRavens$coefficients[2], slope = lmRavens$coefficients[2], color = "salmon")
# better way - to use odds; adds = p/(1-p)
# we are going to put a model on a log of the odds

# let's visualize logistic regression curves
library(manipulate)
x <- seq(-10,10, length = 1000)
manipulate(
  plot(x, exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x)),
       type = "l", lwd = 3, frame = F),
  beta1 = slider(-2,2, step = 0.1, initial = 2),
  beta0 = slider(-2,2, step = 0.1, initial = 0)
)
# let's build a model
logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore, family = "binomial")
summary(logRegRavens)
plot(ravensData$ravenScore, logRegRavens$fitted, pch = 19, col = "blue", xlab = "Score", ylab = "Prob Ravens Win")
exp(logRegRavens$coef)
# explanation: slope 1.11 - 11% increase in probability of win with every score
exp(confint(logRegRavens))
anova(logRegRavens, test = "Chisq")

# Poisson regression
# poisson distribution becomes normal when t*lambda increase
par(mfrow = c(1,3))
plot(0:10, dpois(0:10, lambda = 2), type = "h", frame = F)
plot(0:20, dpois(0:20, lambda = 10), type = "h", frame = F)
plot(0:200, dpois(0:200, lambda = 100), type = "h", frame = F)
# in poisons distribution mean == variance == t*lambda
x <- 0:10000
lambda <- 3
mu <- sum(x*dpois(x, lambda = lambda))
sigma_sq <- sum( (x - mu)^2 * dpois(x, lambda = lambda) )
c(mu, sigma_sq)

# Website traffic per day is perfectly described by Poisson distribution
# if we set t = 1, lambda - web hits per day; t = 24, lambda - hits per an our
library(tibble)
promusculus <- read.table(file = "./data/promusculus-traffic.csv", sep = ',', 
                          header = FALSE, skip = 1, col.names = c("date", "visits"),
                          colClasses = c("Date", "character"))

#promusculus <- tibble(promusculus)
head(promusculus)
names(promusculus)
promusculus$julian <- julian(as.Date(promusculus$date))
promusculus$visits <- as.numeric(promusculus$visits)
head(promusculus)
# fist let's try linear model NHi = b0 + b1*JDi + ei
# NHi - number of hits, b0 - number of hits on Julian day 0
# b1 - increase in number of hits per day, ei - variation due to everything we did't measure
fit <- lm(visits ~ julian, promusculus)
ggplot(promusculus) + 
  geom_point(aes(x = date, y = visits), color = "#ffffff", size = 2) +
  geom_point(aes(x = date, y = visits), color = "#4eb3cc", size = 1) +
  geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2], color = "coral")

# not very good fit (even in case of not promusculus.ru)
# first thind we can try log(NHi) = b0 + b1*JDi + ei
# here b0 - log of ...; b1 - log of ...
# the reason we take log is that exp of coefficients is interpretable with respect to geometric means
fit1 <- lm(I(log(promusculus$visits + 1)) ~ promusculus$julian) # +1 to escape log(0)
round(exp(coef(fit1)), 5)
# interpretetaion of slope 1.00719 - we have 7% increase in traffic per day
ggplot(promusculus) + 
  geom_point(aes(x = date, y = log(visits)), color = "#ffffff", size = 2) +
  geom_point(aes(x = date, y = log(visits)), color = "#4eb3cc", size = 1) +
  geom_abline(intercept = fit1$coefficients[1], slope = fit1$coefficients[2], color = "coral") +
  ylab("log(visits)")
summary(fit)
summary(fit1)
glm1 <- glm(promusculus$visits ~ promusculus$julian, family = 'poisson')
# in logarithmic space
ggplot(promusculus) + 
  geom_point(aes(x = julian, y = log(visits)), color = "#ffffff", size = 2) +
  geom_point(aes(x = julian, y = log(visits)), color = "#4eb3cc", size = 1) +
  geom_line(aes(x = julian, y = predict(glm1)), color = "coral")
# in exponential (normal) space
ggplot(promusculus) + 
  geom_point(aes(x = julian, y = visits), color = "#ffffff", size = 2) +
  geom_point(aes(x = julian, y = visits), color = "#4eb3cc", size = 1) +
  geom_line(aes(x = julian, y = exp(predict(glm1))), color = "gold") +
  geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2], color = "coral") 
summary(fit) 
summary(fit2)
confint(fit)
confint(fit2)

par(mfrow = c(1,1))
plot(promusculus$julian, promusculus$visits, pch=19, col="darkgrey", xlab="Julian day", ylab="Visits")
abline(fit, col="red")
lines(promusculus$julian, fit2$fitted, col="blue")

# very often we are interested not in number of hit per time unit, but it's rate
# to receive rate wejust need to add log(NHi) to linear regression formula
glm2 <- glm(promusculus$visits ~ promusculus$julian, offset = log(promusculus$visits+1),
            family = 'poisson')


# More on regression
# Polynomial regression
n <- 500
x <- seq(0,4*pi,length=n)
y <- sin(x) + rnorm(n,sd=0.3)
knots <- seq(0,8*pi,length=20)
#plot(x, rep(0, length(x)), col='blue', pch = 20)
#points(knots, rep(-0.1, length(knots)), col='red',pch=20)
splineTerms <- sapply(knots, function(knot){ (x>knot)*(x-knot) })
dim(splineTerms)
splineTerms[,1]
xMat <- cbind(1,x,splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
summary(lm(y ~ xMat - 1))
plot(x,y,frame=F,pch=21,bg="lightblue",cex=2)
lines(x,yhat,col="red")
# let's add squared term
splineTerms <- sapply(knots, function(knot){ (x>knot)*(x-knot)^2 })
xMat <- cbind(1,x,x^2,splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x,y,frame=F,pch=21,bg="lightblue",cex=2)
lines(x,yhat,col="red")

# Harmonistic linear model
# Chord finder, playing the white keys on a piano from octave c4-c5
notes4 <- c(261.63, 293.66, 329.63, 349.23, 392.00,440.00,493.88,523.25)
t <- seq(0,2,by=0.001)
n <- length(t)
c4 <- sin(2*pi*notes4[1]*t)
e4 <- sin(2*pi*notes4[3]*t)
g4 <- sin(2*pi*notes4[5]*t)
chord <- c4 + e4 + g4 + rnorm(n,0,0.3)
x <- sapply(notes4, function(freq){ sin(2*pi*freq*t) })
dim(x)
fit <- lm(chord ~ x - 1)
summary(fit)
plot(notes4, fit$coefficients)
ggplot(data = data.frame(x = notes, y = fit$coefficients)) +
  geom_line(aes(x=x,y=y), color = "salmon")
# this is what discrite fourier tranform exactly does: linear model with a lot of sin and cosin terms
a <- fft(chord)
plot(Re(a)^2, type="l")


