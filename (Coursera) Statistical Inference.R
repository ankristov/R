install.packages("manipulate")
library(manipulate)
library(tidyverse)

install.packages("RColorBrewer")
library(RColorBrewer)

df <- tibble(x = rnorm(1000, mean = 5, sd = 1))
summary(df)
ggplot(df) + geom_histogram(aes(x = x), binwidth = 0.1)
cols <- brewer.pal(12, "Set3")
myHist <- function(mu) {
  g <- ggplot(df, aes(x = x)) 
  g <- g + geom_histogram(fill = cols[1], 
                          binwidth = 0.1, aes(y = ..density..),
                          color = "black")
  g <- g + geom_density(size = 2, color = cols[2])
  g <- g + geom_vline(xintercept = mu, size = 1, color = cols[4])
  mse <- round(mean((df$x - mu)^2), 3)
  g <- g + labs(title = paste('mu = ', mu, " MSE = ", mse))
  g
}
manipulate(myHist(mu), mu = slider(0,10, step = 0.01))

# the center of avarages distribution is the same as original distribution 
m <- vector()
for (i in 1:1000){
  x <- rnorm(1000, 4,2)
  m[i] <- mean(x)
}
mean(m)

# Tutorial: Big Contributors Algorithms for Data Science - 2017 (p.22)
read_lines("./data/indiv20/itcont.txt", n_max = 3)
contributors <- read_csv("./data/indiv20/itcont.txt", 
                         col_names = c("CMTE_ID","AMNDT_IND","RPT_TP", "TRANSACTION_PGI","IMAGE_NUM", "TRANSACTION_TP","ENTITY_TP","NAME","CITY","STATE","ZIP_CODE","EMPLOYER","OCCUPATION","TRANSACTION_DT","TRANSACTION_AMT","OTHER_ID","TRAN_ID","FILE_NUM","MEMO_CD","MEMO_TEXT","SUB_ID"),
                         col_types = c("cccccccccccccDnccnccn")
                         )
contributors <- read.table("./data/indiv20/itcont.txt", 
                           col.names = c("CMTE_ID","AMNDT_IND","RPT_TP", "TRANSACTION_PGI","IMAGE_NUM", "TRANSACTION_TP","ENTITY_TP","NAME","CITY","STATE","ZIP_CODE","EMPLOYER","OCCUPATION","TRANSACTION_DT","TRANSACTION_AMT","OTHER_ID","TRAN_ID","FILE_NUM","MEMO_CD","MEMO_TEXT","SUB_ID"),
                           colClasses =  c("cccccccccccccDnccnccn", sep = "|")
)
contributors <- read.csv2("./data/indiv20/itcont.txt", sep = "|")
names(contributors) 
names(contributors) <- c("CMTE_ID","AMNDT_IND","RPT_TP", "TRANSACTION_PGI","IMAGE_NUM", "TRANSACTION_TP","ENTITY_TP","NAME","CITY","STATE","ZIP_CODE","EMPLOYER","OCCUPATION","TRANSACTION_DT","TRANSACTION_AMT","OTHER_ID","TRAN_ID","FILE_NUM","MEMO_CD","MEMO_TEXT","SUB_ID")
names(contributors)
summary(contributors$TRANSACTION_AMT)
range(contributors$TRANSACTION_AMT)

# Back to rolling two dice. Which expression represents the probability of rolling an even number or a number greater than 8?
# P(A U B) = P(A) + P(B) - P(A&B)
# P(A) = 6/12 = 18/36 - probability of rolling an even number
# P(B) = (1/6*1/6 + 1/6*1/6 + 1/6*1/6 + 1/6*1/6) + (1/6*1/6 + 1/6*1/6 + 1/6*1/6) + (1/6*1/6 + 1/6*1/6) + (1/6*1/6) = 10/36 - probability of rolling number greater than 8: (for 9: 6-3,3-6,5-4,4-5), (for 10:6-4,4-6,5-5), (for 11: 5-6,6-5), (for 12: 6-6)
# P(A&B) = P(10) + P(12) = (1/6*1/6 + 1/6*1/6 + 1/6*1/6) + (1/6*1/6) = 4/36 - probability of rolling even number greater than 8: (6+2), (6+4), (6+6)
# P(A U B) = 18/36 + 10/36 - 6/36 = (18+10-4)/36

# Mean & Median
# E(X) = sum(X*P(X))
x <- rnorm(1000,5,2)
ggplot(tibble(x = x)) + geom_histogram(aes(x), binwidth = 0.5) +
  geom_vline(xintercept = mean(x), color = "magenta") +
  geom_vline(xintercept = median(x), color = "greenyellow")
# let's introduce outliers
outliersIndexes <- sample(1:length(x),100) 
x[outliersIndexes] <- 15 
x[outliersIndexes]  
x  
outliersIndexes
ggplot(tibble(x = x)) + geom_histogram(aes(x), binwidth = 0.5) +
  geom_vline(xintercept = mean(x), color = "magenta") +
  geom_vline(xintercept = median(x), color = "greenyellow")
# 2
x <- rnorm(10000, 300,50) # general population income 300$ +- 100$
x[x<0] <- abs(x[x<0])
y <- rnorm(1000, 3000,100) # 10% reach people
z <- c(x,y)
length(z)
ggplot(tibble(z = z)) + geom_histogram(aes(z), binwidth = 10) +
  geom_vline(xintercept = mean(z), color = "magenta") +
  geom_vline(xintercept = median(z), color = "greenyellow")

# Variance
# Population variance sigma^2 = sum((X-mu)^2)/N
# Sample variance S^2 = sum((X-mu)^2)/(N-1)

# For coin flip x=1 probability p variance:
# Var(X) = E[X^2] - E[X]^2 = 
#          (1^2 * p + 0^2 * (1-p)) - (1*p + 0*(1-p))^2 =
#          p - p^2 = p*(1-p)
# this function has max when p = 0.5, let's check
p <- seq(0,1, length = 1000)
y <- p * (1-p)
ggplot(tibble(p = p, y = y)) +
  geom_line(aes(p,y))

# 1 std from mean - normal samples, more - outliers, this is practical meaning of std

# Sample variance VS. Population variance
# is defined as the sum of n squared distances from the sample
# mean divided by (n-1), where n is the number of samples or
# observations. We divide by n-1 because this is the number of
# degrees of freedom in the system. The first n-1 samples or
# observations are independent given the mean. The last one
# isn't independent since it can be calculated from the sample
# mean used in the formula.
# In other words, the sample variance is ALMOST the average
# squared deviation from the sample mean.

# We can show that, if the population is infinite, the
# variance of the sample mean is the population variance
# divided by the sample size. Specifically, Var(X') = sigma^2 / n.
sd(apply(matrix(rnorm(10000),1000),1,mean))
# must me equal to s/sqrt(n)
1 / sqrt(10)


install.packages("UsingR")
library(UsingR)
library(dplyr)
data("father.son")
head(father.son)
x <- father.son$sheight
n <- length(x)
ggplot(tibble(sheight = x)) + 
  geom_histogram(aes(sheight, y = ..density..), binwidth = 1, fill = "skyblue", color = "black") +
  geom_density(aes(sheight), size = 2)
round(c(var(x), var(x)/n, sd(x), sd(x)/sqrt(n)),2)

# Binomial distribution
# Suppose a friend has 8 children, 7 of which are girls
# If each gender has an independent 50% probability for each birth, what's the probability of getting 7 or more girls out of 8 births?

choose(8,7) * 0.5^7 * (1-0.5)^1 + choose(8,8) * 0.5^8
pbinom(2,size = 8, prob = 0.5, lower.tail = F)


# Normal distribution
library(RColorBrewer)
x <- rnorm(1000)
y <- dnorm(x)
cols <- brewer.pal(12, "Set3")
ggplot(tibble(x = x, y = y)) +
  geom_point(aes(x,y)) +
  geom_vline(xintercept = mean(x) + sd(x), color = cols[1], size = 2) +
  geom_vline(xintercept = mean(x) - sd(x), color = cols[1], size = 2) +
  geom_vline(xintercept = quantile(x,0.99), color = cols[3], size =1)

pnorm(1.645, mean = 0, sd = 1) # 95% percentile
pnorm(1.645, mean = 0, sd = 1, lower.tail = F)
pnorm(1.645, mean = 0, sd = 10, lower.tail = F)
# Assume that the number of daily clicks for a company is approximately 
# normally distributed with a mean of 1020 and a standard deviation of
# 50. What's the probability of getting more than 1160 click in a day?
# (1160 - 1020)/50 = 2.8
# 1160 = 1020 + 2.8 * 50 so estimation: it's approximately 3 std from the mean and very low
pnorm(1160, mean = 1020, sd = 50, lower.tail = F)
pnorm(2.8, lower.tail = F)
# What number of dayly clicks would represent the one where 75% of days have fewer clicks (assuming days are independent and identically distributed)
qnorm(0.75, mean = 1020, sd = 50)

# Poison distribution
# The number of people that show up at a bus stop is Poisson with a mean of 2.5 per hour.
# If watching the bus stop for 4 hours, what is the probability that 3 or fewer people show up for the whole time?
ppois(3, lambda = 2.5 * 4)
# Poisson can be an approximation of binomial when n is too large and p too small
# We flip a coin with success probability 0.01 500 times
# What's the probability of 2 or fewer successes?
pbinom(2, size = 500, prob = 0.01)
ppois(2, lambda = 500 * 0.01)
# vizualize poisson with different lambda
ggplot() +
  geom_point(data = tibble(x = seq(-20, 100), y = dpois(seq(-20, 100),5)),
            aes(x,y), color = "red") +
  geom_line(data = tibble(x = seq(-20, 100), y = dpois(seq(-20, 100),30)),
          aes(x,y), color = "green") +
  geom_line(data = tibble(x = seq(-20, 100), y = dpois(seq(-20, 100),50)),
          aes(x,y), color = "blue") +
  geom_line(data = tibble(x = seq(-20, 100), y = dpois(seq(-20, 100),70)),
            aes(x,y), color = "yellow")


# Asymptotics
# Law of large numbers
# The Law of Large Numbers (LLN) says that the average (mean)
# approaches what it's estimating when the number of samples goes to infinity
n <- 10000
cs <- cumsum(rnorm(n))
means <- cs / (1:n)
ggplot(tibble(means = means)) +
  geom_line(aes(x = 1:n, y = means)) +
  geom_hline(yintercept = 0, color = 'red')
# for binomial
means <- cumsum(sample(0:1, n, replace = T)) / (1:n)
ggplot(tibble(means = means)) +
  geom_line(aes(x = 1:n, y = means)) +
  geom_hline(yintercept = 0.5, color = 'red')
# Central Limit Theorem
# It states that the distribution of averages of
# iid variables (properly normalized) becomes normal as 
# the sample size increases

# Notice that the CLT said nothing about the original
# population being normally distributed. That's precisely
# where its usefulness lies! We can assume normality of a
# sample mean no matter what kind of population we have, as
# long as our sample size is large enough and our samples are
# independent.

# Vizualize Central Limit Theorem
# Roll 10/30/50 dice for 1000 times to show means distribution
N <- 100 # try 10, 50, 100; the bigger the more close to normal
s <- sample(x = 1:6, size = 1000 * N, replace = T)
m <- matrix(s, nrow = 1000)
me <- apply(m, 1, mean)
ggplot(as.data.frame(me)) + geom_histogram(aes(me, stat(density)), binwidth = 0.08)
ggplot(as.data.frame(me)) + 
  geom_histogram(aes(me, stat(density)), binwidth = 0.08) +
  geom_point(data = data.frame(rv = seq(0, 6, 0.01), 
                               p = dnorm(x = seq(0, 6, 0.01), 
                                         mean = mean(me), 
                                         sd = sd(me))), 
             aes(rv, p))

# Confidence intervals
# t distribution vs normal
library(manipulate)
k <- 1000
xvals <- seq(-5, 5, length = k) 
myplot <- function(df){
  d <- data.frame(y = c(dnorm(xvals), dt(xvals, df)), x = xvals,
                  dist = factor(rep(c("Normal", "T"), c(k,k)))) 
  g <- ggplot(d, aes(x = x, y = y))
  g <- g + geom_line(size = 2, aes(color = dist))
  g 
}
manipulate(myplot(mu), mu = slider(1, 20, step = 1))
# The difference is perhaps easier to see in the tails. Therefore, 
# the following code plots the upper
# quantiles of the Z distribution by those of the t distribution
pvals <- seq(.5, .99, by = .01) 
myplot2 <- function(df){
  d <- data.frame(n= qnorm(pvals),t=qt(pvals, df), p = pvals)
  g <- ggplot(d, aes(x= n, y = t))
  g <- g + geom_abline(size = 2, col = "lightblue") 
  g <- g + geom_line(size = 2, col = "black")
  g <- g + geom_vline(xintercept = qnorm(0.975))
  g <- g + geom_hline(yintercept = qt(0.975, df)) 
  g
}
manipulate(myplot2(df), df = slider(1, 20, step = 1))

# t-interval example on sleep data
library(dplyr)
library(ggplot2)
data(sleep)
head(sleep)
table(sleep$group)
sleep1 <- filter(sleep, group == 1); head(sleep1)
sleep2 <- filter(sleep, group == 2); head(sleep2)
sleep_tidy <- inner_join(sleep1, sleep2, by = "ID"); head(sleep_tidy)
ggplot() +
  geom_segment(data = sleep_tidy, aes(x = group.x, y = extra.x, xend = group.y, yend = extra.y, color = ID)) + 
  geom_point(data = dplyr::select(sleep_tidy, extra.x, group.x, ID), aes(x = group.x, y = extra.x, color = ID), size = 4) +
  geom_point(data = dplyr::select(sleep_tidy, extra.y, group.y, ID), aes(x = group.y, y = extra.y, color = ID), size = 4) 
# T confidence interval
g1 <- sleep$extra[1:10]
g2 <- sleep$extra[11:20]
difference <- g2 - g1
mn <- mean(difference); mn
s <- sd(difference); s
n <- length(g1); n
Tconf <- mn + c(-1,1) + qt(.975, n-1) * s / sqrt(n); Tconf
# This says that with probability .95 the average difference of
# effects (between the two drugs) for an individual patient is
# between .7 and 2.46 additional hours of sleep.
t.test(difference)
t.test(g2,g1, paired = TRUE)
t.test(extra ~ I(relevel(group,2)), paired = T, data = sleep)

# Independent group t confidence intervals
# Suppose that we want to compare the mean blood pressure between two groups in a randomized trial; those who received the treatment to those who received a placebo. The randomization is useful for attempting to balance unobserved covariates that might contaminate our results. Because of the randomization, it would be reasonable to compare the two groups without considering further variables.
# We cannot use the paired t interval that we just used for Galton’s data, 
# because the groups are independent. Person 1 from the treated group 
# has no relationship with person 1 from the control group. Moreover,
# the groups may have different sample sizes, so taking paired differences 
# may not even be possible even if it isn’t advisable in this setting.
# Example 1: mistakenly treating the sleep data as grouped
n1 <- length(g1)
n2 <- length(g2)
sp <- sqrt( ( (n1-1) * sd(g1)^2 + (n2-1) * sd(g2)^2 ) / (n1 + n2 -2)  )
md <- mean(g2) - mean(g1)
semd <- sp * sqrt(1/n1 + 1/n2)
rbind(
  md + c(-1,1) * qt(.975, n1 + n2 -2) * semd,
  t.test(g2, g1, paired = F, var.equal = T)$conf,
  t.test(g2,g1, paired = T)$conf
)
# Notice that the paired interval (the last row) is entirely above zero. 
# The grouped interval (first two rows) contains zero. Thus, acknowledging 
# the pairing explains variation that would otherwise be absorbed into 
# the variation for the group means. As a result, treating the groups as 
# independent results in wider intervals. Even if it didn’t result in a 
# shorter interval, the paired interval would be correct as the groups 
# are not statistically independent!

# Example 2: ChichWeight data
library(datasets)
data("ChickWeight")
head(ChickWeight)
library(reshape2)
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
head(wideCW)
names(wideCW)
names(wideCW)[-(1:2)] <- paste("time", names(wideCW)[-(1:2)], sep = "")
names(wideCW)
View(wideCW)
library(dplyr)
wideCW <- mutate(wideCW, gain = time21 - time0)
head(wideCW)
library(ggplot2)
ggplot(data = ChickWeight, aes(x = Time, y = weight)) + 
  #geom_point(aes(color = Chick), color = "red", alpha = 0.5) +
  geom_line(aes(group = Chick, color = Diet)) +
  geom_smooth(aes(), se = F) +
  facet_wrap(~ Diet, nrow = 1, ncol = 4)
ggplot(wideCW) +
  geom_violin(aes(x = Diet, y = gain, fill = Diet))
ggplot(wideCW) +
  geom_boxplot(aes(x = Diet, y = gain, fill = Diet))
summary(wideCW$gain)
wideCW %>% group_by(Diet) %>% summarize(mean = mean(gain, na.rm = T), sd = sd(gain, na.rm = T))
# let's estimate confidence intervals
wideCW14 <- subset(wideCW, Diet %in% c(1,4))
rbind(
  t.test(gain ~ Diet, paired = F, var.equal = T, data = wideCW14)$conf,
  t.test(gain ~ Diet, paired = F, var.equal = F, data = wideCW14)$conf
)
# Two group testing
# Unequal variance T test comparing diets 1 and 4
wideCW14 <- subset(wideCW, Diet %in% c(1,4))
t.test(gain ~ Diet, paired = F, var.equal = T, data = wideCW14)


# T test
library(UsingR)
data(father.son)
t.test(father.son$sheight - father.son$fheight)
t.test(father.son$sheight, father.son$fheight, paired = T)

# Hypothesis testing
# We want to choose
# the constant C so that the probability that X is greater than
# C given H_0 is 5%. That is, P(X > C| H_0) is 5%. Sound
# familiar?

# The shaded portion represents 5% of the area under the curve and those X values
# in it are those which are at least 1.64 standard deviations
# less than the mean. The probability of this is 5%. This means
# that if our sample mean fell in this area, we would reject a
# true null hypothesis, mu=mu_0, with probability 5%.

# Here again is the plot to show this. The shaded portion
# represents 5% of the area under the curve and those X values
# in it are those which are at least 1.64 standard deviations
# greater than the mean. The probability of this is 5%. This
# means that if our observed mean fell in this area we would
# reject a true null hypothesis, that mu=mu_0, with probability
# 5%.

# Power
mu0 <- 30
mua <- 32
sigma <- 4
n <- 16
z <- qnorm(1 - 0.05)
pnorm(mu0 + z * sigma/sqrt(n), mean = mu0, sd = sigma/sqrt(n), lower.tail = F)
pnorm(mu0 + z * sigma/sqrt(n), mean = mua, sd = sigma/sqrt(n), lower.tail = F)

library(RColorBrewer)
cols <- brewer.pal(8, "Set1")
library(manipulate)
mu0 <- 30
myplot <- function(sigma,mua,n,alpha) {
  g <- ggplot(data.frame(mu = c(27,36)), aes(x = mu))
  g <- g + stat_function(fun = dnorm, geom = "line",
                         args = list(mean = mu0, sd = sigma/sqrt(n)),
                         size = 1, color = cols[1])
  g <- g + stat_function(fun = dnorm, geom = "line",
                         args = list(mean = mua, sd = sigma/sqrt(n)),
                         size = 1, color = cols[2])
  xitc <- mu0 + qnorm(1 - alpha) * sigma / sqrt(n)
  g <- g + geom_vline(xintercept = xitc, size = 1, color = cols[3])
  g
}
manipulate(
  myplot(sigma, mua, n, alpha),
  sigma = slider(1, 10, step = 1, initial = 4),
  mua = slider(30,35, step = 1, initial = 32),
  n = slider(1,50,step = 1, initial = 16),
  alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05)
)

power.t.test(power = 0.8, delta = 2, sd = 4, type = "one.sample", alt = "one.sided")

# Bootstrapping
library(UsingR)
data("father.son")
x <- father.son$sheight
n <- length(x)
B <- 10000
resample <- matrix(sample(x, n*B, replace = T), B, n)
resampledMedian <- apply(resample, 1, median)
ggplot(data = tibble(resampledMedian)) +
  geom_density(aes(resampledMedian), fill = 'red') +
  geom_vline(xintercept = median(resampledMedian), color = 'green')
sd(resampledMedian)
quantile(resampledMedian, c(0.025, 0.975))
sd(x)
quantile(x, c(0.025,0.975))

# Permutation test B v C
subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
y <- subdata$count

group <- as.character(subdata$spray)
testStat <- function(w,g) {mean(w[g == "B"]) - mean(w[g == "C"])}
observedStat <- testStat(y, group)
permutations <- sapply(1:10000, function(i){testStat(y, sample(group))})
observedStat
mean(permutations > observedStat)

# Practical assignment: Exponential distribution
x <- seq(1,100,0.01)
lambda <- 0.2
y <- dexp(x, lambda)
y1 <- rexp(1000, lambda)
cols <- brewer.pal(8, "Set2")
ggplot(data = tibble(x = x, y = y)) +
  geom_point(aes(x,y)) +
  geom_hline(yintercept = mean(y), color = "magenta")
ggplot(data = tibble(y = y1)) +
  geom_histogram(aes(x = y, y = ..density..), binwidth = 0.5, fill = cols[1], color = cols[5]) +
  geom_vline(xintercept = mean(y), color = cols[2]) +
  geom_density(aes(x = y), color = cols[3]) +
  geom_vline(xintercept = median(y), color = cols[4])
# Illustrate via simulation and associated explanatory text the properties of the distribution of the mean of 40 exponentials. 
# 1 Show the sample mean and compare it to the theoretical mean of the distribution.
lambda <- 0.2
mean_theoretical <- 1/lambda
exp40means <- NULL
exp40 <- tibble()
for (i in 1:1000) {exp40means <- c(exp40means, mean(rexp(1000, lambda)))}
for (i in 1:1000) {exp40 <- rbind(exp40, tiblle(rexp(1000, lambda)))}
x_norm <-  seq(4,6,0.01)
y_norm <- dnorm(x_norm, mean = mean(exp40means), sd = sd(exp40means))
q1 <- qnorm(0.05, mean = mean(exp40means), sd = sd(exp40means))
q2 <- qnorm(0.95, mean = mean(exp40means), sd = sd(exp40means))
ggplot(data = tibble(x = exp40means)) +
  geom_histogram(aes(x = x, y = ..density..), color = cols[5], fill = cols[1]) +
  geom_vline(xintercept = mean(exp40means), color = cols[2]) +
  geom_vline(xintercept = mean_theoretical, color = cols[3]) +
  geom_line(data = tibble(x_norm = x_norm, y_norm = y_norm), aes(x_norm, y_norm), color = cols[6]) +
  scale_x_continuous(name = "40 exps means distribution", breaks = c(4, q1,q2,6), labels = c("4.0", "q_0.05", "q_0.95","6.0"))
mean(exp40means)
sd(exp40means)

# Practical assignment: Explore ToothGrowth data
data("ToothGrowth")
head(ToothGrowth)
table(ToothGrowth$supp)
table(ToothGrowth$dose)
range(ToothGrowth$len)
t.test(len ~ supp, data = ToothGrowth, var.equal = T, paired = T)
t.test(len ~ dose, data = filter(ToothGrowth, dose %in% c(0.5,2)), var.equal = T, paired = T)



