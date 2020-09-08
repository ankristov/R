library(dplyr)
library(ggplot2)

##################################################
# Module 1. Practice Quizz 
##################################################
df <- read.csv('data/PASTAPURCHASE_EDITED.csv')
head(df)
glimpse(df)
dim(df)
summary(df)

# 1 Question: What is the correct mean and standard deviation of the quantity of pasta purchased by time unit by household?
mean(df$PASTA)
sd(df$PASTA)

# Question 2: In which area are located (i) the poorest household and (ii) the wealthiest household?
df %>% group_by(AREA) %>% summarise(mean_pasta = mean(PASTA))
unique(df$AREA)
df %>% filter(df$INCOME == min(df$INCOME)) # area with min income household - 2
df%>% filter(df$INCOME == max(df$INCOME)) # area with max income household - 5

# Question 3: What is the maximum pasta quantity a household has bought over the whole time period? (Sum the quantity of pasta by household over time and indicate the maximum)
df %>% group_by(HHID) %>%
  summarise(sum_pasta = sum(PASTA)) %>% arrange(desc(sum_pasta))
df %>% group_by(HHID) %>%
  summarise(sum_pasta = sum(PASTA)) %>% 
  ggplot() + geom_col(aes(x = HHID, y = sum_pasta))
df %>% group_by(AREA) %>%
  summarise(sum_pasta = sum(PASTA)) %>% 
  ggplot() + geom_col(aes(x = AREA, y = sum_pasta, fill = as.factor(AREA)))

# Question 4: What is the average income of households living in area 4?
df %>% filter(AREA == 4) %>% summarise(mean_income = mean(INCOME))

# Question 5: How many households live in area 2, earn more than 20k, and have purchased more than 30 units of pasta over the whole time period?
df %>% filter(AREA == 2) %>% group_by(HHID) %>%
  summarise(sum_pasta = sum(PASTA), sum_income = sum(INCOME)) %>%
  filter(sum_pasta >= 30, sum_income >= 20000) %>% count()
# Question 6: What is the correlation between the purchases of pasta and the exposures?
cor(df$PASTA, df$EXPOS)
ggplot(df) + geom_point(aes(EXPOS, PASTA))

# Question 7: Which of the following graphs reports the correct histogram by household of the total purchase of pasta made by the household over the whole period? (Sum the purchases by household and make a histogram.)

df %>% group_by(HHID) %>% summarise(sum_pasta = sum(PASTA)) %>%
  ggplot() + geom_histogram(aes(x = sum_pasta), bins = 10)

# Question 8: Which of the following graphs reports the correct time series of the overall total purchase of pasta? (Sum the purchases by time units and plot the quantity by time unit.)
df %>% group_by(TIME) %>% summarise(sum_pasta = sum(PASTA)) %>%
  ggplot() + geom_point(aes(x = TIME, y = sum_pasta)) 

##################################################
# Module 2. 
##################################################
##################################################
# Example 2.1 - Supply chain analytics
##################################################
# Clean memory of current R session
ls(all = T)
rm(list = ls(all = T))
# load data
df <- read.table('data/DATA_2.01_SKU.csv', header = T, sep = ',')
head(df)
dim(df)
class(df)
# explore data
str(df) # ADS - Average Dayly Sells, CV - coefficint of variation
summary(df)
# plot
ggplot(data = df, aes(x = data$CV, y = data$ADS)) + 
  geom_point() +
  geom_vline(xintercept = 0.2) +
  geom_hline(yintercept = 4) +
  geom_text(label = 'Horses', x = 0.15, y = 9.7, color = 'red') +
  geom_text(label = 'Wild Bulls', x = 0.65, y = 9, color = 'red') +
  geom_text(label = 'Crickets', x = 0.8, y = 2, color = 'red') +
  labs(title = 'Average daily sells VS Coefficient of Variation', 
       x = 'CV (Coefficient of variation)', y = 'ADS (Average Daily Sells)')

# copy data and scale
df_test <- as.data.frame(scale(df))
summary(df_test)
class(df_test)
head(df_test)
# scale manually
apply(X = df, MARGIN = 2, FUN = function(x) {(x - mean(x)) / sd(x)})

# hierarcial clustering
d <- dist(x = df_test, method = 'euclidian')
fit_hclust <- hclust(d = d, method = 'ward.D')
plot(fit_hclust)
df$groups <-  as.factor(cutree(fit_hclust, 2))
head(df)
str(df)

# plot
ggplot(df) + 
  geom_point(aes(x = CV, y = ADS, color = groups)) 
# plot with lattice
#install.packages("lattice")
#library(lattice)
#xyplot(ADS ~ CV, main = 'After clustering', data = df_test, groups = groups,
#       type = 'p', auto.key = list(title = 'Goups', space = 'right', cex = 1.0, just = 0.95),
#       par.settings = list(superpose.line = list(pch = 0:18, cex = 1)),
#       col = c('blue', 'red', 'green'))

##################################################
# Example 2.2 - Human Resource Analytics
##################################################
rm(list = ls(all = T)) # cleaning workspace

# load data
df <- read.table(file = 'data/DATA_2.02_HR.csv', header = T, sep = ',')
# S - satisfaction
# LPE - Last Project Evaluation
# NP - Number of Projects done
# ANH - Utilization
# TIC - Time In Company

head(df)
str(df)
summary(df)

# normalize data
df_test <- as.data.frame(scale(df))
head(df_test)
summary(df_test)

# hierarcial clustering
d <- dist(df_test)
fit_hclust <- hclust(d = d, method = 'ward.D')
df$groups <- as.factor(cutree(fit_hclust, k = 4))
head(df)

# Summary statistic for groups: calculate proportion of data in each cluster
agg_df <- aggregate(. ~  groups, data = df, FUN = mean) # Splits the data into subsets, computes summary statistics (mean) for each, and returns the result in a convenient form
agg_df
prop_df <- aggregate(. ~ groups, data = df, FUN = length) # number of observations in each group
prop_df
agg_df$proportion <- prop_df$S / sum(prop_df$S) # proportion of observations
agg_df <- agg_df %>% arrange(desc(proportion))
agg_df

View(agg_df)
# You can see that one segment has 100% of employees with newborns, while the other segments have no newborn at all. This variable is a 0 or 1 outcome and what we observe is an artifact of a binary outcome in a clustering. Those types of variable will typically drive the results like dummy variables. On top of that, as discussed in the videos, we could wonder if the Newborn variable is really relevant in this context. So as discussed in the videos, let's remove the Newborn variable.
df_test <- df %>% select(-Newborn)
df_test <- as.data.frame(scale(df_test))
d <- dist(df_test)
fit_hclust <- hclust(d = d, method = 'ward.D')
df$groups <- as.factor(cutree(fit_hclust, k = 4))
agg_df <- aggregate(. ~  groups, data = df, FUN = mean)
prop_df <- aggregate(. ~ groups, data = df, FUN = length)
agg_df$proportion <- prop_df$S / sum(prop_df$S)
agg_df <- agg_df %>% arrange(desc(proportion))
agg_df

# export result data
write.csv(x = agg_df, file = 'data/HR_result_numerical_output.csv', row.names = F)

##################################################
# Example 2.3 - Telecommunication
##################################################
# clean workspace
rm(list = ls(all = T))

# load data
df <- read.table(file = 'data/DATA_2.03_Telco.csv', header = T, sep = ',')
head(df)
str(df)
summary(df)

df_test <- as.data.frame(scale(df))
summary(df_test)

# Hierarcial clustering
d <- dist(df_test, method = 'euclidian')
fit_hclust <- hclust(d = d, method = 'ward.D')

# Start with 8 clusters
df$groups <- as.factor(cutree(fit_hclust, k = 8))
head(df_test)

# Aggregation
agg_df <- aggregate(. ~ groups, data = df, FUN = mean)
prop_df <- aggregate(Calls ~ groups, data = df, FUN = length)
prop_df
agg_df$proportion <- prop_df$Calls / sum(prop_df$Calls)
agg_df <- agg_df %>% arrange(desc(proportion))
agg_df

# for 5 clusters
df$groups <- cutree(fit_hclust, k = 5)
agg_df <- aggregate(. ~ groups, data = df, FUN = mean)
prop_df <- aggregate(Calls ~ groups, data = df, FUN = length)
agg_df$proportion <- prop_df$Calls / sum(prop_df$Calls)
agg_df <- agg_df %>% arrange(desc(proportion))
agg_df

# export data to csv
write.csv(x = df, file = 'data/Telecom_result_output(raw).csv', row.names = F)
write.csv(x = agg_df, file = 'data/Telecom_result_output.csv', row.names = F)

# visualize
palette(rainbow(12, s = 0.6, v = 0.75))
stars(x = select(agg_df, -groups), len = 0.6, key.loc = c(11,6), xlim = c(2,12),
      main = 'Segments', draw.segments = T, nrow = 2, cex = 0.75, labels = agg_df$groups)

####################################################
# Module 3. Understanding causes and consequencies
####################################################
####################################################
# Example 3.1. Credit Scoring
####################################################
library(ggplot2)
library(dplyr)

rm(list = ls(all = T))

df <- read.table(file = 'data/DATA_3.01_CREDIT.csv', header = T, sep = ',')
head(df)
str(df)
summary(df)

ggplot(df) + geom_histogram(aes(x = Rating), bins = 10, color = 'white')
cor_mat <- cor(df %>% select(-c(Gender, Student, Married, Ethnicity)))
corrplot::corrplot(corr = cor_mat, type = 'lower', method = 'shade', title = 'Correlation between numeric variables in Credit Dataset')

# As explained in class, the output of the correlation function provides us with numbers that do not really tell us which variables are most important, or significant in predicting the credit score. We do not know either how they contribute to that score.
# Play video starting at 5 minutes 38 seconds and follow transcript5:38
# Is it positively or negatively? So as you did it in class, we'll use a regression and learn how to create a regression model in r. To create a linear regression model in r, we use the lm() function.

fit <- lm(Rating ~ ., data = df)

ggplot(data = data.frame(fitted = fit$fitted.values, real = df$Rating)) +
  geom_point(aes(fitted, real)) +
  geom_abline(intercept = 0, slope = 1, color = 'red')
# the linear model performs quite well, only near low Rating values not very well

summary(fit)
# First, the star system in r tells us that the statistically significant variables are 
# the income variable, the student variable, and the balance variable. Because the absolute 
# value of the T value of the balance variable is the largest we conclude that it's the 
# strongest driver. Then the income, and then whether or not the applicant is a student. 
# Finally, we want to know whether the impact on the rating is positive or negative. 
# For each. We can find that information in the estimate column and surprisingly, a negative 
# coefficient indicates a negative impact while a positive coefficient indicates a positive impact. 
# So here, the fact that the applicant is a student, impacts negatively on the score. In 
# other words. It lowers the score. On the other hand, the positive coefficient next to the balance 
# indicates that the higher the balance, the higher the credit score.

# if we want to present results to the business we can plot Rating vs Balance and Income
ggplot(df, aes(x = Income, y = Rating)) + 
  geom_point() + geom_smooth() 
ggplot(df, aes(x = Balance, y = Rating)) + 
  geom_point() + geom_smooth(color = 'red', se = T)

####################################################
# Example 3.2. HR analytics
####################################################
ls()
rm(list = ls(all.names = T))

df <- read.csv('data/DATA_3.02_HR2.csv', header = T, sep = ',')
head(df)
# S - satisfaction
# LPE - Last Project Evaluation
# NP - Number of Projects done
# ANH - Utilization
# TIC - Time In Company
str(df)
summary(df)
pairs(df %>% select(-Newborn))

table(df$left)
prop.table(table(df$left))
table(df$left) / nrow(df)
ggplot(df) + geom_histogram(aes(x = as.factor(left)), stat = 'count')

cor_mat <- cor(df)
cor_mat
corrplot::corrplot(cor_mat, method = 'shade', type = 'lower')

# let's estimate importance of variables
fit <- glm(left ~ . , data = df, family = binomial(link = logit))
summary(fit)

pred <- ifelse(fit$fitted.values > 0.5, 1, 0)
table(pred, df$left)
cor(fit$fitted.values, df$left)
ggplot(data = data.frame(fitted = fit$fitted.values), real = df$left) +
  geom_histogram(aes(x = fitted))


