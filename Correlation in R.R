# https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/

# Data
head(mtcars)
# The variables vs and am are categorical variables, so they are removed for this article:
library(tidyverse)
df <- mtcars %>% select(-vs, -am)
head(df,5)
# Correlation coefficient
# Between two variables
cor(df$hp, df$mpg) # pearson correlation
cor(df$hp, df$mpg, method = "spearman") # spearman correlation
?cor
# Correlation matrix: correlations for all variables
round(cor(df), digits = 2)
# Interpretation of a correlation coefficient
# First of all, correlation ranges from -1 to 1. It gives us an indication on two things:
# The direction of the relationship between the 2 variables
# The strength of the relationship between the 2 variables
# Regarding the direction of the relationship: On the one hand, a negative correlation implies that the two variables under consideration vary in opposite directions, that is, if a variable increases the other decreases and vice versa. On the other hand, a positive correlation implies that the two variables under consideration vary in the same direction, i.e., if a variable increases the other one increases and if one decreases the other one decreases as well.
# Regarding the strength of the relationship: The more extreme the correlation coefficient (the closer to -1 or 1), the stronger the relationship. This also means that a correlation close to 0 indicates that the two variables are independent, that is, as one variable increases, there is no tendency in the other variable to either decrease or increase.
# As an illustration, the Pearson correlation between horsepower (hp) and miles per gallon (mpg) found above is -0.78, meaning that the 2 variables vary in opposite direction. This makes sense, cars with more horsepower tend to consume more fuel (and thus have a lower millage par gallon). On the contrary, from the correlation matrix we see that the correlation between miles per gallon (mpg) and the time to drive 1/4 of a mile (qsec) is 0.42, meaning that fast cars (low qsec) tend to have a worse millage per gallon (low mpg). This again make sense as fast cars tend to consume more fuel.
# Note that it is a good practice to visualize the type of the relationship between the two variables before interpreting the correlation coefficients. The reason is that the correlation coefficient could be biased due to an outlier or due to the type of link between the two variables.
# Visualizations
# The correlation matrix presented above is not easily interpretable, especially when the dataset is composed of many variables. In the following sections, we present some alternatives to the correlation matrix for better readability.
# A scatterplot for 2 variables
ggplot(df) + aes(x = hp, y = mpg) + geom_point(color = "#0c4c8a") #+ theme_minimal()
# Scatterplots for several pairs of variables
pairs(df[,c('mpg', 'hp', 'wt', 'gear')])
# The figure indicates that weight (wt) and horsepower (hp) are positively correlated, whereas miles per gallon (mpg) seems to be negatively correlated with horsepower (hp) and weight (wt).
# Correlation matrix
install.packages("corrplot")
library(corrplot)
corrplot(cor(df), method = "number", type = 'upper')
# Correlation test for 2 vars
# Unlike a correlation matrix which indicates the correlation coefficients between some pairs of variables in the sample, a correlation test is used to test whether the correlation (denoted ρ) between 2 variables is significantly different from 0 or not in the population.
# Actually, a correlation coefficient different from 0 in the sample does not mean that the correlation is significantly different from 0 in the population. This needs to be tested with a hypothesis test—and known as the correlation test.
# The null and alternative hypothesis for the correlation test are as follows:
# H0: ρ=0 (meaning that there is no linear relationship between the two variables)
# H1: ρ≠0
# (meaning that there is a linear relationship between the two variables)
# Via this correlation test, what we are actually testing is whether:
# - the sample contains sufficient evidence to reject the null hypothesis and conclude that the correlation coefficient does not equal 0, so the relationship exists in the population.
# - or on the contrary, the sample does not contain enough evidence that the correlation coefficient does not equal 0, so in this case we do not reject the null hypothesis of no relationship between the variables in the population.
# Note that there are 2 assumptions for this test to be valid:
# - Independence of the data
# - For small sample sizes (usually n < 30), the two variables should follow a normal distribution
# Suppose that we want to test whether the rear axle ratio (drat) is correlated with the time to drive a quarter of a mile (qsec):
# Pearson correlation test
pearson_test <- cor.test(df$drat, df$qsec)
pearson_test
# The p-value of the correlation test between these 2 variables is 0.62. At the 5% significance level, we do not reject the null hypothesis of no correlation. We therefore conclude that we do not reject the hypothesis that there is no linear relationship between the 2 variables.1
# This test proves that even if the correlation coefficient is different from 0 (the correlation is 0.09 in the sample), it is actually not significantly different from 0 in the population.
# Note that the p-value of a correlation test is based on the correlation coefficient and the sample size. The larger the sample size and the more extreme the correlation (closer to -1 or 1), the more likely the null hypothesis of no correlation will be rejected. With a small sample size, it is thus possible to obtain a relatively large correlation in the sample (based on the correlation coefficient), but still find a correlation not significantly different from 0 in the population (based on the correlation test). For this reason, it is recommended to always perform a correlation test before interpreting a correlation coefficient to avoid flawed conclusions.
# Correlation test for several pairs of vars
# Similar to the correlation matrix used to compute correlation for several pairs of variables, the rcorr() function (from the {Hmisc} package) allows to compute p-values of the correlation test for several pairs of variables at once. Applied to our dataset, we have:
install.packages("Hmisc")
library(Hmisc)
# correlation tests for whole dataset
cor_vars <- rcorr(as.matrix(df)) # rcorr() accepts matrices only
round(cor_vars$r,2)
round(cor_vars$n,2)
round(cor_vars$P,2)
# Combination of correlation coefficients and correlation tests
# If you need to do this for a few pairs of variables, I recommend using the ggscatterstats() function from the {ggstatsplot} package. Let’s see it in practice with one pair of variables—wt and mpg:
# plot with statictical results
library(ggstatsplot)
ggscatterstats(
  data = df,
  x = wt,
  y = mpg,
  bf.message = F
)
# If you need to do it for many pairs of variables, I recommend using the the correlation function from the easystats {correlation} package.
# This function allows to combine correlation coefficients and correlation tests for several pairs of variables, all in a single table (thanks to krzysiektr for pointing it out to me):
library(correlation) 
correlation::correlation(data = df, include_factors = T, method = "auto")
# Correlograms
# The table above is very useful and informative, but let see if it is possible to combine the concepts of correlation coefficients and correlations test in one single visualization. A visualization that would be easy to read and interpret.
# Ideally, we would like to have a concise overview of correlations between all possible pairs of variables present in a dataset, with a clear distinction for correlations that are significantly different from 0.
# The figure below, known as a correlogram and adapted from the corrplot() function, does precisely this:
install.packages("GGally")
library(GGally)
ggpairs(df[,c('mpg', 'hp', 'wt')])  
library(ggstatsplot)  
ggcorrmat(
  data = df[,c('mpg', 'hp', 'wt')],
  type = 'parametric', # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)
ggcorrmat(
  data = df,
  type = 'parametric', # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)
  


