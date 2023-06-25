
# 1 Intro to experimental design.mp4
# A basic experiment
# Let's dive into experimental design. Note that all of these concepts will be covered in more detail 
# in the next video, "Hypothesis Testing."
# ToothGrowth is a built-in R dataset from a study that examined the effect of three different doses 
# of Vitamin C on the length of the odontoplasts, the cells responsible for teeth growth in 60 guinea 
# pigs, where tooth length was the measured outcome variable.
# Built-in data can be loaded with the data() function. The dataset will be loaded as a data frame 
# with the same name passed as an argument to data(). For example, you can load the famous iris 
# dataset using data("iris").
# If you wanted to conduct a two-sided t-test with the famous mtcars dataset, it would look like 
# this, where x is the outcome in question, alternative is set to "two.sided", and mu is the value 
# you're testing to see if the mean of mpg is not equal to.
# data(mtcars)
# t.test(x = mtcars$mpg, alternative = "two.sided", mu = 40)
# Suppose you know that the average length of a guinea pigs odontoplasts is 18 micrometers. Conduct a two-sided t-test on the ToothGrowth dataset. Here, a two-sided t-test will check to see if the mean of len is not equal to 18.
# Instructions
# Load the ToothGrowth dataset with the data() function (like shown above.) Run this line and then type ToothGrowth into the console to be sure it loaded.
# Use t.test() to test if the len variable is not equal to 18 micrometers (a two-sided t-test.) There is an example above that you should model your code after.

# Load the ToothGrowth dataset
data("ToothGrowth")
head(ToothGrowth)
# len supp dose
# 1  4.2   VC  0.5
# 2 11.5   VC  0.5
# 3  7.3   VC  0.5
# 4  5.8   VC  0.5
# 5  6.4   VC  0.5
# 6 10.0   VC  0.5
summary(ToothGrowth)
# len        supp         dose      
# Min.   : 4.20   OJ:30   Min.   :0.500  
# 1st Qu.:13.07   VC:30   1st Qu.:0.500  
# Median :19.25           Median :1.000  
# Mean   :18.81           Mean   :1.167  
# 3rd Qu.:25.27           3rd Qu.:2.000  
# Max.   :33.90           Max.   :2.000  
str(ToothGrowth)
# 'data.frame':	60 obs. of  3 variables:
# $ len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
# $ supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
# $ dose: num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
# Perform a two-sided t-test
t.test(x = ToothGrowth$len, alternative = "two.sided", mu = 18)
# One Sample t-test
# data:  ToothGrowth$len
# t = 0.82361, df = 59, p-value = 0.4135
# alternative hypothesis: true mean is not equal to 18
# 95 percent confidence interval:
# 16.83731 20.78936
# sample estimates:
# mean of x 
# 18.81333 
# Excellent job! Given the high p-value, we fail to reject the null hypothesis that the mean of len is equal to 18. That is, we don't have evidence that it is different from 18 micrometers. P-values and hypothesis testing will be covered in more detail in the next video.



# Randomization
# Randomization of subjects in an experiment helps spread any variability that exists naturally between subjects evenly across groups. For ToothGrowth, an example of effective randomization would be randomly assigning male and female guinea pigs into different experimental groups, ideally canceling out any existing differences that naturally exist between male and female guinea pigs.
# In the experiment that yielded the ToothGrowth dataset, guinea pigs were randomized to receive Vitamin C either through orange juice or ascorbic acid, indicated in the dataset by the supp variable. It's natural to wonder if there is a difference in tooth length by supplement type - a question that a t-test can also answer!
# Starting with this exercise, you should use t.test() and other modeling functions with formula notation:
# t.test(outcome ~ explanatory_variable, data = dataset)
# This can be read: "test outcome by explanatory_variable in my dataset." The default test for t.test() is a two-sided t-test.
# You no longer have to explicitly declare dataset$outcome, because the data argument is specified.
# Instructions
# Conduct the proper test to determine if there is a difference in tooth length based on supplement type, and save the results as an object ToothGrowth_ttest.
# Load the broom package.
# Tidy the ToothGrowth_ttest with tidy(). This will print the results to the console.
head(ToothGrowth)
# len supp dose
# 1  4.2   VC  0.5
# 2 11.5   VC  0.5
# 3  7.3   VC  0.5
# 4  5.8   VC  0.5
# 5  6.4   VC  0.5
# 6 10.0   VC  0.5
ToothGrowth %>% group_by(supp) %>% summarize(mean = mean(len), sd = sd(len), n = n())
# A tibble: 2 x 4
#  supp   mean    sd     n
#   <fct> <dbl> <dbl> <int>
# 1 OJ     20.7  6.61    30
# 2 VC     17.0  8.27    30
# Perform a t-test
ToothGrowth_ttest <- t.test(len ~ supp, data = ToothGrowth)
ToothGrowth_ttest
# Welch Two Sample t-test
# data:  len by supp
# t = 1.9153, df = 55.309, p-value = 0.06063
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# -0.1710156  7.5710156
# sample estimates:
# mean in group OJ mean in group VC 
# 20.66333         16.96333 
# Load broom
library(broom)
# Tidy ToothGrowth_ttest
tidy(ToothGrowth_ttest)
# A tibble: 1 x 10
#    estimate estimate1 estimate2 statistic p.value parameter conf.low conf.high
#       <dbl>     <dbl>     <dbl>     <dbl>   <dbl>     <dbl>    <dbl>     <dbl>
#  1     3.70      20.7      17.0      1.92  0.0606      55.3   -0.171      7.57
# … with 2 more variables: method <chr>, alternative <chr>

# Replication
# Recall that replication means you need to conduct an experiment with an adequate number of subjects to achieve an acceptable statistical power. Sample size and power will be discussed in more detail later in this chapter.
# Let's examine the ToothGrowth dataset to make sure they followed the principle of replication. We'll use dplyr to do some exploratory data analysis (EDA). The data has already been loaded for you.
# When using dplyr functions, we can utilize the pipe operator, %>%, to chain functions together. An example using mtcars:
# data(mtcars)
# mtcars %>% count(cyl)
# count() groups mtcars by cyl and then counts how many there are of each number of cylinders.
# Instructions
# Load the dplyr package.
# Use count() to determine how many guinea pigs were given each supplement and dose.
# Count number of observations for each combination of supp and dose
# Load dplyr
library(dplyr)
ToothGrowth %>% 
  count(supp, dose)
# A tibble: 6 x 3
# supp   dose     n
# <fct> <dbl> <int>
# 1 OJ      0.5    10
# 2 OJ      1      10
# 3 OJ      2      10
# 4 VC      0.5    10
# 5 VC      1      10
# 6 VC      2      10
# Great job! The researchers seem to have tested each combination of supp and dose on 10 subjects each, which is low, but was deemed adequate for this experiment.

# Blocking
# Though this is not true, suppose the supplement type is actually a nuisance factor we'd like to control for by blocking, and we're actually only interested in the effect of the dose of Vitamin C on guinea pig tooth growth.
# If we block by supplement type, we create groups that are more similar, in that they'll have the same supplement type, allowing us to examine only the effect of dose on tooth length.
# We'll use the aov() function to examine this. aov() creates a linear regression model by calling lm() and examining results with anova() all in one function call. To use aov(), we'll still need functional notation, as with the randomization exercise, but this time the formula should be len ~ dose + supp to indicate we've blocked by supplement type. (We'll cover aov() and anova() in more detail in the next chapter.)
# ggplot2 is loaded for you.
# Instructions
# Make a boxplot to visually examine if the tooth length is different by dose. dose has been converted to a factor variable for you.
# Use aov() to detect the effect of dose and supp on len. Save as a model object called ToothGrowth_aov.
# Examine ToothGrowth_aov with summary() to determine if dose has a significant effect on tooth length.
# Create a boxplot with geom_boxplot()
ggplot(ToothGrowth, aes(x = dose, y = len)) + 
  geom_boxplot()
# http://joxi.ru/brR3D56SB1ZZDm
ggplot(ToothGrowth, aes(x = dose, y = len)) + 
  geom_boxplot(aes(fill = supp))
# http://joxi.ru/E2pYv1gTv1ELOA
# Create ToothGrowth_aov
ToothGrowth_aov <- aov(len ~ supp + dose, data = ToothGrowth)
# Examine ToothGrowth_aov with summary()
summary(ToothGrowth_aov)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# supp         1  205.4   205.4   14.02 0.000429 ***
# dose         2 2426.4  1213.2   82.81  < 2e-16 ***
# Residuals   56  820.4    14.7                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Congrats! You have just designed your first Randomized Complete Block Design (RCBD) experiment. We'll learn more about this type of experiment in Chapter 3. Given the very small observed p-value for dose, it appears we have evidence to support the hypothesis that mean len is different by dose amount.

#
# One sided vs. Two-sided tests
# Recall in the first exercise that we tested to see if the mean of the guinea pigs' teeth in ToothGrowth was not equal to 18 micrometers. That was an example of a two-sided t-test: we wanted to see if the mean of len is some other number on either side of 18.
# We can also conduct a one-sided t-test, explicitly checking to see if the mean is less than or greater than 18. Whether to use a one- or two-sided test usually follows from your research question. Does an intervention cause longer tooth growth? One-sided, greater than. Does a drug cause the test group to lose more weight? One-sided, less than. Is there a difference in mean test scores between two groups of students? Two-sided test.
# The ToothGrowth data has been loaded for you.
# Instructions
# Test to see if the mean of the length variable of ToothGrowth is less than 18.
# Less than
t.test(x = ToothGrowth$len,
       alternative = "less",
       mu = 18)
# One Sample t-test
# data:  ToothGrowth$len
# t = 0.82361, df = 59, p-value = 0.7933
# alternative hypothesis: true mean is less than 18
# 95 percent confidence interval:
# -Inf 20.46358
# sample estimates:
# mean of x 
# 18.81333 
# Test to see if the mean of the length variable of ToothGrowth is greater than 18.
# Greater than
t.test(x = ToothGrowth$len, alternative = "greater", mu = 18)
# One Sample t-test
# data:  ToothGrowth$len
# t = 0.82361, df = 59, p-value = 0.2067
# alternative hypothesis: true mean is greater than 18
# 95 percent confidence interval:
# 17.16309      Inf
# sample estimates:
# mean of x 
# 18.81333 
# Excellent! It turns out the mean of len is actually very close to 18, so neither of these tests tells us much about the mean of tooth length.

# 2 Hypothesis testing.mp4
library(dplyr)
?power.t.test()
# AK
power.t.test(power = .80, delta = 1)
power.t.test(power = .80, n = 10000)
delta_array <- c()
n_array <- c(10,100,1000,10000,100000)
for (i in 1:length(n_array)){
  delta_array[i] <- power.t.test(power = .8, n = n_array[i])$delta
}
ggplot(data = tibble(n_array = n_array, delta_array = delta_array), aes(x = n_array, y = delta_array)) + 
  geom_point() +
  geom_line()
# Power & Sample Size Calculations
# One key part of designing an experiment is knowing the required sample size you'll need to be able to test your hypothesis.
# The pwr package provides a handy function, pwr.t.test(), which will calculate that for you. However, you do need to know your desired significance level (often 0.05), if the test is one- or two-sided, if the data is from one sample, two samples, or paired, the effect size, and the power. Some of this information will be given to you or can be reasoned from the design.
# A power or sample size calculation is usually different each time you conduct one, and the details of the calculation strongly depend on what kind of experiment you're designing and what your end goals are.
# Instructions
# Load the pwr package.
# Calculate power using an effect size of 0.35, a sample size of 100 in each group, and a significance level of 0.10.
# Calculate power
pwr.t.test(n = 100, 
           d = 0.35,
           sig.level = 0.10,
           type = "two.sample", 
           alternative = "two.sided",
           power = NULL)
# Two-sample t test power calculation 
# n = 100
# d = 0.35
# sig.level = 0.1
# power = 0.7943532
# alternative = two.sided
# NOTE: n is number in *each* group
# Calculate the sample size needed with an effect size of 0.25, a significance level of 0.05, and a power of 0.8.
# Calculate sample size
pwr.t.test(n = NULL, 
           d = 0.25, 
           sig.level = 0.05, 
           type = "one.sample", # determines how to calculate std error?
           alternative = "greater", 
           power = 0.80)
# One-sample t test power calculation 
# n = 100.2877
# d = 0.25
# sig.level = 0.05
# power = 0.8
# alternative = greater
# Because this sample size calculation was for a one-sided test, we only need 101 subjects, not 101 in each group. As you design experiments in the future, the pwr package includes functions for calculating power and sample size for a variety of different tests, including ANOVA (more on that in the next chapter!)

# 3 ANOVA, single and multiple factor experiments.mp4
# Exploratory Data Analysis (EDA) Lending Club
# A sample of 1500 observations from the Lending Club dataset has been loaded for you and is called lendingclub. Let's do some EDA on the data, in hopes that we'll learn what the dataset contains. We'll use functions from dplyr and ggplot2 to explore the data.
# Instructions 
# Use glimpse() from dplyr to look at the variable names, types, and some observations.
# Examine the variables with glimpse()
glimpse(lendingclub)
# Rows: 1,500
# Columns: 12
# $ member_id           <int> 55096114, 1555332, 1009151, 69524202, 72128084, 53…
# $ loan_amnt           <int> 11000, 10000, 13000, 5000, 18000, 14000, 8000, 500…
# $ funded_amnt         <int> 11000, 10000, 13000, 5000, 18000, 14000, 8000, 500…
# $ term                <chr> "36 months", "36 months", "60 months", "36 months"…
# $ int_rate            <dbl> 12.69, 6.62, 10.99, 12.05, 5.32, 16.99, 13.11, 7.8…
# $ emp_length          <chr> "10+ years", "10+ years", "3 years", "10+ years", …
# $ home_ownership      <chr> "RENT", "MORTGAGE", "MORTGAGE", "MORTGAGE", "MORTG…
# $ annual_inc          <dbl> 51000, 40000, 78204, 51000, 96000, 47000, 40000, 3…
# $ verification_status <chr> "Not Verified", "Verified", "Not Verified", "Not V…
# $ loan_status         <chr> "Current", "Fully Paid", "Fully Paid", "Current", …
# $ purpose             <chr> "debt_consolidation", "debt_consolidation", "home_…
# $ grade               <chr> "C", "A", "B", "C", "A", "D", "C", "A", "D", "B", …
# Pipe the dataset into summarize() and find the median loan amount, mean interest rate, and mean annual income. No need to name these outputs anything.
# Find median loan_amnt and mean int_rate, annual_inc with summarize()
lendingclub %>% summarize(median(loan_amnt), mean(int_rate), mean(annual_inc))
#   median(loan_amnt) mean(int_rate) mean(annual_inc)
# 1             13000       13.31472         75736.03
# Use ggplot2 to build a bar chart of counts of responses to the purpose variable. The axes have been flipped for you using coord_flip() so the labels are easier to read.
# Use ggplot2 to build a bar chart of purpose
ggplot(lendingclub, aes(x = purpose)) +
  geom_bar() +
  coord_flip()
# http://joxi.ru/DrlwaoVfKEaOvA
# Run the code to use recode(), creating the purpose_recode variable, which takes purpose and pares 
# it down to a more manageable number of levels; we're creating 4 (debt_related, big_purchase, 
# home_related, life_change.)
# Use recode() to create the new purpose_recode variable
lendingclub$purpose_recode <- lendingclub$purpose %>% recode( 
  "credit_card" = "debt_related", 
  "debt_consolidation" = "debt_related",
  "medical" = "debt_related",
  "car" = "big_purchase", 
  "major_purchase" = "big_purchase", 
  "vacation" = "big_purchase",
  "moving" = "life_change", 
  "small_business" = "life_change", 
  "wedding" = "life_change",
  "house" = "home_related", 
  "home_improvement" = "home_related")
# You can see that the original purpose variable had quite a few levels, which were very detailed. By using recode() here, you created purpose_recode, which has a much more manageable 4 general levels that describe the purpose for people's loans.
ggplot(lendingclub, aes(x = purpose_recode)) + geom_bar() + coord_flip()
# http://joxi.ru/DmBVXLDiqQelVm

# How does loan purpose affect amount funded?
# In the last exercise, we pared the purpose variable down to a more reasonable 4 categories and called it purpose_recode. As a data scientist at Lending Club, we might want to design an experiment where we examine how the loan purpose influences the amount funded, which is the money actually issued to the applicant.
# Remember that for an ANOVA test, the null hypothesis will be that all of the mean funded amounts are equal across the levels of purpose_recode. The alternative hypothesis is that at least one level of purpose_recode has a different mean. We will not be sure which, however, without some post hoc analysis, so it will be helpful to know how ANOVA results get stored as an object in R.
# Instructions
# Use lm() to look at how the purpose_recode variable affects funded_amnt. Save the model as an object called purpose_recode_model.
# Usesummary() to examine purpose_recode_model. These are the results of the linear regression.
# Call anova() on purpose_recode_model. Save as an object called purpose_recode_anova. Print it to the console by typing it.
# Finally, examine the class of purpose_recode_anova.
glimpse(lendingclub)
# Rows: 1,500
# Columns: 13
# $ member_id           <int> 55096114, 1555332, 1009151, 69524202, 72128084, 53…
# $ loan_amnt           <int> 11000, 10000, 13000, 5000, 18000, 14000, 8000, 500…
# $ funded_amnt         <int> 11000, 10000, 13000, 5000, 18000, 14000, 8000, 500…
# $ term                <chr> "36 months", "36 months", "60 months", "36 months"…
# $ int_rate            <dbl> 12.69, 6.62, 10.99, 12.05, 5.32, 16.99, 13.11, 7.8…
# $ emp_length          <chr> "10+ years", "10+ years", "3 years", "10+ years", …
# $ home_ownership      <chr> "RENT", "MORTGAGE", "MORTGAGE", "MORTGAGE", "MORTG…
# $ annual_inc          <dbl> 51000, 40000, 78204, 51000, 96000, 47000, 40000, 3…
# $ verification_status <chr> "Not Verified", "Verified", "Not Verified", "Not V…
# $ loan_status         <chr> "Current", "Fully Paid", "Fully Paid", "Current", …
# $ purpose             <chr> "debt_consolidation", "debt_consolidation", "home_…
# $ grade               <chr> "C", "A", "B", "C", "A", "D", "C", "A", "D", "B", …
# $ purpose_recode      <chr> "debt_related", "debt_related", "home_related", "h…
# Build a linear regression model, purpose_recode_model
purpose_recode_model <- lm(funded_amnt ~ purpose_recode, data = lendingclub)
# Examine results of purpose_recode_model
summary(purpose_recode_model)
# Call:
# lm(formula = funded_amnt ~ purpose_recode, data = lendingclub)
# Residuals:
#    Min     1Q Median     3Q    Max 
# -14472  -6251  -1322   4678  25761 
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      9888.1     1248.9   7.917 4.69e-15 ***
# purpose_recodedebt_related       5433.5     1270.5   4.277 2.02e-05 ***
# purpose_recodehome_related       4845.0     1501.0   3.228  0.00127 ** 
# purpose_recodelife_change        4095.3     2197.2   1.864  0.06254 .  
# purpose_recodeother              -649.3     1598.3  -0.406  0.68461    
# purpose_recoderenewable_energy  -1796.4     4943.3  -0.363  0.71636    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 8284 on 1494 degrees of freedom
# Multiple R-squared:  0.03473,	Adjusted R-squared:  0.0315 
# F-statistic: 10.75 on 5 and 1494 DF,  p-value: 3.598e-10
# Get anova results and save as purpose_recode_anova
purpose_recode_anova <- anova(object = purpose_recode_model)
# Print purpose_recode_anova
purpose_recode_anova
# Analysis of Variance Table
# Response: funded_amnt
# Df     Sum Sq   Mean Sq F value    Pr(>F)    
# purpose_recode    5 3.6888e+09 737756668   10.75 3.598e-10 ***
#   Residuals      1494 1.0253e+11  68629950                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Examine class of purpose_recode_anova
class(purpose_recode_anova)
# [1] "anova"      "data.frame"
# Based on the very small p-value, purpose_recode_anova's results indicate that there is evidence 
# to support the hypothesis that the mean loan amounts are different for at least one combination 
# of purpose_recode's levels. You also saw that the ANOVA results are saved as a data frame, which 
# is nice in case you need to access results later. Loans aren't issued in a vacuum, however, and 
# it's likely that more than just purpose influences the amount funded.

# Which loan purpose mean is different?
# Before we examine other factors besides purpose_recode that might influence the amount of loan funded, let's examine which means of purpose_recode are different. This is the post-hoc test referred to in the last exercise.
# The result of that ANOVA test was statistically significant with a very low p-value. This means we can reject the null hypothesis and accept the alternative hypothesis that at least one mean was different. But which one?
# We should use Tukey's HSD test, which stands for Honest Significant Difference. To conduct Tukey's HSD test in R, you can use TukeyHSD():
# TukeyHSD(aov_model, "outcome_variable_name", conf.level = 0.9)
# This would conduct Tukey's HSD test on some aov_model, looking at a specific "outcome_variable_name", with a conf.level of 90%.
# Instructions
# Build a model using aov() that examines funded_amnt by purpose_recode. Save it as purpose_aov.
# Use TukeyHSD() to conduct the Tukey's HSD test on purpose_aov with a confidence level of 0.95. Save as an object called tukey_output.
# Tidy tukey_output with tidy() from the broom package (which has been loaded for you.)


# From R documentation for TukeyHSD
# "When comparing the means for the levels of a factor in an analysis of variance, a simple comparison using t-tests will inflate the probability of declaring a significant difference when it is not in fact present. This because the intervals are calculated with a given coverage probability for each interval but the interpretation of the coverage is usually with respect to the entire family of intervals.
#  John Tukey introduced intervals based on the range of the sample means rather than the individual differences. The intervals returned by this function are based on this Studentized range statistics.
#  The intervals constructed in this way would only apply exactly to balanced designs where there are the same number of observations made at each level of the factor. This function incorporates an adjustment for sample size that produces sensible intervals for mildly unbalanced designs.
# "
# Use aov() to build purpose_aov
purpose_aov <- aov(funded_amnt ~ purpose_recode, data = lendingclub)
purpose_aov
# Call:
#   aov(formula = funded_amnt ~ purpose_recode, data = lendingclub)
# Terms:
#   purpose_recode    Residuals
# Sum of Squares      3688783338 102533145566
# Deg. of Freedom              5         1494
# Residual standard error: 8284.3
# Estimated effects may be unbalanced
# Conduct Tukey's HSD test to create tukey_output
tukey_output <- TukeyHSD(x = purpose_aov, which = "purpose_recode", conf.level = 0.95)
# Tidy tukey_output to make sense of the results
tidy(tukey_output)
# A tibble: 15 x 6
# term         comparison               estimate conf.low conf.high adj.p.value
# <chr>        <chr>                       <dbl>    <dbl>     <dbl>       <dbl>
# 1 purpose_rec… debt_related-big_purcha…    5434.    2142.     8726.     2.91e-4
# 2 purpose_rec… home_related-big_purcha…    4845.     956.     8734.     1.61e-2
# 3 purpose_rec… life_change-big_purchase    4095.   -1598.     9789.     4.25e-1
# 4 purpose_rec… other-big_purchase          -649.   -4791.     3492.     9.99e-1
# 5 purpose_rec… renewable_energy-big_pu…   -1796.  -14605.    11013.     9.99e-1
# 6 purpose_rec… home_related-debt_relat…    -589.   -2829.     1652.     9.84e-1
# 7 purpose_rec… life_change-debt_related   -1338.   -6061.     3385.     9.78e-1
# 8 purpose_rec… other-debt_related         -6083.   -8737.    -3429.     5.32e-8
# 9 purpose_rec… renewable_energy-debt_r…   -7230.  -19638.     5178.     6.58e-1
# 10 purpose_rec… life_change-home_related    -750.   -5907.     4407.     9.99e-1
# 11 purpose_rec… other-home_related         -5494.   -8861.    -2128.     3.58e-4
# 12 purpose_rec… renewable_energy-home_r…   -6641.  -19221.     5938.     7.46e-1
# 13 purpose_rec… other-life_change          -4745.  -10094.      605.     1.95e-1
# 14 purpose_rec… renewable_energy-life_c…   -5892.  -19141.     7357.     8.59e-1
# 15 purpose_rec… renewable_energy-other     -1147.  -13807.    11513.     1.00e+0
lendingclub %>% group_by(purpose_recode) %>% summarise(n())
# A tibble: 6 x 2
# purpose_recode   `n()`
# <chr>            <int>
# 1 big_purchase        44
# 2 debt_related      1264
# 3 home_related        99
# 4 life_change         21
# 5 other               69
# 6 renewable_energy     3
# categories are unbalanced! For TukeyHSD we need equal number of items in categories!
# Looking at the p-values for each comparison of the levels of purpose_recode, we can see that only 
# a few of the mean differences are statistically significant, for example the differences in the 
# means for the debt_related and big_purchase loan amounts. In this case, these tiny p-values are 
# most likely to be due to large sample size, and further tests would be required to determine 
# what's actually significant in the case of loans (known as the practical significance.)

# Multiple Factor Experiments
# We tested whether the purpose of a loan affects loan amount funded and found that it does. However, 
# we also know that it's unlikely that loans are funded based only on their intended purpose. It's 
# more likely that the company is looking at a holistic picture of an applicant before they decide 
# to issue a loan.
# We can examine more than one explanatory factor in a multiple factor experiment. Like our 
# experiments on ToothGrowth from Chapter 1, an experimenter can try and control two (or more!) 
# different factors and see how they affect the outcome. We're using open data, so we can't quite 
# control the factors here (they're submitted as someone fills out their loan application), but let's look at how a few other factors affect loan amount funded.
# Instructions
# Use aov() to build a linear model and ANOVA in one step, examining how purpose_recode and 
# employment length (emp_length) affect the funded amount. Save as an object purpose_emp_aov and 
# print the result out.
# The printed purpose_emp_aov does not show p-values, which we might be interested in. Display 
# those by calling summary() on the aov object.
# Use aov() to build purpose_emp_aov
purpose_emp_aov <- aov(funded_amnt ~ purpose_recode + emp_length, data = lendingclub)
# Print purpose_emp_aov to the console
purpose_emp_aov
# Call:
# aov(formula = funded_amnt ~ purpose_recode + emp_length, data = lendingclub)
# Terms:
# purpose_recode   emp_length    Residuals
# Sum of Squares      3688783338   2044273211 100488872355
# Deg. of Freedom              5           11         1483
# Residual standard error: 8231.679
# Estimated effects may be unbalanced
# Call summary() to see the p-values
summary(purpose_emp_aov)
# Df    Sum Sq   Mean Sq F value   Pr(>F)    
# purpose_recode    5 3.689e+09 737756668  10.888 2.63e-10 ***
# emp_length       11 2.044e+09 185843019   2.743  0.00161 ** 
# Residuals      1483 1.005e+11  67760534                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Excellent! You could also perform Tukey's HSD test on this model, but given that emp_length has 
# 12 levels, it'll be quite the output. If it was important to the experiment to know the answer, 
# you'd definitely need to look at it.
library(broom)
tukey_output <- TukeyHSD(x = purpose_emp_aov, which = "emp_length", conf.level = 0.95)
# Tidy tukey_output to make sense of the results
tidy(tukey_output) %>% filter(`adj.p.value` < 0.05)
# A tibble: 2 x 6
#   term       comparison        estimate conf.low conf.high adj.p.value
#   <chr>      <chr>                <dbl>    <dbl>     <dbl>       <dbl>
# 1 emp_length 8 years-10+ years   -3401.   -6590.    -213.       0.0248
# 2 emp_length n/a-10+ years       -3283.   -6521.     -44.6      0.0435

# 4 Model validation.mp4
# Pre-modeling EDA
# Let's do some EDA with our experiment in mind. Lending Club has now asked you, their data scientist, to examine what effect their Lending Club-assigned loan grade variable has on the interest rate, int_rate. They're interested to see if the grade they assign the applicant during the process of applying for the loan affects the interest rate ultimately assigned to the applicant during the repayment process.
# The lendingclub data has been loaded for you, as has dplyr and ggplot2.
# Instructions
# Use summary() to look at the int_rate variable, and examine its range and interquartile range.
head(lendingclub)
# member_id loan_amnt funded_amnt      term int_rate emp_length home_ownership
# 1  55096114     11000       11000 36 months    12.69  10+ years           RENT
# 2   1555332     10000       10000 36 months     6.62  10+ years       MORTGAGE
# 3   1009151     13000       13000 60 months    10.99    3 years       MORTGAGE
# 4  69524202      5000        5000 36 months    12.05  10+ years       MORTGAGE
# 5  72128084     18000       18000 36 months     5.32  10+ years       MORTGAGE
# 6  53906707     14000       14000 60 months    16.99    3 years       MORTGAGE
# annual_inc verification_status loan_status            purpose grade
# 1      51000        Not Verified     Current debt_consolidation     C
# 2      40000            Verified  Fully Paid debt_consolidation     A
# 3      78204        Not Verified  Fully Paid   home_improvement     B
# 4      51000        Not Verified     Current   home_improvement     C
# 5      96000        Not Verified     Current        credit_card     A
# 6      47000        Not Verified     Current   home_improvement     D
# Examine the summary of int_rate
summary(lendingclub$int_rate)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.32    9.99   12.99   13.31   16.29   26.77 

# Using dplyr, examine the mean, variance, and median of int_rate by grade.
# Examine int_rate by grade
lendingclub %>% 
  group_by(grade) %>% 
  summarize(mean = mean(int_rate), var = var(int_rate), median = median(int_rate), n = n())
# A tibble: 7 x 5
# grade  mean   var median     n
# <chr> <dbl> <dbl>  <dbl> <int>
# 1 A      7.27 0.961   7.26   254
# 2 B     10.9  2.08   11.0    438
# 3 C     14.0  1.42   14.0    394
# 4 D     17.4  1.62   17.6    244
# 5 E     20.1  2.71   20.0    123
# 6 F     23.6  2.87   23.5     40
# 7 G     26.1  0.198  25.9      7
# # Make a boxplot of int_rate by grade
ggplot(lendingclub, aes(x = grade, y = int_rate)) + geom_boxplot()
# http://joxi.ru/bmoDozVCOvMqk2

# Save a linear model examining this experiment in an object called grade_aov. Print the results by calling summary().
# Use aov() to create grade_aov plus call summary() to print results
grade_aov <- aov(int_rate ~ grade, data = lendingclub)
summary(grade_aov)
# Df Sum Sq Mean Sq F value Pr(>F)    
# grade          6  27013    4502    2637 <2e-16 ***
# Residuals   1493   2549       2                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Excellent job! You can see from the numeric summary and the boxplot that grade seems to heavily 
# influence interest rate. Therefore, the linear model results indicating that int_rate is significantly different by grade are unsurprising.
TukeyHSD(x = grade_aov, conf.level = 0.95)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# Fit: aov(formula = int_rate ~ grade, data = lendingclub)
# $grade
# diff        lwr       upr    p adj
# B-A  3.644545  3.3402900  3.948800 0.00e+00
# C-A  6.707281  6.3968531  7.017709 0.00e+00
# D-A 10.102986  9.7571734 10.448799 0.00e+00
# E-A 12.821898 12.3981189 13.245677 0.00e+00
# F-A 16.315884 15.6596401 16.972128 0.00e+00
# G-A 18.849134 17.3710716 20.327196 0.00e+00
# C-B  3.062736  2.7948719  3.330601 0.00e+00
# D-B  6.458442  6.1502657  6.766617 0.00e+00
# E-B  9.177353  8.7836852  9.571021 0.00e+00
# F-B 12.671339 12.0341251 13.308553 0.00e+00
# G-B 15.204589 13.7348769 16.674301 0.00e+00
# D-C  3.395705  3.0814333  3.709977 0.00e+00
# E-C  6.114617  5.7161586  6.513075 0.00e+00
# F-C  9.608603  8.9684183 10.248787 0.00e+00
# G-C 12.141853 10.6708503 13.612855 0.00e+00
# E-D  2.718912  2.2923087  3.145515 0.00e+00
# F-D  6.212898  5.5548267  6.870968 0.00e+00
# G-D  8.746148  7.2672731 10.225022 0.00e+00
# F-E  3.493986  2.7918039  4.196168 0.00e+00
# G-E  6.027236  4.5282122  7.526259 0.00e+00
# G-F  2.533250  0.9526999  4.113800 4.99e-05



# Post-modeling validation plots + variance
# In the last exercise, we found that int_rate does differ by grade. Now we should validate this model, which for linear regression means examining the Residuals vs. Fitted and Normal Q-Q plots.
# If you call plot() on a model object in R, it will automatically plot both of those plots plus two more. You'll interpret these plots to evaluate model fit. We discussed how to do this in the video.
# Another assumption of ANOVA and linear modeling is homogeneity of variance. Homogeneity means "same", and here that would mean that the variance of int_rate is the same for each level of grade. We can test for homogeneity of variances using bartlett.test(), which takes a formula and a dataset as inputs.
# Instructions
# Run the first line of code with par() so the plots will output in a 2 by 2 grid.
# Call plot() on grade_aov (which has been created for you) to produce the model diagnostic plots.
# Test for homogeneity of variances using bartlett.test().
# For a 2x2 grid of plots:
par(mfrow=c(2, 2))
# Plot grade_aov
plot(grade_aov)
# http://joxi.ru/xAepGD0TMxYQ92
# Bartlett's test for homogeneity of variance
bartlett.test(formula = int_rate ~ grade, data = lendingclub)
# Bartlett test of homogeneity of variances
# data:  int_rate by grade
# Bartlett's K-squared = 78.549, df = 6, p-value = 7.121e-15
# Excellent! The residuals on this model are okay, though the residuals on G have a much smaller 
# range than any other level of grade (the dots are far less spread out.) The Q-Q plot, however, 
# shows that the residuals are fairly normal. However, given the highly significant p-value from 
# Bartlett's test, the assumption of homogeneity of variances is violated, which is one of the 
# assumptions of an ANOVA model. Therefore, ANOVA might not be the best choice for this experiment. It happens!

# Kruskal-Wallis rank sum test
# Given that we found in the last exercise that the homogeneity of variance assumption of linear modeling was violated, we may want to try an alternative.
# One non-parametric alternative to ANOVA is the Kruskal-Wallis rank sum test. For those with some statistics knowledge, it is an extension of the Mann-Whitney U test for when there are more than two groups, like with our grade variable. For us, the null hypothesis for this test would be that all of the int_rates have the same ranking by grade.
# The Kruskal-Wallis rank sum test can be conducted using the kruskal.test() function, available in base R. Luckily for you, the use of this function is very similar to using lm() or aov(): you input a formula and a dataset, and a result is returned.
# Instructions
# Use kruskal.test() to examine whether int_rate varies by grade when a non-parametric model is employed.
# Conduct the Kruskal-Wallis rank sum test
kruskal.test(formula = int_rate ~ grade,
             data = lendingclub)
# Kruskal-Wallis rank sum test
# data:  int_rate by grade
# Kruskal-Wallis chi-squared = 1365.5, df = 6, p-value < 2.2e-16
# Good job! The low p-value indicates that based on this test, we can be confident in our result, which we found across this experiment, that int_rate varies by grade.

# 5 A/B testing.mp4
# Which post-A/B test test?
# We'll be testing the mean loan_amnt, which is the requested amount of money the loan applicants ask for, based on which color header (green or blue) that they saw on the Lending Club website.
# Which statistical test should we use after we've collected enough data?
# Answer the question
# - Kruskal-Wallis Rank Sum test
#   Incorrect
#   This is a non-linear alternative to ANOVA, which is used to test the differences in 3+ groups, so perhaps not.
# - T-test +
# - Chi-Square Test
#   Incorrect
#   Hmm, a chi-square test usually measures differences in counts of two or more categorical variables, so it's probably not the best test to utilize here.
# - Linear Regression
#   Incorrect
#   A linear regression could be used here, but we only have two groups, so it's not necessary.
# Excellent! We'll be looking at the mean difference in the amount of loan the applicants asked for, so a t-test is appropriate.

# Sample size for A/B test
# We know now that we need to analyze our A/B test results with a t-test after we've collected data. We have two pretty important questions we need to answer before we do that: what's the effect size and what's the sample size required for this test?
# In this case, effect size was given to us. Lending Club is looking to detect the relatively small effect size of 0.2. We'll again use the pwr package and calculate sample size using an appropriate function to find out how many we'll need to recruit into each group, A and B.
# Instructions
# Use the correct function from the pwr package to calculate the required sample size for each group with d = 0.2, a power of 0.8, and a 0.05 significance level. Check the pwr help docs with ?pwr if you need help remembering which function to use and what arguments it takes.
# Load the pwr package
library(pwr)
# Use the correct function from pwr to find the sample size
pwr.t.test(n = NULL, 
           d = 0.2, 
           sig.level = 0.05, 
           power = 0.80, 
           type = "two.sample")
# Two-sample t test power calculation 
# n = 393.4057
# d = 0.2
# sig.level = 0.05
# power = 0.8
# alternative = two.sided
# NOTE: n is number in *each* group
# Nice! You can see we need about 400 people per group to reach our desired power in this A/B test.

# Basic A/B test
# Now that we know the sample size required, and we allowed the experiment to run long enough to get at least 400 people in each group, we can analyze our A/B test.
# Remember that when applicants were using the Lending Club website, they were randomly assigned to two groups, A or B, where A was shown a mint green website header and B was shown a light blue website header. Lending Club was interested to see if website header color choice influenced loan_amnt, the amount an applicant asked to borrow.
# http://joxi.ru/D2PajYQcwzp4VA
# A new dataset, lendingclub_ab is available in your workspace. The A/B test was run until there were 500 applicants in each group. Each applicant has been labeled as group A or B. Conduct the proper test to see if the mean of loan_amnt is different between the two groups.
# Instructions
# Create a boxplot of loan_amnt by Group using ggplot2.
# Conduct the two-sided t-test to test the A/B test results.
library(dplyr)
glimpse(lendingclub_ab)
# Rows: 1,000
# Columns: 75
# $ id                          <int> 11976148, 1203719, 54998739, 5801830, 3158…
# $ member_id                   <int> 13968311, 1444848, 58569477, 7233534, 3418…
# $ loan_amnt                   <dbl> 8000, 1200, 15000, 9000, 16000, 15600, 240…
# $ funded_amnt                 <dbl> 8000, 1200, 15000, 9000, 16000, 15600, 240…
# $ funded_amnt_inv             <dbl> 8000.000, 1200.000, 15000.000, 9000.000, 1…
# $ term                        <chr> "36 months", "36 months", "36 months", "60…
# $ int_rate                    <dbl> 9.67, 12.12, 12.69, 12.12, 11.67, 15.10, 1…
# $ installment                 <dbl> 256.90, 39.93, 503.18, 200.75, 528.92, 371…
# $ grade                       <chr> "B", "B", "C", "B", "B", "C", "D", "C", "C…
# $ sub_grade                   <chr> "B1", "B3", "C2", "B3", "B4", "C2", "D1", …
# $ emp_title                   <chr> "Escalation Manager", "new vanderbilt reha…
# $ emp_length                  <chr> "9 years", "4 years", "10+ years", "10+ ye…
# $ home_ownership              <chr> "MORTGAGE", "RENT", "MORTGAGE", "RENT", "M…
# $ annual_inc                  <dbl> 74000, 58000, 109400, 85000, 250000, 43000…
# $ verification_status         <chr> "Verified", "Not Verified", "Not Verified"…
# $ issue_d                     <chr> "Feb-2014", "Apr-2012", "Jul-2015", "Jul-2…
# $ loan_status                 <chr> "Current", "Fully Paid", "Current", "Fully…
# $ pymnt_plan                  <chr> "n", "n", "n", "n", "n", "n", "n", "n", "n…
# $ url                         <chr> "https://www.lendingclub.com/browse/loanDe…
# $ desc                        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ purpose                     <chr> "major_purchase", "credit_card", "debt_con…
# $ title                       <chr> "Major purchase", "Credit Card Loan", "Deb…
# $ zip_code                    <chr> "492xx", "103xx", "935xx", "891xx", "300xx…
# $ addr_state                  <chr> "MI", "NY", "CA", "NV", "GA", "NJ", "IN", …
# $ dti                         <dbl> 7.49, 13.50, 26.18, 7.02, 19.65, 13.71, 22…
# $ delinq_2yrs                 <int> 0, 0, 0, 1, 0, 0, 0, 2, 0, 1, 0, 0, 1, 0, …
# $ earliest_cr_line            <chr> "Aug-2001", "Jul-2003", "May-1996", "Mar-1…
# $ inq_last_6mths              <int> 0, 0, 2, 0, 0, 1, 1, 1, 1, 3, 0, 1, 3, 3, …
# $ mths_since_last_delinq      <int> 24, NA, NA, 10, NA, NA, NA, 13, 57, 17, 67…
# $ mths_since_last_record      <int> NA, NA, 117, NA, NA, NA, NA, NA, NA, NA, N…
# $ open_acc                    <int> 6, 22, 16, 15, 18, 8, 18, 33, 16, 10, 6, 1…
# $ pub_rec                     <int> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16,…
# $ revol_bal                   <int> 3750, 21826, 15250, 28024, 46894, 10292, 8…
# $ revol_util                  <dbl> 61.5, 66.9, 71.0, 19.6, 68.9, 63.9, 63.1, …
# $ total_acc                   <int> 18, 35, 24, 38, 21, 20, 32, 52, 32, 22, 25…
# $ initial_list_status         <chr> "w", "f", "w", "f", "f", "f", "f", "f", "f…
# $ out_prncp                   <dbl> 3388.25, 0.00, 12877.24, 0.00, 10002.45, 0…
# $ out_prncp_inv               <dbl> 3388.25, 0.00, 12877.24, 0.00, 10002.45, 0…
# $ total_pymnt                 <dbl> 5651.800, 1436.912, 3008.500, 10637.502, 7…
# $ total_pymnt_inv             <dbl> 5651.80, 1436.91, 3008.50, 10637.50, 7913.…
# $ total_rec_prncp             <dbl> 4611.75, 1200.00, 2122.76, 9000.00, 5997.5…
# $ total_rec_int               <dbl> 1040.05, 236.91, 885.74, 1637.50, 1915.50,…
# $ total_rec_late_fee          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ recoveries                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ collection_recovery_fee     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ last_pymnt_d                <chr> "Dec-2015", "Apr-2015", "Jan-2016", "Mar-2…
# $ last_pymnt_amnt             <dbl> 256.90, 41.12, 503.18, 6829.83, 528.92, 15…
# $ next_pymnt_d                <chr> "Feb-2016", NA, "Feb-2016", NA, "Feb-2016"…
# $ last_credit_pull_d          <chr> "Jan-2016", "Apr-2015", "Jan-2016", "Jan-2…
# $ collections_12_mths_ex_med  <int> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, …
# $ mths_since_last_major_derog <dbl> NA, NA, NA, NA, NA, NA, NA, 13, 57, 35, 67…
# $ policy_code                 <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
# $ application_type            <chr> "INDIVIDUAL", "INDIVIDUAL", "INDIVIDUAL", …
# $ annual_inc_joint            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ dti_joint                   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ verification_status_joint   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ acc_now_delinq              <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ tot_coll_amt                <dbl> 313, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ tot_cur_bal                 <dbl> 291589, NA, 367506, 28024, 134267, 17546, …
# $ open_acc_6m                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ open_il_6m                  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ open_il_12m                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ open_il_24m                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ mths_since_rcnt_il          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ total_bal_il                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ il_util                     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ open_rv_12m                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ open_rv_24m                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ max_bal_bc                  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ all_util                    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ total_rev_hi_lim            <dbl> 6100, NA, 21400, 143100, 68100, 16100, 139…
# $ inq_fi                      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ total_cu_tl                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ inq_last_12m                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ Group                       <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A…
# AK:!!! Randomization in A/B test - on user level
# Plot the A/B test results
ggplot(lendingclub_ab, aes(x = Group, y = loan_amnt)) + geom_boxplot()
# http://joxi.ru/KAgbKoGT5b4Gn2
# Conduct a two-sided t-test
t.test(formula = loan_amnt ~ Group, data = lendingclub_ab, alternative = "two.sided")
# Welch Two Sample t-test
# data:  loan_amnt by Group
# t = -0.58112, df = 997.06, p-value = 0.5613
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# -1377.1748   747.8748
# sample estimates:
# mean in group A mean in group B 
#        14723.15        15037.80 
# Excellent! By looking at both the boxplot and the results of the t-test, it seems that there is 
# no compelling evidence to support the hypothesis that there is a difference the two A/B test 
# groups' mean loan_amnt, a result which you would use to help make data-driven decisions at 
# Lending Club.

# A/B tests vs. multivariable experiments
# The point of an A/B test is that only one thing is changed and the effect of that change is measured. 
# We saw this with our examples in the video and the last few exercises. On the other hand, a multivariate 
# experiment, such as the ToothGrowth experiment from chapter 1, is where a few things are changed (and is 
# similar to a multiple factor experiment, which we covered earlier in this chapter.)
# A Lending Club multivariate test can combine all of the explanatory variables we've looked at in this 
# chapter. Let's examine how Group, grade, and verification_status affect loan_amnt in the lendingclub_ab dataset.
# Instructions
# Use lm() to examine the effect of all three explanatory variables on loan_amnt. Save as a model object 
# called lendingclub_multi.
# Examine lendingclub_multi with tidy() and draw your conclusions.
# Build lendingclub_multi
lendingclub_multi <-lm(loan_amnt ~ Group + grade + verification_status, data = lendingclub_ab)
# Examine lendingclub_multi results
tidy(lendingclub_multi)
# A tibble: 10 x 5
# term                               estimate std.error statistic  p.value
# <chr>                                 <dbl>     <dbl>     <dbl>    <dbl>
# 1 (Intercept)                          11244.      792.    14.2   8.44e-42
# 2 GroupB                                 205.      515.     0.398 6.91e- 1
# 3 gradeB                                -975.      817.    -1.19  2.33e- 1
# 4 gradeC                                -631.      806.    -0.783 4.34e- 1
# 5 gradeD                                 718.      917.     0.783 4.34e- 1
# 6 gradeE                                1477.     1208.     1.22  2.22e- 1
# 7 gradeF                                5453.     1942.     2.81  5.09e- 3
# 8 gradeG                                3490.     3396.     1.03  3.04e- 1
# 9 verification_statusSource Verified    4528.      637.     7.10  2.30e-12
# 10 verification_statusVerified           5900.      668.     8.84  4.41e-18
# From the results, verification status and having an F grade are the factors in this model that have a 
# significant effect on loan amount. Let's move on to the next chapter and conduct more multivariable 
# experiments like this.

# 6 Intro to NHANES and sampling.mp4
# NHANES dataset construction
# As downloaded from the NHANES website, the NHANES datasets are available only as separate .XPT files, a native format to SAS. Luckily for us, the haven package exists.
# Let's combine the NHANES Demographics, Medical Conditions, and Body Measures datasets, available in their raw .XPT format and accessible through the variables DEMO_file, MCQ_file, and BMX_file. Join all 3 datasets using the SEQN variable. A good way to do this is using Reduce(), which allows you to combine elements in a helpful way.
# The joining code, which is provided for you does the following:
# Creates a list of all 3 datasets (nhanes_demo, nhanes_medical, nhanes_bodymeasures).
# Uses a custom function inside of Reduce() to inner join all 3 datasets with the "SEQN" variable.
# Saves this as the nhanes_combined dataset.
# Instructions
# Load the haven package.
# Import the three data files with separate calls to read_xpt(), where the inputs to these 3 calls to read_xpt() are DEMO_file, MCQ_file, and BMX_file and saved as the datasets as nhanes_demo, nhanes_medical, and nhanes_bodymeasures, respectively.
# Create nhanes_combined by merging the 3 datasets you just imported, using the provided code.
# Load haven
library(haven)
# Import the three datasets using read_xpt()
nhanes_demo <- read_xpt(DEMO_file)
nhanes_medical <- read_xpt(MCQ_file)
nhanes_bodymeasures <- read_xpt(BMX_file)
names(nhanes_demo)
# [1] "SEQN"     "SDDSRVYR" "RIDSTATR" "RIAGENDR" "RIDAGEYR" "RIDAGEMN"
# [7] "RIDRETH1" "RIDRETH3" "RIDEXMON" "RIDEXAGM" "DMQMILIZ" "DMQADFC" 
# [13] "DMDBORN4" "DMDCITZN" "DMDYRSUS" "DMDEDUC3" "DMDEDUC2" "DMDMARTL"
# [19] "RIDEXPRG" "SIALANG"  "SIAPROXY" "SIAINTRP" "FIALANG"  "FIAPROXY"
# [25] "FIAINTRP" "MIALANG"  "MIAPROXY" "MIAINTRP" "AIALANGA" "DMDHHSIZ"
# [31] "DMDFMSIZ" "DMDHHSZA" "DMDHHSZB" "DMDHHSZE" "DMDHRGND" "DMDHRAGE"
# [37] "DMDHRBR4" "DMDHREDU" "DMDHRMAR" "DMDHSEDU" "WTINT2YR" "WTMEC2YR"
# [43] "SDMVPSU"  "SDMVSTRA" "INDHHIN2" "INDFMIN2" "INDFMPIR"
dim(nhanes_demo)
# [1] 9971   47
names(nhanes_medical)
# [1] "SEQN"     "MCQ010"   "MCQ025"   "MCQ035"   "MCQ040"   "MCQ050"  
# [7] "AGQ030"   "MCQ053"   "MCQ080"   "MCQ092"   "MCD093"   "MCQ149"  
# [13] "MCQ151"   "MCQ160A"  "MCQ180A"  "MCQ195"   "MCQ160N"  "MCQ180N" 
# [19] "MCQ160B"  "MCQ180B"  "MCQ160C"  "MCQ180C"  "MCQ160D"  "MCQ180D" 
# [25] "MCQ160E"  "MCQ180E"  "MCQ160F"  "MCQ180F"  "MCQ160G"  "MCQ180G" 
# [31] "MCQ160M"  "MCQ170M"  "MCQ180M"  "MCQ160K"  "MCQ170K"  "MCQ180K" 
# [37] "MCQ160L"  "MCQ170L"  "MCQ180L"  "MCQ160O"  "MCQ203"   "MCQ206"  
# [43] "MCQ220"   "MCQ230A"  "MCQ230B"  "MCQ230C"  "MCQ230D"  "MCQ240A" 
# [49] "MCQ240AA" "MCQ240B"  "MCQ240BB" "MCQ240C"  "MCQ240CC" "MCQ240D" 
# [55] "MCQ240DD" "MCQ240DK" "MCQ240E"  "MCQ240F"  "MCQ240G"  "MCQ240H" 
# [61] "MCQ240I"  "MCQ240J"  "MCQ240K"  "MCQ240L"  "MCQ240M"  "MCQ240N" 
# [67] "MCQ240O"  "MCQ240P"  "MCQ240Q"  "MCQ240R"  "MCQ240S"  "MCQ240T" 
# [73] "MCQ240U"  "MCQ240V"  "MCQ240W"  "MCQ240X"  "MCQ240Y"  "MCQ240Z" 
# [79] "MCQ300A"  "MCQ300B"  "MCQ300C"  "MCQ365A"  "MCQ365B"  "MCQ365C" 
# [85] "MCQ365D"  "MCQ370A"  "MCQ370B"  "MCQ370C"  "MCQ370D"  "OSQ230"  
dim(nhanes_medical)
# [1] 9575   90
names(nhanes_bodymeasures)
# [1] "SEQN"     "BMDSTATS" "BMXWT"    "BMIWT"    "BMXRECUM" "BMIRECUM"
# [7] "BMXHEAD"  "BMIHEAD"  "BMXHT"    "BMIHT"    "BMXBMI"   "BMDBMIC" 
# [13] "BMXLEG"   "BMILEG"   "BMXARML"  "BMIARML"  "BMXARMC"  "BMIARMC" 
# [19] "BMXWAIST" "BMIWAIST" "BMXSAD1"  "BMXSAD2"  "BMXSAD3"  "BMXSAD4" 
# [25] "BMDAVSAD" "BMDSADCM"
dim(nhanes_bodymeasures)
# [1] 9544   26
# Merge the 3 datasets you just created to create nhanes_combined
nhanes_combined <- list(nhanes_demo, nhanes_medical, nhanes_bodymeasures) %>%
  Reduce(function(df1, df2) inner_join(df1, df2, by = "SEQN"), .)
dim(nhanes_combined)
# [1] 9165  161
# AK: the same:
nhanes_combined_1 <- nhanes_demo %>% inner_join(nhanes_medical, by = "SEQN") %>% inner_join(nhanes_bodymeasures, by = "SEQN")
dim(nhanes_combined_1)
# [1] 9165  161
# Awesome! Now that we have the NHANES data assembled, let's get to work on EDA & cleaning.

# NHANES EDA
# Let's examine our newly constructed dataset with a mind toward EDA. As in the last chapter, it's a good idea to look at both numerical summary measures and visualizations. These help with understanding data and are a good way to find data cleaning steps you may have missed. The nhanes_combined dataset has been pre-loaded for you.
# Say we have access to NHANES patients and want to conduct a study on the effect of being told by a physician to reduce calories/fat in their diet on weight. This is our treatment; we're pretending that instead of this being a question asked of the patient, we randomly had physicians counsel some patients on their nutrition. However, we suspect that there may be a difference in weight based on the gender of the patient - a blocking factor!
# Instructions
# Fill in and run the dplyr code to find mean weight (bmxwt) in kg by our treatment (mcq365d). Is there anything interesting about the NA treated patients?
# Fill in the ggplot2 code to look at a boxplot of the IQR of patients' weights by the treatment variable.
# Fill in the dplyr code
nhanes_combined %>% 
  group_by(mcq365d) %>% 
  summarize(mean = mean(bmxwt, na.rm = TRUE))
# A tibble: 4 x 2
# mcq365d  mean
#     <int> <dbl>
# 1       1  90.7
# 2       2  76.5
# 3       9  90.8
# 4      NA  33.5
# Fill in the ggplot2 code
nhanes_combined %>% 
  ggplot(aes(as.factor(mcq365d), bmxwt)) +
  geom_boxplot() +
  labs(x = "Treatment",
       y = "Weight")
# http://joxi.ru/823NOxpTzj4JYm
# Great! Now that we have an idea of some of the problems with the data, namely that children weren't given 
# the treatment - that's why we see an NA age category. We also have some patients have weights missing, 
# thus the warning that the boxplot throws. It's time for some data cleaning.

# NHANES Data Cleaning
# During data cleaning, we discovered that no one under the age of 16 was given the treatment. Recall that we're pretending that the variable that indicates if a doctor has ever advised them to reduce fat or calories in their diet is purposeful nutrition counseling, our treatment. Let's only keep patients who are greater than 16 years old in the dataset.
# You also may have noticed that the default settings in ggplot2 delete any observations with a missing dependent variable, in this case, body weight. One option for dealing with the missing weights, imputation, can be implemented using the simputation package. Imputation is a technique for dealing with missing values where you replace them either with a summary statistic, like mean or median, or use a model to predict a value to use.
# We'll use impute_median(), which takes a dataset and the variable to impute or formula to impute by as arguments. For example, impute_median(ToothGrowth, len ~ dose) would fill in any missing values in the variable len with the median value for len by dose. So, if a guinea pig who received a dose of 2.0 had a missing value for the len variable, it would be filled in with the median len for those guinea pigs with a dose of 2.0.
# Instructions
# Create nhanes_filter by using filter() to keep anyone older than 16 in the dataset, not including those who are 16. Age is stored in the ridageyr variable.
# Load simputation. Use impute_median() to fill in the missing observations of bmxwt in nhanes_filter, grouping by riagendr.
# Recode the nhanes_final$mcq365d variable by setting any observations with a value of 9 to 2 instead. Verify the recoding worked with count().
# AK: ages where mcq365d = NA
nhanes_combined %>% filter(is.na(mcq365d)) %>% select(ridageyr,mcq365d) %>% summary(ridageyr)
# ridageyr        mcq365d    
# Min.   : 1.00   Min.   : NA   
# 1st Qu.: 4.00   1st Qu.: NA   
# Median : 7.00   Median : NA   
# Mean   : 7.48   Mean   :NaN   
# 3rd Qu.:11.00   3rd Qu.: NA   
# Max.   :15.00   Max.   : NA   
# NA's   :3102  
# AK: n participants by age where mcq365d = NA
nhanes_combined %>% filter(is.na(mcq365d)) %>% select(ridageyr,mcq365d) %>% group_by(as.factor(ridageyr)) %>% summarise(n())
# A tibble: 15 x 2
#  `as.factor(ridageyr)` `n()`
#  <fct>                 <int>
# 1 1                       278
# 2 2                       279
# 3 3                       183
# 4 4                       212
# 5 5                       192
# 6 6                       206
# 7 7                       226
# 8 8                       226
# 9 9                       210
#10 10                      200
#11 11                      209
#12 12                      165
#13 13                      172
#14 14                      180
#15 15                      164
# participants <= 16 ages who received treatment (not all of them didn't receive! but we are going to filter all)
# nhanes_combined %>% select(ridageyr,mcq365d) %>% group_by(as.factor(ridageyr), mcq365d) %>% count() %>% filter(!is.na(`mcq365d`))
nhanes_combined %>% filter(ridageyr <= 16) %>% select(ridageyr,mcq365d) %>% group_by(as.factor(ridageyr), mcq365d) %>% count() %>% filter(!is.na(`mcq365d`))
# A tibble: 2 x 3
# Groups:   as.factor(ridageyr), mcq365d [2]
#   `as.factor(ridageyr)` mcq365d     n
#   <fct>                   <int> <int>
# 1 16                          1    31
# 2 16                          2   145
# AK: missing values in dataset
library(tidyr)
library(ggplot2)
nhanes_combined %>% 
  summarize_all(funs(sum(is.na(.))/n())) %>% 
  pivot_longer(cols = everything(), names_to = "features", values_to = "missing_pct") %>% 
  arrange(desc(missing_pct)) %>% 
  ggplot(aes(x = reorder(features, missing_pct), y = missing_pct)) + 
  geom_bar(stat = 'identity') + coord_flip()
# A tibble: 161 x 2
# features missing_pct
# <chr>          <dbl>
# 1 mcq230d         1   
# 2 mcq240d         1   
# 3 mcq240i         1   
# 4 mcq240k         1   
# 5 mcq240r         1   
# 6 mcq240y         1   
# 7 bmxhead         1   
# 8 bmihead         1   
# 9 mcq240c         1.00
# 10 mcq240dk        1.00
# … with 151 more rows
# http://joxi.ru/4AkV3oeijEK6X2
# glimpse(nhanes_combined)
# Filter to keep only those 16+
nhanes_filter <- nhanes_combined %>% filter(ridageyr > 16)
# Load simputation & impute bmxwt by riagendr
library(simputation)
nhanes_final <- impute_median(nhanes_filter, bmxwt ~ riagendr)
# Recode mcq365d with recode() & examine with count()
nhanes_final$mcq365d <- recode(nhanes_final$mcq365d, 
                               `1` = 1,
                               `2` = 2,
                               `9` = 2) # why combined?
nhanes_final %>% count(mcq365d)
# A tibble: 2 x 2
#   mcq365d     n
#     <dbl> <int>
# 1       1  1802
# 2       2  4085
# Excellent! Imputation is a powerful tool for dealing with missing data, but should be used with caution, 
# as you can introduce bias into your data if you're not careful how you impute. Now that we have the dataset 
# cleaned, we're ready to learn about RCBDs so we can analyze our experiment.

# Resampling NHANES data
# The NHANES data is collected on sampled units (people) specifically selected to represent the U.S. population. However, let's resample the nhanes_final dataset in different ways so we get a feel for the different sampling methods.
# We can conduct a simple random sample using sample_n() from dplyr. It takes as input a dataset and an integer of number of rows to sample.
# Stratified sampling can be done by combining group_by() and sample_n(). The function will sample n from each of the groups specified in the group_by().
# The sampling package's cluster() creates cluster samples. It takes in a dataset name, the variable in the set to be used as the cluster variable, passed as a vector with the name as a string (e.g. c("variable")), a number of clusters to select, and a method.
# Instructions
# Use sample_n() to select 2500 observations from nhanes_final and save as nhanes_srs.
# Create nhanes_stratified by using group_by() and sample_n(). Stratify by riagendr and select 2000 of each gender. Confirm that it worked by using count() to examine nhanes_stratified's gender variable.
# Load the sampling package. Use cluster() to divide nhanes_final by "indhhin2" into 6 clusters using the "srswor" method. Assign to nhanes_cluster.
?sample_n
# Use sample_n() to create nhanes_srs
nhanes_srs <- nhanes_final %>% sample_n(size = 2500)
dim(nhanes_srs)
# [1] 2500  161
# Create nhanes_stratified with group_by() and sample_n()
nhanes_stratified <- nhanes_final %>% group_by(riagendr) %>% sample_n(size = 2000)
nhanes_stratified %>% 
  count(riagendr)
# A tibble: 2 x 2
# Groups:   riagendr [2]
#   riagendr     n
#      <int> <int>
# 1        1  2000
# 2        2  2000
# Load sampling package and create nhanes_cluster with cluster()
library(sampling)
nhanes_cluster <- cluster(data = nhanes_final, clustername = c('indhhin2'), size = 6, method = "srswor")
head(nhanes_cluster)
# indhhin2 ID_unit  Prob
# 1        2    3983 0.375
# 2        2    2053 0.375
# 3        2    1553 0.375
# 4        2     353 0.375
# 5        2    1595 0.375
# 6        2    4120 0.375
unique(nhanes_final$indhhin2)
# [1] 10  4  5  7 14  6 15  3  1  8  2 77 NA 12  9 99 13
dim(nhanes_cluster)
# [1] 1765    3
dim(nhanes_final)
# [1] 5887  161
nhanes_cluster %>% count(indhhin2)
# A tibble: 6 x 2
#   indhhin2     n
#      <int> <int>
# 1        2   217
# 2        6   624
# 3        9   353
# 4       10   291
# 5       12   190
# 6       13    90
nhanes_final %>% count(indhhin2)
# A tibble: 17 x 2
#   indhhin2     n
#      <int> <int>
# 1        1   135
# 2        2   217
# 3        3   349
# 4        4   367
# 5        5   356
# 6        6   624
# 7        7   556
# 8        8   470
# 9        9   353
#10       10   291
#11       12   190
#12       13    90
#13       14   542
#14       15   939
#15       77   119
#16       99    90
#17       NA   199
# Excellent! These are some basic sampling methods you can use on your data to create the different kinds of samples that may be necessary in an experiment.

#
# Which is NOT a good blocking factor?
# As discussed in the video, the purpose of blocking an experiment is to make the experimental groups more like one another. Groups are blocked by a variable that is known to introduce variability that will affect the outcome of the experiment but is not of interest to study in the experiment itself.
# A rule of thumb in experimental design is often "block what you can, randomize what you cannot", which means you should aim to block the effects you can control for (e.g. sex) and randomize on those you cannot (e.g. smoking status). Variability inside a block is expected to be fairly small, but variability between blocks will be larger.
# Which of the following would NOT make a good blocking factor for an experiment?
# Answer the question
# Possible Answers
# - When studying the effect of radiation treatment on cancer recurrence, treatment hospital type is used a blocking factor.
#   Incorrect
#   Treatment hospital type is definitely a possible blocking factor here. There are very big differences in treatment at different types of hospitals. For example, an Academic Research hospital is usually better funded than a small Community hospital. You may get better care at one or the other, depending.
# - When studying the effect of four different tips' hardness readings on a metal hardness tester, the known hardness of the metal is used as a block variable.
#   Incorrect
#   This is actually a classic example, used in most experimental design texts and courses! The known hardness of the metal is a great blocking factor.
# - When testing the effect of a type of fertilizer on plant growth, crop type is used as a blocking factor.
#   Incorrect
#   Which crop you plant where is very controllable by the experimenter, and likely they will have pre-knowledge (or access to it) about how that plant grows, so this is a good blocking factor.
# - When testing the effect of a drug on blood pressure, current pregnancy is used as a blocking factor. +
#   Excellent! Current pregnancy is not really something the experimenter can control, and they're also not likely to have enough subjects pregnant to justify blocking by that factor (unless they sampled for pregnant individuals in the first place!)
# Drawing RCBDs with Agricolae
# The agricolae package is very helpful when you want to "draw" out the design of an experiment for yourself using R. It can draw many different kinds of experiments, including a randomized complete block design. Here's an example of one:
#      [,1] [,2] [,3] [,4]
# [1,] "D"  "C"  "A"  "B" 
# [2,] "B"  "A"  "D"  "C" 
# [3,] "D"  "A"  "B"  "C" 
# [4,] "A"  "B"  "D"  "C"
# In this RCBD, we have 4 blocks (each row of the output). Inside of each block, each treatment "A", "B", "C", and "D" is used, because this is a complete design. So if these 4 blocks/rows of the output were four fields of a farmer's, they should give the first field the "D" treatment in the first season, then "C", then "A", then "B".
# Let's draw an RCBD design with 5 treatments and 4 blocks, which go in the r argument. The agricolae package has been loaded for you.
# Instructions
# Create the object called designs using the given code and print it to see all possible designs that agricolae can draw.
# Create designs using ls()
designs <- ls("package:agricolae", pattern = "design")
designs
# [1] "design.ab"      "design.alpha"   "design.bib"     "design.crd"    
# [5] "design.cyclic"  "design.dau"     "design.graeco"  "design.lattice"
# [9] "design.lsd"     "design.rcbd"    "design.split"   "design.strip"  
#[13] "design.youden" 
# Use str() to view design.rcbd's criteria
str(design.rcbd)
# function (trt, r, serie = 2, seed = 0, kinds = "Super-Duper", first = TRUE, 
#          continue = FALSE, randomization = TRUE)  
# Build the treats and rep objects. treats should be a vector containing the letters A through E, created using LETTERS[1:5]. blocks should be equal to 4.
# Build treats and rep
treats <- LETTERS[1:5]
treats
# [1] "A" "B" "C" "D" "E"
blocks <- 4
# Create the my_design_rcbd object. The seed has been set for you, for reproducibility. View the sketch part of the object.
# Build my_design_rcbd and view the sketch
my_design_rcbd <- design.rcbd(trt = treats, r = blocks, seed = 42)
my_design_rcbd$sketch
#     [,1] [,2] [,3] [,4] [,5]
# [1,] "D"  "A"  "C"  "B"  "E" 
# [2,] "E"  "A"  "C"  "D"  "B" 
# [3,] "D"  "B"  "E"  "A"  "C" 
# [4,] "B"  "D"  "E"  "C"  "A" 
# Nice! Now that you have a better idea of what a RCBD looks like, let's try a few examples, including one with the NHANES data we cleaned.

# NHANES RCBD
# Recall that our blocked experiment involved a treatment wherein the doctor asks the patient to reduce their fat or calories in their diet, and we're testing the effect this has on weight (bmxwt). We plan to block by gender, which in the NHANES dataset is stored as riagendr. Recall that blocking is done to create experimental groups that are as similar as possible. Blocking this experiment by gender means that if we observe an effect of the treatment on bmxwt, it's more likely that the effect was actually due to the treatment versus the individual's gender.
# In your R code, you denote a blocked experiment by using a formula that looks like: outcome ~ treatment + blocking_factor in the appropriate modeling function.
# nhanes_final is available.
# Instructions
# Use aov() to create nhanes_rcbd. Recall that the treatment is stored in mcq365d and you're testing the outcome bmxwt, with the blocking factor riagendr.
# Examine the results of nhanes_rcbd with summary().
# Use dplyr functions to examine the mean weights by mcq365d and riagendr.
# Use aov() to create nhanes_rcbd
nhanes_rcbd <- aov(formula = bmxwt ~ mcq365d + riagendr, data = nhanes_final)
nhanes_rcbd
# Call:
# aov(formula = bmxwt ~ mcq365d + riagendr, data = nhanes_final)
# Terms:
# mcq365d  riagendr Residuals
# Sum of Squares   228651.1  159705.7 2365187.4
# Deg. of Freedom         1         1      5884
# Residual standard error: 20.04917
# Estimated effects may be unbalanced
# Check results of nhanes_rcbd with summary()
summary(nhanes_rcbd)
# Df  Sum Sq Mean Sq F value Pr(>F)    
# mcq365d        1  228651  228651   568.8 <2e-16 ***
# riagendr       1  159706  159706   397.3 <2e-16 ***
# Residuals   5884 2365187     402                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
library(broom)
tidy(nhanes_rcbd)
# A tibble: 3 x 6
# term         df    sumsq  meansq statistic    p.value
# <chr>     <dbl>    <dbl>   <dbl>     <dbl>      <dbl>
# 1 mcq365d       1  228651. 228651.      569.  4.33e-120
# 2 riagendr      1  159706. 159706.      397.  1.35e- 85
# 3 Residuals  5884 2365187.    402.       NA  NA        
# Print mean weights by mcq365d and riagendr
nhanes_final %>% 
  group_by(mcq365d, riagendr) %>% 
  summarize(mean_wt = mean(bmxwt))
# A tibble: 4 x 3
# Groups:   mcq365d [2]
#   mcq365d riagendr mean_wt
#     <dbl> <fct>      <dbl>
# 1       1 1           95.1
# 2       1 2           86.7
# 3       2 1           82.6
# 4       2 2           71.3
# AK: more details
nhanes_final %>% 
  group_by(mcq365d, riagendr) %>% 
  summarize(mean_wt = mean(bmxwt), sd_wt = sd(bmxwt), n = n())
# A tibble: 4 x 5
# Groups:   mcq365d [2]
#   mcq365d riagendr mean_wt sd_wt     n
#     <dbl> <fct>      <dbl> <dbl> <int>
# 1       1 1           95.1  23.4   809
# 2       1 2           86.7  23.1   993
# 3       2 1           82.6  18.8  2026
# 4       2 2           71.3  18.1  2059
# Nice! It's pretty clear that there truly is a mean difference in weight by gender, so blocking was a 
# good call for this experiment. We also observed a statistically significant effect of the treatment 
# on bmxwt, which we hope is actually a result of the treatment. Now that we have the RCBD down, let's 
# tackle Balanced Incomplete Block Designs (BIBD).

# RCBD Model Validation
# As we did in the last chapter (and when building any model!) it's a good idea to validate 
# the results. We'll examine the Residuals vs. Fitted and Normal Q-Q plots, though now we'll 
# only see a Constant Leverage plot in place of the other two. A good model has a Q-Q plot 
# showing an approximately normal distribution and no clear patterns across blocks or 
# treatments in the others.
# We can also look at Interaction plots. We hope to see parallel lines, no matter which of the 
# block or the treatment is on the x-axis. If they are, they satisfy a key assumption of the 
# RCBD model called Additivity.
# The nhanes_rcbd model object from the last exercise has been loaded for you. Examine the 
# results with summary(nhanes_rcbd) in the console if you need a refresher.
# Instructions
# Plot the nhanes_rcbd model object, being sure to set up a 2x2 grid of plots beforehand.
# Set up the 2x2 plotting grid and plot nhanes_rcbd
par(mfrow = c(2,2))
plot(nhanes_rcbd)
# http://joxi.ru/a2X1VZQTQ5MvnA
# Run the code to view the interaction plot between the treatment and gender and observe if the lines are parallel.
# # Run the code to view the interaction plots
with(nhanes_final, interaction.plot(mcq365d, riagendr, bmxwt))
# http://joxi.ru/V2VxnLKcBJNkjr
library(ggplot2)
nhanes_final %>% group_by(mcq365d, riagendr) %>% summarise(mean(bmxwt))
# A tibble: 4 x 3
# Groups:   mcq365d [2]
# mcq365d riagendr `mean(bmxwt)`
# <dbl>    <int>         <dbl>
# 1       1        1          95.1
# 2       1        2          86.7
# 3       2        1          82.6
# 4       2        2          71.3
# AK: how to do with geom_segment??
?geom_segment()
# %>% ggplot(aes(x = mcq365d, y = `mean(bmxwt)`)) + geom_point() + geom_segment()
# Run the code to view the interaction plot between gender and the treatment (it'll be a little different!) and observe if the lines are parallel.
# Run the code to view the interaction plots
with(nhanes_final, interaction.plot(riagendr, mcq365d, bmxwt))
# http://joxi.ru/Y2LdjY0txJbMQ2
# Excellent! The initial diganostic plots show that this model is pretty good but not great - 
# especially at the larger end of the data, the Q-Q plot shows the data might not be normal. 
# The interaction plots show nearly parallel lines, so we can move forward with this model.

# 6 Intro to NHANES and sampling.mp4
# Is a BIBD even possible?
# We saw in the video that it's possible a BIBD doesn't exist at all. It's useful to calculate the 
# lambda beforehand, and if the result isn't an integer, there isn't a possible BIBD.
# Don't bust out the pen and paper yet: the lambda() function has been defined for you. It takes as input 
# t = number of treatments, 
# k = number of treatments per block, 
# r = number of repetitions (when not all blocks have all variants we need more repitions to cover all combinations)
# It then calculates lambda according to this formula: r(k - 1) / (t - 1).
# Try the different combinations of t, k, and r using lambda() in your console, and choose the 
# answer which does NOT have a BIBD.
# Instructions
# Possible Answers
# t = 2, k = 2, r = 2
# t = 2, k = 3, r = 4
# t = 3, k = 4, r = 11 +
# t = 12, k = 2, r = 22
# Good job! When designing experiments, you could build your own version of the custom function lambda, or use agricolae to figure it out visually, as we'll see in the next exercise.


# Drawing BIBDs with agricolae
# We can also use agricolae to draw BIBDs. design.bib() takes, at minimum, the treatments (treats), an integer k corresponding to the number of levels of the blocks, and a seed as inputs.
# The main thing you should notice about a BIBD is that not every treatment will be used in each block (column) of the output.
# From the video and the last exercise, however, you know that sometimes a BIBD isn't valid and that you have to do a little math to be sure your BIBD design is possible. design.bib() will return an error message letting you know if a design is not valid.
# Let's draw a few BIBDs with agricolae so we can see the different warning messages and errors the package provides.
# Instructions
# Create my_design_bibd_1 using A, B, and C for the treatments, 4 blocks, and a seed of 42.
# Create my_design_bibd_1
my_design_bibd_1 <- design.bib(LETTERS[1:3], k = 4, seed = 42)
# Error: The number of trials must be at least as large as the minimum blocksize.
my_design_bibd_1
# Error: object 'my_design_bibd_1' not found
# Create my_design_bibd_2 using LETTERS[1:8] for treatments, 3 blocks, and a seed of 42.
# Create my_design_bibd_2
my_design_bibd_2 <- design.bib(LETTERS[1:8], k = 3, seed = 42)
# Error: invalid 'times' argument
my_design_bibd_2
# Error: object 'my_design_bibd_2' not found
# Create my_design_bibd_3 using A, B, C, and D as treatments, 4 blocks, and the same seed. Examine the sketch of the object.
# Create my_design_bibd_3
my_design_bibd_2 <- design.bib(LETTERS[1:4], k = 4, seed = 42)
# Parameters BIB
# ==============
# Lambda     : 2
# treatmeans : 4
# Block size : 4
# Blocks     : 2
# Replication: 2 
# Efficiency factor 1 
# <<< Book >>>
# my_design_bibd_2$sketch
# [,1] [,2] [,3] [,4]
# [1,] "C"  "A"  "D"  "B" 
# [2,] "C"  "D"  "B"  "A" 
# Nice! You saw two different function errors which help lead you in the right direction with your design, plus one that works. When the design does work, the sketch parameter shows the design. The blocks are now the columns, however, unlike with RCBDs
# AK
my_design_bibd_3 <- design.bib(LETTERS[1:4], k = 3, r = 6, seed = 42)
# AK: if number of treatments = number of treatments in a block k, then number of blocks = number of replications (we don't need more blocks to cover all replications)
# AK: if number of treatments > number of treatments in a block k, then number of blocks > number of replications (we do need more blocks to cover replications)
# Parameters BIB
# ==============
# Lambda     : 4
# treatmeans : 4
# Block size : 3
# Blocks     : 8
# Replication: 6 
# Efficiency factor 0.8888889 
# <<< Book >>>
my_design_bibd_3$sketch
#     [,1] [,2] [,3]
# [1,] "D"  "C"  "B" 
# [2,] "C"  "D"  "A" 
# [3,] "D"  "A"  "B" 
# [4,] "C"  "A"  "D" 
# [5,] "B"  "D"  "A" 
# [6,] "B"  "C"  "A" 
# [7,] "C"  "B"  "D" 
# [8,] "A"  "C"  "B" 

# BIBD - cat's kidney function
# To be sure we truly understand what a BIBD looks like, let's build a dataset containing a BIBD 
# from scratch.
# Say we want to test the difference between four different wet foods in cats' diets on their 
# kidney function. Cat food, however, is expensive, so we'll only test 3 foods per block to save 
# some money. Our cats will be blocked by color of cat, as we aren't interested in that as part 
# of our experiment. The outcome will be measured blood creatinine level, an indicator of kidney 
# function and dysfunction in cats and humans alike.
# Instructions
# The custom function lambda() has been loaded for you. Calculate lambda with t = 4, k = 3, and r = 3
# to make sure a BIBD is possible.
# Run the code to assemble the dataset. You can see the order in which the food treatments are used 
# in each block.
# Create cat_model with aov() according to the description of the experiment above and examine the 
# results with summary(). Does type of wet food make a difference on creatinine levels?
# Calculate lambda
lambda(t = 4, k = 3, r = 3)
# [1] 2
# Build the data.frame
creatinine <- c(1.98, 1.97, 2.35, 2.09, 1.87, 1.95, 2.08, 2.01, 1.84, 2.06, 1.97, 2.22)
food <- as.factor(c("A", "C", "D", "A", "B", "C", "B", "C", "D", "A", "B", "D"))
color <- factor(rep(c("Black", "White", "Orange", "Spotted"), each = 3))
cat_experiment <- as.data.frame(cbind(creatinine, food, color))
cat_experiment
#    creatinine food color
# 1        1.98    1     1
# 2        1.97    3     1
# 3        2.35    4     1
# 4        2.09    1     4
# 5        1.87    2     4
# 6        1.95    3     4
# 7        2.08    2     2
# 8        2.01    3     2
# 9        1.84    4     2
# 10       2.06    1     3
# 11       1.97    2     3
# 12       2.22    4     3
# AK: still don't understand: we don't have variant color = food = 2
# AK: as pivot, to save string names of variable
library(tidyr)
cat_experiment_1 <- tibble(
  creatinine = c(1.98, 1.97, 2.35, 2.09, 1.87, 1.95, 2.08, 2.01, 1.84, 2.06, 1.97, 2.22),
  food = c("A", "C", "D", "A", "B", "C", "B", "C", "D", "A", "B", "D"),
  color = rep(c("Black", "White", "Orange", "Spotted"), each = 3)
)
# A tibble: 4 × 5
# color       A     C     D     B
#   <chr>   <dbl> <dbl> <dbl> <dbl>
# 1 Black    1.98  1.97  2.35  NA   
# 2 White    2.09  1.95  NA    1.87
# 3 Orange   NA    2.01  1.84  2.08
# 4 Spotted  2.06  NA    2.22  1.97
cat_experiment_1 %>% pivot_wider(names_from = food, values_from = creatinine)
# Create cat_model and examine with summary()
cat_model <- aov(formula = creatinine ~ food + color, data = cat_experiment)
summary(cat_model)
#             Df  Sum Sq  Mean Sq F value Pr(>F)
# food         1 0.01204 0.012042   0.530  0.485
# color        1 0.00697 0.006971   0.307  0.593
# Residuals    9 0.20461 0.022735   
# Purrfect! It seems there are no differences by type of wet food in kidney function. Hopefully now you can see how a BIBD comes to life!

# NHANES BIBD
# Let's jump back into the NHANES data and pretend we have access to NHANES patients ages 18-45. 
# Blocking the adults by race, stored in NHANES as ridreth1, we prescribe to our groups either no 
# particular upper body weightlifting regimen, a weightlifting regimen, or a weightlifting regimen 
# plus a prescribed daily vitamin supplement. This information is stored in a variable called weightlift_treat.
# Those funding the study decide they want it to be a BIBD where only 2 treatments appear in each 
# block. The outcome, arm circumference, is stored as bmxarmc. The nhanes_final data is loaded for you.
# Instructions
# Calculate lambda where t = 3, k = 2, and r = 2. Does a BIBD exist here?
# Create weightlift_model and examine the results. The experiment evaluates the outcome bmxarmc, 
# where the treatment is stored in weightlift_treat and subjects are blocked by ridreth1.

# Calculate lambda
lambda(t = 3, k = 2, r = 2)
# [1] 1
head(nhanes_final %>% select(bmxarmc, weightlift_treat, ridreth1))
#   bmxarmc weightlift_treat ridreth1
# 1    27.2                1        4
# 2    34.0                2        4
# 3    31.5                3        1
# 4    31.0                1        2
# 5    35.9                2        1
# 6    29.6                3        5
str(nhanes_final %>% select(bmxarmc, weightlift_treat, ridreth1))
# 'data.frame':	2452 obs. of  3 variables:
# $ bmxarmc         : num  27.2 34 31.5 31 35.9 29.6 41.6 32.1 29.7 NA ...
# $ weightlift_treat: num  1 2 3 1 2 3 1 2 3 1 ...
# $ ridreth1        : Factor w/ 5 levels "1","2","3","4",..: 4 4 1 2 1 5 4 4 2 2 ...
nhanes_final %>% select(bmxarmc, weightlift_treat, ridreth1) %>% group_by(weightlift_treat, ridreth1) %>% summarise(mean = mean(bmxarmc, na.rm = T), sd = sd(bmxarmc, na.rm = T), n = n())
# A tibble: 15 x 5
# Groups:   weightlift_treat [3]
#    weightlift_treat ridreth1  mean    sd     n
#               <dbl> <fct>    <dbl> <dbl> <int>
#  1                1 1         33.7  5.12   142
#  2                1 2         33.5  5.65    97
#  3                1 3         33.6  5.09   233
#  4                1 4         34.7  6.43   176
#  5                1 5         31.2  5.37   170
#  6                2 1         33.7  5.58   162
#  7                2 2         33.1  5.34   113
#  8                2 3         32.7  4.97   210
#  9                2 4         35.0  5.64   187
# 10                2 5         31.3  4.80   145
# 11                3 1         34.0  4.77   159
# 12                3 2         33.5  5.53    89
# 13                3 3         33.5  5.87   243
# 14                3 4         35.0  6.10   176
# 15                3 5         31.1  5.19   150
nhanes_final %>% select(bmxarmc, weightlift_treat, ridreth1) %>% ggplot(aes(x = ridreth1, y = bmxarmc)) + geom_boxplot(aes(fill = as.factor(weightlift_treat)))
# Warning message: Removed 115 rows containing non-finite values (stat_boxplot).
# AK: ^ need to impute how we did above ^
# Create weightlift_model & examine results
weightlift_model <- aov(formula = bmxarmc ~ weightlift_treat + ridreth1, data = nhanes_final)
weightlift_model
# Call:
# aov(formula = bmxarmc ~ weightlift_treat + ridreth1, data = nhanes_final)
# Terms:
# weightlift_treat ridreth1 Residuals
# Sum of Squares              3.93  3364.19  69224.12
# Deg. of Freedom                1        4      2331
# Residual standard error: 5.449512
# Estimated effects may be unbalanced
# 115 observations deleted due to missingness
summary(weightlift_model)
# Df Sum Sq Mean Sq F value Pr(>F)    
# weightlift_treat    1      4     3.9   0.132  0.716    
# ridreth1            4   3364   841.0  28.321 <2e-16 ***
#   Residuals        2331  69224    29.7                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 115 observations deleted due to missingness
# Nice! As it turns out, the weight lifting regimen doesn't seem to have a significant effect on 
# arm circumference when the patient population is blocked by race.

# 9 Latin squares.mp4
# NYC SAT Scores EDA
# Math is a subject the U.S. is consistently behind the rest of the world on, so our experiments will focus on Math score. While the original dataset is an open dataset downloaded from Kaggle, throughout this chapter I will add a few variables that will allow you to pretend you are an education researcher conducting experiments ideally aimed at raising students' scores, hopefully increasing the likelihood they will be admitted to colleges.
# Before diving into analyzing the experiments, we should do some EDA to make sure we fully understand the nyc_scores data. In this lesson, we'll do experiments where we block by Borough and Teacher_Education_Level, so let's examine math scores by those variables. The nyc_scores dataset has been loaded for you.
# Instructions
# - Find the mean, variance, and median of Average_Score_SAT_Math by Borough using dplyr methods for EDA as we have used them throughout the course.
str(nyc_scores)
# 'data.frame':	435 obs. of  23 variables:
# $ School_ID                : chr  "02M260" "06M211" "01M539" "02M294" ...
# $ School_Name              : chr  "Clinton School Writers and Artists" "Inwood Early College for Health and Information Technologies" "New Explorations into Science, Technology and Math High School" "Essex Street Academy" ...
# $ Borough                  : chr  "Manhattan" "Manhattan" "Manhattan" "Manhattan" ...
# $ Building_Code            : chr  "M933" "M052" "M022" "M445" ...
# $ Street_Address           : chr  "425 West 33rd Street" "650 Academy Street" "111 Columbia Street" "350 Grand Street" ...
# $ City                     : chr  "Manhattan" "Manhattan" "Manhattan" "Manhattan" ...
# $ State                    : chr  "NY" "NY" "NY" "NY" ...
# $ Zip_Code                 : int  10001 10002 10002 10002 10002 10002 10002 10002 10002 10002 ...
# $ Latitude                 : num  40.8 40.9 40.7 40.7 40.7 ...
# $ Longitude                : num  -74 -73.9 -74 -74 -74 ...
# $ Phone_Number             : chr  "212-695-9114" "718-935-3660  " "212-677-5190" "212-475-4773" ...
# $ Start_Time               : chr  "" "8:30 AM" "8:15 AM" "8:00 AM" ...
# $ End_Time                 : chr  "" "3:00 PM" "4:00 PM" "2:45 PM" ...
# $ Student_Enrollment       : int  NA 87 1735 358 383 416 255 545 329 363 ...
# $ Percent_White            : num  NA 0.03 0.29 0.12 0.03 0.02 0.04 0.45 0.03 0.03 ...
# $ Percent_Black            : num  NA 0.22 0.13 0.39 0.28 0.03 0.24 0.17 0.42 0.4 ...
# $ Percent_Hispanic         : num  NA 0.68 0.18 0.41 0.57 0.06 0.57 0.19 0.49 0.51 ...
# $ Percent_Asian            : num  NA 0.05 0.39 0.06 0.09 0.89 0.13 0.17 0.06 0.06 ...
# $ Average_Score_SAT_Math   : int  NA NA 657 395 418 613 410 634 389 438 ...
# $ Average_Score_SAT_Reading: int  NA NA 601 411 428 453 406 641 395 413 ...
# $ Average_Score_SAT_Writing: int  NA NA 601 387 415 463 381 639 381 394 ...
# $ Percent_Tested           : num  NA NA 0.91 0.79 0.65 0.96 0.6 0.71 0.81 0.36 ...
# $ Teacher_Education_Level  : chr  "Grad Student" "MA" "College Student" "MA" ...
# Mean, var, and median of Math score
nyc_scores %>%
  group_by(Borough) %>% 
  summarise(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
            var = var(Average_Score_SAT_Math, na.rm = TRUE),
            median = median(Average_Score_SAT_Math, na.rm = TRUE),
            n = n())
# A tibble: 5 x 5
# Borough        mean   var median     n
# <chr>         <dbl> <dbl>  <dbl> <int>
# 1 Bronx          404. 2727.   396.   118
# 2 Brooklyn       416. 3658.   395    121
# 3 Manhattan      456. 7026.   433    106
# 4 Queens         462. 5168.   448     80
# 5 Staten Island  486. 6911.   466.    10
# - Find the mean, variance, and median of Average_Score_SAT_Math by Teacher_Education_Level using dplyr EDA methods.
# Mean, var, and median of Math score by Teacher Education Level
nyc_scores %>%
  group_by(Teacher_Education_Level) %>% 
  summarise(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
            var = var(Average_Score_SAT_Math, na.rm = TRUE),
            sd = sd(Average_Score_SAT_Math, na.rm = TRUE),
            median = median(Average_Score_SAT_Math, na.rm = TRUE),
            n = n(),
            q95 = quantile(x = Average_Score_SAT_Math, prob = c(0.95), na.rm = T))
# A tibble: 5 x 7
# Teacher_Education_Level  mean   var    sd median     n   q95
# <chr>                   <dbl> <dbl> <dbl>  <dbl> <int> <dbl>
# 1 BA                       428. 5374.  73.3   406.    92  579.
# 2 College Student          433. 5153.  71.8   410     98  588.
# 3 Grad Student             440. 4748.  68.9   417     74  583 
# 4 MA                       435. 5952.  77.1   418    124  580.
# 5 PhD                      426. 3778.  61.5   416.    47  520.
# AK: look at q95 and median: for PhD median similar but q95 much lower, that means PhD less skewed and less variation
library(ggplot2)
ggplot(nyc_scores, aes(x = Average_Score_SAT_Math, color = Teacher_Education_Level)) + geom_density()
# Warning message: Removed 60 rows containing non-finite values (stat_density).
# http://joxi.ru/Q2KWnY7COJVRR2
ggplot(nyc_scores, aes(x = Teacher_Education_Level, y = Average_Score_SAT_Math, color = Teacher_Education_Level)) + geom_boxplot()
# Warning message: Removed 60 rows containing non-finite values (stat_boxplot).
# http://joxi.ru/KAxZeo9CVXzQ9m
# - Find the mean, variance, and median of Average_Score_SAT_Math by both Borough and Teacher_Education_Level using dplyr EDA methods.
nyc_scores %>%
  group_by(Borough, Teacher_Education_Level) %>% 
  summarise(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
            var = var(Average_Score_SAT_Math, na.rm = TRUE),
            sd = sd(Average_Score_SAT_Math, na.rm = TRUE),
            median = median(Average_Score_SAT_Math, na.rm = TRUE),
            n = n(),
            q95 = quantile(x = Average_Score_SAT_Math, prob = c(0.95), na.rm = T))
# A tibble: 24 x 8
# Groups:   Borough [5]
#   Borough  Teacher_Education_Level  mean   var    sd median     n   q95
#   <chr>    <chr>                   <dbl> <dbl> <dbl>  <dbl> <int> <dbl>
# 1 Bronx    BA                       409. 5346.  73.1   392.    22  506.
# 2 Bronx    College Student          420. 6217.  78.8   413     23  476.
# 3 Bronx    Grad Student             397.  784.  28.0   390     22  430.
# 4 Bronx    MA                       394.  880.  29.7   388.    41  433.
# 5 Bronx    PhD                      419. 1393.  37.3   417     10  476.
# 6 Brooklyn BA                       422. 3479.  59.0   400     25  505.
# 7 Brooklyn College Student          423. 5488.  74.1   395     25  512.
# 8 Brooklyn Grad Student             418. 2908.  53.9   400     24  503.
# 9 Brooklyn MA                       416  4003.  63.3   392     33  539 
# 10 Brooklyn PhD                      392  1228.  35.0   378     14  451.
# … with 14 more rows
# Great job! Now that we've examined the data, we can move on to cleaning it, the next important 
# step before analysis.

# Dealing with Missing Test Scores
# If we want to use SAT scores as our outcome, we should examine missingness. Examine the pattern of missingness across all the variables in nyc_scores using miss_var_summary() from the naniar package. naniar integrates with Tidyverse code styling, including the pipe operator (%>%).
# There are 60 missing scores in each subject. Though there are many R packages which help with more advanced forms of imputation, such as MICE, Amelia, and mi, we will continue to use simputation and impute_median().
# Create a new dataset, nyc_scores_2 by imputing Math score by Borough, but note that impute_median() returns the imputed variable as type "impute". You'll convert the variable to the numeric in a separate step.
# simputation and dplyr are loaded.
# Instructions
# - Load the naniar package.
# - Examine the missingness of variables in nyc_scores by piping it to miss_var_summary().
library(naniar)
# Examine missingness with miss_var_summary()
miss_var_summary(nyc_scores)
# A tibble: 22 x 3
# variable                  n_miss pct_miss
# <chr>                      <int>    <dbl>
# 1 Average_Score_SAT_Math        60    13.8 
# 2 Average_Score_SAT_Reading     60    13.8 
# 3 Average_Score_SAT_Writing     60    13.8 
# 4 Percent_Tested                49    11.3 
# 5 Student_Enrollment             7     1.61
# 6 Percent_White                  7     1.61
# 7 Percent_Black                  7     1.61
# 8 Percent_Hispanic               7     1.61
# 9 Percent_Asian                  7     1.61
# 10 School_ID                      0     0   
# … with 12 more rows
library(tidyr)
nyc_scores %>% summarise_all(funs(sum(is.na(.)) / n())) %>% pivot_longer(cols = everything(), names_to = "variables", values_to = "pct_missing") %>% arrange(desc(pct_missing))
# A tibble: 22 x 2
#   variables                 pct_missing
#   <chr>                           <dbl>
# 1 Average_Score_SAT_Math         0.138 
# 2 Average_Score_SAT_Reading      0.138 
# 3 Average_Score_SAT_Writing      0.138 
# 4 Percent_Tested                 0.113 
# 5 Student_Enrollment             0.0161
# 6 Percent_White                  0.0161
# 7 Percent_Black                  0.0161
# 8 Percent_Hispanic               0.0161
# 9 Percent_Asian                  0.0161
# 10 School_ID                      0     
# … with 12 more rows
# - Create nyc_scores_2 by imputing the Average Math SAT score by Borough (we're only using Math in our experiments.)
# Examine missingness with md.pattern()
 md.pattern(nyc_scores)
#     School_ID School_Name Borough Building_Code Street_Address City State
# 375         1           1       1             1              1    1     1
#  11         1           1       1             1              1    1     1
#  42         1           1       1             1              1    1     1
#   7         1           1       1             1              1    1     1
#             0           0       0             0              0    0     0
#     Zip_Code Latitude Longitude Phone_Number Start_Time End_Time
# 375        1        1         1            1          1        1
#  11        1        1         1            1          1        1
#  42        1        1         1            1          1        1
#   7        1        1         1            1          1        1
#            0        0         0            0          0        0
#     Student_Enrollment Percent_White Percent_Black Percent_Hispanic
# 375                  1             1             1                1
#  11                  1             1             1                1
#  42                  1             1             1                1
#   7                  0             0             0                0
#                      7             7             7                7
#    Percent_Asian Percent_Tested Average_Score_SAT_Math
# 375             1              1                      1
#  11             1              1                      0
#  42             1              0                      0
#   7             0              0                      0
#                 7             49                     60
#     Average_Score_SAT_Reading Average_Score_SAT_Writing    
# 375                         1                         1   0
#  11                         0                         0   3
#  42                         0                         0   4
#   7                         0                         0   9
#                            60                        60 264
library(simputation)
# Impute the Math score by Borough
# nyc_scores_2 <- impute_median(dat = nyc_scores, Average_Score_SAT_Math ~ Borough)
 # AK: let's check. Why values Average_Score_SAT_Math_1 do not correspond to the median by group??
nyc_scores_3 <- nyc_scores
nyc_scores_3$Average_Score_SAT_Math_1 <- nyc_scores$Average_Score_SAT_Math
nyc_scores_3 %>% 
  impute_median(formula = Average_Score_SAT_Math_1 ~ Borough) %>% 
  filter(is.na(Average_Score_SAT_Math)) %>% 
  select(Borough, Average_Score_SAT_Math, Average_Score_SAT_Math_1)
#    Borough Average_Score_SAT_Math Average_Score_SAT_Math_1
# 1  Manhattan                     NA                    395.5
# 2  Manhattan                     NA                    395.5
# 3  Manhattan                     NA                    395.5
# 4  Manhattan                     NA                    395.5
# 5  Manhattan                     NA                    395.5
# 6  Manhattan                     NA                    395.5
# 7  Manhattan                     NA                    395.5
# 8  Manhattan                     NA                    395.5
# 9  Manhattan                     NA                    395.5
# 10 Manhattan                     NA                    395.5
# 11 Manhattan                     NA                    395.5
# 12 Manhattan                     NA                    395.5
# 13 Manhattan                     NA                    395.5
# 14 Manhattan                     NA                    395.5
# 15 Manhattan                     NA                    395.5
# 16 Manhattan                     NA                    395.5
# 17 Manhattan                     NA                    395.5
# 18     Bronx                     NA                    395.5
# 19     Bronx                     NA                    395.0
# 20     Bronx                     NA                    395.0
# 21     Bronx                     NA                    395.0
# 22     Bronx                     NA                    395.0
# 23     Bronx                     NA                    395.0
# 24     Bronx                     NA                    395.0
# 25     Bronx                     NA                    395.0
# 26     Bronx                     NA                    395.0
# 27     Bronx                     NA                    395.0
# 28     Bronx                     NA                    395.0
# 29     Bronx                     NA                    395.0
# 30     Bronx                     NA                    395.0
# 31     Bronx                     NA                    395.0
# 32     Bronx                     NA                    395.0
# 33     Bronx                     NA                    395.0
# 34     Bronx                     NA                    395.0
# 35     Bronx                     NA                    395.0
# 36     Bronx                     NA                    395.0
# 37     Bronx                     NA                    395.0
# 38    Queens                     NA                    433.0
# 39  Brooklyn                     NA                    433.0
# 40  Brooklyn                     NA                    433.0
# 41  Brooklyn                     NA                    433.0
# 42  Brooklyn                     NA                    433.0
# 43  Brooklyn                     NA                    433.0
# 44  Brooklyn                     NA                    433.0
# 45  Brooklyn                     NA                    433.0
# 46  Brooklyn                     NA                    433.0
# 47  Brooklyn                     NA                    433.0
# 48  Brooklyn                     NA                    448.0
# 49  Brooklyn                     NA                    448.0
# 50  Brooklyn                     NA                    448.0
# 51    Queens                     NA                    448.0
# 52    Queens                     NA                    448.0
# 53    Queens                     NA                    448.0
# 54    Queens                     NA                    448.0
# 55    Queens                     NA                    448.0
# 56    Queens                     NA                    448.0
# 57    Queens                     NA                    448.0
# 58    Queens                     NA                    448.0
# 59    Queens                     NA                    448.0
# 60    Queens                     NA                    465.5
nyc_scores %>% group_by(Borough) %>% summarise(median=round(median(Average_Score_SAT_Math, na.rm = T),2))
# A tibble: 5 x 2
#   Borough       median
#   <chr>          <dbl>
# 1 Bronx           396.
# 2 Brooklyn        395 
# 3 Manhattan       433 
# 4 Queens          448 
# 5 Staten Island   466.
nyc_scores_3 %>% group_by(Borough) %>% summarise(median=round(median(Average_Score_SAT_Math_1, na.rm = T),2))
# A tibble: 5 x 2
# Borough       median
# <chr>          <dbl>
# 1 Bronx           396.
# 2 Brooklyn        395 
# 3 Manhattan       433 
# 4 Queens          448 
# 5 Staten Island   466.
# - Convert nyc_scores_2$Average_Score_SAT_Math to numeric.
nyc_scores_2$Average_Score_SAT_Math <- as.numeric(nyc_scores_2$Average_Score_SAT_Math)
# - Use dplyr to examine the median and mean of math score before and after imputation.
# Examine scores by Borough in both datasets, before and after imputation
nyc_scores %>% 
  group_by(Borough) %>% 
  summarize(median = median(Average_Score_SAT_Math, na.rm = TRUE), 
            mean = mean(Average_Score_SAT_Math, na.rm = TRUE))
# A tibble: 5 x 3
B#  Borough       median  mean
#   <chr>          <dbl> <dbl>
# 1 Bronx           396.  404.
# 2 Brooklyn        395   416.
# 3 Manhattan       433   456.
# 4 Queens          448   462.
# 5 Staten Island   466.  486.
nyc_scores_2 %>% 
  group_by(Borough) %>% 
  summarize(median = median(Average_Score_SAT_Math, na.rm = TRUE), 
            mean = mean(Average_Score_SAT_Math, na.rm = TRUE))
# A tibble: 5 x 3
#   Borough       median  mean
#   <chr>          <dbl> <dbl>
# 1 Bronx           395   403.
# 2 Brooklyn        399   418.
# 3 Manhattan       418   446.
# 4 Queens          448   460.
# 5 Staten Island   466.  486.
# Nice job! Did the median scores change before and after imputation? (Hint: they shouldn't have changed by much, but rounding may have offset them by an integer or two.)

# Drawing Latin Squares with agricolae
# We return, once again, to the agricolae package to examine what a Latin Square design can look like. Here's an example:
#     [,1] [,2] [,3] [,4]
# [1,] "B"  "D"  "A"  "C" 
# [2,] "A"  "C"  "D"  "B" 
# [3,] "D"  "B"  "C"  "A" 
# [4,] "C"  "A"  "B"  "D"
# Since a Latin Square experiment has two blocking factors, you can see that in this design, each treatment appears once in both each row (blocking factor 1) and each column (blocking factor 2).
# Look at the help page for design.lsd() by typing ?design.lsd in the console for any help you need designing your Latin Square experiment.
# - Load the agricolae package.
# - Create and view the sketch of a Latin Square design, my_design_lsd, using treatments A, B, C, D, & E, and a seed of 42.
# Load agricolae
library(agricolae)
?design.lsd
# Design a LS with 5 treatments A:E then look at the sketch
my_design_lsd <- design.lsd(trt = LETTERS[1:5], seed = 42)
my_design_lsd$sketch
# [,1] [,2] [,3] [,4] [,5]
# [1,] "E"  "D"  "A"  "C"  "B" 
# [2,] "D"  "C"  "E"  "B"  "A" 
# [3,] "A"  "E"  "B"  "D"  "C" 
# [4,] "C"  "B"  "D"  "A"  "E" 
# [5,] "B"  "A"  "C"  "E"  "D" 
# Perhaps you're thinking to yourself 'This looks a lot like a RCBD'…bingo! It does, but as we know from the video, there are now two blocking factors in a LS design.

# Latin Square with NYC SAT Scores
# To execute a Latin Square design on this data, suppose we want to know the effect of our tutoring program, which includes one-on-one tutoring, two small groups, and an in and after-school SAT prep class. A new dataset nyc_scores_ls is available that represents this experiment. Feel free to explore the dataset in the console.
# We'll block by Borough and Teacher_Education_Level to reduce their known variance on the score outcome. Borough is a good blocking factor because schools in America are funded partly based on taxes paid in each city, so it will likely make a difference in the quality of education.
# Instructions
# Use lm() to test the changes in Average_Score_SAT_Math using nyc_scores_ls.
# Tidy nyc_scores_ls_lm with the appropriate broom function.
# Examine nyc_scores_ls_lm with anova().
str(nyc_scores_ls %>% select(Average_Score_SAT_Math, Tutoring_Program, Borough))
# grouped_df [25 × 3] (S3: grouped_df/tbl_df/tbl/data.frame)
# $ Average_Score_SAT_Math: num [1:25] 390 395 393 398 402 395 475 374 362 386 ...
# $ Tutoring_Program      : chr [1:25] "One-on-One" "Small Groups (2-3)" "Small Groups (4-6)" "SAT Prep Class (school hours)" ...
# $ Borough               : chr [1:25] "Bronx" "Bronx" "Bronx" "Bronx" ...
# Build nyc_scores_ls_lm
nyc_scores_ls_lm <- lm(Average_Score_SAT_Math ~ Tutoring_Program + Borough + Teacher_Education_Level,
                       data = nyc_scores_ls )
# Tidy the results with broom
tidy(nyc_scores_ls_lm)
# A tibble: 13 x 5
#    term                                  estimate std.error statistic    p.value
#    <chr>                                    <dbl>     <dbl>     <dbl>      <dbl>
#  1 (Intercept)                             402.        32.4   12.4       3.35e-8
#  2 Tutoring_ProgramSAT Prep Class (afte…    -2.3       28.4   -0.0809    9.37e-1
#  3 Tutoring_ProgramSAT Prep Class (scho…    -6.3       28.4   -0.222     8.28e-1
#  4 Tutoring_ProgramSmall Groups (2-3)       13.8       28.4    0.485     6.36e-1
#  5 Tutoring_ProgramSmall Groups (4-6)       40.0       28.4    1.41      1.85e-1
#  6 BoroughBrooklyn                           2.8       28.4    0.0985    9.23e-1
#  7 BoroughManhattan                         14.7       28.4    0.517     6.15e-1
#  8 BoroughQueens                           103.        28.4    3.64      3.41e-3
#  9 BoroughStaten Island                     72.8       28.4    2.56      2.50e-2
# 10 Teacher_Education_LevelCollege Stude…   -41.9       28.4   -1.47      1.66e-1
# 11 Teacher_Education_LevelGrad Student     -15.6       28.4   -0.549     5.93e-1
# 12 Teacher_Education_LevelMA               -29.5       28.4   -1.04      3.20e-1
# 13 Teacher_Education_LevelPhD                9.70      28.4    0.341     7.39e-1
# Examine the results with anova
anova(object = nyc_scores_ls_lm)
# Analysis of Variance Table
# Response: Average_Score_SAT_Math
# Df Sum Sq Mean Sq F value  Pr(>F)  
# Tutoring_Program         4   7134  1783.5  0.8826 0.50308  
# Borough                  4  43557 10889.2  5.3886 0.01016 *
# Teacher_Education_Level  4   8841  2210.3  1.0938 0.40315  
# Residuals               12  24250  2020.8                  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Question
# At the 0.05 significance level, do we have evidence to believe the tutoring program has an effect on math SAT scores, when blocked by Borough and Teacher_Education_Level?
# Possible Answers
# - Nope! Given the p-value, we have no reason to reject the null hypothesis. +
# - Yes! Given the p-value, we have reason to believe that the tutoring program had an effect on the Math score.
# Excellent! It seems that when we block for Borough of the school and Teacher_Education_Level, our Tutoring_Program isn't having a statistically significant effect on the Math SAT score.

# 
# NYC SAT Scores Data Viz
# In the last lesson, when discussing Latin Squares, we did numerical EDA in the form of looking at means, variances, and medians of the math SAT scores. Another crucial part of the EDA is data visualization, as it often helps in spotting outliers plus gives you a visual representation of the distribution of your variables.
# ggplot2 has been loaded for you and the nyc_scores dataset is available. Create and examine the requested boxplot. How do the medians differ by Borough? How many outliers are present, and where are they mostly present?
# Instructions
# Create a boxplot of Math SAT scores by Borough.
# Run the code to include a title: "Average SAT Math Scores by Borough, NYC".
# Change the x- and y-axis labels to read "Borough (NYC)" and "Average SAT Math Scores (2014-15)", respectively, using the correct arguments to labs().
# Create a boxplot of Math scores by Borough, with a title and x/y axis labels
ggplot(nyc_scores) +
  geom_boxplot(aes(x = Borough, y = Average_Score_SAT_Math)) + 
  labs(title = "Average SAT Math Scores by Borough, NYC",
       x = "Borough (NYC)",
       y = "Average SAT Math Scores (2014-15)")
# http://joxi.ru/p27l897TLYK6DA
# Beautiful! It's interesting to see the different distribution of scores by Borough and to see that every borough has scores that are outliers, though some more than others.

# Drawing Graeco-Latin Squares with agricolae
# As we've seen, agricolae provides us the ability to draw all of the experimental designs we've used so far, and they can also draw Graeco-Latin squares. One difference in the input to design.graeco() that we haven't seen before is that we'll need to input 2 vectors, trt1 and trt2, which must be of equal length. You can think of trt1 as your actual treatment and trt2 as one of your blocking factors. agricolae has been loaded for you.
# Instructions
# Create vectors trt1 with LETTERS A through E and trt2 with numbers 1 through 5.
# Make my_graeco_design with design.graeco(), using and seed = 42.
# Examine the parameters and sketch of my_graeco_design.
# Create my_graeco_design
my_graeco_design <- design.graeco(trt1 = trt1, trt2 = trt2, seed = 42)
# Examine the parameters and sketch
my_graeco_design$parameters
# $design
# [1] "graeco"
# $trt1
# [1] "A" "B" "C" "D" "E"
# $trt2
# [1] 1 2 3 4 5
# $r
# [1] 5
# $serie
# [1] 2
# $seed
# [1] 42
# $kinds
# [1] "Super-Duper"
# [[8]]
# [1] TRUE
my_graeco_design$sketch
# [,1]  [,2]  [,3]  [,4]  [,5] 
# [1,] "D 5" "A 1" "C 3" "B 4" "E 2"
# [2,] "A 3" "C 4" "B 2" "E 5" "D 1"
# [3,] "C 2" "B 5" "E 1" "D 3" "A 4"
# [4,] "B 1" "E 3" "D 4" "A 2" "C 5"
# [5,] "E 4" "D 2" "A 5" "C 1" "B 3"
# Superb! You can see that this time the sketch object includes your treatment (the capital letter) 
# and a blocking factor (the number.)


# Graeco-Latin Square with NYC SAT Scores
# Recall that our Latin Square exercise in this chapter tested the effect of our tutoring program, blocked by Borough and Teacher_Education_Level.
# For our Graeco-Latin Square, say we also want to block out the known effect of Homework_Type, which indicates what kind of homework the student was given: individual only, small or large group homework, or some combination. We can add this as another blocking factor to create a Graeco-Latin Square experiment.
# Instructions
# - Use lm() to test the changes in Average_Score_SAT_Math using the nyc_scores_gls data.
# - Tidy nyc_scores_gls_lm with the appropriate broom function.
# - Examine nyc_scores_gls_lm with anova().
head(nyc_scores_gls %>% select(Average_Score_SAT_Math, Tutoring_Program, Borough, Teacher_Education_Level, Homework_Type))
# A tibble: 6 x 5
# Groups:   Borough [2]
#   Average_Score_SA… Tutoring_Program   Borough  Teacher_Educati… Homework_Type  
#               <dbl> <chr>              <chr>    <chr>            <chr>          
# 1               669 SAT Prep Class (s… Bronx    College Student  Small Group    
# 2               403 SAT Prep Class (a… Bronx    BA               Large Group    
# 3               407 One-on-One         Bronx    Grad Student     Individual     
# 4               382 Small Groups (4-6) Bronx    MA               Mix of Large G…
# 5               398 Small Groups (2-3) Bronx    PhD              Mix of Small G…
# 6               433 SAT Prep Class (a… Brooklyn College Student  Individual    
# AK: check if number of levels for all variables equal number of treatments
lapply(nyc_scores_gls %>% select(Tutoring_Program, Borough, Teacher_Education_Level, Homework_Type),FUN = unique)
# $Tutoring_Program
# [1] "SAT Prep Class (school hours)" "SAT Prep Class (after school)"
# [3] "One-on-One"                    "Small Groups (4-6)"           
# [5] "Small Groups (2-3)"           
# $Borough
# [1] "Bronx"         "Brooklyn"      "Manhattan"     "Queens"       
# [5] "Staten Island"
# $Teacher_Education_Level
# [1] "College Student" "BA"              "Grad Student"    "MA"             
# [5] "PhD"            
# $Homework_Type
# [1] "Small Group"                   "Large Group"                  
# [3] "Individual"                    "Mix of Large Group/Individual"
# [5] "Mix of Small Group/Individual"
# Build nyc_scores_gls_lm
nyc_scores_gls_lm <- lm(Average_Score_SAT_Math ~ Tutoring_Program + Borough + Teacher_Education_Level + Homework_Type,
                        data = nyc_scores_gls )
# Tidy the results with broom
tidy(nyc_scores_gls_lm)
# A tibble: 17 x 5
#    term                                     estimate std.error statistic p.value
#    <chr>                                       <dbl>     <dbl>     <dbl>   <dbl>
#  1 (Intercept)                                451.        87.3    5.16   8.62e-4
#  2 Tutoring_ProgramSAT Prep Class (after s…     5.6       67.0    0.0836 9.35e-1
#  3 Tutoring_ProgramSAT Prep Class (school …    84.6       67.0    1.26   2.42e-1
#  4 Tutoring_ProgramSmall Groups (2-3)          -9.80      67.0   -0.146  8.87e-1
#  5 Tutoring_ProgramSmall Groups (4-6)          50.8       67.0    0.759  4.70e-1
#  6 BoroughBrooklyn                            -32.        67.0   -0.478  6.46e-1
#  7 BoroughManhattan                            12.0       67.0    0.179  8.62e-1
#  8 BoroughQueens                               49.8       67.0    0.744  4.78e-1
#  9 BoroughStaten Island                        50.4       67.0    0.753  4.73e-1
# 10 Teacher_Education_LevelCollege Student      56.8       67.0    0.848  4.21e-1
# 11 Teacher_Education_LevelGrad Student         49.4       67.0    0.738  4.82e-1
# 12 Teacher_Education_LevelMA                    5.00      67.0    0.0747 9.42e-1
# 13 Teacher_Education_LevelPhD                  33.0       67.0    0.493  6.35e-1
# 14 Homework_TypeLarge Group                  -105.        67.0   -1.57   1.55e-1
# 15 Homework_TypeMix of Large Group/Individ…   -68.6       67.0   -1.02   3.36e-1
# 16 Homework_TypeMix of Small Group/Individ…   -58.8       67.0   -0.878  4.05e-1
# 17 Homework_TypeSmall Group                   -37.2       67.0   -0.556  5.94e-1
# Examine the results with anova
anova(object = nyc_scores_gls_lm)
# Analysis of Variance Table
# Response: Average_Score_SAT_Math
# Df Sum Sq Mean Sq F value Pr(>F)
# Tutoring_Program         4  32113  8028.1  0.7162 0.6040
# Borough                  4  24509  6127.2  0.5466 0.7071
# Teacher_Education_Level  4  13109  3277.3  0.2924 0.8750
# Homework_Type            4  30279  7569.8  0.6753 0.6277
# Residuals                8  89677 11209.6 
# Question
# At the 0.05 significance level, do we have evidence to believe the tutoring program has an effect on math SAT scores, when blocked by Borough, Teacher_Education_Level, and Homework_Type?
# Possible Answers
# Nope! Given the p-value, we have no reason to reject the null hypothesis. +
# Yes! Given the p-value, we have reason to believe that the tutoring program had an effect on the Math score.
# Bravo! It seems that here, when blocked out by all the other factors, our Tutoring program has no effect on the Math score.

# NYC SAT Scores Factorial EDA
# Let's do some more EDA before we dive into the analysis of our factorial experiment.
# Let's test the effect of Percent_Black_HL, Percent_Tested_HL, and Tutoring_Program on the outcome, Average_Score_SAT_Math. The HL stands for high-low, where a 1 indicates respectively that less than 50% of Black students or that less than 50% of all students in an entire school were tested, and a 2 indicates that greater than 50% of either were tested.
# Build a boxplot of each factor vs. the outcome to have an idea of which have a difference in median by factor level (ultimately, mean difference is what's tested.) The nyc_scores dataset has been loaded for you.
# Instructions
# Load ggplot2. Create a boxplot of the outcome versus Tutoring_Program.
# Load ggplot2
library(ggplot2)
# Build the boxplot for the tutoring program vs. Math SAT score
ggplot(nyc_scores, aes(x = Tutoring_Program, y = Average_Score_SAT_Math)) + 
  geom_boxplot()
# http://joxi.ru/bmoDozVCOq9al2
# Using ggplot2, create a boxplot of the outcome versus Percent_Black_HL.
# Build the boxplot for the percent black vs. Math SAT score
ggplot(nyc_scores, aes(x = Percent_Black_HL, y = Average_Score_SAT_Math)) + 
  geom_boxplot()
# http://joxi.ru/VrwKo8lCo38ZDr
# Using ggplot2, create a boxplot of the outcome versus Percent_Tested_HL.
# Build the boxplot for percent tested vs. Math SAT score
ggplot(nyc_scores, aes(Percent_Tested_HL, Average_Score_SAT_Math)) + 
  geom_boxplot()
# http://joxi.ru/5mdlkYMTqLexXm
# Excellent! Now, let's move on to the analysis of these factors on the score.

# 11 Factorial experiments.mp4
# Factorial Experiment with NYC SAT Scores
# Now we want to examine the effect of tutoring programs on the NYC schools' SAT Math score. As noted in the last exercise: the variable Tutoring_Program is simply yes or no, depending on if a school got a tutoring program implemented. For Percent_Black_HL and Percent_Tested_HL, HL stands for high/low. A 1 indicates less than 50% Black students or overall students tested, and a 2 indicates greater than 50% of both.
# Remember that because we intend to test all of the possible combinations of factor levels, we need to write the formula like: outcome ~ factor1 * factor2 * factor3.
# Instructions
# Use aov() to create a model to test how Percent_Tested_HL, Percent_Black_HL, and Tutoring_Program affect the outcome Average_Score_SAT_Math.
# Save the outcome as a model object, nyc_scores_factorial, and examine this with tidy().
# Create nyc_scores_factorial and examine the results
nyc_scores_factorial <- 
  aov(formula = Average_Score_SAT_Math ~ Percent_Tested_HL * Percent_Black_HL * Tutoring_Program, data = nyc_scores)
tidy(nyc_scores_factorial)
# A tibble: 8 x 6
# term                                    df    sumsq meansq statistic   p.value
# <chr>                                <dbl>    <dbl>  <dbl>     <dbl>     <dbl>
# 1 Percent_Tested_HL                        1  476640. 4.77e5   145.     2.09e-28
# 2 Percent_Black_HL                         1  155891. 1.56e5    47.5    2.41e-11
# 3 Tutoring_Program                         1    2959. 2.96e3     0.902  3.43e- 1
# 4 Percent_Tested_HL:Percent_Black_HL       1   87896. 8.79e4    26.8    3.76e- 7
# 5 Percent_Tested_HL:Tutoring_Program       1    1599. 1.60e3     0.487  4.86e- 1
# 6 Percent_Black_HL:Tutoring_Program        1    1726. 1.73e3     0.526  4.69e- 1
# 7 Percent_Tested_HL:Percent_Black_HL:…     1    4996. 5.00e3     1.52   2.18e- 1
# 8 Residuals                              367 1204547. 3.28e3    NA     NA  
# AK: without interaction
nyc_scores_factorial <- 
  aov(formula = Average_Score_SAT_Math ~ Percent_Tested_HL + Percent_Black_HL + Tutoring_Program, data = nyc_scores)
tidy(nyc_scores_factorial)
# A tibble: 4 x 6
#   term                 df    sumsq  meansq statistic   p.value
#   <chr>             <dbl>    <dbl>   <dbl>     <dbl>     <dbl>
# 1 Percent_Tested_HL     1  476640. 476640.   136.     5.60e-27
# 2 Percent_Black_HL      1  155891. 155891.    44.5    9.42e-11
# 3 Tutoring_Program      1    2959.   2959.     0.844  3.59e- 1
# 4 Residuals           371 1300762.   3506.    NA     NA   
# Whoo! We can see from the results that we can reject the null hypothesis that there is no 
# difference in score based on tutoring program availability. We can also see from the low 
# p-values that there are some interaction effects between the Percent Black and Percent Tested 
# and the tutoring program. Next we need to check the model.

# Evaluating the NYC SAT Scores Factorial Model
# We've built our model, so we know what's next: model checking! We need to examine both if our outcome and our model residuals are normally distributed. We'll check the normality assumption using shapiro.test(). A low p-value means we can reject the null hypothesis that the sample came from a normally distributed population.
# Let's carry out the requisite model checks for our 2^k factorial model, nyc_scores_factorial, which has been loaded for you.
# Instructions
# Test the outcome Average_Score_SAT_Math from nyc_scores for normality using shapiro.test().
# Set up a 2 by 2 grid for plots and plot the nyc_scores_factorial model object to create the residual plots.
# Use shapiro.test() to test the outcome
shapiro.test(x = nyc_scores$Average_Score_SAT_Math) 
# Shapiro-Wilk normality test
# data:  nyc_scores$Average_Score_SAT_Math
# W = 0.84672, p-value < 2.2e-16
# Plot nyc_scores_factorial to examine residuals
# AK: let's check visually normality
ggplot(nyc_scores, aes(x = Average_Score_SAT_Math)) + geom_density()
# http://joxi.ru/Y2LdjY0txDEJW2
# Warning message: Removed 60 rows containing non-finite values (stat_density).
par(mfrow = c(2,2))
plot(nyc_scores_factorial)
# Brilliant! The model appears to be fairly well fit, though our evidence indicates the score may 
# not be from a normally distributed population. Looking at the Q-Q plot, we can see that towards 
# the higher end, the points are not on the line, so we may not be dealing with normality here. 
# If we had more time, we might consider a transformation on the outcome to move towards normality.





















