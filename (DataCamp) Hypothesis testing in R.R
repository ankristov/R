library(dplyr)
glimpse(stack_overflow)

tibble(x = seq(-4,4,0.01)) %>% ggplot(aes(x)) + stat_function(fun = dnorm) + ylab("PDF(x)")

# Uses of A/B testing
# In the video, you saw how Electronic Arts used A/B testing on their website when launching SimCity 5. 
# One version of the page showed an advertisement for a discount, and one version did not. Half the users 
# saw one version of the page, and the other half saw the second version of the page.
# What is the main reason to use an A/B test?
# Answer the question
# It lets users vote on their preferred web page.
# It allows you to only give discounts to half your users.
# It is a method used to directly determine the sample size needed for your analysis.
# It provides a way to check outcomes of competing scenarios and decide which way to proceed. +
# It reduces the number of errors in production.
# Ace A/B testing! A/B testing let's you compare scenarios to see which best achieves some goal.

# 1 To the lab for testing.mp4
# Calculating the sample mean
# The late_shipments dataset contains supply chain data on the delivery of medical supplies. Each row 
# represents one delivery of a part. The late columns denotes whether or not the part was delivered late. 
# A value of "Yes" means that the part was delivered late, and a value of "No" means the part was delivered 
# on time.
# Let's begin our analysis by calculating a point estimate (sample statistic), namely the proportion of late 
# shipments.
# late_shipments is available; dplyr is loaded.
# Instructions
# Use View() to view the late_shipments dataset.
# Calculate the proportion of late shipments in the sample. That is, the mean cases where the late 
# column is "Yes".
# View the late_shipments dataset
View(late_shipments)
head(late_shipments)
# Calculate the proportion of late shipments
late_prop_samp <- mean(late_shipments$late == "Yes")
# See the results
late_prop_samp
# [1] 0.067
# Cool calculating! The proportion of late shipments is 0.067, or 6.7%.

# Calculating a z-score
# Since variables have arbitrary ranges and units, we need to standardize them. For example, it would be silly 
# if a hypothesis test gave a different answer if your variables were in Euros instead of US dollars. 
# Standardization avoids that.
# One standardized value of interest in a hypothesis test is called a z-score. To calculate it, we need 
# three numbers: the sample statistic (point estimate), the hypothesized statistic, and the standard error 
# of the statistic (which we estimate from the bootstrap distribution).
# The sample statistic is late_prop_samp.
# late_shipments_boot_distn is a bootstrap distribution of the proportion of late shipments. The proportion 
# of late shipments statistic is in the late_prop column.
# late_prop_samp and late_shipments_boot_distn are available; dplyr is loaded.
# Instructions
# Hypothesize that the proportion of late shipments is 6%.
# Calculate the standard error. That is, the standard deviation of the bootstrap distribution.
# Calculate the z-score.
late_prop_samp
# [1] 0.067
head(late_shipments_boot_distn)
#   replicate late_prop
# 1         1     0.065
# 2         2     0.081
# 3         3     0.069
# 4         4     0.058
# 5         5     0.065
dim(late_shipments_boot_distn)
# [1] 5000    2
s <- late_shipments %>% slice_sample(prop = 0.5, replace = T)
dim(s)
# Hypothesize that the proportion is 6%
late_prop_hyp <- 0.06
# Calculate the standard error
std_error <- sd(late_shipments_boot_distn$late_prop)
std_error
# [1] 0.007929028
# AK: here how we calculate std error from conspect on Hypothesis testing (A-100): p*(1-p)/n
sqrt(0.067 * (1- 0.067) / 1000)
# [1] 0.00790639
# Find z-score of late_prop_samp
z_score <- (late_prop_samp - late_prop_hyp) / std_error
# See the results
z_score
# [1] 0.8828321
# Zesty z-scoring! The z-score is a standardized measure of the difference between the sample statistic and the hypothesized statistic.

# 2 A tail of two z's.mp4
# Criminal trials and hypothesis tests
# In the video, you saw how hypothesis testing follows a similar process to criminal trials.
# Which of the following correctly matches up a criminal trial with properties of a hypothesis test?
# Possible Answers
# - Just as with criminal trials, there are more than two possible results at the conclusion of a hypothesis test.
# - Just as the defendant is initially assumed not guilty, the null hypothesis is first assumed to be true. +
# - The defendant can be both guilty and not guilty for the crime charged and both the null and alternative hypotheses can be chosen at the end of the test.
# - Just as a court can decline to give a verdict, it's possible for neither the null hypothesis nor the alternative hypothesis to be chosen at the end of the test.
# - Just as the defendant is initially assumed guilty, the alternative hypothesis is first assumed to be true.
# I pronounce you not guilty! It's sometimes helpful to think of your hypothesis test as being a trial of the statistic.

# Top tail choices! The tails of the distribution that are relevant depend on whether the alternative hypothesis refers to "greater than", "less than", or "differences between".
# http://joxi.ru/Dr8GKyEiKvNzOA

# Calculating p-values
# In order to determine whether to choose the null hypothesis or the alternative hypothesis, you need to 
# calculate a p-value from the z-score.
# Let's return to the late shipments dataset and the proportion of late shipments.
# The null hypothesis, H0, is that the proportion of late shipments is six percent. The alternative hypothesis,
# HA, is that the proportion of late shipments is greater than six percent.
# The observed sample statistic, late_prop_samp, the null hypothesis statistic, late_prop_hyp (6%), and the 
# bootstrap standard error, std_error are available.
# Question
# What type of test should be used for this alternative hypothesis?
# Possible Answers
# - Two-tailed
# - Left-tailed
# - Right-tailed +
# - It doesn't matter; any one will do.
# - A hypothesis test isn't appropriate to answer this question.
late_prop_samp
# [1] 0.067
mean(late_shipments$late == "Yes")
# [1] 0.067
late_prop_hyp
# [1] 0.06
std_error
# [1] 0.007929028
# Calculate the z-score of late_prop_samp
z_score <- (late_prop_samp - late_prop_hyp) / std_error
z_score
# [1] 0.8828321
# Calculate the p-value
p_value <- pnorm(q = z_score, lower.tail = F)
# See the result
p_value 
# [1] 0.1886635
# Perfect p-value! The p-value is calculated by transforming the z-score with the standard normal cumulative distribution function.

# 3 Statistically significant other.mp4
# Decisions from p-values
# The p-value is a measure of the amount of evidence to reject the null hypothesis or not. By comparing the 
# p-value to the significance level, α, you can make a decision about which hypothesis to support.
# Which of the following is the correct conclusion from the decision rule for a significance level α?
# Answer the question
# Possible Answers
# - If the p ≤ α, reject HA.
# - If the p ≥ α, reject HA.
# - If the p ≤ α, don't reject H0.
# - If the p ≤ α, reject H0. +
# Delightful decision-making. If the p-value is less than or equal to the significance level, you reject the null hypothesis.

# Calculating confidence intervals

# If you give a single estimate of a sample statistic, you are bound to be wrong by some amount. For example, 
# the hypothesized proportion of late shipments was 6%. Even if evidence suggests the null hypothesis that 
# the proportion of late shipments is equal to this, for any new sample of shipments, the proportion is 
# likely to be a little different. Consequently, it's a good idea to state a confidence interval. That is, 
# you say "we are 95% 'confident' the proportion of late shipments is between A and B" (for some value of 
# A and B).
# Sampling in R demonstrated two methods for calculating confidence intervals. Here, you'll use quantiles 
# of the bootstrap distribution to calculate the confidence interval.
# late_prop_samp and late_shipments_boot_distn are available; dplyr is loaded.
# Instructions
# Summarize the prop_late_shipments column of late_shipments_boot_distn to calculate the 95% confidence 
# interval using the quantile method. Label the lower and upper CI values lower and upper.
# Summarize the prop_late_shipments column of late_shipments_boot_distn to calculate the 95% confidence 
# interval using the quantile method. Label the lower and upper CI values lower and upper.
# head(late_shipments_boot_distn)
#   replicate prop_late_shipments
# 1         1               0.065
# 2         2               0.081
# 3         3               0.069
# 4         4               0.058
# 5         5               0.065
# Calculate 95% confidence interval using quantile method
conf_int_quantile <- late_shipments_boot_distn %>%
  summarise(lower = quantile(prop_late_shipments, prob = 0.025),
            upper = quantile(prop_late_shipments, prob = 0.975)        
  )

# See the result
conf_int_quantile
#   lower    upper
# 1 0.052 0.083025

# Question
# Does the confidence interval match up with the conclusion to stick with the original assumption that 6% is 
# a reasonable value for the unknown population parameter?
# - Yes, since 0.06 is included in the 95% confidence interval and we failed to reject H0 due to a large p-value, the results are similar. +
# - No, since 0.06 is included in the 95% confidence interval and we should have rejected H0 due to a large p-value, the results do not match.
# - No, there is no relationship between confidence intervals and hypothesis tests.
# Cool and confident! When you have a confidence interval width equal to one minus the significance level, 
# if the hypothesized population parameter is within the confidence interval, you should fail to reject the 
# null hypothesis.

# Type I and type II errors
# For hypothesis tests and for criminal trials, there are two states of truth, and two possible outcomes. 
# Two combinations are correct test outcomes verdicts, and there are two ways it can go wrong.
# The errors are known as false positives (or "type I errors"), and false negatives (or "type II errors").
# Instructions
# Match the scenarios below to the appropriate error or to not an error for correct decisions.
# http://joxi.ru/52aG1zYile60or
# Distinguished decision-making! There are two ways to get the right decision, and two different ways you can make the wrong decision.

# 4 Is this some kind of test statistic?.mp4
# Two sample mean test statistic
# The hypothesis test for determining if there is a difference between the means of two populations uses a 
# different type of test statistic to the z-scores you saw in Chapter one. It's called "t", and can be 
# calculated from three values from each sample using this equation.
# t = (xchild − xadult) / sqrt(schild^2/nchild + sadult^2/nadult)
# While trying to determine why some shipments are late, you may wonder if the weight of the shipments that were 
# late is different from the weight of the shipments that were on time. The late_shipments dataset has been split 
# into a "yes" group, where late == "Yes" and a "no" group where late == "No". The weight of the shipment is given 
# in the weight_kilograms variable.
# For convenience, the sample means for the two groups are available as xbar_no and xbar_yes. The sample standard deviations are s_no and s_yes. The sample sizes are n_no and n_yes.
# Instructions
# Calculate the numerator of the test statistic.
# Calculate the denominator of the test statistic.
# Use those two numbers to calculate the test statistic.
late_shipments %>% group_by(late) %>% summarise(
    mean_weight = mean(weight_kilograms), 
    std_weight = sd(weight_kilograms), 
    n = n(), 
    median_weight = median(weight_kilograms))
# A tibble: 2 × 5
#  late  mean_weight std_weight      n median_weight
#  <chr>       <dbl>      <dbl>  <int>         <dbl>
# 1 No          2082.      6095.   933           759
# 2 Yes         2378.      2243.    67          1352
xbar_no
# [1] 2082.171
xbar_yes
# [1] 2377.821
s_no
# [1] 6094.609
s_yes
# [1] 2242.502
library(ggplot2)
ggplot(late_shipments) + geom_density(aes(x = weight_kilograms, color = late), alpha = 0.5) + xlim(c(0,10000))
# http://joxi.ru/n2Yy8a5ukyYLa2
# Calculate the numerator of the test statistic
numerator <- xbar_no - xbar_yes
# Calculate the denominator of the test statistic
denominator <- sqrt((s_yes^2/n_yes) + (s_no^2/n_no))
# Calculate the test statistic
t_stat <- numerator / denominator
# See the result
t_stat
# [1] -0.872321
# T-rrific! When testing for differences between means, the test statistic is called 't' rather than 'z', 
# ?and can be calculated using six numbers from the samples. Here, the value is about -0.9

# Hypothesis testing workflow
# You've seen the hypothesis testing workflow for the one sample case where you compared a sample mean to a 
# hypothesized value, and the two sample case where you compared two sample means. In both cases, the workflow shared 
# common steps.
# Instructions
# Place the hypothesis testing workflow steps in order from first to last.
# 1 Identify population parameter that is hypothesized about
# 2 Specify the null and alternative hypotheses
# 3 Determine (standardized) test statistics and corresponding null distribution
# 4 Conduct hypothesis test in R
# 5 Measure evidence against the null hypothesis
# 6 Make a decision comparing evidence to significance level
# 7 Interpret the results in the context of the original problem
# Wild workflow! Regardless of the type of hypothesis test you are performing, it will have a workflow that follows this format.

# 5 Time for t.mp4
# Now we are calculating means rather than proportions, the z-score is replaced with a t test statistic. This is the value calculated in the previous video. The calculation also needs the degrees of freedom, which is the number of observations in both groups, minus two. In the previous slides, we used an approximation using sample information (not bootstrapping) for the test statistic standard error. A consequence of this is that to calculate the p-value, we need to transform the test statistic using the t-distribution CDF instead of the normal distribution CDF. Using this approximation adds more uncertainty and that's why this is a t instead of a z problem. The t distribution allows for more uncertainty when using multiple estimates in a single statistic calculation. 
# Why is t needed?
# The process for calculating p-values is to start with the sample statistic, standardize it to get a test statistic, then transform it via a cumulative distribution function. In Chapter 1, that final transformation was denoted z, and the CDF transformation used the (standard normal) z-distribution. In the last video, the test statistic was denoted t, and the transformation used the t-distribution.
# In which hypothesis testing scenario is a t-distribution needed instead of the z-distribution?
# Answer the question
# Possible Answers
# - The t-distribution is just another name for the z-distribution so they can be used interchangeably.
#   No. The z-distribution is a normal distribution with mean zero and standard deviation one. That's not the same thing as a t-distribution.
# - The t-distribution is the same thing as the z-distribution for very small sample sizes.
#   No. t-distributions are like normal distributions with a correction for degrees of freedom, but that doesn't explain when you need to use it.
# - When a sample standard deviation is used in estimating a standard error. +
#   Terrific t! Using a sample standard deviation to estimate the standard error is computationally easier than using bootstrapping. However, to correct for the approximation, you need to use a t-distribution when transforming the test statistic to get the p-value.
# - When you are comparing the means of two samples, rather than comparing a single sample mean to a value.
#   No. Although we used a normal distribution CDF in the one sample case and a t-distribution CDF in the two sample case, the reason for this difference wasn't the number of sample means.

# The t-distribution
# The t-distribution is used to calculate the p-value from the test statistic, and having a sense of how the 
# PDF and CDF look can help you understand this calculation. It has two parameters: the degrees of freedom, 
# and the non-centrality parameter.
# The plots show the PDF and CDF for a t-distribution (solid black line), and for comparison show a normal 
# distribution with the same mean and variance (gray dotted line).
# Which statement about the the t-distribution is true?
# http://joxi.ru/KAgbKoGT5Vg6D2
# Instructions
# - Like the normal distribution, the PDF of a distribution is always symmetric.
# - As you increase the degrees of freedom, the tails of the t-distribution get fatter.
# - As you increase the degrees of freedom, the t-distribution PDF and CDF curves get closer to those of a normal distribution. +
# - As you increase the non-centrality, the t-distribution PDF and CDF curves get closer to those of a normal distribution.
x <- seq(from = -6, to = 6, by = 0.01)
y_t_pdf <- dt(x = x, df = 1)
y_n_pdf <- dnorm(x = x, mean = 0, sd = 1)
y_t_cdf <- pt(q = x, df = 1, lower.tail = T)
y_n_cdf <- pnorm(q = x, mean = 0, sd = 1, lower.tail = T)
df <- tibble(x = x, y_t_pdf = y_t_pdf, y_n_pdf = y_n_pdf, y_t_cdf = y_t_cdf, y_n_cdf = y_n_cdf)
ggplot(df) + geom_line(aes(x, y_t_pdf)) + geom_line(aes(x, y_n_pdf), lty = 2)
ggplot(df) + geom_line(aes(x, y_t_cdf)) + geom_line(aes(x, y_n_cdf), lty = 2)
# Tip-top t! The normal distribution is essentially a t-distribution with infinite degrees of freedom.

# From t to p
# Previously, you calculated the test statistic for the two-sample problem of whether the mean weight of 
# shipments is lower for shipments that weren't late (late == "No") compared to shipments that were 
# late (late == "Yes"). In order to make decisions about it, you need to transform the test statistic 
# with a cumulative distribution function to get a p-value.
# Recall the hypotheses:
# H0: The mean weight of shipments that weren't late is the same as the mean weight of shipments that 
# were late.
# HA: The mean weight of shipments that weren't late is less than the mean weight of shipments that were 
# late.
# The test statistic, t_stat, is available, as are the samples sizes for each group, n_no and n_yes. 
# Use a significance level of alpha = 0.05.
# Instructions
# Question
# What type of test does the alternative hypothesis indicate that we need?
# Possible Answers
# - Two-tailed
# - Left-tailed +
# - Right-tailed
# Calculate the degrees of freedom for the test.
# Use the test statistic, t_stat, to calculate the p-value.
# Calculate the degrees of freedom
degrees_of_freedom <- n_no + n_yes - 2
t_stat
# [1] -0.872321
# Calculate the p-value from the test stat
p_value <- pt(q = t_stat, df = degrees_of_freedom)
# See the result
p_value
# [1] 0.1916215

# Question
# What decision should you make based on the results of the hypothesis test?
# - Fail to reject the null hypothesis.
# - Reject the null hypothesis.
# - You can't conclude anything from this hypothesis test.
# Perspicacious p-value predictions! When the standard error is estimated from the sample standard 
# deviation and sample size, the test statistic is transformed into a p-value using the t-distribution.

# 6 Pairing is caring.mp4
# Is pairing needed?
# t-tests are used to compare two sample means. However, the test involves different calculations 
# depending upon whether the two samples are paired or not. To make sure you use the correct version 
# of the t-test, you need to be able to identify pairing.
# Instructions
# Match the problem description with whether a paired t-test or non-paired t-test should be conducted.
# http://joxi.ru/LmGYVeDTBpRoGA
# Daring pairing! If you have repeated observations of something, then those observations form pairs.

# Visualizing the difference
# Before you start running hypothesis tests, it's a great idea to perform some exploratory data analysis. 
# That is, calculating summary statistics and visualizing distributions.
# Here, you'll look at the proportion of county-level votes for the Democratic candidate in 2012 and 2016, 
# dem_votes_potus_12_16. Since the counties are the same in both years, these samples are paired. The 
# columns containing the samples are dem_percent_12 and dem_percent_16.
# dem_votes_potus_12_16 is available; dplyr and ggplot2 are loaded.
# Instructions
# View the dem_votes_potus_12_16 dataset.
# Mutate dem_votes_potus_12_16 to add a diff column containing the percentage of votes for the democratic candidate in 2012 minus the votes for the democratic candidate in 2016.
# View the dem_votes_potus_12_16 dataset
View(dem_votes_potus_12_16)
# Calculate the differences from 2012 to 2016
sample_dem_data <- dem_votes_potus_12_16 %>% mutate(diff = dem_percent_12 - dem_percent_16)
# See the result
sample_dem_data
#              state           county dem_percent_12 dem_percent_16        diff
# 1          Alabama          Bullock      76.305900      74.946921  1.35897859
# 2          Alabama          Chilton      19.453671      15.847352  3.60631931
# 3          Alabama             Clay      26.673672      18.674517  7.99915466
# 4          Alabama          Cullman      14.661752      10.028252  4.63350001
# 5          Alabama         Escambia      36.915731      31.020546  5.89518508
# Summarize sample_dem_data to calculate the mean of the diff column as xbar_diff and the standard 
# deviation of that column as s_diff.
# Find mean and standard deviation of differences
diff_stats <- sample_dem_data %>% summarise(xbar_diff = mean(diff), s_diff = sd(diff))
# See the result
diff_stats
#   xbar_diff   s_diff
# 1  6.829313 5.040139
# Using sample_dem_data, plot diff as a histogram with binwidth 1.
# Using sample_dem_data, plot diff as a histogram
ggplot(sample_dem_data) + geom_histogram(aes(x = diff), binwidth = 1)
# http://joxi.ru/v290QPlT4OG6dm
# Delightful difference discovery! Notice that the majority of the histogram lies to the right of zero.

# Using t.test()
# Manually calculating test statistics and transforming them with a CDF to get a p-value is a lot of 
# effort to do every time you need to compare two sample means. The comparison of two sample means is 
# called a t-test, and R has a t.test() function to accomplish it. This function provides some 
# flexibility in how you perform the test.
# As in the previous exercise, you'll explore the difference between the proportion of county-level 
# votes for the Democratic candidate in 2012 and 2016.
# sample_dem_data is available, and has columns diff, dem_percent_12, and dem_percent_16.
# Instructions
# Conduct a t-test on the sample differences (the diff column of sample_dem_data). Use an appropriate 
# alternative hypothesis chosen from "two.sided", "less", and "greater".
# Conduct a t-test on diff
test_results <- t.test(
  x = sample_dem_data$diff,
  alternative = "greater",
  pared = True
)
# See the results
test_results
# 	One Sample t-test
# data:  sample_dem_data$diff
# t = 30.298, df = 499, p-value < 2.2e-16
# alternative hypothesis: true mean is greater than 0
# 95 percent confidence interval:
#   6.45787     Inf
# sample estimates:
#   mean of x 
# 6.829313 

# Conduct a paired test on the democratic votes in 2012 and 2016 (the dem_percent_12 and dem_percent_16 
# columns of sample_dem_data). Use an appropriate alternative hypothesis.
head(sample_dem_data)
# state   county dem_percent_12 dem_percent_16     diff
# 1 Alabama  Bullock       76.30590       74.94692 1.358979
# 2 Alabama  Chilton       19.45367       15.84735 3.606319
# 3 Alabama     Clay       26.67367       18.67452 7.999155
# 4 Alabama  Cullman       14.66175       10.02825 4.633500
# 5 Alabama Escambia       36.91573       31.02055 5.895185
# 6 Alabama  Fayette       22.86685       16.51109 6.355759
head(sample_dem_data)
# Conduct a paired t-test on dem_percent_12 and dem_percent_16
test_results <- t.test(
  x = sample_dem_data$dem_percent_12,
  y = sample_dem_data$dem_percent_16,
  alternative = "greater",
  paired = T
)
# See the results
test_results
# Question
# What's the correct decision from the t-test, assuming α = 0.01?
# Possible Answers
# - Fail to reject the null hypothesis.
# - Reject the null hypothesis. +
# - You can't conclude anything from this hypothesis test

# Question
# Compare the paired t-test to an (inappropriate) unpaired test on the same data. How does the p-value change?
# The p-value from the unpaired test is smaller than the p-value from the paired test.
# The p-value from the unpaired test is equal to the p-value from the paired test.
# The p-value from the unpaired test is greater than than the p-value from the paired test.
# The paired t-test
t.test(
  x = sample_dem_data$dem_percent_12,
  y = sample_dem_data$dem_percent_16,
  paired = TRUE,
  alternative = "greater",
  mu = 0
)
# 1 Paired t-test
# data:  sample_dem_data$dem_percent_12 and sample_dem_data$dem_percent_16
# t = 30.298, df = 499, p-value < 2.2e-16
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#  6.45787     Inf
# sample estimates:
# mean of the differences 
# 6.829313 
# 2 The unpaired t-test
t.test(
  x = sample_dem_data$dem_percent_12,
  y = sample_dem_data$dem_percent_16,
  paired = FALSE,
  alternative = "greater",
  mu = 0
)
# Welch Two Sample t-test
# data:  sample_dem_data$dem_percent_12 and sample_dem_data$dem_percent_16
# t = 7.1816, df = 997.19, p-value = 6.732e-13
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   5.263684      Inf
# sample estimates:
#   mean of x mean of y 
# 38.78294  31.95363 
# Paired t-test party! Using t.test() lets you avoid manual calculation to run your test. When you have 
# paired data, a paired t-test is preferable to the unpaired version because it gives lower p-values, 
# which reduces the chance of a false negative error.

# Visualizing many categories
# So far in this chapter, we've only considered the case of differences in a numeric variable between 
# two categories. Of course, many datasets contain more categories. Before you get to conducting tests 
# on many categories, it's often helpful to perform exploratory data analysis. That is, calculating 
# summary statistics for each group and visualizing the distributions of the numeric variable for 
# each category using box plots.
# Here, we'll return to the late shipments data, and how the price of each package (pack_price) varies 
# between the three shipment modes (shipment_mode): "Air", "Air Charter", and "Ocean".
# late_shipments is available; dplyr and ggplot2 are loaded.
head(late_shipments)
# Using late_shipments, group by shipment mode, and calculate the mean and std dev of pack price
late_shipments %>% 
  group_by(shipment_mode) %>% 
  summarise(xbar_pack_price = mean(pack_price), s_pack_price = sd(pack_price))
# A tibble: 3 × 3
#   shipment_mode xbar_pack_price s_pack_price
#   <chr>                   <dbl>        <dbl>
#  1 Air                     41.8         52.4 
# 2 Air Charter              3.39         1.34
# 3 Ocean                    7.82         9.86
late_shipments %>% group_by(shipment_mode) %>% summarise(
    xbar = mean(pack_price), 
    sd = sd(pack_price), 
    n = n(), 
    q25 = quantile(pack_price, 0.25),
    q50 = quantile(pack_price, 0.50), 
    q75 = quantile(pack_price, 0.75))
# A tibble: 3 × 7
# shipment_mode    xbar    sd     n   q25   q50   q75
# <chr>           <dbl> <dbl> <int> <dbl> <dbl> <dbl>
# 1 Air            41.8  52.4    908  8    27    72   
# 2 Air Charter    3.39  1.34     6  2.20  3.42  4.62
# 3 Ocean          7.82  9.86    84  2.87  6.63  8.64
ggplot(late_shipments) + geom_density(aes(x = pack_price, fill = shipment_mode, alpha = 0.5)) + xlim(c(0,150))
# http://joxi.ru/E2pYv1gTv8G6nA

# Using late_shipments, plot pack_price versus shipment_mode as a box plot with flipped x and y coordinates.
# Using late_shipments, plot pack_price vs. shipment_mode
# as a box plot with flipped x and y coordinates
late_shipments %>%
  ggplot() + geom_boxplot(aes(y = pack_price, x = shipment_mode, fill = shipment_mode)) + coord_flip()
# http://joxi.ru/KAgbKoGT5VKyk2
# Beautiful boxplotting! There certainly looks to be a difference in the pack price between each of the three shipment modes. Do you think the differences are statistically significant?

# Run a linear regression of pack_price versus shipment_mode using the late_shipments dataset. The formula takes the form response ~ explanatory.
# Run a linear regression of pack price vs. shipment mode 
mdl_pack_price_vs_shipment_mode <- lm(pack_price ~ shipment_mode, data = late_shipments)
# See the results
summary(mdl_pack_price_vs_shipment_mode)
# Call:
#   lm(formula = pack_price ~ shipment_mode, data = late_shipments)
# Residuals:
#   Min     1Q Median     3Q    Max 
# -41.83 -33.25 -11.01  28.18 308.18 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                41.825      1.664  25.136  < 2e-16 ***
#   shipment_modeAir Charter  -38.431     20.537  -1.871   0.0616 .  
#   shipment_modeOcean        -34.009      5.718  -5.948 3.76e-09 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 50.14 on 995 degrees of freedom
# Multiple R-squared:  0.03713,	Adjusted R-squared:  0.0352 
# F-statistic: 19.19 on 2 and 995 DF,  p-value: 6.668e-09

# Perform ANOVA on mdl_pack_price_vs_shipment_mode.
anova(mdl_pack_price_vs_shipment_mode)
# Analysis of Variance Table
# Response: pack_price
#                Df  Sum Sq Mean Sq F value    Pr(>F)    
# shipment_mode   2   96468   48234  19.187 6.668e-09 ***
# Residuals     995 2501359    2514                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Question
# Assuming a significance level of 0.1, should you reject the null hypothesis that there is a difference 
# between pack prices between the shipment modes? (AK: mistake: H0 - there is no difference in prices between shipment modes)
# - Yes. The p-value is greater than or equal to the significance level, so the null hypothesis should be rejected.
# - Yes. The p-value is less than or equal to the significance level, so the null hypothesis should be rejected. +
# - No. The p-value is greater than or equal to the significance level, so the null hypothesis should fail to be rejected.
# - No. The p-value is less than or equal to the significance level, so the null hypothesis should fail to be rejected.
# Amazing ANOVA! There is a significant difference in pack prices between the shipment modes. However, 
# we don't know which shipment modes this applies to.
# Pairwise t-tests

# The ANOVA test didn't tell us which categories of shipment mode had significant differences in pack 
# prices. To pinpoint which categories had differences, we could instead use pairwise t-tests (compaires each possible pair of the categorical variable).
# late_shipments is available.
# Instructions
# Perform pairwise t-tests on late_shipments's pack_price variable, grouped by shipment_mode. Don't do 
# any p-value adjustment, and keep the default "two.sided" alternative hypothesis specification.
# Perform pairwise t-tests on pack price, grouped by shipment mode, no p-value adjustment
test_results <- pairwise.t.test(x = late_shipments$pack_price, g = late_shipments$shipment_mode, p.adjust.method = "none", alternative = "two.sided")    
# See the results
test_results
# Pairwise comparisons using t tests with pooled SD 
# data:  late_shipments$pack_price and late_shipments$shipment_mode 
#             Air     Air Charter
# Air Charter 0.12    -          
# Ocean       1.1e-08 0.83       
# P value adjustment method: holm

# Modify the pairwise t-tests to use Bonferroni p-value adjustment
test_results <- pairwise.t.test(
  late_shipments$pack_price,
  late_shipments$shipment_mode,
  p.adjust.method = "bonferroni"
)
# See the results
test_results
# Pairwise comparisons using t tests with pooled SD 
# data:  late_shipments$pack_price and late_shipments$shipment_mode 
#             Air     Air Charter
# Air Charter 0.18    -          
# Ocean       1.1e-08 1.00       
# P value adjustment method: bonferroni 
# Question
# Assuming a significance level of 0.1, for which pairs of shipment modes should you reject the null 
# hypothesis that the pack prices are equal?
# Possible Answers
# - "Ocean" and "Air Charter"; "Air Charter" and "Air".
# - "Ocean" and "Air"; "Air Charter" and "Air".
# - "Ocean" and "Air" only. +
# - "Ocean" and "Air Charter" only
# Pairwise perfection! Pairwise t-tests give you more information than ANOVA about where the differences 
# between categories lie, but since you are conducting more tests, the p-values need to be adjusted, 
# making it more difficult to see a significanct difference.

# 8 Difference strokes for proportions, folks.mp4
# t for proportions?
# Some of the hypothesis tests in this course have used a z test statistic, and some have used a 
# t test statistic. To get the correct p-value, you need to use the right type of test statistic.
# Do tests of proportion(s) use a z or a t test statistic and why?
# Answer the question
# - z: The test statistic for proportion(s) has only one estimate of a parameter instead of two. +
# - t: There are two estimates used for unknown values in the test statistic for proportion(s).
# - z: Since the population standard deviation is always known for proportion(s), we always compute z-scores.
# - t: Proportions are ratios, so you need to estimate the numerator and the denominator.
# Zipadeedoodah for z-scores! The t-test is needed for tests of mean(s) since you are estimating two unknown quantities, which leads to more variability.

# In Chapter 1, you calculated a p-value for a test hypothesizing that the proportion of late shipments 
# was greater than 6%. In that chapter, you used a bootstrap distribution to estimate the standard error 
# of the statistic. A simpler alternative is to use an equation for the standard error based on the 
# sample proportion, hypothesized proportion, and sample size.
# z = (p − p0) /sqrt(p0∗(1 − p0)/n)
# Let's revisit the p-value using this simpler calculation.
# late_shipments is available; dplyr is loaded.
# Instructions
# Hypothesize that the proportion of late shipments is 6%.
# Calculate the sample proportion of shipments where late equals "Yes" as prop_late, and pull out the value to get a numeric value.
# Calculate the number of observations in the sample.
head(late_shipments)
# Hypothesize that the proportion of late shipments is 6%
p_0 <- 0.06
# Calculate the sample proportion of late shipments
p_hat <- mean(late_shipments$late == "Yes")
mean(late_shipments$late == "Yes")
# [1] 0.067
sum(late_shipments$late == "Yes") / length(late_shipments$late)
# [1] 0.067
# Calculate the sample size
n <- nrow(late_shipments)
# [1] 1000

# Calculate the numerator of the z-score as the difference between the sample proportion and the hypothesized proportion.
# Calculate the denominator of the z-score as the sample proportion times one minus the sample proportion, divided by the sample size, all square rooted.
# Calculate the z-score as the ratio of these numbers.
# From previous step
p_0 <- 0.06
p_hat <- late_shipments %>%
  summarize(prop_late = mean(late == "Yes")) %>%
  pull(prop_late)
n <- nrow(late_shipments)
# Calculate the numerator of the test statistic
numerator <- p_hat - p_0
# Calculate the denominator of the test statistic
denominator <- sqrt(p_0 * (1 - p_0) / n)
# Calculate the test statistic
z_score <- numerator / denominator
# See the result
z_score

# Transform the z-score into a p-value, remembering that this is a "greater than" alternative hypothesis.
# From previous step
p_0 <- 0.06
p_hat <- late_shipments %>%
  summarize(prop_late = mean(late == "Yes")) %>%
  pull(prop_late)
n <- nrow(late_shipments)
numerator <- p_hat - p_0
denominator <- sqrt(p_0 * (1 - p_0) / n)
z_score <- numerator / denominator
# Calculate the p-value from the z-score
p_value <- pnorm(q = z_score, lower.tail = F)
# See the result
p_value
# [1] 0.1756447
# Well proportioned! While bootstrapping can be used to estimate the standard error of any statistic, it 
# is computationally intensive. For proportions, using a simple equation of the hypothesized proportion 
# and sample size is easier to compute, and the resulting p-value is almost identical (0.19 rather 
# than 0.18).

#
# Test for two proportions
# You may wonder if the amount paid for freight affects whether or not the shipment was late. Recall 
# that in late_shipments dataset, whether or not the shipment was late is stored in the late column. 
# Freight costs are stored in the freight_cost_group column, and the categories are "expensive" 
# and "reasonable".
# We can form hypotheses to test.
# H0: late expensive − late reasonable = 0
# HA: late expensive − late reasonable > 0 p_hats contains the estimates of population proportions (sample proportions) for the "expensive" and "reasonable" groups. ns contains the sample sizes for these groups.
# Instructions
# Calculate the pooled sample proportion, p^, as the mean of p_hats weighted by ns. Use weighted.mean() 
# or arithmetic with this equation.
# p^ = nexpensive × p^expensive + nreasonable × p^reasonable / (nexpensive + nreasonable)
head(late_shipments)
late_shipments %>% group_by(freight_cost_group) %>% summarise(p_hats_ = mean(late == "Yes"), ns_ = n())
# # A tibble: 2 × 3
#   freight_cost_group p_hats_   ns_
#   <chr>                <dbl> <int>
# 1 expensive           0.0961   541
# 2 reasonable          0.0327   459
# See the sample variables
print(p_hats)
# print(p_hats)
#  expensive reasonable 
# 0.09611830 0.03267974 
print(ns)
#  expensive reasonable 
#        541        459
ns[1]
# expensive 
#       541 
# Calculate the pooled estimate of the population proportion
p_hat <- (ns[1] * p_hats[1] + ns[2] * p_hats[2]) / (ns[1] + ns[2])
# See the result
p_hat
weighted.mean(x = p_hats, w = ns)

# Calculate the standard error of the sample. Use this equation.
# SE(p^expensive − p^reasonable) = sqrt( p^ × (1− p^) / nexpensive + p^ × (1 − p^) / nreasonable )
# Calculate the pooled sample proportion times one minus the pooled sample proportion.
# Divide p_hat_times_not_p_hat by the sample sizes.
# Calculate the square root of the sum of p_hat_times_not_p_hat_over_ns.
# From previous step
p_hat <- weighted.mean(p_hats, ns)
# Calculate sample prop'n times one minus sample prop'n
p_hat_times_not_p_hat <- p_hat * (1 - p_hat)
# Divide this by the sample sizes
p_hat_times_not_p_hat_over_ns <- p_hat_times_not_p_hat / ns
# Calculate std. error
std_error <- sqrt(sum(p_hat_times_not_p_hat_over_ns))
# See the result
std_error

# Calculate the z-score. Use the following equation. You'll need square bracket indexing to access 
# elements of p_hats.
# z = (p^expensive − p^reasonable) / SE(p^expensive − p^reasonable)
# From previous steps
p_hat <- weighted.mean(p_hats, ns)
p_hat_times_not_p_hat <- p_hat * (1 - p_hat)
p_hat_times_not_p_hat_over_ns <- p_hat_times_not_p_hat / ns
std_error <- sqrt(sum(p_hat_times_not_p_hat_over_ns))
# Calculate the z-score
z_score <- (p_hats[1] - p_hats[2]) / std_error
# See the result
z_score
# expensive 
# 3.998343 

# Calculate the p-value from the z-score.
# From previous steps
p_hat <- weighted.mean(p_hats, ns)
p_hat_times_not_p_hat <- p_hat * (1 - p_hat)
p_hat_times_not_p_hat_over_ns <- p_hat_times_not_p_hat / ns
std_error <- sqrt(sum(p_hat_times_not_p_hat_over_ns))
z_score <- (p_hats["expensive"] - p_hats["reasonable"]) / std_error

# Calculate the p-value from the z-score
p_value <- pnorm(q = z_score, lower.tail = F)
# See the result
p_value
#   expensive 
# 3.18937e-05 
# Mad props! You can calculate a p-value for a two sample proportion test using (a rather exhausting amount of) arithmetic.

# prop_test() for two samples
# That took a lot of effort to calculate the p-value, so while it is useful to see how the calculations 
# work, it isn't practical to do in real-world analyses. For daily usage, it's better to use the infer 
# package.
# Recall the hypotheses.
# H0: late expensive − late reasonable = 0
# HA: late expensive − late reasonable > 0
# late_shipments is available; infer is loaded.
# Instructions
# Using the late_shipments dataset, use prop_test() to perform a proportion test appropriate to the 
# hypotheses.
# Specify a hypothesis of late versus freight_cost_group.
# Set the order of the freight cost groups.
# Specify the success value for late and the type of alternative hypothesis.
# Don't use Yates' continuity correction.
install.packages("infer")
library(infer)
# Perform a proportion test appropriate to the hypotheses 
test_results <- prop_test(
  x = late_shipments,
  late ~ freight_cost_group,
  order = c("expensive", "reasonable"),
  success = "Yes",
  alternative = "greater",
  correct = FALSE
)
# See the results
test_results
# A tibble: 1 × 6
#   statistic chisq_df   p_value alternative lower_ci upper_ci
#       <dbl>    <dbl>     <dbl> <chr>          <dbl>    <dbl>
# 1      16.0        1 0.0000319 greater        0.164        1
# Delightful déjà vu! infer's prop_test()' function gives the same results with less effort.

# 10 Declaration of independence.mp4
# The chi-square distribution
# Chi-square hypothesis tests rely on the chi-square distribution. Like the t-distribution, it has degrees 
# of freedom and non-centrality parameters.
# The plots show the PDF and CDF for a chi-square distribution (solid black line), and for comparison 
# show a normal distribution with the same mean and variance (gray dotted line).
# Which statement about the chi-square distribution is true?
# Instructions
# - Like the normal distribution, the chi-square distribution is defined for x values from minus infinity to infinity.
# - As you increase the degrees of freedom or the non-centrality, the chi-square distribution PDF and CDF curves get closer to those of a normal distribution. +
# - As you decrease the degrees of freedom or the non-centrality, the chi-square distribution PDF and CDF curves get closer to those of a normal distribution.
# - The chi-square distribution PDF is symmetric about its peak.
# Très chic chi-square! Like the t-distribution, the chi-square distribution has degrees of freedom and non-centrality parameters. When these numbers are large, the chi-square distribution can be approximated by a normal distribution.

# How many tails for chi-square tests?
# Unlike t.test() and prop_test(), chisq_test() does not have an "alternative" argument to specify 
# which tails are considered by the alternative hypothesis.
# Which tail is almost always considered in chi-square tests?
# Answer the question
# - Left-tailed
# - Right-tailed +
# - Two-tailed
# - Neither: chi-square tests depend on the peak, not the tails
# - It depends on the problem
# Right on! The chi-square test statistic is a square number, so it is always non-negative, so only the 
# right tail tends to be of interest.

# Trade deals often use a form of business shorthand in order to specify the exact details of their 
# contract. These are International Chamber of Commerce (ICC) international commercial terms, or 
# incoterms for short.
# The late_shipments dataset includes a vendor_inco_term that describes the incoterms that applied to 
# a given shipment. The choices are:
# EXW: "Ex works". The buyer pays for transportation of the goods.
# CIP: "Carriage and insurance paid to". The seller pays for freight and insurance until the goods board a ship.
# DDP: "Delivered duty paid". The seller pays for transportation of the goods until they reach a 
# destination port.
# FCA: "Free carrier". The seller pays for transportation of the goods.
# Perhaps the incoterms affect whether or not the freight costs are expensive. Test these hypotheses 
# with a significance level of 0.01.
# H0: vendor_inco_term and freight_cost_group are independent.
# HA: vendor_inco_term and freight_cost_group are associated.
# late_shipments is available; ggplot2 and infer are loaded.
# Instructions
# Using the late_shipments dataset, draw a proportional stacked bar plot of vendor_inco_term with fill 
# color by freight_cost_group.
# Plot vendor_inco_term filled by freight_cost_group.
# Make it a proportional stacked bar plot.
ggplot(late_shipments, aes(x = vendor_inco_term, fill = freight_cost_group)) + geom_bar(position='fill') + ylab("proportion")
# http://joxi.ru/8An4XoVTN80qXm

# Using the late_shipments dataset, perform a chi-square test of independence on freight_cost_group and vendor_inco_term.
# Perform a chi-square test of independence on freight_cost_group and vendor_inco_term
test_results <- late_shipments %>% chisq_test(
  freight_cost_group ~ vendor_inco_term
)
# See the results
test_results
# A tibble: 1 × 3
#  statistic chisq_df       p_value
#      <dbl>    <int>         <dbl>
# 1     44.1        3 0.00000000142
# Question
# What should you conclude from the hypothesis test?
# - Fail to reject the null hypothesis and conclude that vendor_inco_term and freight_cost_group are independent.
# - Reject the null hypothesis and conclude that vendor_inco_term and freight_cost_group are independent.
# - Fail to reject the null hypothesis and conclude that vendor_inco_term and freight_cost_group are associated.
# - Reject the null hypothesis and conclude that vendor_inco_term and freight_cost_group are associated.
# Independence insight! The test to compare proportions of successes in a categorical variable across 
# groups of another categorical variable is called a chi-square test of independence.

# 11 Does this dress make my fit look good?.mp4
# Visualizing goodness of fit
# The chi-square goodness of fit test compares proportions of each level of a categorical variable to 
# hypothesized values. Before running such a test, it can be helpful to visually compare the distribution 
# in the sample to the hypothesized distribution.
# Recall the vendor incoterms in the late_shipments dataset. Let's hypothesize that the four values occur 
# with these frequencies in the population of shipments.
# EXW: 0.75
# CIP: 0.05
# DDP: 0.1
# FCA: 0.1
# late_shipments is available; tibble, dplyr, ggplot2, and infer are loaded.
# Instructions
# Using the late_shipments dataset, count the vendor_inco_terms.
# Get the number of rows in late_shipments.
# Add a column, n to the tibble being defined, containing hypothesized counts for each category.
# Using late_shipments, count the vendor incoterms
vendor_inco_term_counts <- late_shipments %>% count(vendor_inco_term)
late_shipments %>% group_by(vendor_inco_term) %>% summarise(n = n())
# A tibble: 4 × 2
# vendor_inco_term       n
# <fct>              <int>
# 1 EXW                746
# 2 CIP                 54
# 3 DDP                 86
# 4 FCA                111
# Get the number of rows in the whole sample
n_total <- nrow(late_shipments)
hypothesized <- tribble(
   ~ vendor_inco_term, ~ prop,
   "EXW", 0.75,
   "CIP", 0.05,
   "DDP", 0.1,
   "FCA", 0.1
) %>%
  # Add a column of hypothesized counts for the incoterms
  mutate(n = n_total * prop)
# See the results
hypothesized
# # A tibble: 4 × 3
#   vendor_inco_term  prop      n
#    <chr>            <dbl> <dbl>
#  1 EXW               0.75 748.0 
#  2 CIP               0.05  49.8
#  3 DDP               0.1   99.7
#  4 FCA               0.1   99.7

# Using the vendor_inco_term_counts dataset, plot n versus vendor_inco_term.
# Make it a precalculated bar plot (a.k.a. col plot).
# Add points from the hypothesized dataset.
# From previous step
vendor_inco_term_counts <- late_shipments %>% 
  count(vendor_inco_term)
n_total <- nrow(late_shipments)
hypothesized <- tribble(
  ~ vendor_inco_term, ~ prop,
  "EXW", 0.75,
  "CIP", 0.05,
  "DDP", 0.1,
  "FCA", 0.1
) %>%
  mutate(n = prop * n_total)
# Using vendor_inco_term_counts, plot n vs. vendor_inco_term 
ggplot(vendor_inco_term_counts, aes(vendor_inco_term, n)) +
  # Make it a (precalculated) bar plot
  geom_col() +
  # Add points from hypothesized 
  geom_point(data = hypothesized, aes(vendor_inco_term, n, color = "purple", size = 3))
# http://joxi.ru/82QRVQ5c8ZoB7A
# Beautiful bars! Two of the bars in the sample are very close to the hypothesized values, one is a 
# little high, and one is a little low. We'll need a test to see if the differences are statistically 
# significant.

# Chi-square test of goodness of fit
# The bar plot of vendor_inco_term suggested that its distribution across the four categories was quite 
# close to the hypothesized distribution. You'll need to perform a chi-square goodness of fit test to 
# see whether the differences are statistically significant.
# To decide which hypothesis to choose, we'll set a significance level of 0.1.
# late_shipments is available; tibble, dplyr, ggplot2, and infer are loaded.
# Instructions
# Using the late_shipments dataset, perform a chi-square goodness of fit test on vendor_inco_term. 
# The hypothesized proportions are given in hypothesized_props.
hypothesized_props <- c(
  EXW = 0.75, CIP = 0.05, DDP = 0.1, FCA = 0.1
)
# Run chi-square goodness of fit test on vendor_inco_term
test_results <- chisq_test(x = late_shipments,
                           response = vendor_inco_term,
                           p = hypothesized_props
)
# See the results
test_results
# A tibble: 1 × 3
#  statistic chisq_df p_value
#      <dbl>    <dbl>   <dbl>
#  1    3.51        3   0.319

# Question
# What should you conclude from the hypothesis test?
# - Fail to reject the null hypothesis and conclude that vendor_inco_term follows the distribution specified by hypothesized_props. +
# - Reject the null hypothesis and conclude that vendor_inco_term follows the distribution specified by hypothesized_props.
# - Fail to reject the null hypothesis and conclude that vendor_inco_term does not follow the distribution specified by hypothesized_props.
#   Incorrect Submission
#   No. The null hypothesis states that the variable follows the hypothesized distribution.
# - Reject the null hypothesis and conclude that vendor_inco_term does not follow the distribution specified by hypothesized_props.
#   Incorrect Submission
#   No. The null hypothesis states that the variable follows the hypothesized distribution.

# 12 What do you assume?.mp4
# Common assumptions of hypothesis tests
# Hypothesis tests make assumptions about the dataset that they are testing, and the conclusions you draw 
# from the test results are only valid if those assumptions hold. While some assumptions differ between 
# types of test, others are common to all hypothesis tests.
# Which of the following statements is a common assumption of hypothesis tests?
# Possible Answers
# - Sample observations are collected deterministically from the population.
# - Sample observations are correlated with each other.
# - Sample observations have no direct relationship with each other. +
# - Sample sizes are greater than thirty observations.
# Ace assumption! All hypothesis tests assume that the data are collected at random from the population, 
# that each row is independent of the others, and that the sample size is big enough.

# Testing sample size
# In order to conduct a hypothesis test, and be sure that the result is fair, a sample must meet three 
# requirements: it is a random sample of the population; the observations are independent; and there are 
# enough observations. Of these, only the last condition is easily testable with code.
# The minimum sample size depends on the type of hypothesis tests you want to perform. Let's test some 
# scenarios on the late_shipments dataset.
# late_shipments is available; dplyr is loaded.
# Instructions
# Using the late_shipments dataset, get counts by the freight_cost_group columns.
# Insert a suitable number to inspect whether the counts are "big enough" for a two sample t-test.
# Get counts by freight_cost_group
counts <- late_shipments %>% count(freight_cost_group)

# See the result
counts
#   freight_cost_group   n
# 1          expensive 541
# 2         reasonable 459
# Inspect whether the counts are big enough
all(counts$n >= 30)
# TRUE

# Using the late_shipments dataset, get counts by the late column.
# Insert a suitable number to inspect whether the counts are "big enough" for a one sample proportion test.
# Get counts by late
counts <- late_shipments %>% count(late)
# See the result
counts
# Inspect whether the counts are big enough
all(counts$n >= 10)

# Using the late_shipments dataset, get counts by the vendor_inco_term and freight_cost_group columns.
# Insert a suitable number to inspect whether the counts are "big enough" for a chi-square independence test.
# Count the values of vendor_inco_term and freight_cost_group
counts <- late_shipments %>% count(vendor_inco_term, freight_cost_group)
# See the result
counts %>% mutate(`n >= 5` = n >= 5)
counts
#    vendor_inco_term freight_cost_group   n n >= 5
# 1               CIF         reasonable   1  FALSE
# 2               CIP          expensive  14   TRUE
# 3               CIP         reasonable  40   TRUE
# 4               DAP         reasonable   1  FALSE
# 5               DDP          expensive  59   TRUE
# 6               DDP         reasonable  27   TRUE
# 7               DDU         reasonable   1  FALSE
# 8               EXW          expensive 429   TRUE
# 9               EXW         reasonable 317   TRUE
# 10              FCA          expensive  39   TRUE
# 11              FCA         reasonable  72   TRUE
# Inspect whether the counts are big enough
all(counts$n >= 5)
# FALSE

# Using the late_shipments dataset, get counts by the shipment_mode column.
# Insert a suitable number to inspect whether the counts are "big enough" for an ANOVA test.
# Count the values of shipment_mode
counts <- late_shipments %>% count(shipment_mode)
# See the result
counts
#   shipment_mode   n
# 1           Air 909
# 2   Air Charter   6
# 3           N/A   1
# 4         Ocean  84
# Inspect whether the counts are big enough
all(counts$n >= 30)
# [1] FALSE
# Setting a great example for an ample sample! While randomness and independence of observations can't 
# easily be tested programmatically, you can test that your sample sizes are big enough to make a 
# hypothesis test appropriate.

# 13 X-ray specs don't believe the hyp.mp4
# There is only one test
# You've encountered several types of traditional hypothesis test: t-tests, ANOVA tests, proportion tests, 
# and chi-square tests. You may have noticed that there were similarities in the workflow for performing 
# each test.
# Allen Downey proposed that all traditional hypothesis tests were special cases of a generic hypothesis 
# test. He called this the "There is Only One Test" framework, and it forms a "grammar of hypothesis 
# tests", analogous to the "grammar of graphics" implemented by ggplot2.
# In which situations will the "There is Only One Test" framework provide p-value and decision rule 
# results that are different than a traditional method like prop_test()?
# Answer the question
# Possible Answers
# - The results should always be similar.
#   Incorrect
#   No. There are situations when the "Only One" tests give different results to traditional tests.
# - When the assumptions for the traditional method are not met. +
# - When the sample size is large enough that the Central Limit Theorem applies.
# - When groups in the dataset are perfectly balanced.
# One test to rule them all, one test to find them… Simulation-based hypothesis tests allow more flexibility, and 
# are not bound by the assumptions of traditional hypothesis tests.
# (video transcript): generate() is worth mentioning briefly. It creates simulated data reflecting the null hypothesis. 
# Using simulation rather than arithmetic equations to get the test statistic is computationally 
# expensive, so it has only become accessible with modern computing power. However, simulation has 
# a benefit that it works well even when you have small samples or imbalanced data.

# Specifying & hypothesizing
# In Chapter 3, you ran a two sample proportion test on the proportion of late shipments across freight 
# cost groups. Recall the hypotheses.
# H0: late expensive − late reasonable = 0
# HA: late expensive − late reasonable > 0
# Let's compare that traditional approach using prop_test() with a simulation-based infer pipeline.
# late_shipments is available; dplyr and infer are loaded.
# Instructions
# Question
# Run the proportion test code (previously seen in Chapter 3). Assuming a significance level of 
# alpha = 0.05, what does the evidence suggest?
# Perform a proportion test appropriate to the hypotheses 
test_results <- late_shipments %>% 
  prop_test(
    late ~ freight_cost_group,
    order = c("expensive", "reasonable"),
    success = "Yes",
    alternative = "greater",
    correct = FALSE
  )
# See the results
test_results
# A tibble: 1 × 6
#   statistic chisq_df   p_value alternative lower_ci upper_ci
#       <dbl>     <dbl>     <dbl>      <chr>    <dbl>    <dbl>
# 1      16.0        1 0.0000319     greater    0.164        1
# Question
# Run the proportion test code (previously seen in Chapter 3). Assuming a significance level of 
# alpha = 0.05, what does the evidence suggest?
# - The p-value is less than or equal to the significance level, so you should reject the null hypothesis that the proportion of late shipments is the same for each freight cost group. +
# - The p-value is less than or equal to the significance level, so you should fail to reject the null hypothesis that the proportion of late shipments is the same for each freight cost group.
# - The p-value is greater than the significance level, so you should reject the null hypothesis that the proportion of late shipments is the same for each freight cost group.
# - The p-value is greater than the significance level, so you should fail to reject the null hypothesis that the proportion of late shipments is the same for each freight cost group.

# Using the late_shipments dataset, specify that we are interested in late proportions across freight_cost_group, where "Yes" denotes success.
# Specify that we are interested in late proportions across freight_cost_groups, where "Yes" denotes success
specified <- late_shipments %>% specify(late ~ freight_cost_group, success = "Yes")
# See the result
specified
# Response: late (factor)
# Explanatory: freight_cost_group (factor)
# A tibble: 1,000 × 2
#   late  freight_cost_group
#   <fct> <fct>             
# 1 No    reasonable        
# 2 No    expensive         
# 3 No    expensive         
# 4 Yes   expensive         
# 5 No    reasonable        
# 6 No    reasonable        
# 7 No    expensive         
# 8 No    expensive         
# 9 No    expensive         
#10 No    reasonable        
# … with 990 more rows

# Extend the pipeline to declare a null hypothesis that the variables are independent
hypothesized <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence")
# See the result
hypothesized
# Response: late (factor)
# Explanatory: freight_cost_group (factor)
# Null Hypothesis: independence
# A tibble: 1,000 × 2
# late  freight_cost_group
# <fct> <fct>             
# 1 No    reasonable        
# 2 No    expensive         
# 3 No    expensive         
# 4 Yes   expensive         
# 5 No    reasonable        
# 6 No    reasonable        
# 7 No    expensive         
# 8 No    expensive         
# 9 No    expensive         
#10 No    reasonable        
# … with 990 more rows
# Super specifying and hyper hypothesizing! The first two steps in the infer pipeline add attributes 
# to the dataset in order to set up the simulation.

# 14 The generation game.mp4
# Generating & calculating
# The infer pipeline for hypothesis testing requires four steps to calculate the null distribution: 
# specify, hypothesize, generate, and calculate.
# Let's continue the pipeline you began in the previous coding exercise. We'll get a set of differences 
# in proportions that are distributed as though the null hypothesis, that the proportion of late 
# shipments is the same across freight cost groups, is true.
# late_shipments is available; dplyr, infer, and ggplot2 are loaded.
# Instructions
# Extend the infer pipeline to generate two thousand permutation replicates. (Note this will take a 
# few seconds to complete.)
late_shipments %>% count(freight_cost_group, late)
#   freight_cost_group late   n
# 1          expensive   No 489
# 2          expensive  Yes  52
# 3         reasonable   No 444
# 4         reasonable  Yes  15
# Extend the pipeline to generate 2000 permutations
generated <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute")
# See the result
generated
# Response: late (factor)
# Explanatory: freight_cost_group (factor)
# Null Hypothesis: independence
# A tibble: 2,000,000 × 3
# Groups:   replicate [2,000]
#   late  freight_cost_group replicate
#   <fct>  <fct>                  <int>
#  1 No    reasonable                 1
#  2 No    expensive                  1
#  3 No    expensive                  1
#  4 No    expensive                  1
#  5 No    reasonable                 1
#  6 No    reasonable                 1
#  7 No    expensive                  1
#  8 No    expensive                  1
#  9 Yes   expensive                  1
# 10 No    reasonable                 1
# … with 1,999,990 more rows

# Complete the infer pipeline for the null distribution by calculating the difference in proportions, 
# setting the order to expensive proportion minus reasonable proportion.
# Extend the pipeline to calculate the difference in proportions (expensive minus reasonable)
null_distn <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute") %>% 
  calculate(stat = "diff in props", order = c("expensive", "reasonable"))
# See the result
null_distn

# AK: let's check how calculate() works and compare the result with manual calculation
# 1. define generate:
null_distn <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute") 
# 2. calculate()
null_distn %>% 
  calculate(stat = "diff in props", order = c("expensive", "reasonable"))
# Response: late (factor)
# Explanatory: freight_cost_group (factor)
# Null Hypothesis: independence
# A tibble: 2,000 × 2
# replicate     stat
# <int>    <dbl>
# 1         1 -0.0332 
# 2         2 -0.00905
# 3. the same but manual
null_distn %>% group_by(replicate,freight_cost_group) %>% summarise(p = mean(late=="Yes"))
# A tibble: 4,000 × 3
# Groups:   replicate [2,000]
#   replicate freight_cost_group      p
#       <int> <fct>               <dbl>
# 1         1 expensive          0.0518
# 2         1 reasonable         0.0850
# 3         2 expensive          0.0628
# 4         2 reasonable         0.0719
# difference between 1 and 2 is the same -0.0332

# Visualize the null distribution.
# From previous steps
null_distn <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute") %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )
# Visualize the null distribution
null_distn %>% visualise()
# http://joxi.ru/52aG1zYilWpVqr
# Easy as 1, 2, 3, 4! Calling those four functions in order will generate a null distribution. To 
# determine a result from the test, we need to compare this null distribution to the observed statistic.

# Observed statistic and p-value
# You now have a null distribution. In order to get a p-value and weigh up the evidence against the 
# null hypothesis, you need to calculate the difference in proportions that is observed in the 
# late_shipments sample.
# late_shipments is available; dplyr, infer, and ggplot2 are loaded.
# Instructions 
# Copy, paste, and modify the null distribution pipeline to get the observed statistic.
null_distn <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute") %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )
null_distn
# Response: late (factor)
# Explanatory: freight_cost_group (factor)
# Null Hypothesis: independence
# A tibble: 2,000 × 2
# replicate     stat
#       <int>    <dbl>
# 1         1  0.00303
# 2         2  0.00706
# 3         3  0.0191 
# 4         4 -0.0171 
# 5         5  0.0111 
# 6         6  0.0272 
# 7         7  0.00303
# 8         8 -0.0131 
# 9         9 -0.00905
#10        10 -0.0292 
# … with 1,990 more rows
null_distn %>% count(stat)
# Response: late (factor)
# Explanatory: freight_cost_group (factor)
# Null Hypothesis: independence
# A tibble: 27 × 2
#      stat     n
#     <dbl> <int>
# 1 -0.0533     1
# 2 -0.0493     1
# 3 -0.0453     5
# 4 -0.0413     8
# 5 -0.0372    12
# 6 -0.0332    23
# 7 -0.0292    47
# 8 -0.0252    58
# 9 -0.0211    85
#10 -0.0171   113
# … with 17 more rows
# Copy, paste, and modify the pipeline to get the observed statistic
obs_stat <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  # hypothesize(null = "independence") %>% 
  # generate(reps = 2000, type = "permute") %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )
# See the result
obs_stat
# Response: late (factor)
# Explanatory: freight_cost_group (factor)
# A tibble: 1 × 1
# stat
#    <dbl>
# 1 0.0634

# Visualize the null distribution, adding a vertical line at the observed statistic.
# From previous steps
null_distn <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute") %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )
obs_stat <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )
# Visualize the null dist'n, adding a vertical line at the observed statistic
visualize(null_distn) + geom_vline(aes(xintercept = stat), data = obs_stat, color = 'red')
obs_stat$stat
# [1] 0.06343856
# http://joxi.ru/BA0yd0vu1NE1Jr

# Get the p-value from the null distribution and observed statistic, assuming an appropriate direction for the alternative hypothesis.
# From previous steps
null_distn <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute") %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )
obs_stat <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )
# Get the p-value
p_value <- get_p_value(null_distn, obs_stat, direction = "two sided")
# See the result
p_value
# # A tibble: 1 × 1
#   p_value
#     <dbl>
# 1       0
# Proportion test simulation success! The p-value is calculated from the null distribution and the 
# observed statistic. Here, the observed difference in proportions appears way outside of the null 
# distribution histogram, which resulted in a p-value of zero. This can be interpreted as “p is very 
# small”, rather than actually zero.

# 15 Look ma! No parameters!.mp4
# Simulation-based t-test
# In Chapter 2 you manually performed the steps for a t-test to explore these hypotheses.
# H0: The mean weight of shipments that weren't late is the same as the mean weight of shipments that were late.
# HA: The mean weight of shipments that weren't late is less than the mean weight of shipments that were late.
# You can run the test more concisely using infer's t_test().
late_shipments %>% 
   t_test(
     weight_kilograms ~ late,
     order = c("No", "Yes"),
     alternative = "less"
   )
# A tibble: 1 × 7
#   statistic  t_df p_value alternative estimate lower_ci upper_ci
#       <dbl> <dbl>   <dbl> <chr>          <dbl>    <dbl>    <dbl>
# 1     -1.73  86.2  0.0437 less           -507.     -Inf    -19.4
# t_test() assumes that the null distribution is normal. We can avoid assumptions by using a simulation-based non-parametric equivalent.
# late_shipments is available; dplyr and infer are loaded.
# Instructions
# Specify weight in kilograms versus whether or not the shipment was late.
# Declare a null hypothesis of independence.
# Generate 1000 permutation replicates.
# Calculate the difference in means, setting the order as "No" minus "Yes".
# Fill out the null distribution pipeline
null_distn <- late_shipments %>% 
  # Specify weight_kilograms vs. late
  specify(weight_kilograms ~ late) %>% 
  # Declare a null hypothesis of independence
  hypothesize(null = "independence") %>% 
  # Generate 1000 permutation replicates
  generate(reps = 1000, type = "permute") %>% 
  # Calculate the difference in means ("No" minus "Yes")
  calculate(
    stat = "diff in means",
    order = c("No", "Yes")
  )
# See the results
null_distn
# Response: weight_kilograms (numeric)
# Explanatory: late (factor)
# Null Hypothesis: independence
# A tibble: 1,000 × 2
# replicate   stat
#       <int>  <dbl>
# 1         1 -604. 
# 2         2  385. 
# 3         3 -489. 
# 4         4 -315. 
# 5         5  248. 
# 6         6   52.7
# 7         7   33.8
# 8         8 -650. 
# 9         9 -565. 
#10        10  493. 
# … with 990 more rows

# Calculate the difference in means observed in the late_shipments dataset.
# From previous step
null_distn <- late_shipments %>% 
  specify(weight_kilograms ~ late) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))
# Calculate the observed difference in means
obs_stat <- late_shipments %>% 
  specify(weight_kilograms ~ late) %>% 
  # hypothesize(null = "independence") %>% 
  # generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))
# See the result
obs_stat
# Response: weight_kilograms (numeric)
# Explanatory: late (factor)
# A tibble: 1 × 1
# stat
# <dbl>
# 1 -507.

# Get the p-value from the null distribution and the observed difference in means, setting an appropriate direction.
# From previous steps
null_distn <- late_shipments %>% 
  specify(weight_kilograms ~ late) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))
obs_stat <- late_shipments %>% 
  specify(weight_kilograms ~ late) %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))
# Get the p-value
p_value <- get_p_value(null_distn, obs_stat, direction = "less")
# See the result
p_value
# A tibble: 1 × 1
# p_value
#     <dbl>
# 1   0.118
# Nice non-parametric testing! The p-value with the traditional t-test was 0.04, and the p-value from 
# the simulation was close to 0.1. Depending upon the significance level you chose for the tests, 
# this difference in p-values could have important consequences for whether or not to reject the 
# null hypothesis.

# Rank sum tests
# Another class of non-parametric hypothesis tests are called rank sum tests. Ranks are the positions 
# of numeric values from smallest to largest. Think of them as positions in running events: whoever 
# has the fastest (smallest) time is rank 1, second fastest is rank 2, and so on.
# By calculating on the ranks of data instead of the actual values, you can avoid making assumptions 
# about the distribution of the test statistic. It's most robust in the same way that a median is 
# more robust than a mean.
# Two commonly used rank-based tests are the Wilcoxon-Mann-Whitney test, which is like a non-parametric 
# t-test, and the Kruskal-Wallis test, which is like a non-parametric ANOVA.
# late_shipments is available.
# Instructions 
# Using the late_shipments dataset, run a Wilcoxon-Mann-Whitney test on the weight in kilograms 
# versus whether or not the shipment was late.
# Run a Wilcoxon-Mann-Whitney test on weight_kilograms vs. late
test_results <- wilcox.test(
  weight_kilograms ~ late,
  data = late_shipments
)
# See the result
test_results
# Wilcoxon rank sum test with continuity correction
# data:  weight_kilograms by late
# W = 21340, p-value = 1.624e-05
# alternative hypothesis: true location shift is not equal to 0

# Using the late_shipments dataset, run a Kruskal-Wallace test on the weight in kilograms versus the shipment mode.
# Run a Kruskal-Wallace test on weight_kilograms vs. shipment_mode (> 2 categories, AK)
test_results <- kruskal.test(
  weight_kilograms ~ shipment_mode,
  data = late_shipments
)
# See the result
test_results
# Kruskal-Wallis rank sum test
# data:  weight_kilograms by shipment_mode
# Kruskal-Wallis chi-squared = 157.41, df = 2, p-value < 2.2e-16
# They tried to make me use parameters, but I said “No, no, no”. The Wilcoxon-Mann-Whitney and 
# Kruskal-Wallace tests are useful when you cannot satisfy the assumptions for parametric tests, 
# and don't want the computational expense of simulation-based tests.



# Chi-Square Test of Independence in R
# http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
# https://en.wikipedia.org/wiki/Chi-squared_test
# The chi-square test of independence is used to analyze the frequency table (i.e. contengency table) 
# formed by two categorical variables. The chi-square test evaluates whether there is a significant 
# association between the categories of the two variables. 
filepath <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"
housetasks <- read.delim(file = filepath, row.names = 1)
housetasks_1 <- read.delim(file = filepath)
housetasks <- read.table(file = filepath, row.names = 1, header = T)
housetasks
housetasks_1_unpivot <- 
  housetasks_1 %>% pivot_longer(cols = c("Wife", "Alternating","Husband", "Jointly"),
                                names_to = "who", values_to = "values") %>%
  rename('tasks' = 'X')
housetasks_1_unpivot
# The data is a contingency table containing 13 housetasks and their distribution in the couple:
# rows are the different tasks, values are the frequencies of the tasks done :
# - by the wife only
# - alternatively
# - by the husband only
# - or jointly
install.packages("tidyverse")
library(tidyverse)
# plot as tiles
housetasks_1 %>% pivot_longer(cols = c("Wife", "Alternating","Husband", "Jointly"),
                            names_to = "who", values_to = "values") %>% 
  rename('tasks' = 'X') %>%
  ggplot(aes(x = names, y = task)) + 
  geom_tile(aes(fill = values), color = "white") + 
  geom_text(aes(label = values)) +
  scale_fill_gradient(low = "white", high = "steelblue")
# plot as bars %
housetasks_1 %>% pivot_longer(cols = c("Wife", "Alternating","Husband", "Jointly"),
                              names_to = "who", values_to = "values") %>%
  rename('tasks' = 'X') %>%
  ggplot(aes(x = task, y = values)) + 
  geom_bar(aes(fill = who), color = "white", stat = "identity", position = "fill") +
  coord_flip() 
# mosaic plot
install.packages('vcd')
library(vcd)
mosaic(x = as.matrix(housetasks), direction = c('v', 'h'), shade = T)
# contingency table + totals
addmargins(as.matrix(housetasks))
# Compute chi-square test in R
# Chi-square statistic can be easily computed using the function chisq.test() as follow:
chisq <- chisq.test(housetasks)
chisq
# In our example, the row and the column variables are statistically significantly associated (p-value = 0).
# Observed counts
chisq$observed
# Expected counts
round(chisq$expected,2)
# AK: check
# 1 row sum
housetasks %>% mutate(row_sum = Wife + Alternating + Husband + Jointly) %>%
# 2 total sum
summarise(sum(row_sum))
# 3 expected value for the cell #1
176 * sum(housetasks$Wife)/1744
# Nature of the dependence between the row and the column variables
# As mentioned above the total Chi-square statistic is 1944.456196.
# If you want to know the most contributing cells to the total Chi-square score, you just have to calculate the Chi-square statistic for each cell:
# r = (o − e) / sqrt(e)
# The above formula returns the so-called Pearson residuals (r) for each cell (or standardized residuals)
# Pearson residuals can be easily extracted from the output of the function chisq.test():
round(chisq$residuals,3)
# Let’s visualize Pearson residuals using the package corrplot:
library(corrplot)
?corrplot()
?cor()
cor(x = chisq$residuals)
corrplot(corr = chisq$residuals, is.corr = F)
# For a given cell, the size of the circle is proportional to the amount of the cell contribution.
# The sign of the standardized residuals is also very important to interpret the association between rows and columns as explained in the block below.
# Positive residuals are in blue. Positive values in cells specify an attraction (positive association) between the corresponding row and column variables.
# In the image above, it’s evident that there are an association between the column Wife and the rows Laundry, Main_meal.
# There is a strong positive association between the column Husband and the row Repair
# Negative residuals are in red. This implies a repulsion (negative association) between the corresponding row and column variables. For example the column Wife are negatively associated (~ “not associated”) with the row Repairs. There is a repulsion between the column Husband and, the rows Laundry and Main_meal
# The contribution (in %) of a given cell to the total Chi-square score is calculated as follow:
# contrib=r^2 / χ^2, r is the residual of the cell
contrib <- 100 * chisq$residuals^2 / chisq$statistic
round(contrib, 3)
# Visualize the contribution
corrplot(contrib, is.corr = F)
chisq$statistic # χ^2 test statistics
# The relative contribution of each cell to the total Chi-square score give some indication of the nature of the dependency between rows and columns of the contingency table.
# It can be seen that:
# - The column “Wife” is strongly associated with Laundry, Main_meal, Dinner
# - The column “Husband” is strongly associated with the row Repairs
# - The column jointly is frequently associated with the row Holidays
# From the image above, it can be seen that the most contributing cells to the Chi-square are Wife/Laundry (7.74%), Wife/Main_meal (4.98%), Husband/Repairs (21.9%), Jointly/Holidays (12.44%).
# These cells contribute about 47.06% to the total Chi-square score and thus account for most of the difference between expected and observed values.
# This confirms the earlier visual interpretation of the data. As stated earlier, visual interpretation may be complex when the contingency table is very large. In this case, the contribution of one cell to the total Chi-square score becomes a useful way of establishing the nature of dependency.
# Access to the values returned by chisq.test() function
# The result of chisq.test() function is a list containing the following components:
# - statistic: the value the chi-squared test statistic.
# - parameter: the degrees of freedom
# - p.value: the p-value of the test
# - observed: the observed count
# - expected: the expected count

# https://data-flair.training/blogs/chi-square-test-in-r/
head(mtcars)
glimpse(mtcars)
table(mtcars$carb, mtcars$cyl)
addmargins(table(mtcars$carb, mtcars$cyl))
chisq <- chisq.test(x = mtcars$carb, y = mtcars$cyl)
chisq
# We have a high chi-squared value and a p-value of less than 0.05 significance level. So we reject the null hypothesis and conclude that carb and cyl have a significant relationship.
# what about 0 for some carb x cyl pairs??
# Answer from comment: 
# Your mini-challenge example does not satisfy the requirement for the Chi Squared test to be applied. You have 18 cells of which only 3 are above the value of 5. Yates, Moore, and McCabe published that the Chi Squared test has a requirement that 20% or more of the cells in the contingency table are to be of a value of 5 or more. The coding and all else above are correct, but user’s must be aware of the appropriate application/evaluation of the method output
chisq$statistic
chisq$observed
chisq$expected
chisq$residuals
corrplot(chisq$residuals, is.corr = F)

# Chi-square test of independence in R
# https://statsandr.com/blog/chi-square-test-of-independence-in-r/
# For our example, let’s reuse the dataset introduced in the article “Descriptive statistics in R”. This dataset is the well-known iris dataset slightly enhanced. Since there is only one categorical variable and the Chi-square test of independence requires two categorical variables, we add the variable size which corresponds to small if the length of the petal is smaller than the median of all flowers, big otherwise:
df <- iris
head(df)
df$size <- ifelse(df$Sepal.Length < median(df$Sepal.Length), "small", "big")
# Contingency table
table(df$Species, df$size)
ggplot(df) + aes(x = Species, fill = size) + geom_bar()
ggplot(df) + aes(x = Species, fill = size) + geom_bar(position = "fill")
ggplot(df) + aes(x = Species, fill = size) + geom_bar(position = "dodge")
# Chi-square test of independence in R
# For this example, we are going to test in R if there is a relationship between the variables Species and size
chisq <- chisq.test(x = table(df$Species, df$size))
chisq <- chisq.test(x = df$Species, y = df$size)
chisq
chisq$statistic
chisq$p.value
chisq$expected
chisq$observed
# If a warning such as “Chi-squared approximation may be incorrect” appears, it means that the smallest expected frequencies is lower than 5. To avoid this issue, you can either:
# gather some levels (especially those with a small number of observations) to increase the number of observations in the subgroups, or
# use the Fisher’s exact test
# For your information, there are three other methods to perform the Chi-square test of independence in R:
# with the summary() function
# with the assocstats() function from the {vcd} package
# with the ctable() function from the {summarytools} package
summary(table(df$Species, df$size))
library(vcd)
assocstats(table(df$Species, df$size))
install.packages("summarytools")
library(summarytools)
library(dplyr)
df %>%
  ctable(Species, size, prop = 'r', chisq = T, headings = F) %>%
  print(method = 'render', style = 'rmarkdown', footnote = NA)
# Combination of plot and statistical test
library(vcd)
mosaic(~ Species + size, direction = c('v', 'h'), data = df, shade = T)
# Conclusion and interpretation
# From the output and from test$p.value we see that the p-value is less than the significance level of 5%. Like any other statistical test, if the p-value is less than the significance level, we can reject the null hypothesis.
# In our context, rejecting the null hypothesis for the Chi-square test of independence means that there is a significant relationship between the species and the size. Therefore, knowing the value of one variable helps to predict the value of the other variable.
install.packages('ggstatsplot')
library(ggstatsplot)

# Fisher's exact test in R: independence test for a small sample
# https://statsandr.com/blog/fisher-s-exact-test-in-r-independence-test-for-a-small-sample/
# Independence tests are used to determine if there is a significant relationship between two categorical variables. There exists two different types of independence test:
# - the Chi-square test (the most common)
# - the Fisher’s exact test
# On the one hand, the Chi-square test is used when the sample is large enough (in this case the 
# p-value is an approximation that becomes exact when the sample becomes infinite, which is the case 
# for many statistical tests). On the other hand, the Fisher’s exact test is used when the sample is 
# small (and in this case the p-value is exact and is not an approximation). The literature indicates 
# that the usual rule for deciding whether the χ^2 approximation is good enough is that the 
# Chi-square test is not appropriate when the expected values in one of the cells of the contingency 
# table is less than 5, and in this case the Fisher’s exact test is preferred (McCrum-Gardner 2008; Bower 2003, https://statsandr.com/blog/fisher-s-exact-test-in-r-independence-test-for-a-small-sample/#ref-mccrum2008correct).
# Hypotheses
# The hypotheses of the Fisher’s exact test are the same than for the Chi-square test, that is:
# H0: the variables are independent, there is no relationship between the two categorical variables. Knowing the value of one variable does not help to predict the value of the other variable
# H1: the variables are dependent, there is a relationship between the two categorical variables. Knowing the value of one variable helps to predict the value of the other variable
# Example
# Data
# For our example, we want to determine whether there is a statistically significant association between smoking and being a professional athlete. Smoking can only be “yes” or “no” and being a professional athlete can only be “yes” or “no.” The two variables of interest are qualitative variables and we collected data on 14 persons.
# Observed frequencies
# Our data are summarized in the contingency table below reporting the number of people in each subgroup
df <- data.frame(
  "smoke_no" = c(7,0),
  "smoke_yes" = c(2,5),
  row.names = c("Athlete", "Non-athlete"),
  stringsAsFactors = F
)
colnames(df) <- c("Non-smokers", "Smokers")
df
# It is also a good practice to draw a mosaic plot to visually represent the data:
library(graphics)
mosaicplot(df, main = "Mosaic plot", color = T)
# to geom_bar need unpivot data
df %>% 
  rownames_to_column(var = "Athlete?") %>%
  pivot_longer(cols = c("Non-smokers", "Smokers"), names_to = "Smoker?", values_to = "N") %>%
  ggplot() + geom_bar(aes(x = `Athlete?`, y = N, fill = `Smoker?`), stat = "identity", position = "fill")
# We can already see from the plot that the proportion of smokers in the sample is higher among non-athletes than athlete. The plot is however not sufficient to conclude that there is such a significant association in the population.
# Like many statistical tests, this can be done via a hypothesis test. But before seeing how to do it in R, let’s see the concept of expected frequencies.
# Expected frequencies
# Remember that the Fisher’s exact test is used when there is at least one cell in the contingency table of the expected frequencies below 5. To retrieve the expected frequencies, use the chisq.test() function together with $expected:
chisq.test(df)$expected
# The contingency table above confirms that we should use the Fisher’s exact test instead of the Chi-square test because there is at least one cell below 5.
# Tip: although it is a good practice to check the expected frequencies before deciding between the Chi-square and the Fisher test, it is not a big issue if you forget. As you can see above, when doing the Chi-square test in R (with chisq.test()), a warning such as “Chi-squared approximation may be incorrect” will appear. This warning means that the smallest expected frequencies is lower than 5. Therefore, do not worry if you forgot to check the expected frequencies before applying the appropriate test to your data, R will warn you that you should use the Fisher’s exact test instead of the Chi-square test if that is the case.
# (Remember that, as for the Chi-square test of independence, the observations must be independent in order for the Fisher’s exact test to be valid. See more details about the independence assumption in this section.)
# Fisher’s exact test in R
# To perform the Fisher’s exact test in R, use the fisher.test() function as you would do for the Chi-square test:
test_fisher <- fisher.test(df)
test_fisher
# The most important in the output is the p-value. You can also retrieve the p-value with:
test_fisher$p.value
# Note that if your data is not already presented as a contingency table, you can simply use the following code:
# fisher.test(table(dat$variable1, dat$variable2))
# Conclusion and interpretation
# From the output and from test$p.value we see that the p-value is less than the significance level of 5%. Like any other statistical test, if the p-value is less than the significance level, we can reject the null hypothesis. If you are not familiar with p-values, I invite you to read this section. In our context, rejecting the null hypothesis for the Fisher’s exact test of independence means that there is a significant relationship between the two categorical variables (smoking habits and being an athlete or not). Therefore, knowing the value of one variable helps to predict the value of the other variable.  
# Combination of plot and statistical test
# It is possible print the results of the Fisher’s exact test directly on a barplot thanks to the ggbarstats() function from the {ggstatsplot} package (the function has been slightly edited to match our needs).
# It is easier to work with the package when our data is not already in the form of a contingency table so we transform it to a data frame before plotting the results: 
df
x <- c()
for (row in rownames(df)) {
  for (col in colnames(df)) {
    x <- rbind(x, matrix(rep(c(row, col), df[row,col]), ncol = 2, byrow = T))
  }
}
df_raw <- as.data.frame(x)
colnames(df_raw) <- c("Sport_habits", "Smoke_habits")
df_raw
# matrix(rep(c(0,1), 4), ncol = 2, byrow = T)
# Fisher's exact test with raw data
test_fisher <- fisher.test(table(df_raw))
install.packages("ggstatsplot")
library(ggstatsplot)
ggbarstats(
  df_raw, Smoke_habits, Sport_habits,
  results.subtittle = F,
  subtitle = paste0("Fisher's exact test", ", p-value = ", ifelse(test_fisher$p.value < 0.001, "< 0.001", round(test_fisher$p.value, 3)))
)
# From the plot, it is clear that the proportion of smokers among non-athletes is higher than among athletes, suggesting that there is a relationship between the two variables.
# This is confirmed thanks to the p-value displayed in the subtitle of the plot. As previously, we reject the null hypothesis and we conclude that the variables smoking habits and being an athlete or not are dependent (p-value = 0.021).

##################################
# ANOVA in R
# https://statsandr.com/blog/anova-in-r/
install.packages("palmerpenguins")
library(palmerpenguins)
head(penguins)
glimpse(penguins)
summary(penguins)
# The dataset contains data for 344 penguins of 3 different species (Adelie, Chinstrap and Gentoo). The dataset contains 8 variables, but we focus only on the flipper length and the species for this article, so we keep only those 2 variables:
library(tidyverse)
df <- penguins %>% select(species, flipper_length_mm)
summary(df)
df %>% group_by(species) %>% summarise(mean = mean(flipper_length_mm, na.rm = T),
                                       sd = sd(flipper_length_mm, na.rm = T),
                                       p25 = quantile(flipper_length_mm, probs = 0.25, na.rm = T),
                                       p50 = quantile(flipper_length_mm, probs = 0.5, na.rm = T),
                                       p75 = quantile(flipper_length_mm, probs = 0.75, na.rm = T),
                                       )
df %>% ggplot() + geom_boxplot(aes(x = species, y = flipper_length_mm))
df %>% ggplot() + 
  aes(x = species, y = flipper_length_mm, color = species) +
  geom_jitter() + 
  theme()
# The null and alternative hypothesis of an ANOVA are:
# H0: μAdelie = μChinstrap = μGentoo(⇒the 3 species are equal in terms of flipper length)
# H1: at least one mean is different (⇒ at least one species is different from the other 2 species in terms of flipper length)
# Be careful that the alternative hypothesis is not that all means are different. The opposite of all 
# means being equal (H0) is that at least one mean is different from the others (H1). In this sense, 
# if the null hypothesis is rejected, it means that at least one species is different from the other 
# 2, but not necessarily that all 3 species are different from each other.

# Underlying assumptions of ANOVA
# Underlying assumptions of ANOVA
# As for many statistical tests, there are some assumptions that need to be met in order to be able to interpret the results. When one or several assumptions are not met, although it is technically possible to perform these tests, it would be incorrect to interpret the results and trust the conclusions.
# Below are the assumptions of the ANOVA, how to test them and which other tests exist if an assumption is not met:
# - Variable type: ANOVA requires a mix of one continuous quantitative dependent variable (which corresponds to the measurements to which the question relates) and one qualitative independent variable (with at least 2 levels which will determine the groups to compare).
# - Independence: the data, collected from a representative and randomly selected portion of the total population, should be independent between groups and within each group. The assumption of independence is most often verified based on the design of the experiment and on the good control of experimental conditions rather than via a formal test. If you are still unsure about independence based on the experiment design, ask yourself if one observation is related to another (if one observation has an impact on another) within each group or between the groups themselves. If not, it is most likely that you have independent samples. If observations between samples (forming the different groups to be compared) are dependent (for example, if three measurements have been collected on the same individuals as it is often the case in medical studies when measuring a metric (i) before, (ii) during and (iii) after a treatment), the repeated measures ANOVA should be preferred in order to take into account the dependency between the samples.
# - Normality:
# -- In case of small samples, residuals2 should follow approximately a normal distribution. The normality assumption can be tested visually thanks to a histogram and a QQ-plot, and/or formally via a normality test such as the Shapiro-Wilk or Kolmogorov-Smirnov test. If, even after a transformation of your data (e.g., logarithmic transformation, square root, Box-Cox, etc.), the residuals still do not follow approximately a normal distribution, the Kruskal-Wallis test can be applied (kruskal.test(variable ~ group, data = dat in R). This non-parametric test, robust to non normal distributions, has the same goal than the ANOVA—compare 3 or more groups—but it uses sample medians instead of sample means to compare groups.
# -- In case of large samples, normality is not required (this is a common misconception!). By the central limit theorem, sample means of large samples are often well-approximated by a normal distribution even if the data are not normally distributed (Stevens 2013).3 It is therefore not required to test the normality assumption when the number of observations in each group/sample is large (usually n≥30).
# - Equality of variances: the variances of the different groups should be equal in the populations (an assumption called homogeneity of the variances, or even sometimes referred as homoscedasticity, as opposed to heteroscedasticity if variances are different across groups). This assumption can be tested graphically (by comparing the dispersion in a boxplot or dotplot for instance), or more formally via the Levene’s test (leveneTest(variable ~ group) from the {car} package) or Bartlett’s test, among others. If the hypothesis of equal variances is rejected, another version of the ANOVA can be used: the Welch ANOVA (oneway.test(variable ~ group, var.equal = FALSE)). Note that the Welch ANOVA does not require homogeneity of the variances, but the distributions should still follow approximately a normal distribution. Note that the Kruskal-Wallis test does not require the assumptions of normality nor homoscedasticity of the variances.4
# - Outliers: An outlier is a value or an observation that is distant from the other observations. There should be no significant outliers in the different groups, or the conclusions of your ANOVA may be flawed. There are several methods to detect outliers in your data but in order to deal with them, it is your choice to either:
# -- use the non-parametric version (i.e., the Kruskal-Wallis test)
# -- transform your data (logarithmic or Box-Cox transformation, among others)
# -- or remove them (be careful)
# Choosing the appropriate test depending on whether assumptions are met may be confusing so here is a brief summary:
# 1 Check that your observations are independent.
# 2 Sample sizes:
# - In case of small samples, test the normality of residuals:
# -- If normality is assumed, test the homogeneity of the variances:
# --- If variances are equal, use ANOVA.
# --- If variances are not equal, use the Welch ANOVA.
# -- If normality is not assumed, use the Kruskal-Wallis test.
# - In case of large samples normality is assumed, so test the homogeneity of the variances:
# -- If variances are equal, use ANOVA.
# -- If variances are not equal, use the Welch ANOVA.
# Now that we have seen the underlying assumptions of the ANOVA, we review them specifically for our dataset before applying the appropriate version of the test.
# 1 Variable type
# The dependent variable flipper_length_mm is a quantitative variable and the independent variable species is a qualitative one (with 3 levels corresponding to the 3 species). So we have a mix of the two types of variable and this assumption is met.
# 2 Independence
# Independence of the observations is assumed as data have been collected from a randomly selected portion of the population and measurements within and between the 3 samples are not related.
# The independence assumption is most often verified based on the design of the experiment and on the good control of experimental conditions, as it is the case here.
# 3 Normality
# Since the smallest sample size per group (i.e., per species) is 68, we have large samples. Therefore, we do not need to check normality. Normally, we would directly test the homogeneity of the variances without testing normality. However, for the sake of illustration, we act as if the sample sizes were small in order to illustrate what would need to be done in that case.
# Remember that normality of residuals can be tested visually via a histogram and a QQ-plot, and/or formally via a normality test (Shapiro-Wilk test for instance).
# Before checking the normality assumption, we first need to compute the ANOVA (more on that in this section). We then save the results in res_aov :
res_aov <- aov(flipper_length_mm ~ species, data = df)
res_aov
summary(res_aov)
# histogram
ggplot(data = tibble(x = res_aov$residuals)) + geom_histogram(aes(x))
# qq-plot
install.packages('car')
library(car)
par(mfrow = c(1,2)) # combine plots
hist(res_aov$residuals)
qqPlot(res_aov$residuals, id = F)
# Still for the sake of illustration, we also now test the normality assumption via a normality test. You can use the Shapiro-Wilk test or the Kolmogorov-Smirnov test, among others. Remember that the null and alternative hypothesis are:
# H0: data come from a normal distribution
# H1 : data do not come from a normal distribution
# In R, we can test normality of the residuals with the Shapiro-Wilk test thanks to the shapiro.test() function:
shapiro.test(res_aov$residuals)
# P-value of the Shapiro-Wilk test on the residuals is larger than the usual significance level of α = 5%, so we do not reject the hypothesis that residuals follow a normal distribution (p-value = 0.261).
# Remember that if the normality assumption was not reached, some transformation(s) would need to be applied on the raw data in the hope that residuals would better fit a normal distribution, or you would need to use the non-parametric version of the ANOVA—the Kruskal-Wallis test.
# As pointed out by a reader (see comments at the very end of the article), the normality assumption can also be tested on the “raw” data (i.e., the observations) instead of the residuals. However, if you test the normality assumption on the raw data, it must be tested for each group separately as the ANOVA requires normality in each group.
# Testing normality on all residuals or on the observations per group is equivalent, and will give similar results. Indeed, saying “The distribution of Y within each group is normally distributed” is the same as saying “The residuals are normally distributed.”
# Remember that residuals are the distance between the actual value of Y and the mean value of Y for a specific value of X, so the grouping variable is induced in the computation of the residuals.
# So in summary, in ANOVA you actually have two options for testing normality:
# - Checking normality separately for each group on the “raw” data (Y values)
# - Checking normality on all residuals (but not per group)
# In practice, you will see that it is often easier to just use the residuals and check them all together, especially if you have many groups or few observations per group.
# 4 Equality of variances - homogeneity
# Assuming residuals follow a normal distribution, it is now time to check whether the variances are equal across species or not. The result will have an impact on whether we use the ANOVA or the Welch ANOVA.
# This can again be verified visually—via a boxplot or dotplot—or more formally via a statistical test (Levene’s test, among others).
# boxplot
boxplot(flipper_length_mm ~ species, data = df)
library(lattice)
dotplot(flipper_length_mm ~ species, data = df)
# Both the boxplot and the dotplot show a similar variance for the different species. In the boxplot, this can be seen by the fact that the boxes and the whiskers have a comparable size for all species. There are a couple of outliers as shown by the points outside the whiskers, but this does not change the fact that the dispersion is more or less the same between the different species.
# In the dotplot, this can be seen by the fact that points for all 3 species have more or less the same range, a sign of the dispersion and thus the variance being similar.
# Like the normality assumption, if you feel that the visual approach is not sufficient, you can formally test for equality of the variances with a Levene’s or Bartlett’s test. Notice that the Levene’s test is less sensitive to departures from normal distribution than the Bartlett’s test.
# The null and alternative hypothesis for both tests are:
# H0: variances are equal
# H1: at least one variance is different
# In R, the Levene’s test can be performed thanks to the leveneTest() function from the {car} package:
# Levene's test
library(car)
leveneTest(flipper_length_mm ~ species, data = df)
# The p-value being larger than the significance level of 0.05, we do not reject the null hypothesis, so we cannot reject the hypothesis that variances are equal between species (p-value = 0.719).
# This result is also in line with the visual approach, so the homogeneity of variances is met both visually and formally.
# Outliers
# There are several techniques to detect outliers. In this article, we focus on the most simple one (yet very efficient)—the visual approach via a boxplot:
par(mfrow = c(1,1))
boxplot(flipper_length_mm ~ species, data = df)  
# There is one outlier in the group Adelie, as defined by the interquartile range criterion. This point is, however, not seen as a significant outlier so we can assume that the assumption of no significant outliers is met.

# ANOVA
# We showed that all assumptions of the ANOVA are met. We can thus proceed to the implementation of the ANOVA in R, but first, let’s do some preliminary analyses to better understand the research question.
# Preliminary analyses
# A good practice before actually performing the ANOVA in R is to visualize the data in relation to the research question. The best way to do so is to draw and compare boxplots of the quantitative variable flipper_length_mm for each species.
# This can be done with the boxplot() function in base R (same code than the visual check of equal variances):
boxplot(flipper_length_mm ~ species, data = df)
ggplot(df) + geom_boxplot(aes(x = species, y = flipper_length_mm))
# The boxplots above show that, at least for our sample, penguins of the species Gentoo seem to have the biggest flipper, and Adelie species the smallest flipper.
# Besides a boxplot for each species, it is also a good practice to compute some descriptive statistics such as the mean and standard deviation by species. This can be done, for instance, with the aggregate() function:
aggregate(
  flipper_length_mm ~ species, 
  data = df, 
  function(x) round(c(mean = mean(x), sd = sd(x)),2)
  )
# or with summarise()
df %>% group_by(species) %>% summarise(mean = mean(flipper_length_mm, na.rm = T), 
                                       sd = sd(flipper_length_mm, na.rm = T))
# Mean is also the lowest for Adelie and highest for Gentoo. Boxplots and descriptive statistics are, however, not enough to conclude that flippers are significantly different in the 3 populations of penguins.

# ANOVA in R
# As you guessed by now, only the ANOVA can help us to make inference about the population given the sample at hand, and help us to answer the initial research question “Is the length of the flippers different between the 3 species of penguins?”
# ANOVA in R can be done in several ways, of which two are presented below:
# 1 With the oneway.test() function:
oneway.test(flipper_length_mm ~ species, data = df, var.equal = T) # var.equal = T assumes equal variances
# 2 With the summary() and aov() functions:
res_aov <- aov(flipper_length_mm ~ species, data = df)
res_aov
summary(res_aov)
# AK: check for smth. common in linear regression
  fit <- lm(flipper_length_mm ~ species, data = df)
  fit
  summary(fit) # equal: F statistics, residuals std error, degree of freedom
  hist(fit$residuals) # the same like for res_aov$resuduals
# As you can see from the two outputs above, the test statistic (F = in the first method and F value in the second one) and the p-value (p-value in the first method and Pr(>F) in the second one) are exactly the same for both methods, which means that in case of equal variances, results and conclusions will be unchanged.
# The advantage of the first method is that it is easy to switch from the ANOVA (used when variances are equal) to the Welch ANOVA (used when variances are unequal). This can be done by replacing var.equal = TRUE by var.equal = FALSE, as presented below:
oneway.test(flipper_length_mm ~ species, data = df, var.equal = F) # assuming not equal variances
# The advantage of the second method, however, is that:
# the full ANOVA table (with degrees of freedom, mean squares, etc.) is printed, which may be of interest in some (theoritical) cases
# results of the ANOVA (res_aov) can be saved for later use (especially useful for post-hoc tests)
# Interpretations of ANOVA results
# Given that the p-value is smaller than 0.05, we reject the null hypothesis, so we reject the hypothesis that all means are equal. Therefore, we can conclude that at least one species is different than the others in terms of flippers length (p-value < 2.2e-16).
# (For the sake of illustration, if the p-value was larger than 0.05: we cannot reject the null hypothesis that all means are equal, so we cannot reject the hypothesis that the 3 considered species of penguins are equal in terms of flippers length.)
# A nice and easy way to report results of an ANOVA in R is with the report() function from the {report} package:
# install.packages("remotes")  
# remotes::install_github("easystats/report") # You only need to do that once
library(report)
report(res_aov)
# As you can see, the function interprets the results for you and indicates a large and significant main effect of the species on the flipper length (p-value < .001).
# Note that the report() function can be used for other analyses. See more tips and tricks in R if you find this one useful.
# What’s next?
# If the null hypothesis is not rejected (p-value ≥ 0.05), it means that we do not reject the hypothesis that all groups are equal. The ANOVA more or less stops here. Other types of analyses can be performed of course, but—given the data at hand—we could not prove that at least one group was different so we usually do not go further with the ANOVA.
# On the contrary, if the null hypothesis is rejected (as it is our case since the p-value < 0.05), we proved that at least one group is different. We can decide to stop here if we are only interested to test whether all species are equal in terms of flippers length.
# But most of the time, when we showed thanks to an ANOVA that at least one group is different, we are also interested in knowing which one(s) is(are) different. Results of an ANOVA, however, do NOT tell us which group(s) is(are) different from the others.
# To test this, we need to use other types of test, referred as post-hoc tests (in Latin, “after this,” so after obtaining statistically significant ANOVA results) or multiple pairwise-comparison tests.5 This family of statistical tests is the topic of the following sections.
# Post-hoc test
# Issue of multiple testing
# In order to see which group(s) is(are) different from the others, we need to compare groups 2 by 2. In practice, since there are 3 species, we are going to compare species 2 by 2 as follows:
# -Chinstrap versus Adelie
# - Gentoo vs. Adelie
# - Gentoo vs. Chinstrap
# In theory, we could compare species thanks to 3 Student’s t-tests since we need to compare 2 groups and a t-test is used precisely in that case.
# However, if several t-tests are performed, the issue of multiple testing (also referred as multiplicity) arises. In short, when several statistical tests are performed, some will have p-values less than α purely by chance, even if all null hypotheses are in fact true.
# To demonstrate the problem, consider our case where we have 3 hypotheses to test and a desired significance level of 0.05. The probability of observing at least one significant result (at least one p-value < 0.05) just due to chance is:
# P(at least 1 sig. result) = 1 − P(no sig. results ) = 1 − (1 − 0.05)^3 = 0.142625
# So, with as few as 3 tests being considered, we already have a 14.26% chance of observing at least one significant result, even if all of the tests are actually not significant.
# And as the number of groups increases, the number of comparisons increases as well, so the probability of having a significant result simply due to chance keeps increasing. For example, with 10 groups we need to make 45 comparisons and the probability of having at least one significant result by chance becomes 1 − (1 − 0.05)^45 = 90%. So it is very likely to observe a significant result just by chance when comparing 10 groups, and when we have 14 groups or more we are almost certain (99%) to have a false positive!
# Post-hoc tests take into account that multiple tests are done and deal with the problem by adjusting α in some way, so that the probability of observing at least one significant result due to chance remains below our desired significance level.6
# Post-hoc tests in R and their interpretation
# Post-hoc tests are a family of statistical tests so there are several of them. The most common ones are:
# Tukey HSD, used to compare all groups to each other (so all possible comparisons of 2 groups).
# Dunnett, used to make comparisons with a reference group. For example, consider 2 treatment groups and one control group. If you only want to compare the 2 treatment groups with respect to the control group, and you do not want to compare the 2 treatment groups to each other, the Dunnett’s test is preferred.
# Bonferroni correction if have a set of planned comparisons to do.
# The Bonferroni correction is simple: you simply divide the desired global 
# α level by the number of comparisons. In our example, we have 3 comparisons so if we want to keep a global α = 0.05, we have α′ = 0.05/3 = 0.0167. We can then simply perform a Student’s t-test for each comparison, and compare the obtained p-values with this new α′.
# The other two post-hoc tests are presented in the next sections.
# Note that variances are assumed to be equal for all three methods (unless you use the Welch’s t-test instead of the Student’s t-test with the Bonferroni correction). If variances are not equal, you can use the Games-Howell test, among others.
# Tukey HSD test
# In our case, since there is no “reference” species and we are interested in comparing all species, we are going to use the Tukey HSD test.
# In R, the Tukey HSD test is done as follows. This is where the second method to perform the ANOVA comes handy because the results (res_aov) are reused for the post-hoc test:
install.packages("multcomp")
library(multcomp)
# Tukey HSD test
tukey_test <- glht(res_aov, linfct = mcp(species = "Tukey"))
tukey_test
summary(tukey_test)
# In the output of the Tukey HSD test, we are interested in the table displayed after Linear Hypotheses:, and more precisely, in the first and last column of the table. The first column shows the comparisons which have been made; the last column (Pr(>|t|)) shows the adjusted7 p-values for each comparison (with the null hypothesis being the two groups are equal and the alternative hypothesis being the two groups are different).
# It is these adjusted p-values that are used to test whether two groups are significantly different or not, and we can be confident that the entire set of comparisons collectively has an error rate of 0.05.
# In our example, we tested:
# - Chinstrap versus Adelie (line Chinstrap - Adelie == 0)
# - Gentoo vs. Adelie (line Gentoo - Adelie == 0)
# - Gentoo vs. Chinstrap (line Gentoo - Chinstrap == 0)
# All three ajusted p-values are smaller than 0.05, so we reject the null hypothesis for all comparisons, which means that all species are significantly different in terms of flippers length.
# The results of the post-hoc test can be visualized with the plot() function:
par(mar = c(3,8,3,3))
plot(tukey_test)
# We see that the confidence intervals do not cross the zero line, which indicate that all groups are significantly different.
# Note that the Tukey HSD test can also be done in R with the TukeyHSD() function:
TukeyHSD(res_aov)
# With this code, it is the column p adj (also the last column) which is of interest. Notice that the conclusions are the same than above: all species are significantly different in terms of flippers length.
# The results can also be visualized with the plot() function:
plot(TukeyHSD(res_aov))
# Dunnett’s test
# We have seen in this section that as the number of groups increases, the number of comparisons 
# also increases. And as the number of comparisons increases, the post-hoc analysis must lower 
# the individual significance level even further, which leads to lower statistical power (so a 
# difference between group means in the population is less likely to be detected).
# One method to mitigate this and increase the statistical power is by reducing the number of 
# comparisons. This reduction allows the post-hoc procedure to use a larger individual error rate 
# to achieve the desired global error rate. While comparing all possible groups with a Tukey HSD 
# test is a common approach, many studies have a control group and several treatment groups. For 
# these studies, you may need to compare the treatment groups only to the control group, which 
# reduces the number of comparisons.
# Dunnett’s test does precisely this—it only compares a group taken as reference to all other 
# groups, but it does not compare all groups to each others.
# So to recap:
# - the Tukey HSD test allows to compares all groups but at the cost of less power
# - the Dunnett’s test allows to only make comparisons with a reference group, but with the benefit of more power
# Now, again for the sake of illustration, consider that the species Adelie is the reference 
# species and we are only interested in comparing the reference species against the other 2 species.
# In that scenario, we would use the Dunnett’s test.
# In R, the Dunnett’s test is done as follows (the only difference with the code for the Tukey HSD 
# test is in the line linfct = mcp(species = "Dunnett")):
library(multcomp)
# Dunnett's test:
dunnett_test <- glht(res_aov, linfct = mcp(species = "Dunnett"))
dunnett_test
summary(dunnett_test)
# The interpretation is the same as for the Tukey HSD test’s except that in the Dunett’s test we only compare:
# - Chinstrap versus Adelie (line Chinstrap - Adelie == 0)
# - Gentoo vs. Adelie (line Gentoo - Adelie == 0)
# Both adjusted p-values (displayed in the last column) are below 0.05, so we reject the null hypothesis for both comparisons. This means that both the species Chinstrap and Gentoo are significantly different from the reference species Adelie in terms of flippers length. (Nothing can be said about the comparison between Chinstrap and Gentoo though.)
# Again, the results of the post-hoc test can be visualized with the plot() function:
par(mar = c(3,8,3,3))
plot(dunnett_test)
# We see that the confidence intervals do not cross the zero line, which indicate that both the species Gentoo and Chinstrap are significantly different from the reference species Adelie.
# Note that in R, by default, the reference category for a factor variable is the first category in alphabetical order. This is the reason that, by default, the reference species is Adelie.
# The reference category can be changed with the relevel() function (or with the {questionr} addin). Considering that we want Gentoo as the reference category instead of Adelie:
levels(df$species)
df$species <- relevel(df$species, ref = "Gentoo")
# Gentoo now being the first category of the three, it is indeed considered as the reference level.
# In order to perform the Dunnett’s test with the new reference we first need to rerun the ANOVA to take into account the new reference:
res_aov2 <- aov(flipper_length_mm ~ species, data = df)
summary(res_aov2)
# We can then run the Dunett’s test with the new results of the ANOVA:
dunnett_test2 <- glht(res_aov2, linfct = mcp(species = "Dunnett"))
summary(dunnett_test2)
par(mar = c(3,8,3,3))
plot(dunnett_test2)
# From the results above we conclude that Adelie and Chinstrap species are significantly different from Gentoo species in terms of flippers length (adjusted p-values < 1e-10).
# Note that even if your study does not have a reference group which you can compare to the other groups, it is still often better to do multiple comparisons determined by some research questions than to do all-pairwise tests. By reducing the number of post-hoc comparisons to what is necessary only, and no more, you maximize the statistical power.8
# Other p-values adjustment methods
# For the interested readers, note that you can use other p-values adjustment methods by using the pairwise.t.test() function:
pairwise.t.test(df$flipper_length_mm, df$species, p.adjust.method = "holm")  
# By default, the Holm method is applied but other methods exist. See ?p.adjust for all available options.
?p.adjust
# Visualization of ANOVA and post-hoc tests on the same plot
library(ggstatsplot)
ggbetweenstats(
  data = df,
  x = species,
  y = flipper_length_mm,
  type = "parametric", # ANOVA or Kruskal-Wallis
  var.equal = T, # ANOVA or Welch ANOVA
  plot.type = 'box',
  pairwise.comparisons = T,
  pairwise.display = 'significant',
  centrality.plotting = F,
  bf.message = F
)







