
# Introduction.mp4
# Goals of A/B testing
# The "hypothesis" for an A/B testing experiment refers to?
# Answer the question
# Possible Answers
# - The variable you are going to measure.
# - The problem you are interested in testing.
# - The variable you are going to differ between conditions.
# - What you think will happen as a result of the experiment. +
# The hypothesis is your predictions for how the results of the experiment will turn out.

# Preliminary data exploration
# In the video, we read in our preliminary dataset and had a quick look at it. Let's get some practice reading in data and do a little more exploration.
# Instructions
# - Load the tidyverse package.
# - Read in the CSV click_data.csv with read_csv().
# - Take a look at the data. As you can see, it has two columns: 1) visit_date for the date when a person visited the page, and 2) clicked_adopt_today, which is 1 if they clicked the button and 0 if they didn't.
# - Find oldest and most recent date in the data using min() and max() on the visit_date column.

# Preliminary data exploration
# In the video, we read in our preliminary dataset and had a quick look at it. Let's get some practice reading in data and do a little more exploration.
# Instructions
# Load the tidyverse package.
# Read in the CSV click_data.csv with read_csv().
# Take a look at the data. As you can see, it has two columns: 1) visit_date for the date when a person visited the page, and 2) clicked_adopt_today, which is 1 if they clicked the button and 0 if they didn't.
# Find oldest and most recent date in the data using min() and max() on the visit_date column.
# Load tidyverse
library(tidyverse)
library(ggplot2)
# Read in data
click_data <- read_csv("click_data.csv")
# Parsed with column specification:
#   cols(
#     visit_date = col_date(format = ""),
#     clicked_adopt_today = col_double()
#   )
click_data
# A tibble: 3,650 x 2
# visit_date clicked_adopt_today
# <date>                   <dbl>
# 1 2017-01-01                   1
# 2 2017-01-02                   1
# 3 2017-01-03                   0
# 4 2017-01-04                   1
# 5 2017-01-05                   1
# 6 2017-01-06                   0
# 7 2017-01-07                   0
# 8 2017-01-08                   0
# 9 2017-01-09                   0
#10 2017-01-10                   0
# ... with 3,640 more rows
# Find oldest and most recent date
min(click_data$visit_date)
# [1] "2017-01-01"
max(click_data$visit_date)
# [1] "2017-12-31"
# AK: do we have summary for every day? or stat per every user per day?
click_data %>% group_by(visit_date) %>% summarise(n = n(), sum = sum(clicked_adopt_today))
# A tibble: 365 x 3
#   visit_date     n   sum
#   <date>     <int> <dbl>
# 1 2017-01-01    10     3
# 2 2017-01-02    10     2
# 3 2017-01-03    10     2
# 4 2017-01-04    10     3
# 5 2017-01-05    10     4
# 6 2017-01-06    10     2
# 7 2017-01-07    10     0
# 8 2017-01-08    10     1
# 9 2017-01-09    10     4
#10 2017-01-10    10     3
# ... with 355 more rows
# AK: check data for 2017-01-01
click_data%>% filter(visit_date == "2017-01-01")
# A tibble: 10 x 2
# visit_date clicked_adopt_today
# <date>                   <dbl>
# 1 2017-01-01                   1
# 2 2017-01-01                   0
# 3 2017-01-01                   0
# 4 2017-01-01                   0
# 5 2017-01-01                   1
# 6 2017-01-01                   0
# 7 2017-01-01                   0
# 8 2017-01-01                   0
# 9 2017-01-01                   1
#10 2017-01-01                   0
# AK: so this is data per each user per day. Each day we have 10 users. 1 - she converted, 0 - no. No multiple conversions (we didn't have or decided to track only yes/no)
click_data %>% group_by(visit_date) %>% summarise(n = n(), sum = sum(clicked_adopt_today)) %>% 
  ggplot() + geom_line(aes(x = visit_date, y = n), color = 'red') + geom_line(aes(x = visit_date, y = sum))
# http://joxi.ru/l2Z7vR6HlQLpVA

# 2 Baseline conversion rates.mp4
# Current conversion rate day of week
# Just as we did in the slides, let's start by computing our pre-experiment conversion rate as a baseline. Rather than computing it by month though, let's compute by day of the week. The core tidyverse packages and lubridate are pre-loaded for you.
# Instructions
# Read in the CSV click_data.csv using the read_csv() function.
# Use the function wday() from lubridate to compute the conversion rate by day of the week.
# Take a look at the data to see how much conversion rates vary throughout the week.
head(click_data)
# A tibble: 6 x 2
# visit_date clicked_adopt_today
# <date>                   <dbl>
# 1 2017-01-01                   1
# 2 2017-01-02                   1
# 3 2017-01-03                   0
# 4 2017-01-04                   1
# 5 2017-01-05                   1
# 6 2017-01-06                   0
# Calculate the mean conversion rate by day of the week
click_data %>%
  group_by(wday(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))
# A tibble: 7 x 2
# `wday(visit_date)` conversion_rate
# <dbl>           <dbl>
# 1                  1           0.3  
# 2                  2           0.277
# 3                  3           0.271
# 4                  4           0.298
# 5                  5           0.271
# 6                  6           0.267
# 7                  7           0.256
# It looks like conversion rates vary a little bit from day to day, but not too much.

# Current conversion rate week
# Let's try one more grouping for our data and see how much conversion rates vary by week in the calendar year.
# Instructions
# Use the group_by() function to compute conversion rates by week.
# Use the week() function to separate dates into weeks of the year.
# Compute the mean() over the correct column in the dataframe.
# Read in the data
click_data <- read_csv("click_data.csv")
Parsed with column specification:
  cols(
    visit_date = col_date(format = ""),
    clicked_adopt_today = col_double()
  )
head(click_data)
# A tibble: 6 x 2
# visit_date clicked_adopt_today
# <date>                   <dbl>
# 1 2017-01-01                   1
# 2 2017-01-02                   1
# 3 2017-01-03                   0
# 4 2017-01-04                   1
# 5 2017-01-05                   1
# 6 2017-01-06                   0
# Calculate the mean conversion rate by week of the year
click_data %>%
  group_by(week(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))
# A tibble: 53 x 2
# `week(visit_date)` conversion_rate
# <dbl>           <dbl>
# 1                  1           0.229
# 2                  2           0.243
# 3                  3           0.171
# 4                  4           0.129
# 5                  5           0.157
# 6                  6           0.186
# 7                  7           0.257
# 8                  8           0.171
# 9                  9           0.186
#10                 10           0.2  
# ... with 43 more rows
# Great! You've now looked at four different ways to compute baseline conversion rates: overall, by month, by day of the week, and by week of the year.

# Plotting conversion rate seasonality
# Earlier we said that based on the summary by month, it looks like there are seasonal effects. Conversion rates are higher in the summer and in December. Let's visualize that effect, but by week of the year. We'll use ggplot2 to build our plot. The tidyverse packages and lubridate are pre-loaded for you.
# Instructions
# Save your data frame with conversion rates by week of the year to a data frame called click_data_sum.
# Use the newly created data frame to make a plot where the x-axis is the week of the year and the y-axis is the conversion rate.
# Use the geom_point() and geom_line() geoms to build the plot.
# We also updated our plot a bit using scale_y_continuous() to make sure our axes goes from 0 to 1 and converts the values to percentages. The percent setting comes from the scales package.
# Compute conversion rate by week of the year
click_data_sum <- click_data %>%
  group_by(week(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))
head(click_data_sum)
# A tibble: 6 x 2
# `week(visit_date)` conversion_rate
# <dbl>           <dbl>
# 1                  1           0.229
# 2                  2           0.243
# 3                  3           0.171
# 4                  4           0.129
# 5                  5           0.157
# 6                  6           0.186
# Build plot
ggplot(click_data_sum, aes(x = `week(visit_date)`, y = conversion_rate)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), labels = percent)
# http://joxi.ru/12ML1YjCgQL0Gm
# Fantastic! We now have a plot to visualize our knowledge about seasonal conversion rates by week of the year.

# 3 Experimental design, power analysis.mp4
# Randomized vs. sequential
# You're designing a new experiment and you have two conditions. What's the best method for comparing your two conditions?
# Answer the question
# Possible Answers
# - Run control condition for a month and then test condition for a month.
# - Use old data as control condition and run test condition for a month.
# - Run control and test conditions simultaneously for two months. +
# - Run control condition for a month, wait a year, then run test condition for a month.
# Running both conditions simultaneously is best because they are exposed to the same seasonal and other variables. Waiting a year would require too much time to analyze your experiment, and a lot of other variables can change in a year!

# SSizeLogisticBin() documentation
# Let's take a moment to learn more about the SSizeLogisticBin() function from powerMediation that was introduced in the slides. Look at the documentation page for the SSizeLogisticBin() function by calling help(SSizeLogisticBin). The powerMediation package is pre-loaded for you. What is another way to phrase what p2 signifies?
# Instructions
# Possible Answers
# - The probability when X = 0 (the control condition).
# - The probability when X = 1 (the test condition). +
# - The proportion of the dataset where X = 1.
# - The Type I error rate.
# Correct! The argument p2 is the expected value for the test condition.

# Power analysis August
# In the video, we ran a power analysis assuming we were going to run the experiment in January. Run a new power analysis assuming we'll run the experiment in August. To compute the conversion rate for our control and test you'll need to look at the dataset. Be sure to round all values to the hundredth of a percent (e.g., 0.13453 to 0.13). The data click_data_month is available pre-loaded for you to look up the conversion rate for August.
# Instructions
# - Fill in the proportion of the sample from the test condition (B), the alpha (alpha), and the power (power).
# - Fill in the conversion rate for our control condition using the conversion rate for August (p1). Remember to round.
# - Fill in the expected conversion rate for our test condition (p2), assuming a 10 percentage point increase (p1 + 0.1). Remember to round.
# - Look at total_sample_size to see how many data points you need in total across both conditions.
# Load powerMediation
library(powerMediation)
click_data_month
# A tibble: 12 x 2
# `month(visit_date, label = TRUE)` conversion_rate
# <ord>                                       <dbl>
# 1 Jan                                         0.197
# 2 Feb                                         0.189
# 3 Mar                                         0.145
# 4 Apr                                         0.15 
# 5 May                                         0.258
# 6 Jun                                         0.333
# 7 Jul                                         0.348
# 8 Aug                                         0.542
# 9 Sep                                         0.293
#10 Oct                                         0.161
#11 Nov                                         0.233
#12 Dec                                         0.465
click_data %>% group_by(month(visit_date)) %>% summarise(cr = mean(clicked_adopt_today))
# A tibble: 12 x 2
# `month(visit_date)`    cr
# <dbl> <dbl>
# 1                   1 0.197
# 2                   2 0.189
# 3                   3 0.145
# 4                   4 0.15 
# 5                   5 0.258
# 6                   6 0.333
# 7                   7 0.348
# 8                   8 0.542
# 9                   9 0.293
#10                  10 0.161
#11                  11 0.233
#12                  12 0.465
# Compute and look at sample size for experiment in August
total_sample_size <- SSizeLogisticBin(p1 = 0.54,
                                      p2 = 0.64,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size
# [1] 758
# AK: for B = 0.3
total_sample_size <- SSizeLogisticBin(p1 = 0.54,
                                      p2 = 0.64,
                                      B = 0.3,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size
# [1] 906
# Great! We now know if we ran the experiment in August we would need at least 758 data points or 379 per group.

# Power analysis August 5 percentage point increase
# Let's say you've reconsidered your expectations for running the experiment in August. 54% is already a really high conversion rate, so increasing it by 10 percentage points may be difficult. Rerun your power analysis assuming only a 5 percentage point increase in your conversion rate for the test condition.
# Instructions
# - Update your expected value to only be a 5 percentage point increase over conversion rates in August.
# - Fill in the argument names for proportion, alpha, and beta.
# - Take a look at your sample size to see how they've changed.
# Load powerMediation
library(powerMediation)
# Compute and look at sample size for experiment in August with a 5 percentage point increase
total_sample_size <- SSizeLogisticBin(p1 = 0.54,
                                      p2 = 0.59,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size
# [1] 3085
# Wow! If we only expect a 5 percentage point conversion rate increase we need 3085 (about 1543 per group) to see a significant effect, much more than if we predicted a 10 percentage point increase.

# 4 Analyzing results.mp4
# Plotting results
# It turns out there was a bug on January 10th and all of the data from that day was corrupted. A new data frame was created for you called experiment_data_clean, which drops the corrupted data. Re-build our plot showing the two conversion rates, one for the test condition and one for the control condition. The tidyverse and the updated data set are pre-loaded for you.
# Instructions
# Create a new data frame called experiment_data_clean_sum that groups by both condition and visit_date.
# Use the new data frame to build the plot, setting color and group to condition.
head(experiment_data_clean)
# A tibble: 6 x 3
# visit_date condition clicked_adopt_today
# <date>     <chr>                   <dbl>
# 1 2018-01-01 control                     0
# 2 2018-01-01 control                     1
# 3 2018-01-01 control                     0
# 4 2018-01-01 control                     0
# 5 2018-01-01 test                        0
# 6 2018-01-01 test                        0
# Group and summarize data
experiment_data_clean_sum <- experiment_data_clean %>%
  group_by(condition, visit_date) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))
experiment_data_clean_sum
# A tibble: 60 x 3
# Groups:   condition [?]
# condition visit_date conversion_rate
# <chr>     <date>               <dbl>
# 1 control   2018-01-01          0.25  
# 2 control   2018-01-02          0.25  
# 3 control   2018-01-03          0     
# 4 control   2018-01-04          0.364 
# 5 control   2018-01-05          0.111 
# 6 control   2018-01-06          0.111 
# 7 control   2018-01-07          0.286 
# 8 control   2018-01-08          0.0667
# 9 control   2018-01-09          0     
#10 control   2018-01-11          0     
# ... with 50 more rows
summary(experiment_data_clean_sum)
# condition           visit_date         conversion_rate 
# Length:60          Min.   :2018-01-01   Min.   :0.0000  
# Class :character   1st Qu.:2018-01-08   1st Qu.:0.1111  
# Mode  :character   Median :2018-01-16   Median :0.2792  
#                    Mean   :2018-01-16   Mean   :0.2636  
#                    3rd Qu.:2018-01-24   3rd Qu.:0.3774  
#                    Max.   :2018-01-31   Max.   :0.7000  
# Make plot of conversion rates over time
ggplot(experiment_data_clean_sum,
       aes(x = visit_date,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom_line()
# It looks like our results were pretty consistent over the month. The test condition almost always performed better.

# glm() documentation
# To analyze our results, we used the function glm() and set family to binomial. Take a look at the documentation to using ?glm to see what exactly the family argument is.
# Instructions
# Possible Answers
# An optional vector of ‘prior weights’ to be used in the fitting process.
# A symbolic description of the model to be fitted.
# A description of the error distribution and link function to be used in the model.
# Correct! The argument family can be used to express different error distributions.

# Practice with glm()
# In the video, we analyzed our results with a logistic regression, using the function tidy() from 
# the broom package to see a cleaned up version of our results. Run the same analysis using our new 
# data frame experiment_data_clean.
# Instructions
# - Use the newly created experiment_data_clean to find the conversion rates for each condition.
# - Using the glm() function, run a logistic regression of condition by clicked_adopt_today.
# Load package for cleaning model results
library(broom)
head(experiment_data_clean)
# A tibble: 6 x 3
# visit_date condition clicked_adopt_today
# <date>     <chr>                   <dbl>
# 1 2018-01-01 control                     0
# 2 2018-01-01 control                     1
# 3 2018-01-01 control                     0
# 4 2018-01-01 control                     0
# 5 2018-01-01 test                        0
# 6 2018-01-01 test                        0
# View summary of results
experiment_data_clean %>%
  group_by(condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today), n = n())
# A tibble: 2 x 3
#   condition conversion_rate     n
#   <chr>               <dbl> <int>
# 1 control             0.166   283
# 2 test                0.386   285
# Run logistic regression
experiment_results <- glm(clicked_adopt_today ~ condition,
                          family = "binomial",
                          data = experiment_data_clean) %>%
  tidy()
experiment_results
# term  estimate std.error  statistic      p.value
# 1   (Intercept) -1.613684 0.1597307 -10.102533 5.383364e-24
# 2 conditiontest  1.149379 0.2007961   5.724108 1.039787e-08
# Great! Even dropping a day's worth of data our result was still significant. However, if this 
# happened in real life we really should have run the experiment for another day to make sure we 
# got the correct number of data points needed according to the power analysis.

# 5 Designing follow-up experiments.mp4
# Follow-up experiment 1 design
# You're now designing your follow-up experiments. What's the best path forward?
# Answer the question
# Possible Answers
# - Build one experiment where your test condition is a kitten in a hat. If the experiment works, run a second experiment with a kitten in a hat as the control and two kittens in hats as the test.
# - Build one experiment where your control is the cat in a hat and the test is two kittens in hats.
#   Incorrect
#   If you change two things at once, you don't know what your affect is driven by.
# - Build one experiment but with three conditions. Control is the cat in a hat, test one is a kitten in a hat, and test two is two kittens in hats.
#   Incorrect
#   Three conditions will reduce power and make it more complicated to know what's going on.
# - Build one experiment but with three conditions. Control is the cat in a hat, test one is a kitten in a hat, and test two is two adult cats in hats.
#   Incorrect
#   Three conditions will reduce power and make it more complicated to know what's going on.
# Running two sequential experiments is preferred because you can be confident in which variable drove the effect, and be sure to have one control group per test condition.

# Follow-up experiment 1 power analysis
# Let's start your kitten experiment. The hat already increased conversion rates a lot, but you 
# think making the photo a kitten will really make the difference, so you think conversion rates 
# will go up to 59%. Let's run a power analysis to see how much data you need to see a significant effect.
# Instructions
# Load the powerMediation package.
# Set p1 to the conversion rate you found in the previous experiment (39%).
# Set p2 to the expected conversion rate with the picture of a kitten in a hat.
# Name the third, fourth, and fifth arguments that has values of 0.5, 0.05, and 0.8 respectively.
# Load package for running power analysis
#install.packages(powerMediation)
library(powerMediation)
# Run logistic regression power analysis
total_sample_size <- SSizeLogisticBin(p1 = 0.39,
                                      p2 = 0.59,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size
# [1] 194
# Good job! Turns out we only need 194 data points in total (97 per group) since our expected effect is so large.
# AK:
total_sample_size <- SSizeLogisticBin(p1 = 0.39,
                                      p2 = 0.45,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size

# Follow-up experiment 1 analysis
# Based on your power analysis, you have decided to run your experiment and now have data to analyze.
# The tidyverse packages that you need for the exercise (readr and dplyr) and broom have been pre-loaded for you.
# Instructions
# Read in the data from your new experiment followup_experiment_data.csv.
# Note: instead of condition having control and test, the levels are cat_hat and kitten_hat.
# Compute the conversion rate for the two conditions. Notice anything unexpected? (Hint, look at visit_date.)
# Run the logistic regression to see if there is an effect.
# Read in data for follow-up experiment
followup_experiment_data <- read_csv("followup_experiment_data.csv")
# Parsed with column specification:
#   cols(
#     visit_date = col_date(format = ""),
#     condition = col_character(),
#     clicked_adopt_today = col_double()
#   )
head(followup_experiment_data)
# A tibble: 6 x 3
# visit_date condition  clicked_adopt_today
# <date>     <chr>                    <dbl>
# 1 2018-08-01 kitten_hat                   1
# 2 2018-08-01 kitten_hat                   0
# 3 2018-08-01 kitten_hat                   0
# 4 2018-08-01 kitten_hat                   1
# 5 2018-08-02 cat_hat                      0
# 6 2018-08-02 cat_hat                      1
summary(followup_experiment_data)
# visit_date          condition         clicked_adopt_today
# Min.   :2018-08-01   Length:194         Min.   :0.0000     
# 1st Qu.:2018-08-09   Class :character   1st Qu.:1.0000     
# Median :2018-08-15   Mode  :character   Median :1.0000     
# Mean   :2018-08-15                      Mean   :0.8454     
# 3rd Qu.:2018-08-22                      3rd Qu.:1.0000     
# Max.   :2018-08-31                      Max.   :1.0000     
# View conversion rates by condition
followup_experiment_data %>%
  group_by(condition) %>%
  summarise(conversion_rate = mean(clicked_adopt_today), n = n())
# A tibble: 2 x 3
# condition  conversion_rate     n
# <chr>                <dbl> <int>
# 1 cat_hat              0.814    97
# 2 kitten_hat           0.876    97
# Run logistic regression
followup_experiment_results <- glm(clicked_adopt_today ~ condition,
                                   family = "binomial",
                                   data = followup_experiment_data) %>%
  tidy()
followup_experiment_results
# term  estimate std.error statistic      p.value
# 1         (Intercept) 1.4790761 0.2611777  5.663103 1.486597e-08
# 2 conditionkitten_hat 0.4786685 0.4041175  1.184479 2.362236e-01
# You correctly found that the follow-up experiment didn't work (our p-value was about 0.24, 
# which is not less than 0.05). This could be because kittens aren't actually that desirable, or 
# because we went in with bad assumptions. We found our conversion results in our first experiment 
# in January, but ran our second experiment in August, when conversion rates were already high. 
# Remember to always consider what 'control' really means when building your follow-up experiments.

# 6 Pre-follow-up experiment assumptions.mp4
# Plot 8 months data
# Before starting the next experiment, let's take a second to look at the data so far from our original two conditions. Even though the cat in the hat did better, you decided to keep running both versions so you could see how results compared over more time. All necessary libraries and the data frame eight_month_checkin_data has been pre-loaded for you.
# Instructions
# Make a new column called month_text so we have the months formatted more nicely. Use the mutate() function and the visit_date column to create month_text.
# Use our newly created column in the group_by() to find out conversion rates per month for each condition.
# Use our new column as the x value in the plot and update the color and group aesthetics to show different lines for each condition.
head(eight_month_checkin_data)
# A tibble: 6 x 3
# visit_date condition clicked_adopt_today
# <date>     <chr>                   <dbl>
# 1 2018-01-01 cat_hat                     1
# 2 2018-01-01 cat_hat                     1
# 3 2018-01-01 cat_hat                     0
# 4 2018-01-01 cat_hat                     0
# 5 2018-01-01 cat_hat                     0
# 6 2018-01-01 cat_hat                     0
# Compute monthly summary
eight_month_checkin_data_sum <- eight_month_checkin_data %>%
  mutate(month_text = month(visit_date, label = TRUE)) %>%
  group_by(month_text, condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today), n = n())
head(eight_month_checkin_data_sum,5)
# A tibble: 5 x 4
# Groups:   month_text [3]
# month_text condition conversion_rate     n
# <ord>      <chr>               <dbl> <int>
# 1 Jan        cat_hat             0.377   310
# 2 Jan        no_hat              0.165   310
# 3 Feb        cat_hat             0.354   280
# 4 Feb        no_hat              0.225   280
# 5 Mar        cat_hat             0.339   310
# Plot month-over-month results
ggplot(eight_month_checkin_data_sum,
       aes(x = month_text,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom_line()
# http://joxi.ru/DrlwaoVfKjgEzA
# It looks like conversion rates have been consistently higher for our cat in a hat condition.

# Plot styling 1
# This plot actually looks pretty nice and could be useful to share with someone else either on or outside of your team. Let's take some time to clean it up a bit. Some of these functions should be familiar from earlier. The summarized data and packages have been pre-loaded for you.
# Instructions
# Use the function scale_y_continuous() to modify how the y-axis is displayed.
# Set the limitsso the plot displays 0% to 100%.
# Update the x-axis name with to say Month.
# Update the y-axis name with to say Conversion Rate.
# Plot monthly summary
ggplot(eight_month_checkin_data_sum,
       aes(x = month_text,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent) +
  labs(x = "Month",
       y = "Conversion Rate")
# http://joxi.ru/GrqQMXgSzq5EVr

# Plot styling 2
# Let's do just a couple more updates and then we'll be ready to hand off our plot to others to show how great our first experiment was.
# Instructions
# Make the dots larger by setting the size parameter in geom_point(). It should be equal to 4.
# To make the lines thicker, use the lwd parameter in geom_line(). Set the line thickness to 1.
# Plot monthly summary
ggplot(eight_month_checkin_data_sum,
       aes(x = month_text,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point(size = 4) +
  geom_line(lwd = 1) +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent) +
  labs(x = "Month",
       y = "Conversion Rate")
# http://joxi.ru/gmvLRvgCeRoEjA
# Great! Now we can see a lot more information about what's going on before getting ready for our next experiment.

# 7 Follow-up experiment assumptions.mp4
# Conversion rate between years
# In the video, I computed the conversion rate between our no-hat condition and our hat condition for the most recent year. We should also check if conversion rates have changed between years for our no-hat condition. The dataset with both years worth of data summarized by month and year and any packages are pre-loaded for you. You can find the data in no_hat_data_sum.
# Instructions
# Fill in year and conversion_rate in spread() to make one column per year.
# Create a new column called year_diff that is the difference between the columns `2017` and `2018`.
# Take a look at the data. Notice that there are missing values for 2018.
# Compute the mean() for the year_diff column and update the sd() function to be sure you're not including the months with missing data for 2018.
head(no_hat_data_sum)
# A tibble: 6 x 3
# year month conversion_rate
# <dbl> <ord>           <dbl>
# 1  2017 Jan             0.177
# 2  2017 Feb             0.168
# 3  2017 Mar             0.129
# 4  2017 Apr             0.143
# 5  2017 May             0.252
# 6  2017 Jun             0.290
summary(no_hat_data_sum)
# year          month   conversion_rate 
# Min.   :2017   Jan    :2   Min.   :0.1290  
# 1st Qu.:2017   Feb    :2   1st Qu.:0.1670  
# Median :2017   Mar    :2   Median :0.2408  
# Mean   :2017   Apr    :2   Mean   :0.2695  
# 3rd Qu.:2018   May    :2   3rd Qu.:0.3163  
# Max.   :2018   Jun    :2   Max.   :0.5806  
# (Other):8    
# AK: visualize year to year CR
library(ggplot2)
ggplot(no_hat_data_sum, aes(x = month, y = conversion_rate,  group = as.factor(year), color = as.factor(year))) + 
  geom_line() + geom_point()
# http://joxi.ru/J2b0eVlTqW6ayr
# Compute difference over time
no_hat_data_diff <- no_hat_data_sum %>%
  spread(year, conversion_rate) %>%
  mutate(year_diff = `2018` - `2017`)
no_hat_data_diff
# A tibble: 12 x 4
# month `2017` `2018` year_diff
# <ord>  <dbl>  <dbl>     <dbl>
# 1 Jan    0.177  0.165  -0.0129 
# 2 Feb    0.168  0.225   0.0571 
# 3 Mar    0.129  0.135   0.00645
# 4 Apr    0.143  0.137  -0.00667
# 5 May    0.252  0.268   0.0161 
# 6 Jun    0.290  0.307   0.0167 
# 7 Jul    0.390  0.345  -0.0452 
# 8 Aug    0.506  0.581   0.0742 
# 9 Sep    0.297 NA      NA      
#10 Oct    0.2   NA      NA      
#11 Nov    0.23  NA      NA      
#12 Dec    0.445 NA      NA      
# Compute summary statistics
mean(no_hat_data_diff$year_diff, na.rm = TRUE)
# [1] 0.01323157
sd(no_hat_data_diff$year_diff, na.rm = T)
# [1] 0.03817146
# Looks like our conversion rates have been pretty consistent for the no_hat condition. This is 
# good! It means we can reference last year's data for months that haven't happened yet.

# Re-run power analysis for follow-up
# Let's rerun our power analysis for our new experiment now taking into consideration the time of year we're running our new experiment: September. To figure out our baseline assumptions, we'll give you some introductory information: 1) the conversion rate for the "no hat" condition in 2017 was 30% (or 0.3), and 2) the average difference between the "no hat" condition and the "cat hat" condition is 19% (0.19). Use this information to run an updated power analysis.
# Instructions
# Fill in the value for p1 (our expected conversion rate for "cat hat" condition in September). Do this by summing the conversion rate for "no hat" condition (0.3), and the difference between the conditions (0.19).
# Fill in the value for p2 (our expected conversion rate for "kitten hat" condition in September) assuming an increase of 15 percentage points (0.15).
# Fill in the parameter names of other values. Remember you can always do help(SSizeLogisticBin) if you've forgotten.
# Load package for power analysis
library(powerMediation)
# Run power analysis for logistic regression
total_sample_size <- SSizeLogisticBin(p1 = 0.49,
                                      p2 = 0.64,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size
# [1] 341
# Now our power analysis says we need 341 data points in total, much higher than when we ran it before.

# Re-run glm() for follow-up
# Now that we updated our power analysis, I ran our experiment for September, pulling 171 data points per condition. The data has been pre-loaded for you in the data frame followup_experiment_data_sep.
# Instructions
# Compute the conversion_rate for the two conditions using the summarize() function.
# Fill in the dependent and independent values for the glm().
# Use the tidy() function from broom to look at a cleaned up version of the result.
# # Load package to clean up model outputs
library(broom)
head(followup_experiment_data_sep)
# A tibble: 6 x 3
# visit_date condition clicked_adopt_today
# <date>     <chr>                   <dbl>
# 1 2018-09-10 cat_hat                     0
# 2 2018-09-08 cat_hat                     0
# 3 2018-09-18 cat_hat                     1
# 4 2018-09-02 cat_hat                     0
# 5 2018-09-15 cat_hat                     0
# 6 2018-09-15 cat_hat                     1
# summary(followup_experiment_data_sep)
#         visit_date          condition         clicked_adopt_today
# Min.   :2018-09-01   Length:342         Min.   :0.0000     
# 1st Qu.:2018-09-09   Class :character   1st Qu.:0.0000     
# Median :2018-09-17   Mode  :character   Median :1.0000     
# Mean   :2018-09-16                      Mean   :0.5409     
# 3rd Qu.:2018-09-24                      3rd Qu.:1.0000     
# Max.   :2018-09-30                      Max.   :1.0000  
# View summary of data
followup_experiment_data_sep %>%
  group_by(condition) %>%
  summarise(conversion_rate = mean(clicked_adopt_today))
# A tibble: 2 x 2
# condition  conversion_rate
# <chr>                <dbl>
# 1 cat_hat              0.468
# 2 kitten_hat           0.614
# Run logistic regression
followup_experiment_sep_results <- glm(clicked_adopt_today ~ condition,
                                       family = "binomial",
                                       data = followup_experiment_data_sep) %>% 
  tidy()
followup_experiment_sep_results
#                  term   estimate std.error  statistic     p.value
# 1         (Intercept) -0.1288329 0.1532613 -0.8406096 0.400566704
# 2 conditionkitten_hat  0.5931385 0.2194637  2.7026718 0.006878462
# Our follow-up experiment was successful! Now that we pulled the correct number of data points, we 
# can see that there is a boost by using a kitten over a cat.

# 8 AB testing research questions.mp4
# Article click frequency monthly
# In the video, we saw there were four different types of variables we've been collecting for our website. We looked at one of them. Compute the monthly average for how often people click on one of the articles on the homepage.
# The data has been pre-loaded for you in the data frame viz_website_2017 and all packages have been pre-loaded.
# Instructions
# - Group by the month of visit_date to find monthly conversion rates.
# - Use the appropriate dplyr function to summarize the data.
# - Find the conversion rate for the clicked_article column.
library(tidyverse)
library(lubridate)
head(viz_website_2017)
# A tibble: 6 x 5
# visit_date time_spent_homepage_sec clicked_article clicked_like clicked_share
# <date>                       <dbl>           <dbl>        <dbl>         <dbl>
# 1 2017-01-01                    41.4               0            0             0
# 2 2017-01-01                    64.9               0            0             0
# 3 2017-01-01                    48.7               1            0             0
# 4 2017-01-01                    59.6               0            0             0
# 5 2017-01-01                    56.2               1            0             0
# 6 2017-01-01                    72.2               1            1             0
# Compute summary by month
viz_website_2017 %>%
  group_by(month(visit_date, label = T)) %>%
  summarise(avg_time_spent = mean(time_spent_homepage_sec),
            cr_clicked_article = mean(clicked_article),
            cr_clicked_like = mean(clicked_like),
            cr_share = mean(clicked_share))
# A tibble: 12 x 5
# `month(visit_date, ~   avg_time_spent cr_clicked_arti~ cr_clicked_like cr_share
#   <ord>                         <dbl>            <dbl>           <dbl>    <dbl>
# 1 Jan                            59.0            0.501           0.197   0.0516
# 2 Feb                            60.0            0.601           0.118   0.0113
# 3 Mar                            70.0            0.696           0.148   0.0192
# 4 Apr                            49.9            0.602           0.166   0.0296
# 5 May                            48.0            0.702           0.212   0.0501
# 6 Jun                            59.9            0.647           0.297   0.0701
# 7 Jul                            60.1            0.445           0.404   0.0203
# 8 Aug                            60.1            0.602           0.125   0.0104
# 9 Sep                            70.0            0.697           0.153   0.0177
#10 Oct                            80.0            0.495           0.202   0.0385
#11 Nov                            89.9            0.405           0.249   0.0607
#12 Dec                           100.             0.549           0.294   0.0188
# Well done! We see that overall conversion rates are pretty high, meaning that if someone comes 
# to the website, they are pretty likely to click an article.

# 'Like' click frequency plot
# Let's get some practice computing another variable, this time also plotting the results. Let's look at the amount of time someone clicked 'like' on one of the articles.
# Instructions
# Get the conversion rate for clicking 'like'.
# Initialize the plot.
# Set the y-axis to our computed column.
# Use percent to ensure the y-axis is displayed as a percent.
# Compute 'like' click summary by month
viz_website_2017_like_sum <- viz_website_2017 %>%
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarize(like_conversion_rate = mean(clicked_like))
viz_website_2017_like_sum
# A tibble: 12 x 2
#   month like_conversion_rate
#   <ord>                <dbl>
# 1 Jan                  0.197
# 2 Feb                  0.118
# 3 Mar                  0.148
# 4 Apr                  0.166
# 5 May                  0.212
# 6 Jun                  0.297
# 7 Jul                  0.404
# 8 Aug                  0.125
# 9 Sep                  0.153
#10 Oct                  0.202
#11 Nov                  0.249
#12 Dec                  0.294
# Plot 'like' click summary by month
ggplot(viz_website_2017_like_sum,
       aes(x = month, y = like_conversion_rate, group = 1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), labels = percent)
# http://joxi.ru/8An4XoVTNk3aom
# It looks like conversion rates are below 50% and peaked in July.

# 'Like' / 'Share' click frequency plot
# Instead of just computing another conversion rate, here, we want to plot the 'like' conversion rate and the 'share' conversion rate side by side. I did some work ahead of time for you and created a new data frame viz_website_2017_like_share_sum. Take a look at it and you'll see there are two new columns: action, which refers to clicking 'like' or clicking 'share' and conversion_rate which is the percentage of the time the action was done for a given month.
# Instructions
# Use the new data frame to build your plot, setting the color and group aesthetics to action.
# Fill in the missing geom_*() we've used in previous exercises.
# Make sure the y-axis ranges from 0 to 1, so it's 0% to 100%.
head(viz_website_2017_like_share_sum,20)
# A tibble: 20 x 3
# month action  conversion_rate
# <ord>  <chr>            <dbl>
# 1 Jan   like            0.197 
# 2 Feb   like            0.118 
# 3 Mar   like            0.148 
# 4 Apr   like            0.166 
# 5 May   like            0.212 
# 6 Jun   like            0.297 
# 7 Jul   like            0.404 
# 8 Aug   like            0.125 
# 9 Sep   like            0.153 
#10 Oct   like            0.202 
#11 Nov   like            0.249 
#12 Dec   like            0.294 
#13 Jan   share           0.0516
#14 Feb   share           0.0113
#15 Mar   share           0.0192
#16 Apr   share           0.0296
#17 May   share           0.0501
#18 Jun   share           0.0701
#19 Jul   share           0.0203
#20 Aug   share           0.0104
# Plot comparison of 'like'ing and 'sharing'ing an article
ggplot(viz_website_2017_like_share_sum,
       aes(x = month, y = conversion_rate, color = action, group = action)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0,1), labels = percent)
# http://joxi.ru/Vm6gyx7U38oJW2
# Great! It looks like people are even less likely to 'share' an article as they are to 'like' it.

# 9 Assumptions and types of AB testin.mp4
# Between vs. within
# Which of these experiments describes a within-participant experiment?
# Answer the question
# Possible Answers
# - You randomly sample a group of people where a button color is blue. One month later you sample a new group where the button color is green.
#   Incorrect
#   Since you run the experiments one month apart (not good!) you can't be sure you have the same group of people.
# - You have a group of email users and for two weeks randomly show one of two "Inbox" designs whenever a person logs in. +
#   Correct
#   Since a given user sees both versions of the Inbox, this is a within-participant experiment.
# - You run one condition of homepage images simultaneously. One is tested on people with IP addresses in the United States and one with IP addresses in Italy.
#   Incorrect
#   Since groups are determined by IP address, it is almost impossible for a given person to see both versions of the homepage image.
# - You use a new design that speeds up loading times. Every visitor who shows up on the website either gets the old version or the new, faster version. You don't track who your visitors are.
#   Incorrect
#   Since you don't track who your visitors are, you have no way to know how many times a given user has seen both versions of the website.

# Plotting A/A data
# Before running our experiment, let's say we ran an A/A experiment first to be sure our "random" assignment of visitors really was random. Remember, a significant effect in an A/A experiment can mean our two groups of participants actually are different in some way. For this experiment, we're going to look at clicking 'like' conversion. We'll start by summarizing and plotting our data.
# All packages and the data are pre-loaded for you. The data is in the data frame viz_website_2018_01.
# Instructions
# Create a new data frame called viz_website_2018_01_sum by grouping by the condition column. (Note we're not grouping by visit_date since this data was all collected in a single month.)
# Set the x value in our plot to the same variable we grouped by.
# To make a bar plot, use geom_bar(). Here use stat = "identity" so it plots our computed values, rather than make bars of counts.
head(viz_website_2018_01)
# A tibble: 6 x 6
# visit_date condition time_spent_home~ clicked_article clicked_like
# <date>     <chr>                <dbl>           <dbl>        <dbl>
# 1 2018-01-01 A1                    57.9               1            1
# 2 2018-01-01 A1                    59.2               1            0
# 3 2018-01-01 A1                    59.7               1            0
# 4 2018-01-01 A1                    58.3               1            1
# 5 2018-01-01 A1                    58.8               1            0
# 6 2018-01-01 A1                    58.1               1            0
# ... with 1 more variable: clicked_share <dbl>
c(min(viz_website_2018_01$visit_date), max(viz_website_2018_01$visit_date))
# [1] "2018-01-01" "2018-01-31"
# Compute conversion rates for A/A experiment
viz_website_2018_01_sum <- viz_website_2018_01 %>%
  group_by(condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))
viz_website_2018_01_sum
# A tibble: 2 x 2
# condition like_conversion_rate
# <chr>                    <dbl>
# 1 A1                       0.201
# 2 A2                       0.197
# Plot conversion rates for two conditions
ggplot(viz_website_2018_01_sum,
       aes(x = condition, y = like_conversion_rate)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 1), labels = percent)
# http://joxi.ru/YmE4VaDTGxE9er
# Based on these bar plots the two A conditions look very similar. That's good!

# Analyzing A/A data
# Just as for a normal A/B experiment, we need to statistically analyze our A/A results to be sure there really was no effect. Here we'll build a logistic regression again, this time hoping for a null effect.
# Instructions
# - Load the package we've used in the past to clean up modeling results.
# - Set our independent variable to condition.
# - Set the family argument to the expected value for a logistic regression.
# Load library to clean up model outputs
library(broom)
# Run logistic regression
aa_experiment_results <- glm(clicked_like ~ condition,
                             family = "binomial",
                             data = viz_website_2018_01) %>%
  tidy()
aa_experiment_results
#          term    estimate  std.error   statistic   p.value
# 1 (Intercept) -1.38025691 0.02004420 -68.8606672 0.0000000
# 2 conditionA2 -0.02591398 0.02845802  -0.9106038 0.3625041
# There was no statistical difference between our two conditions. We can now safely say we randomly selected two groups of participants.

# 10 Confounding variables.mp4
# Examples of confounding variables
# Let's go back to our cat adoption website for a second. You run an experiment where in one condition, the homepage is a tabby cat, and in another condition, it is a black cat. Both conditions are run at the same time. At the beginning of the experiment, a children's movie comes out starring a black cat. Which of these statements is true?
# Answer the question
# Possible Answers
# - If the black cat condition results in a higher conversion rate it may be due to lack of randomization.
#   Incorrect
#   Both conditions are run at the same time, so randomization is not a concern.
# - If the black cat condition results in a higher conversion rate it may be due to an internal confounding variable.
#   Incorrect
#   The movie coming out is not related to your experimental design, so it is not internal.
# - If the black cat condition results in a higher conversion rate it may be due to an external confounding variable.
#   Correct! The movie coming out is an external factor that could confound your results.
# - If the black cat condition results in a higher conversion rate it may be due to a lack of an independent variable.
#   Incorrect
#   There is an independent variable, the type of cat in the picture.

# Confounding variable example analysis
# You have decided to run your "Tips" versus "Tools" experiment and look at percentage of 'like's. You run the experiment over the month of February. In the later half of February, an article goes viral called "Tips Just Get you Started, but Tools Open all the Doors". Let's see how this article affected results.
# The data frame is viz_website_2018_02. All packages have already been loaded for you.
# Instructions
# Group by the week column and the column related to condition.
# Get the mean of the column related to clicking like.
# To compute the conversion rate depending on if the article was published or not, group by the column that codes if the article is published or not.
# Name your summarized column the same as you did for the previous aggregation.
head(viz_website_2018_02)
# A tibble: 6 x 7
#   visit_date condition time_spent_home~ clicked_article clicked_like
#   <date>     <chr>                <dbl>           <dbl>        <dbl>
# 1 2018-02-01 tips                  58.1               1            0
# 2 2018-02-01 tips                  61.4               1            0
# 3 2018-02-01 tips                  59.8               0            1
# 4 2018-02-01 tips                  59.6               1            0
# 5 2018-02-01 tips                  60.7               0            0
# 6 2018-02-01 tips                  59.3               1            0
# ... with 2 more variables: clicked_share <dbl>, article_published <chr>
glimpse(viz_website_2018_02)
# Rows: 28,000
# Columns: 7
# $ visit_date              <date> 2018-02-01, 2018-02-01, 2018-02-01, 2018-0...
# $ condition               <chr> "tips", "tips", "tips", "tips", "tips", "ti...
# $ time_spent_homepage_sec <dbl> 58.12213, 61.35828, 59.75128, 59.62236, 60....
# $ clicked_article         <dbl> 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1...
# $ clicked_like            <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
# $ clicked_share           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
# $ article_published       <chr> "no", "no", "no", "no", "no", "no", "no", "...
# Compute 'like' conversion rate by week and condition
viz_website_2018_02 %>%
  mutate(week = week(visit_date)) %>%
  group_by(week, condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))
# A tibble: 10 x 3
# Groups:   week [?]
#    week condition like_conversion_rate
#   <dbl> <chr>                    <dbl>
# 1     5 tips                    0.109 
# 2     5 tools                   0.0156
# 3     6 tips                    0.124 
# 4     6 tools                   0.0238
# 5     7 tips                    0.115 
# 6     7 tools                   0.0492
# 7     8 tips                    0.124 
# 8     8 tools                   0.110 
# 9     9 tips                    0.114 
#10     9 tools                   0.147 
# Compute 'like' conversion rate by if article published and condition
viz_website_2018_02 %>%
  group_by(article_published, condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))
# A tibble: 4 x 3
# Groups:   article_published [?]
# article_published condition like_conversion_rate
# <chr>             <chr>                    <dbl>
# 1 no                tips                    0.118 
# 2 no                tools                   0.0200
# 3 yes               tips                    0.119 
# 4 yes               tools                   0.106
# Clearly there was an effect of the article coming out. First we saw a switch in 'like' rates in 
# the second half of February and then saw it was directly related to whether the article was 
# published or not.

# Confounding variable example plotting
# Let's see if we can tell when 'like' rates really started to change by plotting daily like rates.
# The data frame summarized by day is viz_website_2018_02_sum. All packages have already been loaded for you.
# Instructions
# Fill in the missing two geom_*()s we've used in previous exercises.
# Set color to condition.
# Set linetype to article_published.
# Find the date when the article was first published. Now add that date as the value for xintercept in geom_vline(). (Note: it should be of the format "YYYY-MM-DD".)
head(viz_website_2018_02_sum)
# A tibble: 6 x 4
# Groups:   visit_date, condition [6]
# visit_date condition article_published like_conversion_rate
# <date>     <chr>     <chr>                            <dbl>
# 1 2018-02-01 tips      no                             0.112  
# 2 2018-02-01 tools     no                             0.0171 
# 3 2018-02-02 tips      no                             0.109  
# 4 2018-02-02 tools     no                             0.0143 
# 5 2018-02-03 tips      no                             0.118  
# 6 2018-02-03 tools     no                             0.00996
df <- viz_website_2018_02_sum %>% filter(article_published == 'yes')
article_published_min_date <- min(df$visit_date)
article_published_min_date
# [1] "2018-02-15"
# Plot 'like' conversion rates by date for experiment
ggplot(viz_website_2018_02_sum,
       aes(x = visit_date,
           y = like_conversion_rate,
           color = condition,
           linetype = article_published,
           group = interaction(condition, article_published))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-02-15")), color='grey') +
  scale_y_continuous(limits = c(0, 0.3), labels = percent)
# http://joxi.ru/v290QPlT4wezwm
# AK: without article_published
ggplot(viz_website_2018_02_sum,
aes(x = visit_date,
    y = like_conversion_rate,
    color = condition,
    group = condition)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 0.3), labels = percent)
# http://joxi.ru/823NOxpTzle8Zm
# It looks like the article had a pretty clear effect, and the effect seemed to increase over 
# time as it presumably got more hits.

# 11 Side effects.mp4
# Confounding variable vs. side effect
# For our cat adoption website with an experiment comparing a tabby cat and a black cat, the photo of the tabby cat loads in 5 seconds and the black cat in 2 seconds. The tabby cat is 1 year old and the black cat is a 6 years old. Which of these statements is true?
# Answer the question
# Possible Answers
# - Both the load time and age of the cats are confounding variables.
#   Incorrect
#   The load time is not a part of your design choice.
# - Both the load time and age of the cats are side effects.
#   Incorrect
#   The cat age is a not a consequence of your design choice.
# - The load time is a confounding variable, the age of the cats is a side effect.
#   Incorrect
#   The load time is not a part of your design choice, and the cat age is a not a consequence of your design choice.
# - The load time is a side effect, the age of the cats is a confounding variable.
#   Correct! When choosing your pictures you introduced a confounding variable of cat age, when adding the pictures you had a side effect of load times.

# Side effect load time plot
# The viral article has died down, so you've decided to rerun your experiment on "tips" vs. "tools". However, after running it for a month you realized there was a larger load delay for the "tools" homepage than the "tips" homepage. You then added a delay to the "tips" homepage so that they even out. To start, let's visualize the effect the delay has on a 'like' rates on a daily basis.
# Instructions
# - First create the aggregated data frame. Group by visit_date and condition.
# - In addition to computing the 'like' rate, we also want to find out the mean page load time in a given day. Find the column in the data frame associated with page load time and fill it in.
# - In building your ggplot(), use the newly created page load column as your x-variable.
# - Use geom_point() to display dots for your data.
glimpse(viz_website_2018_03)
# Rows: 31,000
# Columns: 7
# $ visit_date              <date> 2018-03-01, 2018-03-01, 2018-03-01, 2018-0...
# $ condition               <chr> "tips", "tips", "tips", "tips", "tips", "ti...
# $ time_spent_homepage_sec <dbl> 71.17112, 69.92806, 72.07211, 70.19193, 69....
# $ clicked_article         <dbl> 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1...
# $ clicked_like            <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0...
# $ clicked_share           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
# $ pageload_time           <dbl> 4.778857, 5.411389, 5.070054, 5.846337, 4.9...
# Compute 'like' conversion rate and mean pageload time by day
viz_website_2018_03_sum <- viz_website_2018_03 %>%
  group_by(visit_date, condition) %>%
  summarize(mean_pageload_time = mean(pageload_time),
            like_conversion_rate = mean(clicked_like))
viz_website_2018_03_sum
# A tibble: 62 x 4
# Groups:   visit_date [?]
#    visit_date condition mean_pageload_time like_conversion_rate
#    <date>     <chr>                  <dbl>                <dbl>
#  1 2018-03-01 tips                    4.98               0.146 
#  2 2018-03-01 tools                   6.99               0.0543
#  3 2018-03-02 tips                    5.01               0.151 
#  4 2018-03-02 tools                   7.00               0.0514
#  5 2018-03-03 tips                    5.01               0.136 
#  6 2018-03-03 tools                   7.02               0.048 
#  7 2018-03-04 tips                    5.01               0.169 
#  8 2018-03-04 tools                   6.98               0.0569
#  9 2018-03-05 tips                    4.98               0.143 
# 10 2018-03-05 tools                   7.00               0.0576
# ... with 52 more rows
# Plot effect of 'like' conversion rate by pageload time
ggplot(viz_website_2018_03_sum,
       aes(x = mean_pageload_time, y = like_conversion_rate, color = condition)) +
  geom_point()
# http://joxi.ru/vAWkDKZI3LbL9A
# There's clearly a correlation here. Longer load times lead to lower conversion rates.

# Side effects experiment plot
# Let's end by plotting our new conversion rate, seeing if we can find the effect of when the page load delay was added.
# Instructions
# Set linetype the column that codes for if the delay was added or not.
# Fill in the missing geom_*()s.
# Set the argument to show a vertical line.
glimpse(viz_website_2018_03_sum)
# Rows: 62
# Columns: 4
# Groups: visit_date, condition [62]
# $ visit_date           <date> 2018-03-01, 2018-03-01, 2018-03-02, 2018-03-0...
# $ condition            <chr> "tips", "tools", "tips", "tools", "tips", "too...
# $ pageload_delay_added <chr> "no", "no", "no", "no", "no", "no", "no", "no"...
# $ like_conversion_rate <dbl> 0.14613779, 0.05426357, 0.15139442, 0.05138340...
# Plot 'like' conversion rate by day
ggplot(viz_website_2018_03_sum,
       aes(x = visit_date,
           y = like_conversion_rate,
           color = condition,
           linetype = pageload_delay_added,
           group = interaction(condition, pageload_delay_added))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-15")), color='grey') +
  scale_y_continuous(limits = c(0, 0.3), labels = percent)
# http://joxi.ru/L217LzbHwxY3xr
# Great job! We can clearly see the effect that adding the delay had on the two conditions.

# 12 Power analyses.mp4
# Logistic regression power analysis
# In the previous chapters you got some practice with power analyses with logistic regression (you didn't think you got to forget about that did you?). Let's test your recall knowledge by running that kind of power analysis assuming we're running an experiment on whether or not someone clicked 'like' on an article.
# Instructions
# Load the package you need to run the logistic regression power analysis.
# Fill in p1 and p2 assuming a control value of 17% click 'like' (the conversion rate for April 2017) and a 10 percentage point increase in the test condition.
# Fill in the names for the arguments that are set to 0.05 and 0.8.
# Load package to run power analysis
library(powerMediation)
# Run power analysis for logistic regression
total_sample_size <- SSizeLogisticBin(p1 = 0.17,
                                      p2 = 0.27,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size
# [1] 537
# Great! We need 537 data points in total to see an increase in 'like' clicks by 10 percentage points.

install.packages("pwr")
library(pwr)
?pwr.t.test()
# pwr.t.test() documentation
# I mentioned in the video that there are additional arguments that can be provided to pwr.t.test(). Take a look at the documentation to see what the difference is between type and alternative. Note, you'll need to load the pwr package.
# Instructions
# Possible Answers
# - type refers to number and type of samples; alternative to hypothesis. +
# - type relates to Type 1 error; alternative relates to Type 2 error.
# - type refers to hypothesis; alternative to number and type of samples.
# - type relates to Type 2 error; alternative relates to Type 1 error
# Correct! You use type to describe how your data points and alternative to talk about the hypothesis of the experiment.

# T-test power analysis
# Now that we know how to run power analyses for continuous data using a t-test, let's try running a power analysis for an experiment on time spent on the homepage.
# Instructions
# - Load the pwr package.
# - Set the effect size (d) to 0.3.
# - Set our significance level to 0.05 and power to 0.8.
# Load package to run power analysis
library(pwr)
# Run power analysis for t-test
sample_size <- pwr.t.test(d = 0.3,
                          sig.level = 0.05,
                          power = 0.8)
sample_size
# Two-sample t test power calculation 
# n = 175.3847
# d = 0.3
# sig.level = 0.05
# power = 0.8
# alternative = two.sided
# NOTE: n is number in *each* group

# 13 Statistical tests.mp4
# Logistic regression
# In the previous lessons, we got some practice with logistic regression. Let's do the same here looking at our experiment to see if people clicked 'like' or not depending on if the homepage has "Tips" (the control) or "Tools" (the test) in the title. You can assume this is after all confounding and side effects were figured out.
# The data frame is pre-loaded as viz_website_2018_04.
# Instructions
# Fill in the rest of the logistic regression sample code provided.
# Use the function tidy() to look at a cleaned up version of the results.
glimpse(viz_website_2018_04)
# Rows: 30,000
# Columns: 6
# $ visit_date              <date> 2018-04-01, 2018-04-01, 2018-04-01, 2018-0...
# $ condition               <chr> "tips", "tips", "tips", "tips", "tips", "ti...
# $ time_spent_homepage_sec <dbl> 49.01161, 48.86452, 49.07467, 49.26011, 50....
# $ clicked_article         <dbl> 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1...
# $ clicked_like            <dbl> 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1...
# $ clicked_share           <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
# Load package to clean up model outputs
library(broom)
# Run logistic regression
ab_experiment_results <- glm(clicked_like ~ condition,
                             family = "binomial",
                             data = viz_website_2018_04) %>%
  tidy()
ab_experiment_results
#             term   estimate  std.error statistic       p.value
# 1    (Intercept) -1.6123207 0.02192998 -73.52131  0.000000e+00
# 2 conditiontools -0.9887948 0.03895874 -25.38057 4.133837e-142
# Based on our logistic regression there was a significant effect, however not in the direction 
# we wanted. It look like 'Tools' actually had lower 'like' click rates than 'Tips'.

# T-test
# Let's now analyze one of our dependent variables for a continuous dependent variable using a t-test. The data frame is pre-loaded for you as viz_website_2018_04.
# Instructions
# Use the t.test() function, setting the dependent variable to the column referring to time spent on the homepage and the independent variable to the column for condition.
# Run t-test
ab_experiment_results <- t.test(time_spent_homepage_sec ~ condition,
                                data = viz_website_2018_04)
ab_experiment_results
# Welch Two Sample t-test
# data:  time_spent_homepage_sec by condition
# t = 0.36288, df = 29997, p-value = 0.7167
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.01850573  0.02691480
# sample estimates:
#   mean in group tips mean in group tools 
# 49.99909            49.99489 
dim(viz_website_2018_04)
# [1] 30000     6
# AK: the same with lm()
lm(time_spent_homepage_sec ~ condition, data = viz_website_2018_04) %>% summary()
# Call:
#   lm(formula = time_spent_homepage_sec ~ condition, data = viz_website_2018_04)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.8992 -0.6728  0.0033  0.6727  4.0290 
# Coefficients:
# Estimate Std. Error  t value Pr(>|t|)    
#   (Intercept)    49.999093   0.008193 6102.679   <2e-16 ***
#   conditiontools -0.004205   0.011587   -0.363    0.717    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# Residual standard error: 1.003 on 29998 degrees of freedom
# Multiple R-squared:  4.39e-06,	Adjusted R-squared:  -2.895e-05 
# F-statistic: 0.1317 on 1 and 29998 DF,  p-value: 0.7167
# It looks like our experiment had no effect of time spent on the homepage, despite a lower 'like' click rate.

# 
# What is a sequential analysis?
# What is "sequential analysis"?
# Answer the question
# Possible Answers
# - Sequential analysis is when you adjust your p-value to allow for looking at your data at various stopping points. +
# - Sequential analysis is running multiple experiments one after the other and use different p-value cutoffs for both.
# - Sequential analysis is running two separate types of analyses on the same data set, but with different p-value cutoffs.
# - Sequential analysis is running two experiments at the same time, with the same p-values cuttoffs.
# Correct! Sequential analysis builds in stopping points to look at your data.

# Sequential analysis three looks
# In the video, we built a sequential analysis with four looks at the data. In this exercise, you will build a sequential analysis for only three looks.
# Instructions
# Load the package gsDesign.
# Look at the data three times.
# Use Pocock as our method for the spending function.
# Take a look at the result of only using three looks.
# Load package to run sequential analysis
library(gsDesign)
# Run sequential analysis
seq_analysis_3looks <- gsDesign(k = 3,
                                test.type = 1,
                                alpha = 0.05,
                                beta = 0.2,
                                sfu = "Pocock")
seq_analysis_3looks
# One-sided group sequential design with
# 80 % power and 5 % Type I Error.
# Sample
# Size 
# Analysis Ratio*  Z   Nominal p  Spend
# 1  0.394 1.99    0.0232 0.0232
# 2  0.789 1.99    0.0232 0.0155
# 3  1.183 1.99    0.0232 0.0113
# Total                       0.0500 
# ++ alpha spending:
# Pocock boundary.
# * Sample size ratio compared to fixed design with no interim
# Boundary crossing probabilities and expected sample size
# assume any cross stops the trial
# Upper boundary (power or Type I Error)
# Analysis
# Theta      1      2      3 Total   E{N}
# 0.0000 0.0232 0.0155 0.0113  0.05 1.1591
# 2.4865 0.3334 0.2875 0.1791  0.80 0.8070
help(gsDesign)
# It looks like our p-value cuttoff is higher if we only look at the data three times.

# Sequential analysis sample sizes
# Now that we've built our sequential analysis with three looks, let's see what our stopping points are.
# Instructions
# - You've been given permission to run the experiment to collect a grand total of 3000 data points. Fill in max_n with this number.
# - Compute the max_n_per_group by filling in the missing variable.
# - Call the timing element from our sequential analysis object.
# Run sequential analysis
seq_analysis_3looks <- gsDesign(k = 3,
                                test.type = 1,
                                alpha = 0.05,
                                beta = 0.2,
                                sfu = "Pocock")
seq_analysis_3looks$timing
# [1] 0.3333333 0.6666667 1.0000000
# Fill in max number of points and compute points per group and find stopping points
max_n <- 3000
max_n_per_group <- max_n / 2
stopping_points <- max_n_per_group * seq_analysis_3looks$timing
stopping_points
# [1]  500 1000 1500
# Based on this we should run our analysis at 500, 1000, and 1500 data points per group.

# 15 Multivariate testing.mp4
# Plotting time homepage in multivariate experiment
# In the video, I ran our statistical analysis but didn't plot our data. Remember, it's always important to plot your data first so you're sure you have a sense of what's going on. Let's plot the means for our four conditions for time spent on the homepage.
# The data is preloaded in a data frame for you called viz_website_2018_05.
# Instructions
# Group by the two columns that represent our two independent variables.
# Name the parameter that "identity" is set to in the plot.
# Set position to "dodge".
glimpse(viz_website_2018_05)
# Rows: 62,000
# Columns: 7
# $ visit_date              <date> 2018-05-01, 2018-05-01, 2018-05-01, 2018-0...
# $ word_one                <chr> "tips", "tips", "tips", "tips", "tips", "ti...
# $ word_two                <chr> "better", "better", "better", "better", "be...
# $ time_spent_homepage_sec <dbl> 48.38507, 47.26086, 49.15316, 48.56957, 48....
# $ clicked_article         <dbl> 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1...
# $ clicked_like            <dbl> 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0...
# $ clicked_share           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1...
# Compute summary values for four conditions
viz_website_2018_05_sum <- viz_website_2018_05 %>%
  group_by(word_one, word_two) %>%
  summarize(mean_time_spent_homepage_sec = mean(time_spent_homepage_sec))
viz_website_2018_05_sum
# A tibble: 4 x 3
# Groups:   word_one [?]
#   word_one word_two mean_time_spent_homepage_sec
#   <chr>    <chr>                           <dbl>
# 1 tips     amazing                          48.0
# 2 tips     better                           48.0
# 3 tools    amazing                          53.0
# 4 tools    better                           48.0
# Plot summary values for four conditions
ggplot(viz_website_2018_05_sum,
       aes(x = word_one,
           y = mean_time_spent_homepage_sec,
           fill = word_two)) +
  geom_bar(stat = "identity", position = "dodge")
# http://joxi.ru/8An4XoVTNQqLem
# The results of our regression look validated. Indeed time spent on the homepage was highest in 
# the 'Tools'/'Amazing' combination.

# Plotting 'like' clicks in multivariate experiment
# Now that we've seen that there was an interaction for time spent on the homepage, let's take look at the conversion rates for clicking 'like' to see if there was an additive effect.
# All packages you need are pre-loaded. The data is preloaded in the data frame viz_website_2018_05.
# Instructions
# - In the plot set x to the first variable.
# - In the plot set fill to the second variable.
# - In the plot make sure the labels on the y-axis display as percents.
# # Compute summary values for four conditions
viz_website_2018_05_sum <- viz_website_2018_05 %>%
  group_by(word_one, word_two) %>%
  summarize(like_conversion_rate = mean(clicked_like))
viz_website_2018_05_sum
# A tibble: 4 x 3
# Groups:   word_one [?]
#   word_one word_two like_conversion_rate
#   <chr>    <chr>                   <dbl>
# 1 tips     amazing                 0.21 
# 2 tips     better                  0.213
# 3 tools    amazing                 0.413
# 4 tools    better                  0.109
# Plot summary values for four conditions
ggplot(viz_website_2018_05_sum,
       aes(x = word_one,
           y = like_conversion_rate,
           fill = word_two)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 1), labels = percent)
# http://joxi.ru/82QRVQ5c8z1ebA
# Looks like we have another interaction! This one appears to be even stronger. While the 'like' 
# click rate decreased with 'Tools' with 'Better', it looks to be even higher than the control 
# with 'Amazing'.

# Multivariate design statistical test
# Let's also run a statistical analysis for 'like' conversion rate.
# Instructions
# Be sure that word_one has the levels in order of tips and then tools.
# Update the second variable.
# In the model set the dependent variable to look at whether a person clicked 'like' or not.
# Load package for cleaning model output
library(broom)
# Organize variables and run logistic regression
viz_website_2018_05_like_results <- viz_website_2018_05 %>%
  mutate(word_one = factor(word_one,
                           levels = c("tips", "tools"))) %>%
  mutate(word_two = factor(word_two,
                           levels = c("better", "amazing"))) %>%
  glm(clicked_like ~ word_one * word_two,
      family = "binomial",
      data = .) %>%
  tidy()
viz_website_2018_05_like_results
#                            term    estimate  std.error   statistic        p.value
# 1                   (Intercept) -1.30558956 0.01961052 -66.5759940   0.000000e+00
# 2                 word_onetools -0.79640382 0.03239365 -24.5851815  1.819678e-133 # statistic is negative what tells that it became worse
# 3               word_twoamazing -0.01933586 0.02781111  -0.6952566   4.868945e-01
# 4 word_onetools:word_twoamazing  1.77174758 0.04128273  42.9174069   0.000000e+00 # statistic is positive what tells that it became better
# Once again we found a significant interaction. While there was no effect of word_two for the baseline of 'Tips', there likely was an effect for 'Tools'.























































































