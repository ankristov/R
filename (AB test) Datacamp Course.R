

head(click_data)
# A tibble: 6 x 2
# visit_date clicked_adopt_today
# <date>                   <dbl>
#  1 2017-01-01                   1
#  2 2017-01-02                   1
#  3 2017-01-03                   0
#  4 2017-01-04                   1
#  5 2017-01-05                   1
#  6 2017-01-06                   0

# Plotting conversion rate seasonality
# Compute conversion rate by week of the year
click_data_sum <- click_data %>%
  group_by(week(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))
head(click_data_sum)
# Build plot
ggplot(click_data_sum, aes(x = `week(visit_date)`,
                           y = conversion_rate)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent)
# We also updated our plot a bit using scale_y_continuous() to make sure our axes goes from 0 to 1 and converts the values to percentages. The percent setting comes from the scales package.

# N samples
# Power analysis in R
library(powerMediation)
total_sample_size <- SSizeLogisticBin(
  p1 = 0.2,
  p2 = 0.3,
  B = 0.5,
  alpha = 0.05,
  power = 0.8)
total_sample_size
# p1 - The probability when X = 0 (the control condition)
# p2 - The probability when X = 1 (the test condition)

# Power analysis August
# In the video, we ran a power analysis assuming we were going to run the experiment in January. Run a new power analysis assuming 
# we'll run the experiment in August. To compute the conversion rate for our control and test you'll need to look at the dataset. 
# Be sure to round all values to the hundredth of a percent (e.g., 0.13453 to 0.13). The data click_data_month is available 
# pre-loaded for you to look up the conversion rate for August.
# monthly cr
click_data %>% group_by(month(visit_date)) %>% summarise(cr = mean(clicked_adopt_today))
# august cr = 0.54
total_sample_size <- SSizeLogisticBin(p1 = 0.54,
                                      p2 = 0.64,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size

# Power analysis August 5 percentage point increase
# Let's say you've reconsidered your expectations for running the experiment in August. 54% is already a really high conversion rate, 
# so increasing it by 10 percentage points may be difficult. Rerun your power analysis assuming only a 5 percentage point increase 
# in your conversion rate for the test condition.
total_sample_size <- SSizeLogisticBin(p1 = 0.54,
                                      p2 = 0.59,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size

# Plotting results
# Group and summarize data
experiment_data_clean_sum <- experiment_data_clean %>%
  group_by(condition, visit_date) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))
head(experiment_data_clean)
# A tibble: 6 x 3
#  visit_date condition clicked_adopt_today
#  <date>     <chr>                   <dbl>
#  1 2018-01-01 control                     0
#  2 2018-01-01 control                     1
#  3 2018-01-01 control                     0
#  4 2018-01-01 control                     0
#  5 2018-01-01 test                        0
#  6 2018-01-01 test                        0
# Make plot of conversion rates over time
ggplot(experiment_data_clean_sum,
       aes(x = visit_date,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom_line()

# Practice with glm()
# In the video, we analyzed our results with a logistic regression, using the function tidy() from the broom package 
# to see a cleaned up version of our results. Run the same analysis using our new data frame experiment_data_clean.
# Load package for cleaning model results
library(broom)
head(experiment_data_clean)
# View summary of results
experiment_data_clean %>%
  group_by(condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))
# A tibble: 2 x 2
# condition   conversion_rate
# <chr>                 <dbl>
# 1 control             0.166
# 2 test                0.386 # cat in a hat
# Run logistic regression
experiment_results <- glm(clicked_adopt_today ~ condition,
                          family = "binomial",
                          data = experiment_data_clean) %>%
  tidy()
experiment_results
#            term  estimate std.error  statistic      p.value
# 1   (Intercept) -1.613684 0.1597307 -10.102533 5.383364e-24
# 2 conditiontest  1.149379 0.2007961   5.724108 1.039787e-08
# Great! Even dropping a day's worth of data our result was still significant. However, if this happened in real life 
# we really should have run the experiment for another day to make sure we got the correct number of data points needed 
# according to the power analysis.

# Follow-up experiment 1 design
# You're now designing your follow-up experiments. What's the best path forward?
# Build one experiment where your test condition is a kitten in a hat. If the experiment works, run a second experiment 
# with a kitten in a hat as the control and two kittens in hats as the test.
# Let's start your kitten experiment. The hat already increased conversion rates a lot, but you think making the photo 
# a kitten will really make the difference, so you think conversion rates will go up to 59%. Let's run a power analysis 
# to see how much data you need to see a significant effect.
# Load package for running power analysis
install.packages(powerMediation)
library(powerMediation)
# Run logistic regression power analysis
total_sample_size <- SSizeLogisticBin(p1 = 0.39, # conversion rate you found in the previous experiment 
                                      p2 = 0.59, # expected conversion rate with the picture of a kitten in a hat
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size
# Good job! Turns out we only need 194 data points in total (97 per group) since our expected effect is so large
# Based on your power analysis, you have decided to run your experiment and now have data to analyze.
# The tidyverse packages that you need for the exercise (readr and dplyr) and broom have been pre-loaded for you.
followup_experiment_data <- read_csv("followup_experiment_data.csv")
head(followup_experiment_data)
# A tibble: 6 x 3
# visit_date condition  clicked_adopt_today
#  <date>     <chr>                    <dbl>
# 1 2018-08-01 kitten_hat                   1
# 2 2018-08-01 kitten_hat                   0
# 3 2018-08-01 kitten_hat                   0
# 4 2018-08-01 kitten_hat                   1
# 5 2018-08-02 cat_hat                      0
# 6 2018-08-02 cat_hat                      1
# View conversion rates by condition
followup_experiment_data %>%
  group_by(condition) %>%
  summarise(conversion_rate = mean(clicked_adopt_today))
# A tibble: 2 x 2
# condition  conversion_rate
# <chr>                <dbl>
# 1 cat_hat              0.814 # in previous experiment it was 0.386, why???
# 2 kitten_hat           0.876
# Run logistic regression
followup_experiment_results <- glm(clicked_adopt_today ~ condition,
                                   family = "binomial",
                                   data = followup_experiment_data) %>%
  tidy()
followup_experiment_results
#                  term  estimate std.error statistic      p.value
# 1         (Intercept) 1.4790761 0.2611777  5.663103 1.486597e-08
# 2 conditionkitten_hat 0.4786685 0.4041175  1.184479 2.362236e-01
# You correctly found that the follow-up experiment didn't work (our p-value was about 0.24, which is not less than 0.05). 
# This could be because kittens aren't actually that desirable, or because we went in with bad assumptions. We found our 
# conversion results in our first experiment in January, but ran our second experiment in August, when conversion rates were 
# already high. Remember to always consider what 'control' really means when building your follow-up experiments.
# But! we ran second experiment in August and our previous with cat in hat - in January. Need to consider that CR different 
# because of seasonality
# Plot 8 months data
# Before starting the next experiment, let's take a second to look at the data so far from our original two conditions. 
# Even though the cat in the hat did better, you decided to keep running both versions so you could see how results 
# compared over more time. All necessary libraries and the data frame eight_month_checkin_data has been pre-loaded for you.
head(eight_month_checkin_data)
# A tibble: 6 x 3
# visit_date condition clicked_adopt_today
#    <date>     <chr>                   <dbl>
#  1 2018-01-01 cat_hat                     1
#  2 2018-01-01 cat_hat                     1
#  3 2018-01-01 cat_hat                     0
#  4 2018-01-01 cat_hat                     0
#  5 2018-01-01 cat_hat                     0
#  6 2018-01-01 cat_hat                     0
eight_month_checkin_data_sum <- eight_month_checkin_data %>%
  mutate(month_text = month(visit_date, label = TRUE)) %>%
  group_by(month_text, condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))
# Plot month-over-month results
ggplot(eight_month_checkin_data_sum,
       aes(x = month_text,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom
# It looks like conversion rates have been consistently higher for our cat in a hat condition (http://joxi.ru/DmBVXLDiq6MNMm)

# Plot styling 1
# This plot actually looks pretty nice and could be useful to share with someone else either on or outside of your team. 
# Let's take some time to clean it up a bit. Some of these functions should be familiar from earlier. The summarized data 
# and packages have been pre-loaded for you.
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

# Plot styling 2
# Let's do just a couple more updates and then we'll be ready to hand off our plot to others to show how great our first 
# experiment was.
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
# http://joxi.ru/gmvLRvgCe0PLDA
# Great! Now we can see a lot more information about what's going on before getting ready for our next experiment.

# Computing conversion rate difference
# we see that there is clear defference between cr for test and control. Let's calculate it.
eight_month_checkin_data_diff <- eight_month_checkin_data_sum %>%
  spread(condition, conversion_rate) %>%
  mutate(condition_diff = cat_hat - no_hat)
mean(eight_month_checkin_data_diff$condition_diff)
# 0.1876
sd(eight_month_checkin_data_diff$condition_diff)
# 0.0389
# If we take the mean and standard deviation of our new column we see that on average there has been a difference of 19%, 
# and a standard deviation of about 4%, suggesting that the effect is pretty consistent. If our effect hadn't been consistent 
# we should take some time to think about why that is. It would require some additional data exploration. Maybe a different 
# feature was optimized through another A/B experiment? There's no clear cutoff for what to call a "consistent" effect. 
# The more time you spend with your data though, the better sense you should have about when something is off and more 
# digging is needed

# Conversion rate between years
# In the video, I computed the conversion rate between our no-hat condition and our hat condition for the most recent year. 
# We should also check if conversion rates have changed between years for our no-hat condition. The dataset with both years 
# worth of data summarized by month and year and any packages are pre-loaded for you. You can find the data in no_hat_data_sum.
# Compute difference over time
no_hat_data_diff <- no_hat_data_sum %>%
  spread(year, conversion_rate) %>%
  mutate(year_diff = `2018` - `2017`)
no_hat_data_diff
# Compute summary statistics
mean(no_hat_data_diff$year_diff, na.rm = TRUE)
# [1] 0.01323157
sd(no_hat_data_diff$year_diff, na.rm = T)
# [1] 0.03817146
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
no_hat_data_sum %>% spread(year, conversion_rate)
#  A tibble: 12 x 4
#  month `2017` `2018` year_diff
#  <ord>  <dbl>  <dbl>     <dbl>
#  1 Jan    0.177  0.165  -0.0129 
#  2 Feb    0.168  0.225   0.0571 
#  3 Mar    0.129  0.135   0.00645
#  4 Apr    0.143  0.137  -0.00667
#  5 May    0.252  0.268   0.0161 
#  6 Jun    0.290  0.307   0.0167 
#  7 Jul    0.390  0.345  -0.0452 
#  8 Aug    0.506  0.581   0.0742 
#  9 Sep    0.297 NA      NA      
# 10 Oct    0.2   NA      NA      
# 11 Nov    0.23  NA      NA      
# 12 Dec    0.445 NA      NA 
# Looks like our conversion rates have been pretty consistent for the no_hat condition. This is good! It means we can reference 
# last year's data for months that haven't happened yet

# Re-run power analysis for follow-up
# Let's rerun our power analysis for our new experiment now taking into consideration the time of year we're running our new 
# experiment: September. To figure out our baseline assumptions, we'll give you some introductory information: 1) the conversion rate 
# for the "no hat" condition in 2017 (Sep) was 30% (or 0.3), and 2) the average difference between the "no hat" condition and the 
# "cat hat" condition is 19% (0.19). Use this information to run an updated power analysis.
# Fill in the value for p1 (our expected conversion rate for "cat hat" condition in September). Do this by summing 
# the conversion rate for "no hat" condition (0.3), and the difference between the conditions (0.19).
# Fill in the value for p2 (our expected conversion rate for "kitten hat" condition in September) assuming 
# an increase of 15 percentage points (0.15).
# Load package for power analysis
library(powerMediation)

# Run power analysis for logistic regression
total_sample_size <- SSizeLogisticBin(p1 = 0.49, # baseline 'cat hat' cr for Sep (when we conduct second experiment)
                                      p2 = 0.64, # expected CR for 'kitten hat'
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size
# Now our power analysis says we need 341 data points in total, much higher than when we ran it before.

# Re-run glm() for follow-up
# Now that we updated our power analysis, I ran our experiment for September, pulling 171 data points per condition. 
# The data has been pre-loaded for you in the data frame followup_experiment_data_sep.
# Load package to clean up model outputs
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
# Our follow-up experiment was successful! Now that we pulled the correct number of data points, we can see that there is a boost by using a kitten over a cat.

# Chapter 3
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
mean(viz_website_2017$time_spent_homepage_sec)
# [1] 67.29971
viz_website_2017 %>% summarise(mean(time_spent_homepage_sec))
# A tibble: 1 x 1
# `mean(time_spent_homepage_sec)`
# <dbl>
#  1                            67.3
# Compute summary of time spent on home page by month
viz_website_2017 %>%
  group_by(month(visit_date)) %>%
  summarise(article_conversion_rate = mean(time_spent_homepage_sec))
# # A tibble: 12 x 2
#    `month(visit_date)` article_conversion_rate
#                  <dbl>                   <dbl>
#  1                   1                    59.0
#  2                   2                    60.0
#  3                   3                    70.0
#  4                   4                    49.9
#  5                   5                    48.0
#  6                   6                    59.9
#  7                   7                    60.1
#  8                   8                    60.1
#  9                   9                    70.0
# 10                  10                    80.0
# 11                  11                    89.9
# 12                  12                   100. 
# Article click frequency monthly
# In the video, we saw there were four different types of variables we've been collecting for our website. We looked at one of them. Compute the monthly average for how often people click on one of the articles on the homepage.
# The data has been pre-loaded for you in the data frame viz_website_2017 and all packages have been pre-loaded.
# Compute summary of clicked articles by month
viz_website_2017 %>%
  group_by(month(visit_date)) %>%
  summarise(article_conversion_rate = mean(clicked_article))
# A tibble: 12 x 2
#    `month(visit_date)` article_conversion_rate
#                 <dbl>                   <dbl>
#  1                   1                   0.501
#  2                   2                   0.601
#  3                   3                   0.696
#  4                   4                   0.602
#  5                   5                   0.702
#  6                   6                   0.647
#  7                   7                   0.445
#  8                   8                   0.602
#  9                   9                   0.697
# 10                  10                   0.495
# 11                  11                   0.405
# 12                  12                   0.549
# Well done! We see that overall conversion rates are pretty high, meaning that if someone comes to the website, they are pretty likely to click an article.

# 'Like' click frequency plot
# Let's get some practice computing another variable, this time also plotting the results. Let's look at the amount of time someone clicked 'like' on one of the articles.
# Compute 'like' click summary by month
viz_website_2017_like_sum <- viz_website_2017 %>%
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarize(like_conversion_rate = mean(clicked_like))
head(viz_website_2017_like_sum)
# A tibble: 12 x 2
# month like_conversion_rate
#  <ord>                <dbl>
#  1 Jan                  0.197
#  2 Feb                  0.118
#  3 Mar                  0.148
#  4 Apr                  0.166
#  5 May                  0.212
#  6 Jun                  0.297
#  7 Jul                  0.404
#  8 Aug                  0.125
#  9 Sep                  0.153
# 10 Oct                  0.202
# 11 Nov                  0.249
# 12 Dec                  0.294
# Plot 'like' click summary by month
ggplot(viz_website_2017_like_sum,
       aes(x = month, y = like_conversion_rate, group = 1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), labels = percent)
# http://joxi.ru/Vm6gyx7U309Za2
# It looks like conversion rates are below 50% and peaked in July.

# 'Like' / 'Share' click frequency plot
# Instead of just computing another conversion rate, here, we want to plot the 'like' conversion rate and the 'share' 
# conversion rate side by side. I did some work ahead of time for you and created a new data frame viz_website_2017_like_share_sum. 
# Take a look at it and you'll see there are two new columns: action, which refers to clicking 'like' or clicking 'share' 
# and conversion_rate which is the percentage of the time the action was done for a given month.
head(viz_website_2017_like_share_sum,20)
# A tibble: 20 x 3
# month action conversion_rate
# <ord> <chr>            <dbl>
#  1 Jan   like            0.197 
#  2 Feb   like            0.118 
#  3 Mar   like            0.148 
#  4 Apr   like            0.166 
#  5 May   like            0.212 
#  6 Jun   like            0.297 
#  7 Jul   like            0.404 
#  8 Aug   like            0.125 
#  9 Sep   like            0.153 
# 10 Oct   like            0.202 
# 11 Nov   like            0.249 
# 12 Dec   like            0.294 
# 13 Jan   share           0.0516
# 14 Feb   share           0.0113
# 15 Mar   share           0.0192
# 16 Apr   share           0.0296
# 17 May   share           0.0501
# 18 Jun   share           0.0701
# 19 Jul   share           0.0203
# 20 Aug   share           0.0104
ggplot(viz_website_2017_like_share_sum,
       aes(x = month, y = conversion_rate, color = action, group = action)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0,1), labels = percent)
# http://joxi.ru/KAxZeo9CVqez9m
# Great! It looks like people are even less likely to 'share' an article as they are to 'like' it

# Assumptions and types of A/B testing
# Within group vs. between group ex
# One phrase you may have heard before is "within" versus "between" group or participants. A "within" experiment is one in which each 
# participant sees both conditions of an experiment, so you can see if that particular person behaved differently between the two 
# conditions. A "between" experiment puts a participant in one of the two conditions, and then you compare how the two groups of 
# participants behaved. Both methods can be used. Generally "within" experiments have higher power, but if you have a website where 
# you don't know if a specific visitor will come to your website more than once, it makes more sense to do a "between" experiment. 
# One assumption of a "between" experiment is that participants in each condition should come from the same random group of possible 
# participants, so that there is nothing qualitatively different between the two groups, except which condition they saw. In the 
# case of our new experiment we're going to run a "between" experiment, since we don't know how often a given person comes to the website

# Types of A/B testing
# Within A/B testing, there are various types of experiments you can run based on different conditions. So far we've focused on 
# what's classically known as A/B testing, where you have one control condition (generally the current version of the website, 
# in our case the use of the word "Tips") versus a test condition (in our case the use of the word "Tools"). Another type of 
# testing though is A/A, where you actually have two identical control conditions, in our case two groups of "Tips". This type 
# of testing can be useful to confirm that your control condition really is stable, or that the way you're building a between 
# experiment is actually testing two similar groups of people. If you get a significant effect for an A/A experiment, 
# something is wrong, because in theory you are running two of the exact same condition. For example, if you get a significant 
# effect it could mean there is an error in how you are randomly assigning your participants, and the two groups of people in your 
# experiment are different in some way, when they should be the same. A final type of testing is A/B/N where you can have a control 
# condition and any number of test conditions. For example here we could have "tips" (our control condition) versus "tools" versus "
# strategies" (two separate test conditions). A/B/N can seem like an exciting fast way to test more things quickly, but the 
# statistics are more complicated and you'll need more data points to be sure of an effect, so in general it's safer to just  
# use A/B when starting out.

# Which of these experiments describes a within-participant experiment?
# - You randomly sample a group of people where a button color is blue. One month later you sample a new group where the button color is green.
#   Incorrect
#   Since you run the experiments one month apart (not good!) you can't be sure you have the same group of people
# - You have a group of email users and for two weeks randomly show one of two "Inbox" designs whenever a person logs in.
#   Correct
#   Since a given user sees both versions of the Inbox, this is a within-participant experiment.
# - You run one condition of homepage images simultaneously. One is tested on people with IP addresses in the United States and one with IP addresses in Italy.
#   Incorrect
#   Since groups are determined by IP address, it is almost impossible for a given person to see both versions of the homepage image
# - You use a new design that speeds up loading times. Every visitor who shows up on the website either gets the old version or the new, faster version. You don't track who your visitors are.
#   Incorrect
#   Since you don't track who your visitors are, you have no way to know how many times a given user has seen both versions of the website.

# Plotting A/A data
# Before running our experiment, let's say we ran an A/A experiment first to be sure our "random" assignment of visitors really 
# was random. Remember, a significant effect in an A/A experiment can mean our two groups of participants actually are 
# different in some way. For this experiment, we're going to look at clicking 'like' conversion. We'll start by summarizing 
# and plotting our data.
# All packages and the data are pre-loaded for you. The data is in the data frame viz_website_2018_01.
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
# Compute conversion rates for A/A experiment
viz_website_2018_01_sum <- viz_website_2018_01 %>%
  group_by(condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))
viz_website_2018_01_sum
# A tibble: 2 x 2
# condition   like_conversion_rate
#     <chr>                  <dbl>
# 1      A1                  0.201
# 2      A                   0.197
# Plot conversion rates for two conditions
ggplot(viz_website_2018_01_sum,
       aes(x = condition, y = like_conversion_rate)) +
  geom_bar(stat = "identity") +       # To make a bar plot, use geom_bar(). Here use stat = "identity" so it plots our computed values, rather than make bars of counts
  scale_y_continuous(limits = c(0, 1), labels = percent)
# Based on these bar plots the two A conditions look very similar. That's good!

# Analyzing A/A data
# Just as for a normal A/B experiment, we need to statistically analyze our A/A results to be sure there really was no effect. Here we'll build a logistic regression again, this time hoping for a null effect.
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

# Confounding variables
# 1. Confounding Variables
# In the last lesson we talked about some of the assumptions of A/B testing as well as some different types of A/B testing. Here we'll go over one aspect that could hurt your experiment: confounding variables.
# 2. Confounding variables
# You may recall that the phase "confounding variables" was mentioned once in a previous lesson. I'm defining it here as "an element of the environment that could affect your ability to find out the *truth* of an A/B experiment". So what exactly do I mean by that?
# 3. Confounding variables - internal
# Sometimes a confounding variable is internal to how the experiment was designed. Let's come back to our experiment for a second. Recall that we have two conditions. One with "Tips" in the title and one with "Tools" in the title, and we want to see how often someone clicks a 'like' button. Let's say we run the experiment for 1 month and we find that for
# 4. Confounding variables - internal
# the original "tips" page 20% of people clicked 'like', but on the "tools" page only 10% of people clicked 'like'. Looks like people like the word "tips" better! Is it really specifically that word though? Other effects could be driving this. For example
# 5. Confounding variables - internal
# what about word length or word frequency? Maybe people like "tips" because it's shorter, or because it is less frequent and more novel. For any future experiment comparing "tips" to another word, these kinds of things should be considered.
# 6. Confounding variables - external
# Confounding variables can be external to your experiment too. Let's say you run your experiment for a second month and you find
# 7. Confounding variables - external
# that in the second month "tools" was actually much more likely to result in "likes". What happened? Well let's say you were able to collect demographic information about who came to visit your website. In the first month
# 8. Confounding variables - external
# your traffic was evenly split between people ages 20 to 35, and people over 35. However, at the beginning of the second month, your website was sent in an alumni listserv for a university class of 2000, so people over 35 (http://joxi.ru/GrqQMXgSzyBvlr).
# 9. Confounding variables - external
# Now your traffic is 90% people over 35. So it's possible your effect isn't because "tools" is a better word across all your typical user base, but preferred by a certain age group. This is why it's important to track information like who your users are and when they come to your website, to know if an effect is really driven by your design change, or some external factor about your user base. If this jump in 90% of visitors being over 35 isn't the norm for your website, and then you change the design only to have it go back to an even split (50% of users age 20 to 35, 50% of users age 35 plus), you've just hurt your website by not taking into account the confounding variable of visitor age.
# 10. Let's practice!
# Let's get some practice looking at the effects of confounding variables in our own dataset.

# Examples of confounding variables
# Let's go back to our cat adoption website for a second. You run an experiment where in one condition, the homepage is a tabby cat, and in another condition, it is a black cat. Both conditions are run at the same time. At the beginning of the experiment, a children's movie comes out starring a black cat. Which of these statements is true?
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
head(viz_website_2018_02)
# A tibble: 28,000 x 7
# visit_date condition time_spent_home~ clicked_article clicked_like
# <date>     <chr>                <dbl>           <dbl>        <dbl>
#  1 2018-02-01 tips                  58.1               1            0
# 2 2018-02-01 tips                  61.4               1            0
# 3 2018-02-01 tips                  59.8               0            1
# 4 2018-02-01 tips                  59.6               1            0
# 5 2018-02-01 tips                  60.7               0            0
# 6 2018-02-01 tips                  59.3               1            0
# ... with 2 more variables: clicked_share <dbl>, article_published <chr>
# Compute 'like' conversion rate by week and condition
viz_website_2018_02 %>%
  mutate(week = week(visit_date)) %>%
  group_by(week, condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))
# A tibble: 10 x 3
# Groups:   week [?]
#     week condition like_conversion_rate
#    <dbl>     <chr>               <dbl>
#  1     5      tips               0.109 
#  2     5     tools               0.0156
#  3     6      tips               0.124 
#  4     6     tools               0.0238
#  5     7      tips               0.115 
#  6     7     tools               0.0492
#  7     8      tips               0.124 
#  8     8     tools               0.110 
#  9     9      tips               0.114 
# 10     9     tools               0.147 
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
# Clearly there was an effect of the article coming out. First we saw a switch in 'like' rates in the second half of February and then saw it was directly related to whether the article was published or not.

# Confounding variable example plotting
# Let's see if we can tell when 'like' rates really started to change by plotting daily like rates.
# The data frame summarized by day is viz_website_2018_02_sum. All packages have already been loaded for you.
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
# Plot 'like' conversion rates by date for experiment
ggplot(viz_website_2018_02_sum,
       aes(x = visit_date,
           y = like_conversion_rate,
           color = condition,
           linetype = article_published,
           group = interaction(condition, article_published))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date(article_published_min_date)), color='grey') +
  scale_y_continuous(limits = c(0, 0.3), labels = percent)
# http://joxi.ru/J2b0eVlTqYa1gr
# It looks like the article had a pretty clear effect, and the effect seemed to increase over time as it presumably got more hits.

# Side effects
# 1. Side Effects
# In the last lesson we looked at how confounding variables can negatively affect experiment results. Here we'll talk about another variable to account for: side effects.
# 2. Side effects
# When designing an experiment, I've talked about how it's important to only change one thing at a time to really be sure of what you're testing. However, some changes are out of our control, and this is where side effects come in. A side effect is an unintended consequence of a change you made. What's a potential example of this?
# 3. Side effects
# Let's take our example again. We found that 'like' click rates dropped by 10% in the "tools" condition. We also noted that "tools" is one letter longer than "tips". Well what if I told you that one letter made a huge difference on load times, and while the "tips" homepage loads in
# 4. Side effects
# 5 seconds, the "tools" homepage takes
# 5. Side effects
# 7 seconds to load! So we don't really know if our drop in 'like' rates is due to the word, or due to the side effect of the longer load times. This is of course an exaggerated example, but often times, adding new features can slow down a website, which can negatively impact performance. One way to account for this side effect would be to add a delay to the original "tips" homepage to be sure both versions have the same load time.
# 6. Examples of side effects
# Some common examples of side effects in A/B testing are load times, as already discussed and the amount of information above the fold. The fold is a reference to newspapers, and in the context of websites, it's what a person sees without having to do any scrolling. Adding a larger header image for example may push down other content to a place that requires scrolling to get to. People may give up before scrolling, resulting in less use of the website. Other side effects will come up depending on your design. The key is to try and be aware of them and account for them in both conditions whenever possible. This is why spending time to carefully think about how you're going to design your experiment is incredibly important rather than just jumping in.
# 7. Let's practice!
# Let's look at some possible side effects in our own data.

# Confounding variable vs. side effect
# For our cat adoption website with an experiment comparing a tabby cat and a black cat, the photo of the tabby cat loads in 5 seconds and the black cat in 2 seconds. The tabby cat is 1 year old and the black cat is a 6 years old. Which of these statements is true?
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
# The viral article has died down, so you've decided to rerun your experiment on "tips" vs. "tools". However, after running 
# it for a month you realized there was a larger load delay for the "tools" homepage than the "tips" homepage. You then added 
# a delay to the "tips" homepage so that they even out. To start, let's visualize the effect the delay has on a 'like' rates 
# on a daily basis.
head(viz_website_2018_03)
# A tibble: 6 x 7
# visit_date condition time_spent_home~ clicked_article clicked_like
# <date>     <chr>                <dbl>           <dbl>        <dbl>
# 1 2018-03-01 tips                  71.2               1            0
# 2 2018-03-01 tips                  69.9               1            0
# 3 2018-03-01 tips                  72.1               1            0
# 4 2018-03-01 tips                  70.2               1            0
# 5 2018-03-01 tips                  69.4               1            0
# 6 2018-03-01 tips                  70.3               1            1
# ... with 2 more variables: clicked_share <dbl>, pageload_time <dbl>
# Compute 'like' conversion rate and mean pageload time by day
viz_website_2018_03_sum <- viz_website_2018_03 %>%
  group_by(visit_date, condition) %>%
  summarize(mean_pageload_time = mean(pageload_time),
            like_conversion_rate = mean(clicked_like))
viz_website_2018_03_sum
# A tibble: 62 x 4
# Groups:   visit_date [?]
# visit_date    condition mean_pageload_time like_conversion_rate
# <date>        <chr>                  <dbl>                <dbl>
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
       aes(x = visit_date, y = mean_pageload_time, color = condition)) +
  geom_point()
# http://joxi.ru/MAjv0olHdZyVPA

# # Plot effect of 'like' conversion rate by pageload time
ggplot(viz_website_2018_03_sum,
       aes(x = mean_pageload_time, y = like_conversion_rate, color = condition)) +
  geom_point()
# http://joxi.ru/1A5qbxpc4k3KWA
# There's clearly a correlation here. Longer load times lead to lower conversion rates.

# Side effects experiment plot
# Let's end by plotting our new conversion rate, seeing if we can find the effect of when the page load delay was added.

viz_website_2018_03_sum
# A tibble: 62 x 4
# Groups:   visit_date, condition [?]
# visit_date condition pageload_delay_added like_conversion_rate
#  <date>     <chr>     <chr>                               <dbl>
#  1 2018-03-01 tips      no                                 0.146 
#  2 2018-03-01 tools     no                                 0.0543
#  3 2018-03-02 tips      no                                 0.151 
#  4 2018-03-02 tools     no                                 0.0514
#  5 2018-03-03 tips      no                                 0.136 
#  6 2018-03-03 tools     no                                 0.048 
#  7 2018-03-04 tips      no                                 0.169 
#  8 2018-03-04 tools     no                                 0.0569
#  9 2018-03-05 tips      no                                 0.143 
# 10 2018-03-05 tools     no                                 0.0576
# ... with 52 more rows
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
# http://joxi.ru/82QRVQ5c83kdZA
# Great job! We can clearly see the effect that adding the delay had on the two conditions

# Power Analysis
# Logistic regression power analysis
# In the previous chapters you got some practice with power analyses with logistic regression (you didn't think you got to forget 
# about that did you?). Let's test your recall knowledge by running that kind of power analysis assuming we're running an 
# experiment on whether or not someone clicked 'like' on an article.
# Fill in p1 and p2 assuming a control value of 17% click 'like' (the conversion rate for April 2017) and a 10 percentage point increase in the test condition.
# Load package to run power analysis
library(powerMediation)
# Run power analysis for logistic regression
total_sample_size <- SSizeLogisticBin(p1 = 0.17,
                                      p2 = 0.27,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size
# Great! We need 537 data points in total to see an increase in 'like' clicks by 10 percentage points

# What is the meaning of type argument in pwr.t.test()?
# Correct! You use type to describe how your data points and alternative to talk about the hypothesis of the experiment

# T-test power analysis
# Now that we know how to run power analyses for continuous data using a t-test, let's try running a power analysis for an 
# experiment on time spent on the homepage.
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
# At an effect size of 0.3, we need a sample size of 176 data points per group.

# Statistical Tests
# Logistic regression
# In the previous lessons, we got some practice with logistic regression. Let's do 
# the same here looking at our experiment to see if people clicked 'like' or not 
# depending on if the homepage has "Tips" (the control) or "Tools" (the test) in the 
# title. You can assume this is after all confounding and side effects were figured out.
# The data frame is pre-loaded as viz_website_2018_04.
head(viz_website_2018_04)
# A tibble: 6 x 6
# visit_date condition time_spent_home~ clicked_article clicked_like
# <date>     <chr>                <dbl>           <dbl>        <dbl>
# 1 2018-04-01 tips                  49.0               1            0
# 2 2018-04-01 tips                  48.9               1            0
# 3 2018-04-01 tips                  49.1               1            0
# 4 2018-04-01 tips                  49.3               0            1
# 5 2018-04-01 tips                  50.4               0            1
# 6 2018-04-01 tips                  49.1               1            0
# ... with 1 more variable: clicked_share <dbl>
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
# Based on our logistic regression there was a significant effect, however not in 
# the direction we wanted. It look like 'Tools' actually had lower 'like' click 
# rates than 'Tips'.

# T-test
# Let's now analyze one of our dependent variables for a continuous dependent variable using a t-test. 
# The data frame is pre-loaded for you as viz_website_2018_04
# Use the t.test() function, setting the dependent variable to the column referring to time spent on the homepage and the independent variable to the column for condition
str(viz_website_2018_04)
# tibble [30,000 x 6] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
# $ visit_date             : Date[1:30000], format: "2018-04-01" "2018-04-01" ...
# $ condition              : chr [1:30000] "tips" "tips" "tips" "tips" ...
# $ time_spent_homepage_sec: num [1:30000] 49 48.9 49.1 49.3 50.4 ...
# $ clicked_article        : num [1:30000] 1 1 1 0 0 1 1 1 1 0 ...
# $ clicked_like           : num [1:30000] 0 0 0 1 1 0 0 0 0 0 ...
# $ clicked_share          : num [1:30000] 1 0 0 0 0 0 0 0 0 0 ...
# - attr(*, "spec")=
#   .. cols(
#     ..   visit_date = col_date(format = ""),
#     ..   condition = col_character(),
#     ..   time_spent_homepage_sec = col_double(),
#     ..   clicked_article = col_double(),
#     ..   clicked_like = col_double(),
#     ..   clicked_share = col_double()
#     .. )

# Run t-test
ab_experiment_results <- t.test(time_spent_homepage_sec ~ condition,
                                data = viz_website_2018_04)
ab_experiment_results
# Welch Two Sample t-test
# data:  time_spent_homepage_sec by condition
# t = 0.36288, df = 29997, p-value = 0.7167
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.01850573  0.02691480
# sample estimates:
#   mean in group tips mean in group tools 
# 49.99909            49.99489 
# the same
pt(q=0.36288, df = 29997, lower.tail = F) * 2
# 0.7166971
# It looks like our experiment had no effect of time spent on the homepage, despite a lower 'like' click rate

# Stopping Rules ans Sequential Analysis
# What is a sequential analysis?
# Sequential analysis is when you adjust your p-value to allow for looking at your data at various stopping points.
# Correct! Sequential analysis builds in stopping points to look at your data.
# Sequential analysis three looks
# In the video, we built a sequential analysis with four looks at the data. In this exercise, you will build a sequential analysis for only three looks.
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
# Analysis Ratio*      Z   Nominal p    Spend
# 1         0.394   1.99      0.0232   0.0232
# 2         0.789   1.99      0.0232   0.0155
# 3         1.183   1.99      0.0232   0.0113
# Total                       0.0500 
# ++ alpha spending:
#   Pocock boundary.
# * Sample size ratio compared to fixed design with no interim
# Boundary crossing probabilities and expected sample size
# assume any cross stops the trial
# Upper boundary (power or Type I Error)
# Analysis
# Theta      1      2      3 Total   E{N}
# 0.0000 0.0232 0.0155 0.0113  0.05 1.1591
# 2.4865 0.3334 0.2875 0.1791  0.80 0.8070
# It looks like our p-value cuttoff is higher if we only look at the data three times

# Sequential analysis sample sizes
# Now that we've built our sequential analysis with three looks, let's see what our stopping points are.
# You've been given permission to run the experiment to collect a grand total of 3000 data points. Fill in max_n with this number.
# Compute the max_n_per_group by filling in the missing variable.
# Call the timing element from our sequential analysis object
# Load package to run sequential analysis
library(gsDesign)
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
# Based on this we should run our analysis at 500, 1000, and 1500 data points per group

# Mu;tivariate Testing
# Plotting time homepage in multivariate experiment
# In the video, I ran our statistical analysis but didn't plot our data. Remember, it's always important to plot your data first so you're sure you have a sense of what's going on. Let's plot the means for our four conditions for time spent on the homepage.
# The data is preloaded in a data frame for you called viz_website_2018_05.
head(viz_website_2018_05)
# A tibble: 6 x 7
# visit_date word_one word_two time_spent_home~ clicked_article clicked_like
# <date>     <chr>    <chr>               <dbl>           <dbl>        <dbl>
# 1 2018-05-01 tips     better               48.4               0            0
# 2 2018-05-01 tips     better               47.3               0            0
# 3 2018-05-01 tips     better               49.2               1            1
# 4 2018-05-01 tips     better               48.6               0            0
# 5 2018-05-01 tips     better               48.5               1            1
# 6 2018-05-01 tips     better               48.3               1            0
# ... with 1 more variable: clicked_share <dbl>
names(viz_website_2018_05)
# [1] "visit_date"              "word_one"               
# [3] "word_two"                "time_spent_homepage_sec"
# [5] "clicked_article"         "clicked_like"  
viz_website_2018_05_sum <- viz_website_2018_05 %>%
  group_by(word_one, word_two) %>%
  summarize(mean_time_spent_homepage_sec = mean(time_spent_homepage_sec))
viz_website_2018_05_sum
# A tibble: 4 x 3
# Groups:   word_one [?]
# word_one word_two mean_time_spent_homepage_sec
#   <chr>    <chr>                           <dbl>
# 1 tips     amazing                          48.0
# 2 tips     better                           48.0
# 3 tools    amazing                          53.0
# 4 tools    better                           48.0
# http://joxi.ru/8An4XoVTN0ODxm
# The results of our regression look validated. Indeed time spent on the homepage was highest in the 'Tools'/'Amazing' combination

# Plotting 'like' clicks in multivariate experiment
# Now that we've seen that there was an interaction for time spent on the homepage, let's take look at the conversion rates for clicking 'like' to see if there was an additive effect.
# All packages you need are pre-loaded. The data is preloaded in the data frame viz_website_2018_05
# Compute summary values for four conditions
viz_website_2018_05_sum <- viz_website_2018_05 %>%
  group_by(word_one, word_two) %>%
  summarize(like_conversion_rate = mean(clicked_like))
viz_website_2018_05_sum
# A tibble: 4 x 3
# Groups:   word_one [?]
# word_one word_two like_conversion_rate
#  <chr>    <chr>                   <dbl>
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
# http://joxi.ru/D2PajYQcw1LQRA
# Looks like we have another interaction! This one appears to be even stronger. While the 'like' click rate decreased with 'Tools' with 'Better', it looks to be even higher than the control with 'Amazing'.

# Multivariate design statistical test
# Let's also run a statistical analysis for 'like' conversion rate.
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
#                            term    estimate  std.error   statistic         p.value
# 1                   (Intercept) -1.30558956 0.01961052 -66.5759940    0.000000e+00
# 2                 word_onetools -0.79640382 0.03239365 -24.5851815   1.819678e-133
# 3               word_twoamazing -0.01933586 0.02781111  -0.6952566    4.868945e-01
# 4 word_onetools:word_twoamazing  1.77174758 0.04128273  42.9174069    0.000000e+00
# Once again we found a significant interaction. While there was no effect of word_two for the baseline of 'Tips', there likely was an effect for 'Tools'.












[7] "clicked_share" 



