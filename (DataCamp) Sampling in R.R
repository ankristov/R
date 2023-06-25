
#
# Reasons for sampling
# Sampling is an important technique in your statistical arsenal. It isn't always appropriate though—you 
# need to know when to use it and when to work with the whole dataset.
# Which of the following is not a good scenario to use sampling?
# Possible Answers
# - You've been handed one terabyte of data about error logs for your company's IoT device.
# - You wish to learn information about the travel habits of all Pakistani adult citizens.
# - You've finished collecting data on a small study on the size of the wings of 10 butterflies. +
# - You are working to predict customer turnover on a big data project for your marketing firm.
# Commendations on your justifications for not sampling! Ten butterflies is a small dataset, so sampling isn't useful.
library(dplyr)
?slice_sample() # for data frames
?sample # for vectors
df <- tibble(x = seq(0,10,by = 1))
df %>% select(x) %>% slice_sample(n = 2)
sample(df$x, size = 2)

# 1 Living the sample life.mp4
# Simple sampling with dplyr
# Throughout this chapter you'll be exploring song data from Spotify. Each row of the dataset represents 
# a song, and there are 41656 rows. Columns include the name of the song, the artists who performed it, 
# the release year, and attributes of the song like its duration, tempo, and danceability. We'll start 
# by looking at the durations.
# Your first task is to sample the song dataset and compare a calculation on the whole population and on a sample.
# spotify_population is available and dplyr is loaded.
# Instructions 
# Use View() to view the spotify_population dataset. Explore it in the viewer until you are clear on 
# what it contains.
# Use dplyr to sample 1000 rows from spotify_population, assigning to spotify_sample.
# View the whole population dataset
View(spotify_population)
dim(spotify_population)
# [1] 41656    20
# Sample 1000 rows from spotify_population
spotify_sample <- spotify_population %>% slice_sample(n=1000)
# See the result
spotify_sample
dim(spotify_sample)
# [1] 1000   20

# Using the spotify_population dataset, calculate the mean duration in minutes. Call the calculated 
# column mean_dur.
# Using the spotify_sample dataset, perform the same calculation in another column called mean_dur.
# Look at the two values. How different are they?
# From previous step
spotify_sample <- spotify_population %>% slice_sample(n = 1000)
# Calculate the mean duration in mins from spotify_population
mean_dur_pop <- mean(spotify_population$duration_ms)
mean_dur_pop <- spotify_population %>% summarize(mean_dur = mean(duration_ms))
# Calculate the mean duration in mins from spotify_sample
mean_dur_samp <- mean(spotify_sample$duration_ms) # 
mean_dur_samp <- spotify_sample %>% summarize(mean_dur = mean(duration_ms)) # data frame
# See the results
mean_dur_pop 
# A tibble: 1 × 1
# mean_dur
#      <dbl>
# 1     3.85
mean_dur_samp 
# A tibble: 1 × 1
# mean_dur
#      <dbl>
# 1     3.87
# Super summarization! Notice that the mean duration of songs in the sample is similar, but not identical 
# to the mean duration of songs in the whole population.

# Simple sampling with base-R
# While dplyr provides great tools for sampling data frames, if you want to work with vectors you can 
# use base-R.
# Let's turn it up to eleven and look at the loudness property of each song.
# spotify_population is available.
# Instructions 
# Get the loudness column of spotify_population, assigning to loudness_pop.
# Using base-R, sample loudness_pop to get 100 random values, assigning to loudness_samp.
str(spotify_population)
# tibble [41,656 × 20] (S3: tbl_df/tbl/data.frame)
# $ acousticness    : num [1:41656] 0.972 0.321 0.00659 0.0039 0.122 0.0666 0.00747 0.0792 0.0004 0.759 ...
# $ artists         : chr [1:41656] "['David Bauer']" "['Etta James']" "['Quasimoto']" "['Millencolin']" ...
# $ danceability    : num [1:41656] 0.567 0.821 0.706 0.368 0.501 0.829 0.352 0.974 0.505 0.17 ...
# $ duration_ms     : num [1:41656] 313293 360240 202507 173360 344200 ...
# $ duration_minutes: num [1:41656] 5.22 6 3.38 2.89 5.74 ...
# $ energy          : num [1:41656] 0.227 0.418 0.602 0.977 0.511 0.614 0.985 0.604 0.617 0.0504 ...
# $ explicit        : num [1:41656] 0 0 1 0 0 1 0 0 0 0 ...
# $ id              : chr [1:41656] "0w0D8H1ubRerCXHWYJkinO" "4JVeqfE2tpi7Pv63LJZtPh" "5pxtdhLAi0RTh1gNqhGMNA" "3jRsoe4Vkxa4BMYqGHX8L0" ...
# $ instrumentalness: num [1:41656] 0.601 0.000372 0.000138 0 0 0.00085 0.00102 0 0.0131 0.948 ...
# $ key             : num [1:41656] 10 9 11 11 7 1 2 7 8 4 ...
# $ liveness        : num [1:41656] 0.11 0.222 0.4 0.35 0.279 0.0975 0.367 0.0528 0.421 0.112 ...
# $ loudness        : num [1:41656] -13.44 -9.84 -8.31 -2.76 -9.84 ...
# $ mode            : num [1:41656] 1 0 0 0 0 1 1 1 1 0 ...
# $ name            : chr [1:41656] "Shout to the Lord" "Miss You" "Real Eyes" "Penguins & Polarbears" ...
# $ popularity      : num [1:41656] 47 51 44 52 53 46 40 39 41 47 ...
# $ release_date    : chr [1:41656] "2000" "2000-12-12" "2000-06-13" "2000-02-22" ...
# $ speechiness     : num [1:41656] 0.029 0.0407 0.342 0.127 0.0291 0.267 0.22 0.036 0.0251 0.0377 ...
# $ tempo           : num [1:41656] 136.1 117.4 89.7 165.9 78 ...
# $ valence         : num [1:41656] 0.0396 0.803 0.479 0.548 0.113 0.496 0.241 0.998 0.417 0.0963 ...
# $ year            : num [1:41656] 2000 2000 2000 2000 2000 2000 2000 2000 2000 2000 ...
# Get the loudness column of spotify_population
loudness_pop <- spotify_population$loudness
# Sample 100 values of loudness_pop
loudness_samp <- sample(loudness_pop, size = 100)
# See the results
loudness_samp
#  [1]  -4.764  -5.297  -4.110  -3.425  -8.415  -5.901  -9.720 -12.851  -4.262
# [10]  -3.875  -9.454  -6.701  -5.862  -6.099  -7.142  -4.150  -6.415  -5.111
# [19] -11.812  -5.691  -5.180 -11.805  -5.570  -5.683  -6.247  -5.086  -4.489
# [28]  -7.231 -20.304  -4.209  -5.871 -10.324  -7.340  -6.408  -5.840  -4.143
# [37] -16.879  -4.086  -4.016 -14.968  -3.635  -6.166  -4.273 -14.032  -8.686
# [46]  -7.474  -5.883  -7.768  -7.814  -4.233  -1.297  -7.728  -5.679  -5.171
# [55]  -7.302 -11.562  -5.954  -8.149 -12.979  -5.628  -4.030  -8.219  -2.687
# [64]  -2.931  -6.596  -5.290  -4.768  -8.454 -10.736  -7.988  -4.638  -6.065
# [73]  -6.826  -4.908  -9.584  -9.867  -6.251  -3.751  -6.934  -7.990  -5.423
# [82]  -7.003  -7.631  -1.731  -5.016  -7.848  -4.158  -5.387  -9.488  -2.954
# [91]  -5.019  -6.911  -8.658  -7.052  -4.667  -5.196  -6.232  -6.680 -15.400
# [100]  -5.750

# Calculate the standard deviation of loudness_pop.
# Calculate the standard deviation of loudness_samp.
# Look at the two values. How different are they?
# From previous step
loudness_pop <- spotify_population$loudness
loudness_samp <- sample(loudness_pop, size = 100)
# Calculate the standard deviation of loudness_pop
sd_loudness_pop <- sd(loudness_pop)
# Calculate the standard deviation of loudness_samp
sd_loudness_samp <- sd(loudness_samp)
# See the results
sd_loudness_pop
# [1] 4.524076
sd_loudness_samp
# [1] 3.669686
# Devious standard deviating! Again, notice that the calculated value (the standard deviation) is close 
# but not identical in each case.

# 2 A little too convenient.mp4
# Are findings from the sample generalizable?
# You just saw how convenience sampling—collecting data via the easiest method can result in samples 
# that aren't representative of the whole population. Equivalently, this means findings from the sample 
# are not generalizable to the whole population. Visualizing the distributions of the population and 
# the sample can help determine whether or not the sample is representative of the population.
# The Spotify dataset contains a column named acousticness, which is a confidence measure from zero 
# to one of whether the track is acoustic, that is, it was made with instruments that aren't plugged 
# in. Here, you'll look at acousticness in the total population of songs, and in a sample of those songs.
# spotify_population and spotify_mysterious_sample are available; dplyr and ggplot2 are loaded.
# Instructions
# Using spotify_population, draw a histogram of acousticness with binwidth of 0.01.
# Visualize the distribution of acousticness as a histogram with a binwidth of 0.01
ggplot(spotify_population, aes(x = acousticness)) + geom_histogram(binwidth = 0.01)
# http://joxi.ru/gmvLRvgCeZqQWA

# Update the histogram code to use the spotify_mysterious_sample dataset.
# Set the x-axis limits from zero to one (for easier comparison with the previous plot).
dim(spotify_population)
# [1] 41656    20
dim(spotify_mysterious_sample)
# [1] 1107   20
# Update the histogram to use spotify_mysterious_sample with x-axis limits from 0 to 1
ggplot(spotify_mysterious_sample, aes(acousticness)) + geom_histogram(binwidth = 0.01) + xlim(0,1)
# http://joxi.ru/vAWkDKZI36gayA

# Question
# Compare the two histograms you drew. Are the acousticness values in the sample generalizable to 
# the general population?
# Possible Answers
# Yes. Any sample should lead to a generalizable result about the population.
# Yes. The sample selected is likely a random sample of all songs in our population.
# No. Samples can never lead to generalizable results about the population.
# No. The acousticness samples are consistently higher than those in the general population. +
# No. The acousticness samples are consistently lower than those in the general population.
# Ace acouticness analysis! The acousticness values in the sample are all greater than 0.95, whereas 
# they range from 0 to 1 in the whole population.

# Are the findings generalizable? 2
# Let's look at another sample to see if it is representative of the population. This time, you'll look 
# at the duration_minutes column of the Spotify dataset, which contains the length of the song in minutes.
# spotify_population and spotify_mysterious_sample2 are available; dplyr and ggplot2 are loaded.
# Instructions 
# Using spotify_population, draw a histogram of duration_minutes with binwidth of 0.5.
# Visualize the distribution of duration_minutes as a histogram with a binwidth of 0.5
ggplot(spotify_population, aes(x = duration_minutes)) + geom_histogram(binwidth = 0.5)
# http://joxi.ru/D2PajYQcwaqeRA

# Update the histogram code to use the spotify_mysterious_sample2 dataset.
# Set the x-axis limits from zero to fifteen (for easier comparison with the previous plot).
# Update the histogram to use spotify_mysterious_sample2 with x-axis limits from 0 to 15
ggplot(spotify_mysterious_sample2, aes(duration_minutes)) +
  geom_histogram(binwidth = 0.01) +
  xlim(0,15)
# http://joxi.ru/MAjv0olHdajNEA
dim(spotify_population)
# [1] 41627    20
dim(spotify_mysterious_sample2)
# [1] 50 20

# Question
# Compare the two histograms you drew. Are the duration values in the sample generalizable to the general population?
# Possible Answers
# Yes. Any sample should lead to a generalizable result about the population.
# Yes. The sample selected is likely a random sample of all songs in our population. +
# No. Samples can never lead to generalizable results about the population.
# No. The duration samples are consistently higher than those in the general population.
# No. The duration samples are consistently lower than those in the general population.
# Delightful duration distribution analysis! The duration values in the sample show a similar distribution to those in the whole population, so the results are generalizable.

# 3 How does Sue do sampling?.mp4
args(rnorm)
# Simple random sampling
# The simplest method of sampling a population is the one you've seen already. It is known as simple 
# random sampling (sometimes abbreviated to "SRS"), and involves picking rows at random, one at a time, 
# where each row has the same chance of being picked as any other.
# To make it easier to see which rows end up in the sample, it's helpful to include a row ID column in 
# the dataset before you take the sample.
# In this chapter, we'll look at sampling methods using a synthetic (fictional) employee attrition 
# dataset from IBM, where "attrition" means leaving the company.
# attrition_pop is available; dplyr is loaded.
# Instructions
# View the attrition_pop dataset. Explore it in the viewer until you are clear on what it contains.
# Set the random seed to a value of your choosing.
# Add a row ID column to the dataset, then use simple random sampling to get 200 rows.
# View the sample dataset, attrition_samp. What do you notice about the row IDs?
# View the attrition_pop dataset
View(attrition_pop)
# Set the seed
set.seed(7)
attrition_samp <- attrition_pop %>% 
  # Add a row ID column
  rowid_to_column %>% 
  # Get 200 rows using simple random sampling
  slice_sample(n = 200)
# View the attrition_samp dataset
attrition_samp
# # A tibble: 200 × 32
#     rowid   Age Attrition BusinessTravel    DailyRate Department DistanceFromHome
#     <int> <int> <fct>     <fct>                 <int> <fct>                 <int>
#  1  1322    54 No        Non-Travel              142 Human_Res…               26
#  2  1439    50 No        Travel_Frequently      1421 Research_…                2
#  3   476    33 No        Travel_Rarely          1075 Human_Res…                3
#  4   706    38 No        Travel_Frequently       594 Research_…                2
#  5   218    47 No        Non-Travel              543 Sales                     2
#  6   630    32 No        Travel_Rarely           427 Research_…                1
#  7  1416    48 No        Travel_Rarely          1224 Research_…               10
#  8  1016    51 No        Travel_Rarely          1302 Research_…                2
#  9   835    29 Yes       Travel_Rarely           806 Research_…                7
# 10   168    31 Yes       Travel_Rarely           542 Sales                    20
# … with 190 more rows, and 25 more variables: Education <ord>,
#   EducationField <fct>, EnvironmentSatisfaction <ord>, Gender <fct>,
#   HourlyRate <int>, JobInvolvement <ord>, JobLevel <int>, JobRole <fct>,
#   JobSatisfaction <ord>, MaritalStatus <fct>, MonthlyIncome <int>,
#   MonthlyRate <int>, NumCompaniesWorked <int>, OverTime <fct>,
#   PercentSalaryHike <int>, PerformanceRating <ord>,
#   RelationshipSatisfaction <ord>, StockOptionLevel <int>, …

# Systematic sampling
# One sampling method that avoids randomness is called systematic sampling. Here, you pick rows from 
# the population at regular intervals.
# For example, if the population dataset had one thousand rows and you wanted a sample size of five, 
# you'd pick rows 200, 400, 600, 800, and 1000.
# attrition_pop is available; dplyr and tibble are loaded.
# Instructions 
# Set the sample size to 200.
# Get the population size from attrition_pop.
# Calculate the interval between rows to be sampled.
# Set the sample size to 200
sample_size <- 200
# Get the population size from attrition_pop
pop_size <- nrow(attrition_pop)
# Calculate the interval
interval <- pop_size %/% sample_size
interval
# [1] 7

seq_len(length.out = 5)

# Get the row indexes for the sample as a numeric sequence of interval, 2 * interval, up to 
# sample_size * interval.
# Systematically sample attrition_pop, assigning to attrition_sys_samp.
# Add a row ID column to attrition_pop.
# Get the rows of the population corresponding to row_indexes.
# From previous step
sample_size <- 200
pop_size <- nrow(attrition_pop)
interval <- pop_size %/% sample_size
# Get row indexes for the sample
row_indexes <- seq_len(sample_size) * interval
attrition_sys_samp <- attrition_pop %>% 
  # Add a row ID column
  rowid_to_column %>% 
  # Get 200 rows using systematic sampling
  slice(row_indexes)
# AK: the same
attrition_pop %>% 
  # Add a row ID column
  rowid_to_column %>% filter(rowid %in% row_indexes)
# See the result
View(attrition_sys_samp)

# Is systematic sampling OK?
# Systematic sampling has a problem: if the data has been sorted, or there is some sort of pattern or 
# meaning behind the row order, then the resulting sample may not be representative of the whole 
# population. The problem can be solved by shuffling the rows, but then systematic sampling is 
# equivalent to simple random sampling.
# Here you'll look at how to determine whether or not there is a problem.
# attrition_sys_samp is available and has been given a row ID column; dplyr and ggplot2 are loaded.
# Instructions
# Add a row ID column to attrition_pop.
# Using the attrition_pop_id dataset, plot YearsAtCompany versus rowid as a scatter plot, with a smooth 
# trend line.
# Add a row ID column to attrition_pop
attrition_pop_id <- attrition_pop %>% rowid_to_column()
# Using attrition_pop_id, plot YearsAtCompany vs. rowid
ggplot(attrition_pop_id, aes(rowid, YearsAtCompany)) +
  # Make it a scatter plot
  geom_point() +
  # Add a smooth trend line
  geom_smooth()
# http://joxi.ru/Vm6gyx7U37jBk2

# Shuffle the rows of attrition_pop.
# Add a row ID column to attrition_pop.
# Repeat the plot of YearsAtCompany versus rowid with points and a smooth trend line, this time using 
# attrition_shuffled.
# Shuffle the rows of attrition_pop then add row IDs
attrition_shuffled <- attrition_pop %>% slice_sample(prop = 1) %>% rowid_to_column
# Using attrition_shuffled, plot YearsAtCompany vs. rowid
# Add points and a smooth trend line
ggplot(attrition_shuffled, aes(rowid, YearsAtCompany)) + geom_point() + geom_smooth()
# http://joxi.ru/GrqQMXgSz6GQqr

# Question
# Does a systematic sample always produce a sample similar to an simple random sample?
# Possible Answers
# Yes. All sampling (random or non-random) schemes will lead us to similar results.
# Yes. We should always expect a representative sample for both systematic and simple random sampling.
# No. This only holds if a seed has been set for both processes.
# No. This is not true if the data is sorted in some way. +
# Your sample skills are ample! Systematic sampling has problems when the data are sorted or contain a pattern. Shuffling the rows makes it equivalent to simple random sampling.

# 5 Cant get no stratisfaction.mp4
# Which sampling method?
# You've learned about several sampling methods, including simple random sampling and stratified 
# sampling. It's important to know when to use each of them.
# Instructions
# Choose the appropriate sampling method for each situation.
# http://joxi.ru/DrlwaoVfK3yjbA
# Classy classification! Stratified sampling is useful if you care about subgroups. Otherwise, simple random sampling is more appropriate.

# Proportional stratified sampling
# If you are interested in subgroups within the population, then you may need to carefully control the counts of each subgroup within the population. Proportional stratified sampling results in subgroup sizes within the sample that are representative of the subgroup sizes within the population. It is equivalent to performing a simple random sample on each subgroup.
# attrition_pop is available; dplyr is loaded.
# Instructions 
# Get the counts of employees by Education level from attrition_pop, sorted by descending count
# Add a percent column of percentages (100 times the count divided by the total count).
education_counts_pop <- attrition_pop %>% 
  # Count the employees by Education level, sorting by n
  count(Education, sort = T) %>% 
  # Add a percent column
  mutate(percent = (n / sum(n)) * 100)
# See the results
education_counts_pop
# A tibble: 5 × 3
# Education         n percent
#   <ord>         <int>   <dbl>
# 1 Bachelor        572   38.9 
# 2 Master          398   27.1 
# 3 College         282   19.2 
# 4 Below_College   170   11.6 
# 5 Doctor           48    3.27

# Use proportional stratified sampling on attrition_pop to get 40% of each Education group. That is, 
# group by Education and perform a simple random sample of proportion 0.4 on each group.
# Ungroup the stratified sample.
# From previous step
attrition_pop %>% 
  count(Education, sort = TRUE) %>% 
  mutate(percent = 100 * n / sum(n))
# Use proportional stratified sampling to get 40% of each Education group
attrition_strat <- attrition_pop %>% group_by(Education) %>% slice_sample(prop = 0.4) %>% ungroup()
# See the result
attrition_strat

# As you did with attrition_pop, get the counts of employees by Education level from attrition_strat, 
# sorted by descending count, then add a percent column of percentages.
# From previous steps
attrition_pop %>% 
  count(Education, sort = TRUE) %>% 
  mutate(percent = 100 * n / sum(n))
# A tibble: 5 × 3
#   Education         n percent
#   <ord>         <int>   <dbl>
# 1 Bachelor        572   38.9 
# 2 Master          398   27.1 
# 3 College         282   19.2 
# 4 Below_College   170   11.6 
# 5 Doctor           48    3.27
attrition_strat <- attrition_pop %>% 
  group_by(Education) %>% 
  slice_sample(prop = 0.4) %>% 
  ungroup()
# Get the counts and percents from attrition_strat
education_counts_strat <- attrition_strat %>%
  count(Education, sort = T) %>%
  mutate(percent = (n / sum(n)) * 100)

# See the results
education_counts_strat
# A tibble: 5 × 3
#   Education         n percent
#   <ord>         <int>   <dbl>
# 1 Bachelor        228   38.9 
# 2 Master          159   27.1 
# 3 College         112   19.1 
# 4 Below_College    68   11.6 
# 5 Doctor           19    3.24
# Perfectly proportioned! By grouping then sampling, the size of each group in the sample is 
# representative of the size of the sample in the population.

# Equal counts stratified sampling
# If one subgroup is larger than another subgroup in the population, but you don't want to reflect 
# that difference in your analysis, then you can use equal counts stratified sampling to generate 
# samples where each subgroup has the same amount of data. For example, if you are analyzing blood 
# types, O is the most common blood type worldwide, but you may wish to have equal amounts of O, A, B, 
# and AB in your sample.
# attrition_pop is available; dplyr is loaded.
# Instructions
# Use equal counts stratified sampling on attrition_pop to get 30 employees from each Education group. That is, group by Education and perform a simple random sample of size 30 on each group.
# Ungroup the stratified sample.
# Use equal counts stratified sampling to get 30 employees from each Education group
attrition_eq <- attrition_pop %>% group_by(Education) %>% slice_sample(n = 30) %>% ungroup()
# See the results
attrition_eq

# Get the counts of employees by Education level from attrition_eq, sorted by descending count.
# Add a percent column of percentages (100 times the count divided by the total count).
# From previous step
attrition_eq <- attrition_pop %>%
  group_by(Education) %>% 
  slice_sample(n = 30) %>%
  ungroup()
# Get the counts and percents from attrition_eq
education_counts_eq <- attrition_eq %>% 
  count(Education, sort = T) %>% 
  mutate(percent = (n / sum(n)) * 100)
# See the results
education_counts_eq
# A tibble: 5 × 3
#   Education         n percent
#   <ord>         <int>   <dbl>
# 1 Below_College    30      20
# 2 College          30      20
# 3 Bachelor         30      20
# 4 Master           30      20
# 5 Doctor           30      20
# Elegant equal count creation! If you want each subgroup to have equal weight in your analysis, 
# then equal counts stratified sampling is the appropriate technique.

# Weighted sampling
# Stratified sampling provides rules about the probability of picking rows from your dataset at the 
# subgroup level. A generalization of this is weighted sampling, which lets you specify rules about 
# the probability of picking rows at the row level. The probability of picking any given row is 
# proportional to the weight value for that row.
# attrition_pop is available; dplyr and ggplot2 are loaded.
# Instructions
# Using attrition_pop, plot YearsAtCompany as a histogram with a binwidth of 1.
# Using attrition_pop, plot YearsAtCompany as a histogram with binwidth 1
ggplot(attrition_pop, aes(x = YearsAtCompany)) + geom_histogram(binwidth = 1)
# http://joxi.ru/brR3D56SB3Oxym

# Sample 400 employees from attrition_pop weighted by YearsAtCompany.
# Sample 400 employees weighted by YearsAtCompany
attrition_weight <- attrition_pop %>% slice_sample(n = 400, weight_by = YearsAtCompany)
# See the results
attrition_weight

# Using attrition_weight, plot YearsAtCompany as a histogram with binwidth 1.
# From previous step
attrition_weight <- attrition_pop %>% 
  slice_sample(n = 400, weight_by = YearsAtCompany)
# Using attrition_weight, plot YearsAtCompany as a histogram with binwidth 1
ggplot(attrition_weight, aes(x = YearsAtCompany)) + geom_histogram(binwidth = 1)
# http://joxi.ru/82QRVQ5c8R9nyA

# Question
# Which is higher? The weighted sample mean YearsAtCompany or the population mean YearsAtCompany.
# From previous step
attrition_weight <- attrition_pop %>%
  slice_sample(n = 400, weight_by = YearsAtCompany)
# Calculate mean YearsAtCompany using attrition_pop
attrition_pop %>% summarise(mean = mean(YearsAtCompany))
# A tibble: 1 × 1
# mean
#   <dbl>
# 1  7.01
# Calculate mean YearsAtCompany using attrition_weight
attrition_weight %>% summarise(mean = mean(YearsAtCompany))
# A tibble: 1 × 1
# mean
#   <dbl>
# 1  10.8
# Possible Answers
# - Sample mean.
# - Population mean.
# - Both means are identical.
# - It is impossible to calculate the two means
# Marvelous means! The weighted sample mean is around 11, which is higher than the population mean of 
# around 7. The fact that the two numbers are different means that the weighted simple random sample is biased.

# 
# Benefits of clustering
# Cluster sampling is two-stage sampling technique that is closely related to stratified sampling. 
# First you randomly sample which subgroups to include in the sample, then for each subgroup you 
# randomly sample rows within that group.
# In which of the following situations would cluster sampling be preferable to stratified sampling?
# ossible Answers
# - If the interest is on ensuring each rare group will be represented in the sample selected.
#   Incorrect
#   Cluster sampling will not include every subgroup from the population in the sample.
# - If cost is not a limitation and time can be spent carefully sampling from each group in the population.
#   Incorrect
#   If you have the resources to sample every subgroup, then you don't need to reduce the number of subgroups in the sample.
# - If collecting an overall sample requires lots of travel from one group to another to collect samples within each group. +
# - If the focus is on comparing particular subgroups within the population.
#   Incorrect
#   If you care about comparing subgroups, then it doesn't make sense to randomly select which subgroups are contained in the sample.
# Delightful decision making! The main benefit of cluster sampling over stratified sampling is that you can save time or money by not including every subgroup in your sample.

# Cluster sampling
# Now that you know when to use cluster sampling, it's time to put it into action. In this exercise 
# you'll explore the JobRole column of the attrition dataset. You can think of each job role as a 
# subgroup of the whole population of employees.
# attrition_pop is available; dplyr is loaded.
# Instructions
# Get the unique JobRole values from attrition_pop.
# Randomly sample four JobRole values from job_roles_pop.
# Get unique JobRole values
job_roles_pop <- unique(attrition_pop$JobRole)
job_roles_pop
# [1] Research_Scientist        Sales_Representative     
# [3] Laboratory_Technician     Human_Resources          
# [5] Sales_Executive           Manufacturing_Director   
# [7] Healthcare_Representative Research_Director        
# [9] Manager                  
# 9 Levels: Healthcare_Representative Human_Resources ... Sales_Representative
# Randomly sample four JobRole values
job_roles_samp <- sample(x = job_roles_pop, size = 4)
# See the result
job_roles_samp
# [1] Manufacturing_Director Sales_Representative   Research_Director     
# [4] Manager               
# 9 Levels: Healthcare_Representative Human_Resources ... Sales_Representative

# Filter attrition_pop for the sampled job roles. That is, filter for rows where JobRole is in 
# job_roles_samp.
# For each job role in the filtered dataset, take a random sample of ten rows.
# From previous step
job_roles_pop <- unique(attrition_pop$JobRole)
job_roles_samp <- sample(job_roles_pop, size = 4)
# Filter for rows where JobRole is in job_roles_samp
attrition_filtered <- attrition_pop %>% filter(JobRole %in% job_roles_samp)
# Randomly sample 10 employees from each sampled job role
attrition_clus <- attrition_filtered %>% group_by(JobRole) %>% slice_sample(n = 10) %>% ungroup()
# See the result
attrition_clus
unique(attrition_filtered$JobRole)
# [1] Research_Scientist        Sales_Representative     
# [3] Healthcare_Representative Research_Director        
# 9 Levels: Healthcare_Representative Human_Resources ... Sales_Representative
# Classy cluster sampling! The two-stage sampling technique gives you control over sampling both 
# between subgroups and within subgroups.

# 3 kinds of sampling
# Let's compare the performance of point estimates using simple, stratified, and cluster sampling. 
# Before we do that, you'll have to set up the samples.
# In these exercises, we'll use the RelationshipSatisfaction column of the attrition dataset, 
# which categorizes the employee's relationship with the company. It's an ordered factor with four levels: 
# Low, Medium, High, and Very_High,
# Instructions
# Perform simple random sampling on attrition_pop to get one quarter of the population.
# Perform simple random sampling to get 0.25 of the population
attrition_srs <- attrition_pop %>% slice_sample(prop = 1/4)
dim(attrition_pop)
# [1] 1470   31
dim(attrition_srs)
# [1] 367  31

# Perform stratified sampling on attrition_pop to get one quarter of the population of each 
# RelationshipSatisfaction group. Remember to ungroup the result.
# Perform stratified sampling to get 0.25 of each relationship group
attrition_strat <- attrition_pop %>% 
  group_by(RelationshipSatisfaction) %>% 
  slice_sample(prop = 1/4) %>%
  ungroup()
dim(attrition_pop)
# [1] 1470   31
dim(attrition_strat)
# [1] 366  31

# Get unique values of attrition_pop's RelationshipSatisfaction column.
# Randomly sample satisfaction_unique to get two values.
# Perform cluster sampling on the selected satisfaction group. Remember to ungroup the result.
# Get unique values of RelationshipSatisfaction
satisfaction_unique <- unique(attrition_pop$RelationshipSatisfaction)
satisfaction_unique
# [1] Very_High High      Low       Medium   
# Levels: Low < Medium < High < Very_High
# Randomly sample for 2 of the unique satisfaction values
satisfaction_samp <- sample(x = satisfaction_unique, size = 2)
satisfaction_samp
# [1] High   Medium
# Levels: Low < Medium < High < Very_High
# Perform cluster sampling on the selected group getting 0.25 of the population
attrition_clust <- attrition_pop %>% 
  filter(RelationshipSatisfaction %in% satisfaction_samp) %>%
  group_by(RelationshipSatisfaction) %>%
  slice_sample(n = nrow(attrition_pop) * 0.25) %>%
  ungroup()
dim(attrition_pop)
# [1] 1470   31
dim(attrition_clust)
# [1] 579  31
attrition_pop %>% count(RelationshipSatisfaction, sort = T)
# A tibble: 4 × 2
#   RelationshipSatisfaction     n
#   <ord>                    <int>
# 1 High                       459
# 2 Very_High                  432
# 3 Medium                     303
# 4 Low                        276
nrow(attrition_pop) * 0.25
# [1] 367.5
# Terrific triple! Now we have the three samples set up, let's calculate some summary statistics.

# Summary statistics on different kinds of sample
# Now you have three types of sample (simple, stratified, cluster), you can compare point estimates 
# from each sample to the population parameter. That is, you can calculate the same summary statistic 
# on each sample and see how it compares to the summary statistic for the population.
# Here, we'll look at how satisfaction with the company affects whether or not the employee leaves 
# the company. That is, you'll calculate the proportion of employees who left the company (they have 
# an Attrition value of "Yes"), for each value of RelationshipSatisfaction.
# attrition_pop, attrition_srs, attrition_strat, and attrition_clust are available; dplyr is loaded.
# Instructions
# Group by RelationshipSatisfaction level.
# Summarize to calculate a column named mean_attrition as the mean of the cases where Attrition is 
# equal to "Yes".
# Use the whole population dataset 
mean_attrition_pop <- attrition_pop %>% 
  # Group by relationship satisfaction level
  group_by(RelationshipSatisfaction) %>% 
  # Calculate the proportion of employee attrition
  summarise(mean_attrition = mean(Attrition == "Yes"))
# See the result
mean_attrition_pop
# A tibble: 4 × 2
# RelationshipSatisfaction mean_attrition
#   <ord>                             <dbl>
# 1 Low                               0.207
# 2 Medium                            0.149
# 3 High                              0.155
# 4 Very_High                         0.148

# Calculate the proportion of employee attrition for each relationship satisfaction group, this 
# time on the simple random sample, attrition_srs.
# Calculate the same thing for the simple random sample 
mean_attrition_srs <- attrition_srs %>%
  group_by(RelationshipSatisfaction) %>%
  summarise(mean_attrition = mean(Attrition == "Yes"))
# See the result
mean_attrition_srs
# A tibble: 4 × 2
# RelationshipSatisfaction mean_attrition
#   <ord>                             <dbl>
# 1 Low                               0.188
# 2 Medium                            0.139
# 3 High                              0.156
# 4 Very_High                         0.133

# Calculate the proportion of employee attrition for each relationship satisfaction group, this time 
# on the stratified sample, attrition_strat.
# Calculate the same thing for the stratified sample 
mean_attrition_strat <- attrition_strat %>%
  group_by(RelationshipSatisfaction) %>%
  summarise(mean_attrition = mean(Attrition == "Yes"))
# See the result
mean_attrition_strat
# A tibble: 4 × 2
# RelationshipSatisfaction mean_attrition
#  <ord>                             <dbl>
# 1 Low                               0.188
# 2 Medium                            0.107
# 3 High                              0.167
# 4 Very_High                         0.167

# Calculate the proportion of employee attrition for each relationship satisfaction group, this time 
# on the cluster sample, attrition_clust.
# Calculate the same thing for the cluster sample 
mean_attrition_clust <- attrition_clust %>%
  group_by(RelationshipSatisfaction) %>%
  summarise(mean_attrition = mean(Attrition == "Yes"))
# See the result
mean_attrition_clust
# A tibble: 2 × 2
# RelationshipSatisfaction mean_attrition
#   <ord>                             <dbl>
# 1 Low                               0.207
# 2 Very_High                         0.158
# Super summary statistics! The numbers are all fairly similar, with the notable exception that cluster 
# sampling only gives results for the clusters included in the sample.

# 8 An ample sample.mp4
# Calculating relative errors
# The size of the sample you take affects how accurately the point estimates reflect the corresponding 
# population parameter. For example, when you calculate a sample mean, you want it to be close to the 
# population mean. However, if your sample is too small, this might not be the case.
# The most common metric for assessing accuracy is relative error. This is the absolute difference 
# between the population parameter and the point estimate, all divided by the population parameter. 
# It is sometimes expressed as a percentage.
# attrition_pop and mean_attrition_pop are available; dplyr is loaded.
# Instructions 
# Generate a simple random sample from attrition_pop of ten rows.
# Summarize to calculate the mean proportion of employee attrition (Attrition equals "Yes").
# Calculate the relative error between mean_attrition_srs10 and mean_attrition_pop as a percentage.
# Generate a simple random sample of 10 rows 
attrition_srs10 <- attrition_pop %>% slice_sample(n = 10)
# Calculate the proportion of employee attrition in the sample
mean_attrition_srs10 <- attrition_srs10 %>%
  summarise(mean = mean(Attrition == "Yes"))
mean_attrition_srs10
# A tibble: 1 × 1
# mean
#    <dbl>
#  1   0.1
# Calculate the relative error percentage
rel_error_pct10 <- 100 * abs(mean_attrition_pop - mean_attrition_srs10) / mean_attrition_pop
# See the result
rel_error_pct10
# mean_attrition
# 1       37.97468
mean_attrition_pop
# A tibble: 1 × 1
# mean_attrition
#          <dbl>
# 1        0.161

# Calculate the relative error percentage again. This time, use a simple random sample of one hundred rows of attrition_pop.
# Calculate the relative error percentage again with a sample of 100 rows
mean_attrition_srs100 <- attrition_pop %>% 
  slice_sample(n = 100) %>%
  summarise(mean_attrition = mean(Attrition == "Yes"))
rel_error_pct100 <- 100 * abs(mean_attrition_pop - mean_attrition_srs100) / mean_attrition_pop
# See the result
rel_error_pct100
#   mean_attrition
# 1        17.8481
# Samply the best! As you increase the sample size, on average the sample mean gets closer to the population mean and the relative error decreases.

# Relative error vs. sample size
# The plot shows the relative error in the proportion of employee attritions, using simple random sampling, for sample sizes from 2 to 1470 (the size of the population).
# # Clicking "Regenerate plot" will create new samples for each sample size, and calculate the relative 
# errors again.
# Which statement about relative errors and sample sizes is true?
# Instructions
# - For any given sample size, the relative error between the sample mean and the population mean is fixed at a specific value.
# - When the sample is as large as the whole population, the relative error is small, but never zero.
# - If the sample mean is greater than the population mean, the relative error can be less than zero.
# - The relative error can never be greater than 100%.
#   Incorrect
#   No. Regenerate the plot a few times, and look closely at the y-axis labels.
# - For small sample sizes, each additional entry in a sample can result in substantial decreases to the relative error. +
# You're relatively great at this! As you increase sample size, the relative error decreases quickly at first, then more slowly as it drops to zero.
# http://joxi.ru/1A5qbxpc4QvGGA

# 9 Baby back dist-rib-ution.mp4
# Replicating samples
# When you calculate a point estimate such as a sample mean, the value you calculate depends on the rows 
# that were included in the sample. That means that there is some randomness in the answer. In order to 
# quantify the variation caused by this randomness, you can create many samples and calculate the sample 
# mean (or other statistic) for each sample.
# attrition_pop is available; dplyr and ggplot2 are loaded.
# Instructions
# Replicate the provided code so that it runs 500 times. Assign the resulting vector of sample means to 
# mean_attritions.
?replicate
# Replicate this code 500 times
mean_attritions <- replicate(
  n = 500,
  expr =
    attrition_pop %>% 
    slice_sample(n = 20) %>% 
    summarize(mean_attrition = mean(Attrition == "Yes")) %>% 
    pull(mean_attrition)
)
# See the result
head(mean_attritions)
# [1] 0.10 0.15 0.15 0.15 0.25 0.35

# Create a tibble with a column named sample_mean to store mean_attritions.
# Using sample_means, draw a histogram of the sample_mean column with a binwidth of 0.05.
# From previous step
mean_attritions <- replicate(
  n = 500,
  attrition_pop %>% 
    slice_sample(n = 20) %>% 
    summarize(mean_attrition = mean(Attrition == "Yes")) %>% 
    pull(mean_attrition)
)
# Store mean_attritions in a tibble in a column named sample_mean
sample_means <- tibble(sample_mean = mean_attritions)
# Plot a histogram of the `sample_mean` column, binwidth 0.05
attrition_pop_mean <-
  attrition_pop %>% summarise(mean_attrition = mean(Attrition == "Yes"))
ggplot(sample_means, aes(x = sample_mean)) + 
  geom_histogram(binwidth = 0.05) + 
  geom_vline(xintercept = attrition_pop_mean$mean_attrition, color = 'red')
# A tibble: 1 × 1
# mean_attrition
#        <dbl>
# 1      0.161
# Resplendent replicating! By generating the sample statistic many times with different samples, you can quantify the amount of variation in those statistics.

# Replication parameters
# The dashboard shows a histogram of sample mean proportions of employee attrition. There are two parameters: the size of each simple random sample, and the number of replicates. It's important to understand how each of these parameters affects the result. Use the parameter sliders to explore different values and note their effect on the histogram.
# Which statement about the effect of each parameter on the distribution of sample means is true?
# Instructions
# - As the sample size increases, the range of calculated sample means tends to increase.
# - As the number of replicates increases, the range of calculated sample means tends to increase.
# - As the sample size increases, the range of calculated sample means tends to decrease. +
# - As the number of replicates increases, the range of calculated sample means tends to decrease.
# http://joxi.ru/82QRVQ5c8R6XQA
# http://joxi.ru/823NOxpTz5MRam
# Powerful parameter play! As sample size increases, on average each sample mean has a lower relative error compared to the population mean, thus reducing the range of the distribution.

# 10 Be our guess, put our samples to the test.mp4
# Exact sampling distribution
# To quantify how the point estimate (sample statistic) you are interested in varies, you need to know 
# all the possible values it can take, and how often. That is, you need to know its distribution.
# The distribution of a sample statistic is called the sampling distribution. When we can calculate this 
# exactly, rather than using an approximation, it is known as the exact sampling distribution.
# Let's take another look at the sampling distribution of dice rolls. This time, we'll look at five 
# eight-sided dice. (These have the numbers one to eight.)
# shutterstock_231673213_8_sided_die.jpg
# tidyr, dplyr, and ggplot2 are loaded.
# Instructions
# Expand a grid representing 5 8-sided dice. That is, create a tibble with five columns, named die1 
# to die5. The rows should contain all possibilities for throwing five dice, each numbered 1 to 8.
?expand_grid()
# Expand a grid representing 5 8-sided dice
dice <- expand_grid(
  die1 = 1:8,
  die2 = 1:8,
  die3 = 1:8,
  die4 = 1:8,
  die5 = 1:8
)
# See the result
dice
# A tibble: 32,768 × 5
# die1  die2  die3  die4  die5
# <int> <int> <int> <int> <int>
# 1     1     1     1     1     1
# 2     1     1     1     1     2
# 3     1     1     1     1     3
# 4     1     1     1     1     4
# 5     1     1     1     1     5
# 6     1     1     1     1     6
# 7     1     1     1     1     7
# 8     1     1     1     1     8
# 9     1     1     1     2     1
#10     1     1     1     2     2
# … with 32,758 more rows

# Add a column, mean_roll, to dice, that contains the mean of the five rolls.
dice <- expand_grid(
  die1 = 1:8,
  die2 = 1:8,
  die3 = 1:8,
  die4 = 1:8,
  die5 = 1:8
) %>% 
  # Add a column of mean rolls
  mutate(mean_roll = (die1 + die2 + die3 + die4 + die5) / 5)
dice
# A tibble: 32,768 × 6
# die1  die2  die3  die4  die5 mean_roll
#   <int> <int> <int> <int> <int>     <dbl>
# 1     1     1     1     1     1       1  
# 2     1     1     1     1     2       1.2
# 3     1     1     1     1     3       1.4
# 4     1     1     1     1     4       1.6
# 5     1     1     1     1     5       1.8
# 6     1     1     1     1     6       2  
# 7     1     1     1     1     7       2.2
# 8     1     1     1     1     8       2.4
# 9     1     1     1     2     1       1.2
#10     1     1     1     2     2       1.4
# … with 32,758 more rows

# Using the dice dataset, plot mean_roll, converted to a factor, as a bar plot.
# Using dice, draw a bar plot of mean_roll as a factor
ggplot(dice, aes(factor(mean_roll))) + geom_bar()
# http://joxi.ru/LmGYVeDTBnvRMA
# Exactly right! The exact sampling distribution shows all possible variations of the point estimate that you are interested in.
# AK
ggplot(dice, aes(mean_roll)) + geom_histogram(binwidth = 0.1)

# Approximate sampling distribution
# Calculating the exact sampling distribution is only possible in very simple situations. With just five eight-sided dice, the number of possible rolls is 8 ^ 5, which is over thirty thousand. When the dataset is more complicated, for example where a variable has hundreds or thousands or categories, the number of possible outcomes becomes too difficult to compute exactly.
# In this situation, you can calculate an approximate sampling distribution by simulating the exact sampling distribution. That is, you can repeat a procedure over and over again to simulate both the sampling process and the sample statistic calculation process.
# tibble and ggplot2 are loaded.
# Instructions
# Sample one to eight, five times, with replacement. Assign to five_rolls.
# Calculate the mean of five_rolls.
# Sample one to eight, five times, with replacement
five_rolls <- sample(
  x = 1:8, size = 5, replace = T
)
five_rolls
# [1] 5 6 1 3 6
# Calculate the mean of five_rolls
mean(five_rolls)
# [1] 4.2

# Replicate the sampling code 1000 times, assigning to sample_means_1000.
sample_means_1000 <- replicate(
  n = 1000,
  expr = 
    {
      five_rolls <- sample(1:8, size = 5, replace = TRUE)
      mean(five_rolls)
    }
)
# See the result
sample_means_1000

# Create a tibble, and store sample_means_1000 in the a column named sample_mean.
# From previous step
sample_means_1000 <- replicate(
  n = 1000,
  expr = {
    five_rolls <- sample(1:8, size = 5, replace = TRUE)
    mean(five_rolls)
  }
)
# Wrap sample_means_1000 in the sample_mean column of a tibble
sample_means <- tibble(sample_mean = sample_means_1000)
# See the result
sample_means
# A tibble: 1,000 × 1
# sample_mean
# <dbl>
# 1         3.6
# 2         2.4
# 3         4.6
# 4         3.8
# 5         4.2
# 6         6  
# 7         4.8
# 8         3  
# 9         5.8
#10         3.8
# … with 990 more rows

# Using the sample_means dataset, plot sample_mean, converted to a factor, as a bar plot.
# From previous steps
sample_means_1000 <- replicate(
  n = 1000,
  expr = {
    five_rolls <- sample(1:8, size = 5, replace = TRUE)
    mean(five_rolls)
  }
)
sample_means <- tibble(
  sample_mean = sample_means_1000
)
# Using sample_means, draw a bar plot of sample_mean as a factor
ggplot(sample_means, aes(factor(sample_mean))) + geom_bar()
# http://joxi.ru/p27l897TL59jGA
# Applaudable approximating! Once your dataset gets sufficiently big, exact sampling distributions 
# cannot be calculated, so an approximate sampling distribution has to be used. Notice that the 
# histogram is close to but not exactly the same as the histogram from the previous exercise.

# Exact vs. approximate
# You've seen two types of sampling distribution now (exact and approximate). You need to be clear 
# about when each should be computed.
# Should we always be able to compute the exact sampling distribution directly?
# Possible Answers
# - No, the computational time and resources needed to look at the population of values could be too much for our problem. +
# - No, the exact sampling distribution is always unknown even for calculating the sample mean of a small number of die tosses like 2 or 3.
# - Yes, the population will always be known ahead of time so one extra calculation is no problem.
# - Yes, the exact sampling distribution can be generated using the replicate() function so it should be used in all circumstances.
# Delightful sampling distribution distinguishing! The exact sampling distribution can only be calculated 
# if you know what the population is, and if the problems is small and simple enough to compute. 
# Otherwise, the approximate sampling distribution must be used.

# 11 Err on the side of Gaussian.mp4
# Population & sampling distribution means
# One of the useful features of sampling distributions is that you can quantify them. In particular, 
# you can calculate summary statistics on them. Here, we'll look at the relationship between the mean 
# of the sampling distribution and the population parameter that the sampling is supposed to estimate.
# Three sampling distributions are provided. In each case, the employee attrition dataset was sampled 
# using simple random sampling, then the mean attrition was calculated. This was done 1000 times to get 
# a sampling distribution of mean attritions. One sampling distribution used a sample size of 5 for each 
# replicate, one used 50, and one used 500.
# attrition_pop, sampling_distribution_5, sampling_distribution_50, and sampling_distribution_500 are 
# available; dplyr is loaded.
# Instructions 
# Using sampling_distribution_5, calculate the mean across all the replicates of the mean_attritions 
# (a mean of sample means). Store this in a column called mean_mean_attrition.
# Do the same calculation using sampling_distribution_50 and sampling_distribution_500.
dim(sampling_distribution_5)
# [1] 1000    2
head(sampling_distribution_5)
# A tibble: 6 × 2
# replicate mean_attrition
# <int>          <dbl>
# 1         1            0.2
# 2         2            0  
# 3         3            0.4
# 4         4            0.4
# 5         5            0.2
# 6         6            0.2
# Calculate the mean across replicates of the mean attritions in sampling_distribution_5
mean_of_means_5 <- sampling_distribution_5 %>%
  summarise(mean_mean_attrition = mean(mean_attrition))
# Do the same for sampling_distribution_50
mean_of_means_50 <- sampling_distribution_50 %>%
  summarise(mean_mean_attrition = mean(mean_attrition))
# ... and for sampling_distribution_500
mean_of_means_500 <- sampling_distribution_500 %>%
  summarise(mean_mean_attrition = mean(mean_attrition))
# For comparison: the mean attrition in the population
mean_attrition_pop <- attrition_pop %>% summarise(mean_attrition = mean(Attrition == "Yes"))

# Question
# How does sample size affect the mean of the sample means?
# - As the sample size increases, the mean of the sampling distribution decreases until it reaches the population mean.
# - As the sample size increases, the mean of the sampling distribution increases until it reaches the population mean.
# - Regardless of sample size, the mean of the sampling distribution is a close approximation to the population mean. +
# - Regardless of sample size, the mean of the sampling distribution is biased and cannot approximate the population mean.
# Mind-blowing mean manipulation! Even for small sample sizes, the mean of the sampling distribution is a good approximation of the population mean.

# Population and sampling distribution variation
# You just calculated the mean of the sampling distribution and saw how it is an estimate of the 
# corresponding population parameter. Similarly, as a result of the central limit theorem, the 
# standard deviation of the sampling distribution has an interesting relationship with the population 
# parameter's standard deviation and the sample size.
# attrition_pop, sampling_distribution_5, sampling_distribution_50, and sampling_distribution_500 are 
# available; dplyr is loaded.
# Instructions
# Using sampling_distribution_5, calculate the standard deviation across all the replicates of 
# the mean_attritions (a standard deviation of sample means). Store this in a column called 
# sd_mean_attrition.
# Do the same calculation using sampling_distribution_50 and sampling_distribution_500.
dim(sampling_distribution_5)
# [1] 1000    2
head(sampling_distribution_5)
# A tibble: 6 × 2
# replicate mean_attrition
#       <int>          <dbl>
# 1         1            0.2
# 2         2            0  
# 3         3            0.4
# 4         4            0.4
# 5         5            0.2
# 6         6            0.2
# Calculate the standard deviation across replicates of the mean attritions in sampling_distribution_5
sd_of_means_5 <- sampling_distribution_5 %>%
  summarise(sd_mean_attrition = sd(mean_attrition))
# Do the same for sampling_distribution_50
sd_of_means_50 <- sampling_distribution_50 %>%
  summarise(sd_mean_attrition = sd(mean_attrition))
# ... and for sampling_distribution_500
sd_of_means_500 <- sampling_distribution_500 %>%
  summarise(sd_mean_attrition = sd(mean_attrition))
# See the results
sd_of_means_5
# A tibble: 1 × 1
# sd_mean_attrition
#               <dbl>
#  1           0.164
sd_of_means_50
# A tibble: 1 × 1
# sd_mean_attrition
#               <dbl>
# 1            0.0506
sd_of_means_500
# A tibble: 1 × 1
# sd_mean_attrition
#               <dbl>
# 1            0.0134

# Question
# How are the standard deviations of the sampling distributions related to the population standard 
# deviation and the sample size?
# For comparison: population standard deviation
sd_attrition_pop <- attrition_pop %>% 
  summarize(sd_attrition = sd(Attrition == "Yes")) %>% 
  pull(sd_attrition)
sd_attrition_pop
# [1] 0.367863
# The sample sizes of each sampling distribution
sample_sizes <- c(5, 50, 500)
sd_attrition_pop / sqrt(sample_sizes)
# [1] 0.16451335 0.05202369 0.01645133

#
# Principles of bootstrapping
# Bootstrapping is, in some sense, the opposite of sampling from a population. Sampling treats your 
# dataset as the population, and you generate a random subset. Bootstrapping treats your dataset as 
# a sample and uses it to build up a theoretical population.
# http://joxi.ru/8An4XoVTNnkYjm
# Incorrect
# Bootstrap distributions are generated using sampling with replacement; sampling distributions are created using sampling without replacement.
# http://joxi.ru/8An4XoVTNnkYjm

# With or without replacement
# So far in the course, you've seen sampling with and without replacement. It's important to know 
# when to use each method.
# Instructions
# Read the different scenarios and determine whether sampling without replacement from a population 
# or sampling with replacement from a sample is the most appropriate strategy.
# http://joxi.ru/Dr8GKyEiKy6KaA
# Incorrect
# The census includes everyone, so it makes sense to think of it as the population. Your analysis will 
# be on a smaller subset.
# Radical replacement reasoning! The key to deciding whether to sample without or with replacement is 
# whether or not your dataset is best thought of as being the whole population or not.

# Generating a bootstrap distribution
# The process for generating a bootstrap distribution is remarkably similar to the process for 
# generating a sampling distribution; only the first step is different.
# To make a sampling distribution, you start with the population and sample without replacement. 
# To make a bootstrap distribution, you start with a sample and sample that with replacement. After that, the steps are the same: calculate the summary statistic that you are interested in on that sample/resample, then replicate the process many times. In each case, you can visualize the distribution with a histogram.
# Here, spotify_sample is a subset of the spotify_population dataset. To make it easier to see how 
# resampling works, a row ID column has been added, and only the artist name, song name, and danceability columns have been included.
# spotify_sample is available; dplyr and ggplot2 are loaded.
# Instructions
# Generate a single bootstrap resample from spotify_sample.
dim(spotify_population)
# [1] 41656    20
dim(spotify_sample)
# [1] 1000    3
head(spotify_sample)
# A tibble: 6 × 3
# artists            name                  danceability
# <chr>              <chr>                        <dbl>
# 1 ['Lana Del Rey']   National Anthem              0.532
# 2 ['Cameo']          Single Life                  0.755
# 3 ['Sufjan Stevens'] Jacksonville                 0.597
# 4 ['Roar']           I Can't Handle Change        0.247
# 5 ['Eminem']         Stronger Than I Was          0.781
# 6 ['Pretty Lights']  The Time Has Come            0.497
# Generate 1 bootstrap resample
spotify_1_resample <- spotify_sample %>% slice_sample(prop=1, replace = T)
# See the result
spotify_1_resample
# A tibble: 1,000 × 3
#   artists                               name                       danceability
#   <chr>                                 <chr>                             <dbl>
# 1 ['Grupo Laberinto']                   "Mi Adolescencia A Los Cu…        0.692
# 2 ['Norah Jones']                       "Cold Cold Heart"                 0.603
# 3 ['XXXTENTACION', 'Ski Mask The Slump… "Off the Wall!"                   0.834
# 4 ['Grizzly Bear']                      "Knife"                           0.226
# 5 ['Vertical Worship']                  "Yes I Will"                      0.43 
# 6 ['Bruce Adler']                       "Arabian Nights - From \"…        0.496
# 7 ['Big Sean']                          "Dance (A$$)"                     0.769
# 8 ['Giacomo Puccini', 'Renée Fleming',… "Gianni Schicchi: \"O mio…        0.217
# 9 ['United Pursuit']                    "Simple Gospel (Live)"            0.453
#10 ['JAY-Z']                             "Tom Ford"                        0.863
# … with 990 more rows

# Summarize to calculate the mean danceability of spotify_1_resample as mean_danceability, then pull out this value to get a numeric vector of length 1.
# From previous step
spotify_1_resample <- spotify_sample %>% 
  slice_sample(prop = 1, replace = TRUE)
# Calculate mean danceability of resample
mean_danceability_1 <- spotify_1_resample %>% summarise(mean_danceability = mean(danceability)) %>% pull()
# See the result
mean_danceability_1
# [1] 0.5868232
# AK: before pull()
# A tibble: 1 × 1  
# mean_danceability
#                   <dbl>
#     1             0.584

# Replicate the expression provided 1000 times.
# Replicate this 1000 times
mean_danceability_1000 <- replicate(
  n = 1000,
  expr = {
    spotify_1_resample <- spotify_sample %>% 
      slice_sample(prop = 1, replace = TRUE)
    spotify_1_resample %>% 
      summarize(mean_danceability = mean(danceability)) %>% 
      pull(mean_danceability)
  }
)
# See the result
mean_danceability_1000

# Store mean_danceability_1000 in a tibble, in a column named resample_mean.
# Using bootstrap_distn, draw a histogram of the resample_means with binwidth 0.002.
# Store the resamples in a tibble
bootstrap_distn <- tibble(
  resample_mean = mean_danceability_1000
)
# Draw a histogram of the resample means with binwidth 0.002
ggplot(bootstrap_distn, aes(x = resample_mean)) + geom_histogram(binwidth = 0.002)
# http://joxi.ru/brR3D56SB3o3Jm
# Beautiful bootstrapping! From the smaller sample of Spotify songs, we can estimate the mean 
# danceability statistic in the population. Since we have a distribution of statistics, we can even 
# quantify how accurate our estimate is.

# 13 A breath of fresh error.mp4
 #Bootstrap statistics and population statistics
# Bootstrap distribution statistics can be used to estimate population parameters. But can you always 
# rely on them to give an accurate estimate of an unknown population parameter?
# Should the mean and the standard deviation of the bootstrap distribution both be used to estimate 
# the corresponding values of the population?
# Answer the question
# - No, the mean of the bootstrap distribution will always be near the sample mean, which may not necessarily be very close to the population mean. +
# - No, the standard deviation of the bootstrap distribution (if divided by the square root of the sample size) will tend to be near the sample standard deviation, which may not necessarily be very close to the population standard deviation.
#   Incorrect
#   The standard deviation of the bootstrap distribution MULTIPLIED by the square root of the sample size tends to be a good estimate of the population standard deviation.
# - Yes, both estimates should match up closely with the population values in all scenarios.
#   Incorrect
#   One of these estimates often does not match up.
# - Yes, the variability of the sample, the population, the bootstrap distribution, and the sampling distribution will all be similar regardless of sample size selected.
#   Incorrect
#   The sample may not be representative of the population, particularly if the sample size is small.
# Super standard error reasoning! If the sample is not closely representative of the population, then the mean of the bootstrap distribution will not be representative of the population mean. This is less of a problem for standard errors.

# Sampling distribution vs. bootstrap distribution
# The sampling distribution and bootstrap distribution are closely linked. In situations where you 
# can repeatedly sample from a population (these occasions are rare) and as you learn about both, 
# it's helpful to generate both the sampling distribution and the bootstrap distribution, one after 
# the other, to see how they are related.
# Here, the statistic you are interested in is the mean popularity score of the songs.
# spotify_population (the whole dataset) and spotify_sample (500 rows only representing an original 
# sample) are available; dplyr is loaded.
# Instructions
# Generate a sampling distribution of 2000 replicates.
# Sample 500 rows of the population without replacement.
# Calculate the statistic of interest (the mean popularity) in the column mean_popularity.
# Pull out the statistic so it is a single numeric value (not a tibble).
glimpse(spotify_population)

# Generate a sampling distribution
mean_popularity_2000_samp <- replicate(
  # Use 2000 replicates
  rep = 2000,
  expr = {
    # Start with the population
    spotify_population %>% 
      # Sample 500 rows without replacement
      slice_sample(n = 500, replace = F) %>% 
      # Calculate the mean popularity as mean_popularity
      summarize(mean_popularity = mean(pop)) %>% 
      # Pull out the mean popularity
      pull()
  }
)

# See the result
mean_popularity_2000_samp
# [1] 53.466 54.584 53.998 54.906 55.784 55.782 54.622 54.754 54.984 53.958
# [11] 54.864 53.770 54.816 54.806 54.408 54.820 54.706 54.620 55.448 55.152
# [21] 54.192 55.056 54.012 54.924 54.910 55.066 54.812 54.122 55.052 54.614
# ...

# Generate a bootstrap distribution of 2000 replicates.
# Sample 500 rows of the sample with replacement.
# Calculate the statistic of interest (the mean popularity) in the column mean_popularity.
# Pull out the statistic so it is a single numeric value (not a tibble).
dim(spotify_sample)
# [1] 500  21
# Generate a bootstrap distribution
mean_popularity_2000_boot <- replicate(
  # Use 2000 replicates
  n = 2000,
  expr = {
    # Start with the sample
    spotify_sample %>% 
      # Sample same number of rows with replacement
      slice_sample(n = 500, replace = T) %>% 
      # Calculate the mean popularity
      summarise(mean_popularity = mean(popularity)) %>% 
      # Pull out the mean popularity
      pull()
  }
)
# See the result
mean_popularity_2000_boot
# [1] 55.458 55.584 55.714 55.072 55.440 55.102 54.594 55.122 55.274 55.628
# [11] 55.534 55.034 55.208 55.814 55.622 55.276 56.012 55.406 56.004 54.524
# [21] 54.990 54.486 55.628 54.868 54.644 54.784 55.138 55.116 54.168 55.464
# Dazzling distributions! The sampling distribution and bootstrap distibution are closely related, 
# and so is the code to generate them.

# Compare sampling and bootstrap means
# To make calculation easier, the distributions from the previous exercise have been included in tibbles. mean_popularity_2000_samp is in the sample_mean column of sampling_distribution, and mean_popularity_2000_boot is in the resample_mean column of bootstrap_distribution.
# spotify_population, spotify_sample, sampling_distribution, and bootstrap_distribution are available; dplyr is loaded.
# Instructions
# Calculate the mean popularity with summarize() in 4 ways.
# Population: from spotify_population, take the mean of popularity.
# Sample: from spotify_sample, take the mean of popularity.
# Sampling distribution: from sampling_distribution, take the mean of sample_mean.
# Bootstrap distribution: from bootstrap_distribution, take the mean 
sampling_distribution
# A tibble: 2,000 × 1
# sample_mean
#          <dbl>
# 1        53.5
# 2        54.6
# 3        54.0
# 4        54.9
# 5        55.8
# 6        55.8
# 7        54.6
# 8        54.8
# 9        55.0
#10        54.0
# … with 1,990 more rows
bootstrap_distribution
# A tibble: 2,000 × 1
# resample_mean
#           <dbl>
# 1          54.5
# 2          55.5
# 3          55.3
# 4          54.6
# 5          56.1
# 6          55.7
# 7          55.3
# 8          55.6
# 9          54.6
#10          55.4
# … with 1,990 more rows
dim(spotify_population)
# [1] 41656    20
dim(spotify_sample)
# [1] 500  21
# Calculate the true population mean popularity
pop_mean <- spotify_population %>% summarise(mean_popularity = mean(popularity))
# Calculate the original sample mean popularity
samp_mean <- spotify_sample %>% summarise(mean_popularity = mean(popularity))
# Calculate the sampling dist'n estimate of mean popularity
samp_distn_mean <- sampling_distribution %>% summarise(mean_popularity = mean(sample_mean))
# Calculate the bootstrap dist'n estimate of mean popularity
boot_distn_mean <- bootstrap_distribution %>% summarise(mean_popularity = mean(resample_mean))
# See the results
c(pop = pop_mean, samp = samp_mean, sam_distn = samp_distn_mean, boot_distn = boot_distn_mean)
# $pop.mean_popularity
# [1] 54.83714
# $samp.mean_popularity
# [1] 55.07
# $sam_distn.mean_popularity
# [1] 54.86677
# $boot_distn.mean_popularity
# [1] 55.07377

# Question
# Based on the four means you just calculated, which statement is true?
# - The sampling distribution mean is closest to the original sample mean; the bootstrap distribution mean is the best estimate of the true population mean.
#   Incorrect 
#   No. Look closely at the four means. Which pairs of numbers are almost the same?
# - The sampling distribution mean is the best estimate of the true population mean; the bootstrap distribution mean is closest to the original sample mean.
# - The sampling distribution mean and the bootstrap distribution mean are equally good estimates of the original sample mean.
#   Incorrect Submission
#   No. Look closely at the sampling distribution mean and the sample mean. Are these numbers almost the same?
# - The sampling distribution mean and the bootstrap distribution mean are equally good estimates of the true population mean.
#   Incorrect Submission
#   No. Look closely at the bootstrap distribution mean and the population mean. Are these numbers almost the same?
# Magnificent means! The sampling distribution mean can be used to estimate the population mean, but that is not the case with the boostrap distribution.

# Compare sampling and bootstrap standard deviations
# In the same way that you looked at how the sampling distribution and bootstrap distribution could be used to estimate the population mean, you'll now take a look at how they can be used to estimate variation, or more specifically, the standard deviation, in the population.
# spotify_population, spotify_sample, sampling_distribution, and bootstrap_distribution are available; dplyr is loaded.
# Instructions
# Calculate the standard deviation of popularity with summarize() in 4 ways.
# Population: from spotify_population, take the standard deviation of popularity.
# Original sample: from spotify_sample, take the standard deviation of popularity.
# Sampling distribution: from sampling_distribution, take the standard deviation of sample_mean and multiply by the square root of the sample size (500).
# Bootstrap distribution: from bootstrap_distribution, take the standard deviation of resample_mean and multiply by the square root of the sample size.
# Calculate the true popluation std dev popularity
pop_sd <- spotify_population %>% summarise(sd_popularity = sd(popularity))
# Calculate the true sample std dev popularity
samp_sd <- spotify_sample %>% summarise(sd_popularity = sd(popularity))
sampling_distribution
# A tibble: 2,000 × 1
#   sample_mean
#         <dbl>
# 1        53.5
# 2        54.6
# 3        54.0
# 4        54.9
# 5        55.8
# 6        55.8
# 7        54.6
# 8        54.8
# 9        55.0
#10        54.0
# … with 1,990 more rows
# Calculate the sampling dist'n estimate of std dev popularity
samp_distn_sd <- sampling_distribution %>% summarise(sd_popularity = sd(sample_mean)) * sqrt(500)
bootstrap_distribution
# A tibble: 2,000 × 1
#    resample_mean
#            <dbl>
# 1          54.5
# 2          55.5
# 3          55.3
# 4          54.6
# 5          56.1
# 6          55.7
# 7          55.3
# 8          55.6
# 9          54.6
#10          55.4
# … with 1,990 more rows
# Calculate the bootstrap dist'n estimate of std dev popularity
boot_distn_sd <- bootstrap_distribution %>% summarise(sd_popularity = sd(resample_mean)) * sqrt(500)
# See the results
c(pop = pop_sd, samp = samp_sd, sam_distn = samp_distn_sd, boot_distn = boot_distn_sd)
$pop.sd_popularity
# [1] 10.8802
$samp.sd_popularity
# [1] 11.04269
$sam_distn.sd_popularity
# [1] 10.92885
$boot_distn.sd_popularity
# [1] 11.12226

# Question
# Based on the four results you just calculated, which statement is true?
# - The calculation from the sampling distribution is closest to the original sample standard deviation; the calculation from the bootstrap distribution is the best estimate of the true population standard deviation.
#   Incorrect Submission
#   No. Check the differences between the results again.
# - The sample standard deviation is the best estimate of the true population standard deviation; the calculation from the bootstrap distribution is closest to the original sample standard deviation.
# - The calculations from both the sampling distribution and from the bootstrap distribution are equally close to the original sample standard deviation.
# - While not the closest of the values given, the calculation from the bootstrap distribution provides a good estimate of the true population standard deviation. +
# Super standard devations! This is an important property of the bootstrap distribution. When you don't know have all the values from the true population, you can use bootstrapping to get a good estimate of the population standard deviation.
# AK: but why in this case we can't use sample sd? it's closer to the pop sd and we don't neet calculate bootstrap distr???

# 
# Confidence interval interpretation
# When reporting results, it is common to provide a confidence interval alongside an estimate.
# What does that confidence interval provide?
# Answer the question
# - A range of all values that a variable/column from a sample may take on.
#   Incorrect
#   No. The confidence interval typically will not include some values that the sample could take; only those close to the statistic of interest.
# - All numbers between 0 and 1.
#   Incorrect
#   No. If the statistic you are calculating a confidence interval for is not close to zero or one, then the confidence interval may not contain values between zero and one.
# - A range of plausible values for a variable measured in our population (such as popularity in spotify_population).
#   Incorrect
#   No. The range of a variable in the population would be a population parameter, similar to a population mean, not a confidence interval.
# - A range of plausible values for an unknown quantity.
# I'm confident you are correct! Confidence intervals account for uncertainty in our estimate of a 
# population parameter by providing a range of possible values. We are confident that the true value 
# lies somewhere in the interval specified by that range.

# Calculating confidence intervals
# We can use the cumulative distribution function and its inverse to calculate confidence intervals in R. 
# You'll do so with the Spotify data now.
# Instructions
# Generate a 95% confidence interval using the quantile method.
# Summarize to get the 0.025 quantile as lower, and the 0.975 quantile as upper.
head(bootstrap_distribution)
# A tibble: 6 × 1
# resample_mean
#           <dbl>
# 1          54.5
# 2          55.5
# 3          55.3
# 4          54.6
# 5          56.1
# 6          55.7
# Generate a 95% confidence interval using the quantile method
conf_int_quantile <- bootstrap_distribution %>% 
  summarise(
    lower = quantile(resample_mean, 0.025),
    upper = quantile(resample_mean, 0.975)
  )
# See the result
conf_int_quantile
# A tibble: 1 × 2
#   lower upper
#   <dbl> <dbl>
# 1  54.1  56.1

# Generate a 95% confidence interval using the standard error method.
# Calculate point_estimate as the mean of resample_mean, and standard_error as the standard deviation of resample_mean.# Calculate lower as the 0.025 quantile of an inv. CDF from a normal distribution with mean point_estimate and standard deviation standard_error.
# Calculate upper as the 0.975 quantile of that same inv. CDF.
?qnorm()
# Generate a 95% confidence interval using the std error method
conf_int_std_error <- bootstrap_distribution %>% 
  summarise(
    point_estimate = mean(resample_mean),
    standard_eror = sd(resample_mean),
    lower = qnorm(p = 0.025, mean = point_estimate, sd = standard_eror, lower.tail = TRUE),
    upper = qnorm(p = 0.975, mean = point_estimate, sd = standard_eror, lower.tail = TRUE)
  )
# See the result
conf_int_std_error
# A tibble: 1 × 4
# point_estimate standard_eror lower upper
#            <dbl>         <dbl> <dbl> <dbl>
# 1           55.1         0.497  54.1  56.0
# Standard excellence! The standard error method for calculating the confidence interval assumes that 
# the bootstrap distribution is normal. This assumption should hold if the sample size and number of 
# replicates are sufficiently large.
























