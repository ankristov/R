

# 2 Measures of center.mp4
# Mean and median
# In this chapter, you'll be working with the 2018 Food Carbon Footprint Index from nu3. The 
# food_consumption dataset contains information about the kilograms of food consumed per person per 
# year in each country in each food category (consumption) as well as information about the carbon 
# footprint of that food category (co2_emissions) measured in kilograms of carbon dioxide, or CO2, 
# per person per year in each country.
# In this exercise, you'll compute measures of center to compare food consumption in the US and Belgium 
# using your dplyr skills.
# dplyr is loaded for you and food_consumption is available.
# Create two data frames: one that holds the rows of food_consumption for "Belgium" and the another that 
# holds rows for "USA". Call these belgium_consumption and usa_consumption.
# Calculate the mean and median of kilograms of food consumed per person per year for both countries.
head(food_consumption)
# A tibble: 6 × 4
# country   food_category consumption co2_emission
# <chr>     <fct>               <dbl>        <dbl>
# 1 Argentina pork                10.5         37.2 
# 2 Argentina poultry             38.7         41.5 
# 3 Argentina beef                55.5       1712   
# 4 Argentina lamb_goat            1.56        54.6 
# 5 Argentina fish                 4.36         6.96
# 6 Argentina eggs                11.4         10.5 
# Filter for Belgium
belgium_consumption <- food_consumption %>% filter(country == "Belgium")
# Filter for USA
usa_consumption <- food_consumption %>% filter(country == "USA")
# Calculate mean and median consumption in Belgium
mean(belgium_consumption$consumption)
# [1] 42.13273
median(belgium_consumption$consumption)
# [1] 12.59
# Calculate mean and median consumption in USA
mean(usa_consumption$consumption)
# [1] 44.65
median(usa_consumption$consumption)
# [1] 14.58
# Filter food_consumption for rows with data about Belgium and the USA.
# Group the filtered data by country.
# Calculate the mean and median of the kilograms of food consumed per person per year in each country. 
# Call these columns mean_consumption and median_consumption.
food_consumption %>%
  # Filter for Belgium and USA
  filter(country %in% c("Belgium", "USA")) %>%
  # Group by country
  group_by(country) %>%
  # Get mean_consumption and median_consumption
  summarise(mean_consumption = mean(consumption),
            median_consumption = median(consumption))
# A tibble: 2 × 3
# country mean_consumption median_consumption
# <chr>              <dbl>              <dbl>
# 1 Belgium             42.1               12.6
# 2 USA                 44.6               14.6
# Marvelous mean and median calculation! When you want to compare summary statistics between groups, 
# it's much easier to do a group_by() and one summarize() instead of filtering and calling the same 
# functions multiple times.

# Mean vs. median
# In the video, you learned that the mean is the sum of all the data points divided by the total number of 
# data points, and the median is the middle value of the dataset where 50% of the data is less than the 
# median, and 50% of the data is greater than the median. In this exercise, you'll compare these two 
# measures of center.
# dplyr and ggplot2 are loaded and food_consumption is available.
# Instructions
# Filter food_consumption to get the rows where food_category is "rice".
# Create a histogram using ggplot2 of co2_emission for rice.
head(food_consumption)
# A tibble: 6 × 4
# country   food_category consumption co2_emission
# <chr>     <fct>               <dbl>        <dbl>
# 1 Argentina pork                10.5         37.2 
# 2 Argentina poultry             38.7         41.5 
# 3 Argentina beef                55.5       1712   
# 4 Argentina lamb_goat            1.56        54.6 
# 5 Argentina fish                 4.36         6.96
# 6 Argentina eggs                11.4         10.5 
food_consumption %>%
  # Filter for rice food category
  filter(food_category == "rice") %>%
  # Create histogram of co2_emission
  ggplot(aes(co2_emission)) +
  geom_histogram()
# http://joxi.ru/zANK0YVC1K308r
# Question
# Take a look at the histogram of the CO2 emissions for rice you just plotted. Which of the following 
# terms best describes the shape of the data?
# Possible Answers
# - No skew
# - Left-skewed
# - Right-skewed +

# Filter food_consumption to get the rows where food_category is "rice".
# Summarize the data to get the mean and median of co2_emission, calling them mean_co2 and median_co2.
food_consumption %>%
  # Filter for rice food category
  filter(food_category == "rice") %>% 
  # Get mean_co2 and median_co2
  summarise(mean_co2 = mean(co2_emission),
            median_co2 = median(co2_emission))
# A tibble: 1 × 2
#   mean_co2 median_co2
#      <dbl>      <dbl>
# 1     37.6       15.2

# Question
# Given the skew of this data, what measure of central tendency best summarizes the kilograms of CO2 
# emissions per person per year for rice?
# Possible Answers
# - Mean
# - Median +
# - Both
# Great work! The mean is substantially higher than the median since it's being pulled up by the high 
# values over 100 kg/person/year.

#
# Quartiles, quantiles, and quintiles
# Quantiles are a great way of summarizing numerical data since they can be used to measure center and 
# spread, as well as to get a sense of where a data point stands in relation to the rest of the dataset. 
# For example, you might want to give a discount to the 10% most active users on a website.
# In this exercise, you'll calculate quartiles, quintiles, and deciles, which split up a dataset into 4, 
# 5, and 10 pieces, respectively.
# The dplyr package is loaded and food_consumption is available.
# Instructions
# Calculate the quartiles of the co2_emission column of food_consumption.
# # Calculate the quartiles of co2_emission
quantile(food_consumption$co2_emission)
#     0%       25%       50%       75%      100% 
# 0.0000    5.2100   16.5300   62.5975 1712.0000 
# Calculate the quintiles of co2_emission
quantile(food_consumption$co2_emission, prob = seq(from = 0, to = 1, by = 0.2))
#      0%      20%      40%      60%      80%     100% 
#   0.000    3.540   11.026   25.590   99.978 1712.000 
# Calculate the deciles of co2_emission
quantile(food_consumption$co2_emission, prob = seq(from = 0, to = 1, length.out = 11))
#    0%      10%      20%      30%      40%      50%      60%      70%      80%      90%     100% 
# 0.000    0.668    3.540    7.040   11.026   16.530   25.590   44.271   99.978  203.629 1712.000 
# Those are some high-quality quantiles! While calculating more quantiles gives you a more detailed look 
# at the data, it also produces more numbers, making the summary more difficult to quickly understand.

# Variance and standard deviation
# Variance and standard deviation are two of the most common ways to measure the spread of a variable, 
# and you'll practice calculating these in this exercise. Spread is important since it can help inform 
# expectations. For example, if a salesperson sells a mean of 20 products a day, but has a standard 
# deviation of 10 products, there will probably be days where they sell 40 products, but also days where 
# they only sell one or two. Information like this is important, especially when making predictions.
# Both dplyr and ggplot2 are loaded, and food_consumption is available.
# Instructions
# Calculate the variance and standard deviation of co2_emission for each food_category by grouping by 
# and summarizing variance as var_co2 and standard deviation as sd_co2.
# Create a histogram of co2_emission for each food_category using facet_wrap().
food_consumption %>% 
  group_by(food_category) %>% 
  summarise(var_co2 = var(co2_emission), sd_co2 = sd(co2_emission))
# A tibble: 11 × 3
# food_category   var_co2  sd_co2
# <fct>             <dbl>   <dbl>
# 1 beef          88748.    298.   
# 2 eggs             21.4     4.62 
# 3 fish            922.     30.4  
# 4 lamb_goat     16476.    128.   
# 5 dairy         17672.    133.   
# 6 nuts             35.6     5.97 
# 7 pork           3095.     55.6  
# 8 poultry         245.     15.7  
# 9 rice           2281.     47.8  
# 10 soybeans          0.880   0.938
# 11 wheat            71.0     8.43 
# Plot food_consumption with co2_emission on x-axis
ggplot(food_consumption) +
  # Create a histogram
  geom_histogram(aes(co2_emission)) +
  # Create a separate sub-graph for each food_category
  facet_wrap(~ food_category)
# http://joxi.ru/Q2KWnY7COWBgN2
# Superb spread measurement! Beef has the biggest amount of variation in its CO2 emissions, while eggs, nuts, and soybeans have relatively small amounts of variation.

# Finding outliers using IQR
# Outliers can have big effects on statistics like mean, as well as statistics that rely on the mean, 
# such as variance and standard deviation. Interquartile range, or IQR, is another way of measuring 
# spread that's less influenced by outliers. IQR is also often used to find outliers. If a value is 
# less than Q1 − 1.5 × IQR or greater than Q3 + 1.5 × IQR, it's considered an outlier. In fact, this is 
# how the lengths of the whiskers in a ggplot2 box plot are calculated.
# Diagram of a box plot showing median, quartiles, and outliers
# In this exercise, you'll calculate IQR and use it to find some outliers. Both dplyr and ggplot2 are 
# loaded and food_consumption is available.
# Instructions
# Calculate the total co2_emission per country by grouping by country and taking the sum of co2_emission. 
# Call the sum total_emission and store the resulting data frame as emissions_by_country
# Calculate total co2_emission per country: emissions_by_country
emissions_by_country <- food_consumption %>%
  group_by(country) %>%
  summarise(total_emission = sum(co2_emission))
emissions_by_country
# A tibble: 130 × 2
# country    total_emission
#   <chr>               <dbl>
# 1 Albania             1778.
# 2 Algeria              708.
# 3 Angola               413.
# 4 Argentina           2172.
# 5 Armenia             1110.
# 6 Australia           1939.
# 7 Austria             1211.
# 8 Bahamas             1193.
# 9 Bangladesh           374.
#10 Barbados             889.
# … with 120 more rows

# Calculate total co2_emission per country: emissions_by_country
emissions_by_country <- food_consumption %>%
  group_by(country) %>%
  summarize(total_emission = sum(co2_emission))

# Compute the first and third quartiles and IQR of total_emission
q1 <- quantile(emissions_by_country$total_emission, prob = 0.25)
q3 <- quantile(emissions_by_country$total_emission, prob = 0.75)
iqr <- q3 - q1
c(q1, q3, iqr)
#       25%       75%       75% 
#  446.6600 1111.1525  664.4925 

# Calculate the lower and upper cutoffs for outliers of total_emission, and store these as lower and upper.
# Calculate the lower and upper cutoffs for outliers
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr
c(q1, q3, iqr, lower, upper)
#      25%       75%       75%       25%       75% 
# 446.6600 1111.1525  664.4925 -550.0788 2107.8912 

# Use filter() to get countries with a total_emission greater than the upper cutoff or a total_emission less than the lower cutoff.
# Filter emissions_by_country to find outliers
emissions_by_country %>%
  filter(total_emission < lower | total_emission > upper)
# A tibble: 1 × 2
#   country   total_emission
#   <chr>              <dbl>
# 1 Argentina          2172.
#  Outstanding outlier detection! It looks like Argentina has a substantially higher amount of CO2 emissions per person than other countries in the world.

# 4 What are the chances?.mp4
# http://joxi.ru/J2b0eVlTqo9PEr

# Calculating probabilities
# You're in charge of the sales team, and it's time for performance reviews, starting with Amir. As 
# part of the review, you want to randomly select a few of the deals that he's worked on over the past 
# year so that you can look at them more deeply. Before you start selecting deals, you'll first figure 
# out what the chances are of selecting certain deals.
# Recall that the probability of an event can be calculated by
# P(event) = # ways event can happen / total # of possible outcomes
# dplyr is loaded and amir_deals is available.
# Instructions
# Count the number of deals Amir worked on for each product type.
head(amir_deals)
#     product  client status  amount num_users
# 1 Product F Current    Won 7389.52        19
# 2 Product C     New    Won 4493.01        43
# 3 Product B     New    Won 5738.09        87
# 4 Product I Current    Won 2591.24        83
# 5 Product E Current    Won 6622.97        17
# 6 Product B     New    Won 5496.27         2
unique(amir_deals$product)
# [1] Product F Product C Product B Product I Product E Product N Product D
# [8] Product A Product G Product J Product H
# 14 Levels: Product A Product B Product C Product D Product E ... Product P
# Count the deals for each product
amir_deals %>% count(product)
#      product  n
# 1  Product A 23
# 2  Product B 62
# 3  Product C 15
# 4  Product D 40
# 5  Product E  5
# 6  Product F 11
# 7  Product G  2
# 8  Product H  8
# 9  Product I  7
# 10 Product J  2
# 11 Product N  3

# Create a new column called prob by dividing n by the total number of deals Amir worked on.
# Calculate probability of picking a deal with each product
amir_deals %>%
  count(product) %>%
  mutate(prob = `n` / sum(`n`))
# product  n       prob
# 1  Product A 23 0.12921348
# 2  Product B 62 0.34831461
# 3  Product C 15 0.08426966
# 4  Product D 40 0.22471910
# 5  Product E  5 0.02808989
# 6  Product F 11 0.06179775
# 7  Product G  2 0.01123596
# 8  Product H  8 0.04494382
# 9  Product I  7 0.03932584
# 10 Product J  2 0.01123596
# 11 Product N  3 0.01685393

# Question
# If you randomly select one of Amir's deals, what's the probability that the deal will involve Product C?
# Possible Answers
# - 15%
# - 80.43%
# - 8.43% +
# - 22.5%
# Perfect probability calculations! Now that you know what the chances are, it's time to start sampling.

# Sampling deals
# In the previous exercise, you counted the deals Amir worked on. Now it's time to randomly pick five 
# deals so that you can reach out to each customer and ask if they were satisfied with the service they 
# received. You'll try doing this both with and without replacement.
# Additionally, you want to make sure this is done randomly and that it can be reproduced in case you 
# get asked how you chose the deals, so you'll need to set the random seed before sampling from the deals.
# dplyr is loaded and amir_deals is available.
# Instructions
# Set the random seed to 31.
# Take a sample of 5 deals without replacement.
# Set random seed to 31
set.seed(31)
# Sample 5 deals without replacement
amir_deals %>% sample_n(size = 5, replace = F)
# product  client status  amount num_users
# 1 Product D Current   Lost 3086.88        55
# 2 Product C Current   Lost 3727.66        19
# 3 Product D Current   Lost 4274.80         9
# 4 Product B Current    Won 4965.08         9
# 5 Product A Current    Won 5827.35        50

# Take a sample of 5 deals with replacement.
# Set random seed to 31
set.seed(31)
# Sample 5 deals with replacement
amir_deals %>% sample_n(size = 5, replace = T)
# product  client status  amount num_users
# 1 Product D Current   Lost 3086.88        55
# 2 Product C Current   Lost 3727.66        19
# 3 Product D Current   Lost 4274.80         9
# 4 Product B Current    Won 4965.08         9
# 5 Product A Current    Won 5827.35        50
# Spectactular sampling! It's important to consider how you'll take a sample since there's no 
# one-size-fits-all way to sample, and this can have an effect on your results.

# Create a histogram of group_size
# Creating a probability distribution
# A new restaurant opened a few months ago, and the restaurant's management wants to optimize its 
# seating space based on the size of the groups that come most often. On one night, there are 10 groups 
# of people waiting to be seated at the restaurant, but instead of being called in the order they arrived, 
# they will be called randomly. In this exercise, you'll investigate the probability of groups of 
# different sizes getting picked first. Data on each of the ten groups is contained in the 
# restaurant_groups data frame.
# Remember that expected value can be calculated by multiplying each possible outcome with its corresponding probability and taking the sum. The restaurant_groups data is available and dplyr and ggplot2 are loaded.
# Instructions
restaurant_groups
# group_id group_size
# 1         A          2
# 2         B          4
# 3         C          6
# 4         D          2
# 5         E          2
# 6         F          2
# 7         G          3
# 8         H          2
# 9         I          4
# 10        J          2
# Create a histogram of the group_size column of restaurant_groups, setting the number of bins to 5.
ggplot(restaurant_groups, aes(group_size)) +
  geom_histogram(bins = 5)
# http://joxi.ru/4AkV3oeijR3aw2

# Count the number of each group_size in restaurant_groups, then add a column called probability that 
# contains the probability of randomly selecting a group of each size. Store this in a new data frame 
# called size_distribution.
# Create probability distribution
size_distribution <- restaurant_groups %>%
  # Count number of each group size
  count(group_size) %>%
  # Calculate probability
  mutate(probability = `n` / sum(`n`))
size_distribution
#   group_size n probability
# 1          2 6         0.6
# 2          3 1         0.1
# 3          4 2         0.2
# 4          6 1         0.1

# Calculate the expected value of the size_distribution, which represents the expected group size.
# Calculate expected group size
expected_val <- sum(size_distribution$group_size *
                      size_distribution$probability)
expected_val
# [1] 2.9

# Calculate the probability of randomly picking a group of 4 or more people by filtering and summarizing.
# Calculate probability of picking group of 4 or more
size_distribution %>%
  # Filter for groups of 4 or larger
  filter(group_size >= 4) %>%
  # Calculate prob_4_or_more by taking sum of probabilities
  summarise(prob_4_or_more = sum(probability))
prob_4_or_more
# 1            0.3
# Dexterous distribution utilization! You'll continue to build upon these skills since many statistical tests and methods use probability distributions as their foundation.
# Impressive identification! Since the histogram depicts a sample and not the actual probability distribution, each outcome won't happen the exact same number of times due to randomness, but they're similar in number.

# Expected value vs. sample mean
# The app to the right will take a sample from a discrete uniform distribution, which includes the 
# numbers 1 through 9, and calculate the sample's mean. You can adjust the size of the sample using the 
# slider. Note that the expected value of this distribution is 5.
# A sample is taken, and you win twenty dollars if the sample's mean is less than 4. There's a catch: 
# you get to pick the sample's size.
# Which sample size is most likely to win you the twenty dollars?
# - 10 +
# - 100
# - 1000
# - 5000
# - 10000
# Nice work! Since the sample mean will likely be closer to 5 (the expected value) with larger sample sizes, you have a better chance of getting a sample mean further away from 5 with a smaller sample.

# 6 Continuous distributions.mp4
# http://joxi.ru/823NOxpTz5WXPm

# Data back-ups
# The sales software used at your company is set to automatically back itself up, but no one knows 
# exactly what time the back-ups happen. It is known, however, that back-ups happen exactly every 
# 30 minutes. Amir comes back from sales meetings at random times to update the data on the client he 
# just met with. He wants to know how long he'll have to wait for his newly-entered data to get backed 
# up. Use your new knowledge of continuous uniform distributions to model this situation and answer 
# Amir's questions.
# Instructions
# To model how long Amir will wait for a back-up using a continuous uniform distribution, save his 
# lowest possible wait time as min and his longest possible wait time as max. Remember that back-ups 
# happen every 30 minutes.
# Min and max wait times for back-up that happens every 30 min
min <- 0
max <- 30
# Calculate the probability that Amir has to wait less than 5 minutes, and store in a variable called prob_less_than_5.
# Calculate probability of waiting less than 5 mins
prob_less_than_5 <- punif(q = 5, min = min, max = max, lower.tail = T)
prob_less_than_5
# [1] 0.1666667
# Calculate the probability that Amir has to wait more than 5 minutes, and store in a variable called prob_greater_than_5.
# Calculate probability of waiting more than 5 mins
prob_greater_than_5 <- punif(q = 5, min = min, max = max, lower.tail = F)
prob_greater_than_5
# [1] 0.8333333
# Calculate the probability that Amir has to wait between 10 and 20 minutes, and store in a variable called prob_between_10_and_20.
# Calculate probability of waiting 10-20 mins
prob_between_10_and_20 <- punif(q = 20, min = min, max = max, lower.tail = T) - punif(q = 10, min = min, max = max, lower.tail = T)
prob_between_10_and_20
# [1] 0.3333333
# Wonderful wait-time calculations! There's a 33% chance that Amir will wait 10-20 minutes. In the next exercise, you'll make sure this calculation holds up by simulating some wait times.

# Simulating wait times
# To give Amir a better idea of how long he'll have to wait, you'll simulate Amir waiting 1000 times and create a 
# histogram to show him what he should expect. Recall from the last exercise that his minimum wait time is 0 
# minutes and his maximum wait time is 30 minutes.
# A data frame called wait_times is available and dplyr and ggplot2 are loaded.
# Instructions
# Set the random seed to 334.
dim(wait_times)
# [1] 1000    1
head(wait_times)
# A tibble: 6 × 1
# simulation_nb
#           <int>
# 1             1
# 2             2
# 3             3
# 4             4
# 5             5
# 6             6
dim(wait_times)
# [1] 1000    1
# Set random seed to 334
set.seed(334)

# Generate 1000 wait times from the continuous uniform distribution that models Amir's wait time. Add 
# this as a new column called time in the wait_times data frame.
# Generate 1000 wait times between 0 and 30 mins, save in time column
wait_times %>%
  mutate(time = sample(x = seq(from = 0, to = 30, by = 1), size = 1000, replace = T))
wait_times %>%
  mutate(time = runif(1000, min = 0, max = 30))
# Create a histogram of the simulated wait times with 30 bins.
wait_times %>%
  mutate(time = runif(1000, min = 0, max = 30)) %>%
  # Create a histogram of simulated times
  ggplot(aes(x = time)) +
  geom_histogram(bins = 30)
# http://joxi.ru/a2X1VZQTQVbGjA
# Superb simulating! Unless Amir figures out exactly what time each backup happens, he won't be able to 
# time his data entry so it gets backed up sooner, but it looks like he'll wait about 15 minutes on average.

# 7 The binomial distribution.mp4
# Simulating sales deals
# Assume that Amir usually works on 3 deals per week, and overall, he wins 30% of deals he works on. 
# Each deal has a binary outcome: it's either lost, or won, so you can model his sales deals with a 
# binomial distribution. In this exercise, you'll help Amir simulate a year's worth of his deals so he 
# can better understand his performance.
# Instructions
# Set the random seed to 10 and simulate a single deal.
# Set random seed to 10
set.seed(10)
# Simulate a single deal
rbinom(n = 11, size = 1, prob = 0.3)
# [1] 0

# Simulate a typical week of Amir's deals, or one week of 3 deals.
# Simulate 1 week of 3 deals
rbinom(n = 1, size = 3, prob = 0.3)
# [1] 1

# Simulate a year's worth of Amir's deals, or 52 weeks of 3 deals each, and store in deals.
# Calculate the mean number of deals he won per week.
# Simulate 52 weeks of 3 deals
deals <- rbinom(n = 52, size = 3, prob = 0.3)
deals
# [1] 1 0 1 1 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 2 2 1 1 1 1 1 2 0 1 1 1 0 0 2 1 1 2 2
# [39] 1 1 0 0 0 1 0 0 0 1 0 2 1 2
# Calculate mean deals won per week
mean(deals)
# [1] 0.8076923
# Brilliant binomial simulation! In this simulated year, Amir won 0.8 deals on average each week.

# Calculating binomial probabilities
# Just as in the last exercise, assume that Amir wins 30% of deals. He wants to get an idea of how likely 
# he is to close a certain number of deals each week. In this exercise, you'll calculate what the 
# chances are of him closing different numbers of deals using the binomial distribution.
# Instructions
# What's the probability that Amir closes all 3 deals in a week?
# Probability of closing 3 out of 3 deals
dbinom(x = 3, size = 3, prob = 0.3)
# [1] 0.027
# What's the probability that Amir closes 1 or fewer deals in a week?
# Probability of closing <= 1 deal out of 3 deals
pbinom(q = 1, size = 3, prob = 0.3, lower.tail = T)
# [1] 0.784
# Probability of closing > 1 deal out of 3 deals
pbinom(q = 1, size = 3, prob = 0.3, lower.tail = F)
# [1] 0.216
# Powerful probability calculations! Amir has about a 22% chance of closing more than one deal in a week.

# How many sales will be won?
# Now Amir wants to know how many deals he can expect to close each week if his win rate changes. 
# Luckily, you can use your binomial distribution knowledge to help him calculate the expected value 
# in different situations. Recall from the video that the expected value of a binomial distribution 
# can be calculated by n × p.
# Instructions
# Calculate the expected number of sales out of the 3 he works on that Amir will win each week if he 
# maintains his 30% win rate.
# Calculate the expected number of sales out of the 3 he works on that he'll win if his win rate drops to 25%.
# Calculate the expected number of sales out of the 3 he works on that he'll win if his win rate rises to 35%.
# Expected number won with 30% win rate
won_30pct <- 3 * 0.3
won_30pct
# [1] 0.9
# Expected number won with 25% win rate
won_25pct <- 3 * 0.25
won_25pct
# [1] 0.75
# Expected number won with 35% win rate
won_35pct <- 3 * 0.35
won_35pct
# [1] 1.05
# Excellent expectation experimentation! If Amir's win rate goes up by 5%, he can expect to close more than 1 deal on average each week.

# 8 The normal distribution.mp4
# Distribution of Amir's sales
# Since each deal Amir worked on (both won and lost) was different, each was worth a different amount of money. These values are stored in the amount column of amir_deals As part of Amir's performance review, you want to be able to estimate the probability of him selling different amounts, but before you can do this, you'll need to determine what kind of distribution the amount variable follows.
# Both dplyr and ggplot2 are loaded and amir_deals is available.
# Instructions
# Create a histogram with 10 bins to visualize the distribution of the amount.
head(amir_deals)
# product  client status  amount num_users
# 1 Product F Current    Won 7389.52        19
# 2 Product C     New    Won 4493.01        43
# 3 Product B     New    Won 5738.09        87
# 4 Product I Current    Won 2591.24        83
# 5 Product E Current    Won 6622.97        17
# 6 Product B     New    Won 5496.27         2
# Histogram of amount with 10 bins
ggplot(data = amir_deals, aes(x = amount)) + geom_histogram(bins = 10)
# http://joxi.ru/82QRVQ5c8gyjZA

# Probabilities from the normal distribution
# Since each deal Amir worked on (both won and lost) was different, each was worth a different amount of money. These values are stored in the amount column of amir_deals and follow a normal distribution with a mean of 5000 dollars and a standard deviation of 2000 dollars. As part of his performance metrics, you want to calculate the probability of Amir closing a deal worth various amounts.
# What's the probability of Amir closing a deal worth less than $7500?
mean(amir_deals$amount)
# [1] 4812
sd(amir_deals$amount)
# [ 1] 2058.173
pnorm(q = 7500, mean = 5000, sd = 2000)
# [1] 0.8943502
# What's the probability of Amir closing a deal worth more than $1000?
# Probability of deal > 1000
pnorm(q = 1000, mean = 5000, sd = 2000, lower.tail = F)
# [1] 0.9772499
# What's the probability of Amir closing a deal worth between $3000 and $7000?
# Probability of deal between 3000 and 7000
pnorm(7000, 5000, 2000, lower.tail = T) - pnorm(3000, 5000, 2000, lower.tail = T)
# [1] 0.6826895
# What amount will 75% of Amir's sales be more than?
# Calculate amount that 75% of deals will be more than
qnorm(p = 0.75, mean = 5000, sd = 2000, lower.tail = F)
# [1] 3651.02
# Nifty normal distribution usage! You know that you can count on Amir 75% of the time to make a sale 
# worth at least $3651.02, and this information will be useful in making company-wide sales projections.

# Simulating sales under new market conditions
# The company's financial analyst is predicting that next quarter, the worth of each sale will 
# increase by 20% and the volatility, or standard deviation, of each sale's worth will increase by 
# 30%. To see what Amir's sales might look like next quarter under these new market conditions, 
# you'll simulate new sales amounts using the normal distribution and store these in the new_sales 
# data frame, which has already been created for you.
# In addition, dplyr and ggplot2 are loaded.
# Instructions
# Currently, Amir's average sale amount is $5000. Calculate what his new average amount will be if 
# it increases by 20% and store this in new_mean.
# Amir's current standard deviation is $2000. Calculate what his new standard deviation will be 
# if it increases by 30% and store this in new_sd.
# Add a new column called amount to the data frame new_sales, which contains 36 simulated amounts 
# from a normal distribution with a mean of new_mean and a standard deviation of new_sd.
# Plot the distribution of the new_sales amounts using a histogram with 10 bins.
new_sales
# sale_num    amount
# 1         1  7845.898
# 2         2  5118.462
# ......
# 35       35  7861.834
# 36       36 12993.613
# Calculate new average amount
new_mean <- 5000 * 1.2
# Calculate new standard deviation
new_sd <- 2000 * 1.3
# Simulate 36 sales
new_sales <- new_sales %>% 
  mutate(amount = rnorm(n = 36, mean = new_mean, sd = new_sd))
# Create histogram with 10 bins
ggplot(new_sales, aes(amount)) + geom_histogram(bins = 10)
# http://joxi.ru/V2VxnLKcBeKO5r
# Successful simulating! Although the average sale amount went up, the variation also increased, so 
# it's not straightforward to decide whether these sales are better than his current ones. In the 
# next exercise, you'll explore the effects of higher variation.

# 9 The central limit theorem.mp4
# Which market is better?
# The key metric that the company uses to evaluate salespeople is the percent of sales they make over $1000 since the time put into each sale is usually worth a bit more than that, so the higher this metric, the better the salesperson is performing.
# Recall that Amir's current sales amounts have a mean of $5000 and a standard deviation of $2000, and Amir's predicted amounts in next quarter's market have a mean of $6000 and a standard deviation of $2600.
# Based only on the metric of percent of sales over $1000, does Amir perform better in the current market or the predicted market?
# Possible Answers
# Amir performs much better in the current market.
# Amir performs much better in next quarter's predicted market.
# Amir performs about equally in both markets. +
#  In the current market, Amir makes sales over $1000 about 97.7% of the time, and about 97.3% of the time in the predicted market, so there's not much of a difference. However, his average sale amount is higher in the predicted market, so the company may want to consider other metrics as well.
# Visualizing sampling distributions
# On the right, try creating sampling distributions of different summary statistics from samples of different distributions. Which distribution does the central limit theorem not apply to?
# Discrete uniform distribution
# Continuous uniform distribution
# Binomial distribution
# All of the above
# None of the above +
# http://joxi.ru/ZrJ6VYyTQg8DMr
# Victorious visualizing! Regardless of the shape of the distribution you're taking sample means from, the central limit theorem will apply if the sampling distribution contains enough sample means.

# The CLT in action
# The central limit theorem states that a sampling distribution of a sample statistic approaches the normal distribution as you take more samples, no matter the original distribution being sampled from.
# In this exercise, you'll focus on the sample mean and see the central limit theorem in action while examining the num_users column of amir_deals more closely, which contains the number of people who intend to use the product Amir is selling.
# Both dplyr and ggplot2 are loaded and amir_deals is available.
# Instructions
# Create a histogram of the num_users column of amir_deals. Use 10 bins.
# head(amir_deals)
# product  client status  amount num_users
# 1 Product F Current    Won 7389.52        19
# 2 Product C     New    Won 4493.01        43
# 3 Product B     New    Won 5738.09        87
# 4 Product I Current    Won 2591.24        83
# 5 Product E Current    Won 6622.97        17
# 6 Product B     New    Won 5496.27         2
# Create a histogram of num_users
ggplot(amir_deals, aes(num_users)) + geom_histogram(bins = 10)
# http://joxi.ru/YmE4VaDTGWYjDr
# Set the seed to 104.
# Take a sample of size 20 with replacement from the num_users column of amir_deals, and take the mean.
# Set seed to 104
set.seed(104)
dim(amir_deals)
# [1] 178   5
# Sample 20 num_users with replacement from amir_deals
sample(x = amir_deals$num_users, size = 20, replace = T) %>%
  # Take mean
  mean()
# [1] 30.35
# Repeat this 100 times and store as sample_means. This will take 100 different samples and calculate the mean of each.
# Repeat the above 100 times
sample_means <- replicate(n = 100, sample(amir_deals$num_users, size = 20, replace = TRUE) %>% mean())
# A data frame called samples has been created for you with a column mean, which contains the values from sample_means. Create a histogram of the mean column with 10 bins.
# Create data frame for plotting
samples <- data.frame(mean = sample_means)
# Histogram of sample means
ggplot(samples, aes(x = mean)) + geom_histogram(bins = 10)
# A data frame called samples has been created for you with a column mean, which contains the values from sample_means. Create a histogram of the mean column with 10 bins.
# http://joxi.ru/Y2LdjY0txgzbO2
# Fabulous job! You've just seen the central limit thorem at work. Even though the distribution of num_users is not normal, the distribution of its sample mean resembles the normal distribution.

# The mean of means
# You want to know what the average number of users (num_users) is per deal, but you want to know this number for the entire company so that you can see if Amir's deals have more or fewer users than the company's average deal. The problem is that over the past year, the company has worked on more than ten thousand deals, so it's not realistic to compile all the data. Instead, you'll estimate the mean by taking several random samples of deals, since this is much easier than collecting data from everyone in the company.
# The user data for all the company's deals is available in all_deals.
# Instructions
# Set the random seed to 321.
# Take 30 samples of size 20 from all_deals$num_users and take the mean of each sample. Store the sample means in sample_means.
# Take the mean of sample_means.
# Take the mean of the num_users column of amir_deals.
# # Set seed to 321
set.seed(321)
head(all_deals)
# product num_users
# 1    3544        19
# 2    5073        43
# 3    6149        87
# 4    7863        83
# 5      14        17
# 6   10247         2
dim(all_deals)
# [1] 10548     2
all_deals %>% group_by(product) %>% count()
# A tibble: 6 × 2
# Groups:   product [6]
# product     n
# <int> <int>
# 1      14  1758
# 2    3544  1758
# 3    5073  1758
# 4    6149  1758
# 5    7863  1758
# 6   10247  1758
head(amir_deals)
# product  client status  amount num_users
# 1 Product F Current    Won 7389.52        19
# 2 Product C     New    Won 4493.01        43
# 3 Product B     New    Won 5738.09        87
# 4 Product I Current    Won 2591.24        83
# 5 Product E Current    Won 6622.97        17
# 6 Product B     New    Won 5496.27         2
# Take 30 samples of 20 values of num_users, take mean of each sample
sample_means <- replicate(n = 30, sample(x = all_deals$num_users, size = 20, replace = T) %>% mean())
# Calculate mean of sample_means
mean(sample_means)
# [1] 37.025
# Calculate mean of num_users in amir_deals
mean(amir_deals$num_users)
# [1] 37.65169
mean(all_deals$num_users) # why we need to bootstrap? mean of population is available
# [1] 37.23919
# Magnificent mean calculation! Amir's average number of users is very close to the overall average, so it looks like he's meeting expectations. Make sure to note this in his performance review!

# 10 The Poisson distribution.mp4
# Tracking lead responses
# Your company uses sales software to keep track of new sales leads. It organizes them into a queue so that anyone can follow up on one when they have a bit of free time. Since the number of lead responses is a countable outcome over a period of time, this scenario corresponds to a Poisson distribution. On average, Amir responds to 4 leads each day. In this exercise, you'll calculate probabilities of Amir responding to different numbers of leads.
# Instructions
# What's the probability that Amir responds to 5 leads in a day, given that he responds to an average of 4?
# # Probability of 5 responses
dpois(x = 5, lambda = 4)
# [1] 0.1562935
# Amir's coworker responds to an average of 5.5 leads per day. What is the probability that she answers 5 leads in a day?
# Probability of 5 responses from coworker
dpois(x = 5, lambda = 5.5)
# [1] 0.1714007
# What's the probability that Amir responds to 2 or fewer leads in a day?
# Probability of 2 or fewer responses
ppois(q = 2, lambda = 4, lower.tail = T)
# [1] 0.2381033
dpois(x = 0, lambda = 4) + dpois(x = 1, lambda = 4) + dpois(x = 2, lambda = 4)
# [1] 0.2381033
# What's the probability that Amir responds to more than 10 leads in a day?
# Probability of > 10 responses
ppois(q = 10, lambda = 4, lower.tail = F)
# [1] 0.002839766
# Perfect Poisson probabilities! Note that if you provide dpois() or ppois() with a non-integer, it returns 0 and throws a warning since the Poisson distribution only applies to integers.

# 11 More probability distributions.mp4
# Modeling time between leads
# To further evaluate Amir's performance, you want to know how much time it takes him to respond to a lead after he opens it. On average, it takes 2.5 hours for him to respond. In this exercise, you'll calculate probabilities of different amounts of time passing between Amir receiving a lead and sending a response.
# Instructions
# What's the probability it takes Amir less than an hour to respond to a lead?
# Probability response takes < 1 hour
# Hint
# Remember that the rate is equal to 1 divided by the average time.
pexp(q = 1, rate = 1/2.5, lower.tail = T)
# [1] 0.32968
# What's the probability it takes Amir more than 4 hours to respond to a lead?
# Probability response takes > 4 hours
pexp(q = 4, rate = 1/2.5, lower.tail = F)
# [1] 0.2018965
# What's the probability it takes Amir 3-4 hours to respond to a lead?
# Probability response takes 3-4 hours
pexp(q = 4, rate = 1/2.5, lower.tail = T) - pexp(q = 3, rate = 1/2.5, lower.tail = T)
# [1] 0.09929769

# Correlation.mp4
# Relationships between variables
# In this chapter, you'll be working with a dataset world_happiness containing results from the 2019 World Happiness Report. The report scores various countries based on how happy people in that country are. It also ranks each country on various societal aspects such as social support, freedom, corruption, and others. The dataset also includes the GDP per capita and life expectancy for each country.
# In this exercise, you'll examine the relationship between a country's life expectancy (life_exp) and happiness score (happiness_score) both visually and quantitatively. Both dplyr and ggplot2 are loaded and world_happiness is available.
# Instructions
# Create a scatterplot of happiness_score vs. life_exp using ggplot2.
head(world_happiness)
# country social_support freedom corruption generosity gdp_per_cap life_exp
# 1     Finland              2       5          4         47       42400     81.8
# 2     Denmark              4       6          3         22       48300     81.0
# 3      Norway              3       3          8         11       66300     82.6
# 4     Iceland              1       7         45          3       47900     83.0
# 5 Netherlands             15      19         12          7       50500     81.8
# 6 Switzerland             13      11          7         16       59000     84.3
# happiness_score
# 1             155
# 2             154
# 3             153
# 4             152
# 5             151
# 6             150
# Create a scatterplot of happiness_score vs. life_exp
ggplot(world_happiness, aes(x = life_exp, y = happiness_score)) + geom_point()
# Add a linear trendline to the scatterplot, setting se to FALSE.
# Add a linear trendline to scatterplot
ggplot(world_happiness, aes(life_exp, happiness_score)) +
  geom_point() +
  geom_smooth(se = F, method = "lm")
# `geom_smooth()` using formula 'y ~ x'
# http://joxi.ru/VrwKo8lCozQK1r
# Correlation between life_exp and happiness_score
cor(world_happiness$life_exp, world_happiness$happiness_score)
# [1] 0.7802249

# 13 What can't correlation measure.mp4
# What can't correlation measure?
# While the correlation coefficient is a convenient way to quantify the strength of a relationship between two variables, it's far from perfect. In this exercise, you'll explore one of the caveats of the correlation coefficient by examining the relationship between a country's GDP per capita (gdp_per_cap) and happiness score.
# Both dplyr and ggplot2 are loaded and world_happiness is available.
# Instructions
# Create a scatterplot showing the relationship between gdp_per_cap (on the x-axis) and life_exp (on the y-axis).
# Scatterplot of gdp_per_cap and life_exp
ggplot(world_happiness, aes(gdp_per_cap, life_exp)) +
  geom_point()
# http://joxi.ru/ZrJ6VYyTQgOMpr
# Correlation between gdp_per_cap and life_exp
cor(world_happiness$gdp_per_cap, world_happiness$life_exp)
# [1] 0.7019548
# Correct! The correlation coefficient can't account for any relationships that aren't linear, regardless of strength.

# Transforming variables
# When variables have skewed distributions, they often require a transformation in order to form a linear relationship with another variable so that correlation can be computed. In this exercise, you'll perform a transformation yourself.
# Both dplyr and ggplot2 are loaded and world_happiness is available.
# Instructions
# Create a scatterplot of happiness_score versus gdp_per_cap.
# Calculate the correlation between happiness_score and gdp_per_cap.
# Scatterplot of happiness_score vs. gdp_per_cap
ggplot(world_happiness, aes(gdp_per_cap, happiness_score)) +
  geom_point()
# Calculate correlation
cor(world_happiness$gdp_per_cap, world_happiness$happiness_score)
# [1] 0.7279733
# Add a new column to world_happiness called log_gdp_per_cap that contains the log of gdp_per_cap.
# Create a scatterplot of happiness_score versus log_gdp_per_cap.
# Calculate the correlation between happiness_score and log_gdp_per_cap.
# Create log_gdp_per_cap column
world_happiness <- world_happiness %>%
  mutate(log_gdp_per_cap = log(gdp_per_cap))
head(world_happiness)
# country social_support freedom corruption generosity gdp_per_cap life_exp
# 1     Finland              2       5          4         47       42400     81.8
# 2     Denmark              4       6          3         22       48300     81.0
# 3      Norway              3       3          8         11       66300     82.6
# 4     Iceland              1       7         45          3       47900     83.0
# 5 Netherlands             15      19         12          7       50500     81.8
# 6 Switzerland             13      11          7         16       59000     84.3
# happiness_score log_gdp_per_cap
# 1             155        10.65490
# 2             154        10.78519
# 3             153        11.10195
# 4             152        10.77687
# 5             151        10.82973
# 6             150        10.98529
# Scatterplot of happiness_score vs. log_gdp_per_cap
ggplot(world_happiness, aes(log_gdp_per_cap, happiness_score)) +
  geom_point()
# Calculate correlation
cor(world_happiness$log_gdp_per_cap, world_happiness$happiness_score)
# [1] 0.8043146
# http://joxi.ru/BA0yd0vu1zavVr
# Terrific transforming! The relationship between GDP per capita and happiness became more linear by applying a log transformation. Log transformations are great to use on variables with a skewed distribution, such as GDP.

# Does sugar improve happiness?
# A new column has been added to world_happiness called grams_sugar_per_day, which contains the average amount of sugar eaten per person per day in each country. In this exercise, you'll examine the effect of a country's average sugar consumption on its happiness score.
# Both dplyr and ggplot2 are loaded and world_happiness is available.
# Instructions 
# Create a scatterplot showing the relationship between grams_sugar_per_day (on the x-axis) and happiness_score (on the y-axis).
# Calculate the correlation between grams_sugar_per_day and happiness_score.
head(world_happiness)
# country social_support freedom corruption generosity gdp_per_cap life_exp
# 1     Finland              2       5          4         47       42400     81.8
# 2     Denmark              4       6          3         22       48300     81.0
# 3      Norway              3       3          8         11       66300     82.6
# 4     Iceland              1       7         45          3       47900     83.0
# 5 Netherlands             15      19         12          7       50500     81.8
# 6 Switzerland             13      11          7         16       59000     84.3
# happiness_score grams_sugar_per_day
# 1             155                86.8
# 2             154               152.0
# 3             153               120.0
# 4             152               132.0
# 5             151               122.0
# 6             150               166.0
# Scatterplot of grams_sugar_per_day and happiness_score
ggplot(world_happiness, aes(x = grams_sugar_per_day, happiness_score)) + geom_point()
# http://joxi.ru/LmGYVeDTBLkpKA
# Correlation between grams_sugar_per_day and happiness_score
cor(world_happiness$grams_sugar_per_day, world_happiness$happiness_score)
# [1] 0.69391
# Question
# Based on this data, which statement about sugar consumption and happiness scores is true?
# Possible Answers
# - Increased sugar consumption leads to a higher happiness score.
# - Lower sugar consumption results in a lower happiness score
# - Increased sugar consumption is associated with a higher happiness score. +
# - Sugar consumption is not related to happiness
# Nice interpretation of correlation! If correlation always implied that one thing causes another, people may do some nonsensical things, like eat more sugar to be happier.

# Confounders
# A study is investigating the relationship between neighborhood residence and lung capacity. Researchers measure the lung capacity of thirty people from neighborhood A, which is located near a highway, and thirty people from neighborhood B, which is not near a highway. Both groups have similar smoking habits and a similar gender breakdown.
# Which of the following could be a confounder in this study?
# Answer the question
# Possible Answers
# - Lung capacity
# - Neighborhood
# - Air pollution
#   Correct! You would expect there to be more air pollution in the neighborhood situated near the highway, which can cause lower lung capacity.
# - Smoking status
#   Incorrect
#   Nope, since smoking status is similar between the two groups, this will affect the average lung capacity of both groups equally.
# - Gender
#   Incorrect
#   Nope, since gender is similar between the two groups, this will affect the average lung capacity of both groups equally.

# Longitudinal vs. cross-sectional studies
# A company manufactures thermometers, and they want to study the relationship between a thermometer's age and its accuracy. To do this, they take a sample of 100 different thermometers of different ages and test how accurate they are. Is this data longitudinal or cross-sectional?
# Answer the question
# Possible Answers
# - Longitudinal
# - Cross-sectional +
# - Both
# - Neither
# Perfect! This is a cross-sectional study since researchers aren't following the same set of thermometers over time and repeatedly measuring their accuracy at different ages.




