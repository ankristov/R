
# Coin flips with prop_model

# The function prop_model has been loaded into your workspace. It implements a Bayesian model that assumes that:
  
# The data is a vector of successes and failures represented by 1s and 0s.
# There is an unknown underlying proportion of success.
# Prior to being updated with data any underlying proportion of success is equally likely.
# Assume you just flipped a coin four times and the result was heads, tails, tails, heads. If you code heads as a success and tails as a failure then the following R codes runs prop_model with this data

data <- c(1, 0, 0, 1)
prop_model(data)
# http://joxi.ru/EA4JNzviXZeW5m
# The output of prop_model is a plot showing what the model learns about the underlying proportion of success from each data point in the order you entered them. At n=0 there is no data, and all the model knows is that it's equally probable that the proportion of success is anything from 0% to 100%. At n=4 all data has been added, and the model knows a little bit more.
# Take prop_model for a spin by entering the R-code above into the script window to the right.
library(purrr)
prop_model <- function(data)
{
  data <- as.logical(data)
  proportion_success <- c(0, seq(0, 1, length.out = 100), 1)
  data_indices <- round(seq(0, length(data), length.out = min(length(data) + 1, 20)))
  post_curves <- map_dfr(data_indices, function(i) {
    value <- ifelse(i == 0, "Prior", ifelse(data[i], "Success", "Failure"))
    label <- paste0("n=", i)
    probability <- dbeta(proportion_success, 
                         prior_prop[1] + sum(data[seq_len(i)]), 
                         prior_prop[2] + sum(!data[seq_len(i)]))
    probability <- probability/max(probability)
    data_frame(value, label, proportion_success, probability)
  })
  post_curves$label <- fct_rev(factor(post_curves$label, levels = paste0("n=", data_indices)))
  post_curves$value <- factor(post_curves$value, levels = c("Prior", "Success", "Failure"))
  p <- 
    ggplot(post_curves, aes(x = proportion_success, y = label, height = probability, fill = value)) + 
    geom_joy(stat = "identity", color = "white", alpha = 0.8, panel_scaling = TRUE, size = 1) + 
    scale_y_discrete("", expand = c(0.01, 0)) + 
    scale_x_continuous("Underlying proportion of success") + 
    scale_fill_manual(values = hcl(120 * 2:0 + 15, 100, 65), 
                      name = "", 
                      drop = FALSE, 
                      labels = c("Prior   ", "Success   ", "Failure   ")) + 
    theme_light(base_size = 18) + 
    theme(legend.position = "top")
  if (show_plot) {print(p)}
  invisible(rbeta(n_draws, prior_prop[1] + sum(data), prior_prop[2] + sum(!data)))
}
  
# Question: Looking at the final probability distribution at n=4, what information does the model have regarding the underlying proportion of heads?
# That's right! The model knows it's not close to 0% or close to 100%, but believes it could be anything in between.

# Zombie drugs with prop_model
# If we really were interested in the underlying proportion of heads of this coin then prop_model 
# isn't particularly useful. Since it assumes that any underlying proportion of success is equally 
# likely prior to seeing any data it will take a lot of coin flipping to convince prop_model that 
# the coin is fair. This model is more appropriate in a situation where we have little background 
# knowledge about the underlying proportion of success
# Let's say the zombie apocalypse is upon us and we have come up with a new experimental drug to 
# cure zombieism. We have no clue how effective it's going to be, but when we gave it to 13 zombies 
# two of them turned human again. Change the data argument to prop_model to estimate the underlying 
# proportion of success of curing a zombie.
data = c(1, 0, 0, 1,0,0,0,0,0,0,0,0,0)
prop_model(data)
# It seems like it's not a perfect drug, but between 5% to 40% cured zombies is better than nothing!

# Looking at samples from prop_model
# Here again is the prop_model function which has been given the data from our zombie experiment where 
# two out of 13 zombies got cured. In addition to producing a plot, prop_model also returns a large 
# random sample from the posterior over the underlying proportion of success.
data = c(1, 0, 0, 1, 0, 0,0, 0, 0, 0, 0, 0, 0)
# Extract and explore the posterior
# Assign the return value of prop_model to a variable called posterior and take a look at the first number of samples using the command head(posterior).
posterior <- prop_model(data)
head(posterior)
# [1] 0.1693174 0.1563166 0.2091882 0.2463102 0.3167312 0.2204189
# Looking at these first few samples confirms what is already shown in the plot: That the underlying 
# proportion of cured zombies is likely somewhere between 5% and 50%. But these were just the first 
# six samples in posterior which currently contain 10,000 samples (the default of prop_model).
# Take a look at the distribution of all the samples in posterior by plotting it as a histogram using  
# the hist() function with posterior as the first argument.
# Plot the histogram of the posterior
hist(posterior)
# http://joxi.ru/J2b0eVlTqwGZ7r
# Compare this histogram to the plot produced directly by prop_model (you'll find it if you go to the previous plot in the Plots tab). You should notice that the histogram and the posterior distribution (at n=13) describe the same distribution.
# OK, so you got a vector of samples from prop_model and confirmed that they are a good representation of the posterior distribution. What have you gained by doing this? So far, not much, but the next exercise will show you some examples of why samples are so convenient to work with. But before you leave:
# Make the histogram prettier by adding the arguments:
# breaks = 30 to make it smoother.
# xlim = c(0, 1) to make it cover the whole range of possible proportions from 0 to 1.
# col = "palegreen4" to give it a color that is more suitable for zombie statistics
hist(posterior, breaks = 30, xlim = c(0,1), col = "palegreen4")
# http://joxi.ru/823NOxpTz4DZDm

# Summarizing the zombie drug experiment
# The point of working with samples from a probability distribution is that it makes it easy to calculate 
# new measures of interest. The following tasks are about doing just this!
# A point estimate is a single number used to summarize what's known about a parameter of interest. 
# It can be seen as a "best guess" of the value of the parameter. A commonly used point estimate is 
# the median of the posterior. It's the midpoint of the distribution, and it's equally probable for 
# the parameter value to be larger than the median as it is to be smaller than it.
# Calculate the median of posterior using the median() function built into R.
median(posterior)
# [1] 0.1871902
mean(posterior)
# [1] 0.2005229
# So, a best guess is that the drug would cure around 18% of all zombies. Another common summary 
# is to report an interval that includes the parameter of interest with a certain probability. 
# This is called a credible interval (CI). With a posterior represented as a vector of samples 
# you can calculate a CI using the quantile() function.
# quantile() takes the vector of samples as its first argument and the second argument is a vector 
# defining how much probability should be left below and above the CI. For example, the vector 
# c(0.05, 0.95) would yield a 90% CI and c(0.25, 0.75) would yield a 50% CI.
# Calculate a 90% credible interval of posterior using quantile().
# Calculate the credible interval
quantile(x=posterior,probs = c(0.05, 0.5, 0.95))
#         5%        50%        95% 
# 0.06260286 0.18767772 0.38491335 

# the same for binom distribution
dbinom(x=2,size = 100,prob = 2/13)
d <- rbinom(n=10000, size = 100, prob = 2/13)/100
mean(d)
median(d)
q <- quantile(x=d, probs = c(0.05,0.95))
ggplot(data=tibble(d=d)) + 
  geom_histogram(aes(x=d), binwidth = 0.01) + 
  geom_vline(xintercept = q[1], color='red', lty = 2) +
  geom_vline(xintercept = q[2], color='red', lty = 2)

# According to the credible interval, there is a 90% probability that the proportion of zombies the drug 
# would cure is between 6% and 38%. (Here we have to be careful to remember that the percentage of cured 
# zombies and the percentage of probability are two different things.)
# Now, there is a rival zombie laboratory that is also working on a drug. They claim that they are 
# certain that their drug cures 7% of the zombies it's administered to. Can we calculate how probable 
# it is that our drug is better? Yes, we can! But it's a two stage process.
# First, use sum to count how many samples in posterior that are larger than 7%. Do this by giving 
# posterior > 0.07 as the argument to sum.
# Calculate the sum
sum(posterior > 0.07)
# [1] 9299
# To turn this count into a probability we now need to normalize it, that is, divide it by the total 
# number of samples in posterior.
# Divide the result of sum by the number of samples in posterior.
# You can get the number of samples in posterior using the length function.
# Calculate the probability
sum(posterior > 0.07) / length(posterior)
# 0.9332
# It seems there is a large probability (93%) that our zombie drug is better!

# Conclusion: 
# Given the data of two cured and 11 relapsed zombies, and using the Bayesian model described 
# before, there is a 90% probability that our drug cures between 6% and 39% of treated zombies. 
# Further, there is 93% probability that our drug cures zombies at a higher rate than current 
# state of the art drugs. Sounds like science to me.

# Take a generative model for a spin
# To the right you have the R code that implements the generative model we just developed.
# Run this code to generate a simulated dataset. Assume the underlying proportion of success 
# of curing a zombie is 42% and that you administer the drug to 100 zombies.
# The generative zombie drug model
# Set parameters
prop_success <- 0.42
n_zombies <- 100
# Simulating data
data <- c()
for(zombie in 1:n_zombies) {
  data[zombie] <- runif(1, min = 0, max = 1) < prop_success # vector of TRUE/FALSE
  # data[zombie] <- ifelse(runif(1, min = 0, max = 1) < prop_success,1,0) # vector of 0/1
}
data <- as.numeric(data)
data
# Nice! Instead of representing cured zombies as a vector of 1s and 0s it could be represented as a count 
# of the number of cured out of the total number of treated.
# Run this code to generate a new simulated dataset, but first change the code to count how many 
# zombies in data were cured. Use the sum function for this and assign this count to data instead 
# of the vector of 1s and 0s.
# Count cured
data <- sum(as.numeric(data))
data
# [1] 38
# Perfect! Some zombies got cured in this simulation, but far from all.

# Take the binomial distribution for a spin
# It turns out that the generative model you ran last exercise already has a name. It's called the 
# binomial process or the binomial distribution. In R you can use the rbinom function to simulate 
# data from a binomial distribution. The rbinom function takes three arguments:
# - n The number of times you want to run the generative model
# - size The number of trials. (For example, the number of zombies you're giving the drug.)
# - prob The underlying proportion of success as a number between 0.0 and 1.0.
# Replicate the result from the last exercise using the rbinom function: Simulate one count of the 
# number of cured zombies out of a 100 treated, where the underlying proportion of success is 42%.
# Try out rbinom
rbinom(n = 1, size = 100, prob = 0.42)
# [1] 41
# [1] 48
# Ok! A nice thing with rbinom is that it makes it easy to rerun the generative model many times.
# Now, change the code to run the simulation 200 times, rather than just one time.
# # Change the parameters
rbinom(n = 200, size = 100, prob = 0.42)
#   [1] 35 47 43 41 39 48 45 39 49 43 43 45 35 39 49 45 40 41 48 36 35 42 48 37 43
#  [26] 46 45 42 46 44 48 49 44 34 51 36 43 43 44 37 40 43 35 39 50 45 46 47 44 37
#  [51] 34 41 42 42 41 48 43 45 50 42 42 47 42 39 38 45 41 41 44 45 35 45 44 51 46
#  [76] 47 36 42 43 43 47 52 43 43 34 51 46 44 33 49 39 35 37 44 35 36 43 37 44 39
# [101] 47 40 50 46 47 38 42 44 39 42 46 39 45 46 42 40 45 42 45 45 48 43 38 40 49
# [126] 40 52 41 42 44 36 40 39 47 38 41 35 42 39 45 42 48 41 42 44 39 47 44 47 48
# [151] 36 41 38 48 35 41 49 43 43 37 32 47 43 43 42 43 41 46 39 45 41 48 41 42 45
# [176] 39 49 47 42 42 38 41 46 40 40 44 42 45 46 42 41 35 44 44 50 42 42 42 45 36
# Nice! That's a lot of simulated zombies right there.

# How many visitors could your site get (1)?
# To get more visitors to your website you are considering paying for an ad to be shown 100 times on a 
# popular social media site. According to the social media site, their ads get clicked on 10% of the time.
# Assume that 10% is a reasonable number, and assume that the binomial distribution is a reasonable 
# generative model for how people click on ads.
# Fill in the missing parameters and use the rbinom function to generate a sample that represents the 
# probability distribution over what the number of visitors to your site is going to be.
# Visualize this distribution using hist.
# Fill in the parameters
n_samples <- 100000
n_ads_shown <- 100
proportion_clicks <- 0.1
n_visitors <- rbinom(n_samples, size = n_ads_shown, 
                     prob = proportion_clicks)

# Visualize n_visitors
hist(n_visitors)
# http://joxi.ru/E2pYv1gTvXvB9A
# Question
# You would like the ad campaign to result in at least 5 visitors to your site.
# Eyeballing the plot you just produced, what is the probability you will get 5 or more visitors because 
# of the ad?
# 10%
# 20%
# 70%
# 90% +

# Adding a prior to the model
# You're not so sure that your ad will get clicked on exactly 10% of the time. Instead of assigning 
# proportion_clicks a single value you are now going to assign it a large number of values drawn from 
# a probability distribution.
# For now, we are going to assume that it's equally likely that proportion_clicks could be as low 
# as 0% or as high as 20%. These assumptions translate into a uniform distribution which you can 
# sample from in R like this:
# x <- runif(n = n_samples, min = 0.0, max = 0.2)
# Replace the single value that is assigned to proportion_clicks with n_samples samples produced by runif as above
n_samples <- 100000
n_ads_shown <- 100
# Update proportion_clicks
proportion_clicks <- runif(n = n_samples, min = 0.0, max = 0.2)
n_visitors <- rbinom(n = n_samples, size = n_ads_shown, prob = proportion_clicks)
# Because the rbinom function is vectorized the first value of proportion_clicks is used to sample the 
# first value in n_visitors, the second value in proportion_clicks is used for the second in n_visitors, 
# and so on. The result is that the samples in n_visitors now also incorporate the uncertainty in what 
# the underlying proportion of clicks could be.
# First, visualize the uncertainty in proportion_clicks using the hist function.
# Visualize proportion clicks
hist(proportion_clicks)
# http://joxi.ru/82QRVQ5c8d8ajA
# You shouldn't be surprised to see that the uncertainty over proportion_clicks is just as you specified 
# it to be: Uniform between 0.0 and 0.2 (except for some small variations in the height of the bars 
# because we took a random sample using runif).
# Now, visualize the uncertainty in n_visitors again using the hist function.
hist(n_visitors)
# http://joxi.ru/52aG1zYilDlXOr
# Question
# This looks very different from the histogram of n_visitors we got in the last exercise when 
# proportion_clicks was exactly 0.1. With the added uncertainty in proportion_clicks the uncertainty 
# over the number of visitors we 'll get also increased.
# Eyeballing the plot you just produced, what is the probability you will get 5 or more visitors 
# because of the ad?
# 10%
# 20%
# 70% +
# 90% 

# Update a Bayesian model with data
# You ran your ad campaign, and 13 people clicked and visited your site when the ad was shown a 100 
# times. You would now like to use this new information to update the Bayesian model.
# The model you put together in the last exercise resulted in two vectors: (1) proportion_clicks that 
# represents the uncertainty regarding the underlying proportion of clicks and (2) n_visitors which 
# represents the uncertainty regarding the number of visitors you would get. We have now put these 
# vectors into a data frame for you called prior.
# Take a look at the first rows of prior using the head() function.
# Create the prior data frame
prior <- data.frame(proportion_clicks, n_visitors)
# Examine the prior data frame
head(prior)
# proportion_clicks n_visitors
# 1        0.05751550          6
# 2        0.15766103          9
# 3        0.08179538         11
# 4        0.17660348         11
# 5        0.18809346         16
# 6        0.00911130          2
# The reason we've called it prior is because it represents the uncertainty before (that is, prior to) 
# having included the information in the data. Let's do that now!
# prior$n_visitors represented the uncertainty over how many visitors you would get because of the ad 
# campaign. But now you know you got exactly 13 visitors.
# Update prior to include this information by conditioning on this data. That is, filtering away all 
# rows where prior$n_visitors isn't equal to 13. Store the resulting data frame in posterior.
# Remember that you can subset data frame using the []-operator in R. For example, the following would 
# return only those rows where n_visitors == 13:
# prior[prior$n_visitors == 13, ]
# Create the posterior data frame
posterior <- prior[n_visitors == 13,]
# Great! The reason that we call it posterior is because it represents the uncertainty after (that 
# is, posterior to) having included the information in the data. So what does posterior contain now? 
# Well, posterior$n_visitors isn't too exciting, that's just 13 repeated many times.
# But take a look at posterior$proportion_clicks using the hist function.
# Visualize posterior proportion clicks
hist(posterior$proportion_clicks)
# http://joxi.ru/8An4XoVTNRNZpm
# Question
# This doesn't look at all like the uniform distribution between 0.0 and 0.2 we put into proportion_clicks before. The whole distribution of samples now represent the posterior (after the data) probability distribution over what proportion_clicks could be.
# Looking at the probability distribution over proportion_clicks what does the model know about the underlying proportion of visitors clicking on the ad?
# Possible Answers
# It's likely between 0% and 20%
# It's likely between 5% and 10%
# It's likely between 7% and 19% +
# It's likely between 15% and 20%

# How many visitors could your site get (3)?
# In the last exercise, you updated the probability distribution over the underlying proportions of 
# clicks (proportion_clicks) using new data. Now we want to use this updated proportion_clicks to 
# predict how many visitors we would get if we reran the ad campaign.

# The result from the last exercise is still in the data frame posterior, but if you look at 
# posterior$n_visits you'll see it's just 13 repeated over and over again. This makes sense as posterior 
# represents what the model knew about the outcome of the last ad campaign after having seen the data.
# Assign posterior to a new variable called prior which will represent the uncertainty regarding 
# the new ad campaign you haven't run yet.
# Assign posterior to a new variable called prior
prior <- posterior
# Take a look at the first rows in prior using the head() function.
# Take a look at the first rows in prior
head(prior)
# While proportion_clicks represents the uncertainty regarding the underlying proportion of clicks for the next ad campaign, n_visitors has not been updated yet.
# Replace prior$n_visitors with a new sample drawn using rbinom with prior$proportion_clicks as an argument.
# Plot the resulting prior$n_visitors using hist.
# Here is some example code you can use for this, just replace ___ .
# Replace prior$n_visitors with a new sample and visualize the result
n_samples <-  nrow(prior)
n_ads_shown <- 100
prior$n_visitors <- rbinom(n_samples, size = n_ads_shown,
                           prob = prior$proportion_click)
hist(prior$n_visitors)
# http://joxi.ru/Q2KWnY7COdgd32

# The plot shows a probability distribution over the number of site visitors a new ad campaign would 
# bring in. It now looks pretty likely that there would be more than, say, 5 visitors because of the 
# campaign.
# Calculate the probability that you will get 5 or more visitors next time you run your ad campaign.
# Do this by summing up the number of draws with more than five visitors ( sum(prior$n_visitors >= 5) ) 
# and divide by the total number of draws ( length(prior$n_visitors) ).
# # Calculate the probability that you will get 5 or more visitors
sum(prior$n_visitors >= 5) / length(prior$n_visitors)
# [1] 0.9860671
# Question
# According to the new model, what is the probability of getting five or more visitors?
# 75%
# 87%
# 93%
# 99% +
# That seems about right! It's pretty probable that you'll get more than 5 visitors.

# Explore using the Beta distribution as a prior
# The Beta distribution is a useful probability distribution when you want model uncertainty over a 
# parameter bounded between 0 and 1. Here you'll explore how the two parameters of the Beta distribution 
# determine its shape.
# One way to see how the shape parameters of the Beta distribution affect its shape is to generate a 
# large number of random draws using the rbeta(n, shape1, shape2) function and visualize these as a 
# histogram. The code to the right generates 1,000,000 draws from a Beta(1, 1) distribution: A Beta 
# distribution with both shape parameters set to 1.
# Visualize these draws using the hist function.
# Explore using the rbeta function
beta_sample <- rbeta(n = 1000000, shape1 = 1, shape2 = 1)
# Visualize the results
hist(beta_sample)
# http://joxi.ru/ZrJ6VYyTQWe59r
# Right! A Beta(1,1) distribution is the same as a uniform distribution between 0 and 1. It is useful as a so-called non-informative prior as it expresses than any value from 0 to 1 is equally likely.
# Try to set one of the shape parameters to a negative number.
# Take a look at the first few values using head(beta_sample).
# Modify the parameters
beta_sample <- rbeta(n = 1000000, shape1 = -1, shape2 = 1)
# Explore the results
head(beta_sample)
# [1] NaN NaN NaN NaN NaN NaN
# Modify the parameters
beta_sample <- rbeta(n = 1000000, shape1 = 100, shape2 = 100)
# Visualize the results
hist(beta_sample, col = 'palegreen4', breaks = 30)
# http://joxi.ru/YmE4VaDTGD9VLr
# So the larger the shape parameters are, the more concentrated the beta distribution becomes. When 
# used as a prior, this Beta distribution encodes the information that the parameter is most likely 
# close to 0.5 .
# See what happens if you set shape2 to something smaller than shape1. Let's set shape2 = 20 
# while keeping shape1 = 100.
# Modify the parameters
beta_sample <- rbeta(n = 1000000, shape1 = 100, shape2 = 20)
# Visualize the results
hist(beta_sample)
# http://joxi.ru/BA0yd0vu1VwYXr
# So the larger the shape1 parameter is the closer the resulting distribution is to 1.0 and the 
# larger the shape2 the closer it is to 0.

# Pick the prior that best captures the information
# The new information you got from the social media company was:
# Most ads get clicked on 5% of the time, but for some ads it is as low as 2% and for others as high as 8%.
# There are many different probability distributions that one can argue captures this information.
# Out of the four Beta distribution shown below, which captures this information the best?
# http://joxi.ru/brR3D56SBn4ODm
# Possible Answers
# A. rbeta(shape1 = 3, shape2 = 12)
# B. rbeta(shape1 = 15, shape2 = 75)
# C. rbeta(shape1 = 5, shape2 = 95) + 
# D. rbeta(shape1 = 30, shape2 = 10)

data <- rbeta(100000, shape1 = 5, shape2 = 95)
ggplot(data = tibble(d=data)) + geom_histogram(aes(d))
head(data)

# Change the model to use an informative prior
# The code to the right is the old model you developed from scratch in chapter 2.
# Change this model to use the new informative prior for proportion_clicks that you just selected:
# rbeta(n_draws, shape1 = 5, shape2 = 95)
n_draws <- 100000
n_ads_shown <- 100
# Change the prior on proportion_clicks
# proportion_clicks <- runif(n_draws, min = 0.0, max = 0.2)
proportion_clicks <- rbeta(n = n_draws, shape1 = 5, shape2 = 95)
n_visitors <- 
  rbinom(n_draws, size = n_ads_shown, 
         prob = proportion_clicks)
prior <- 
  data.frame(proportion_clicks, n_visitors)
posterior <- 
  prior[prior$n_visitors == 13, ]
# This plots the prior and the posterior in the same plot
par(mfcol = c(2, 1))
hist(prior$proportion_clicks, 
     xlim = c(0, 0.25))
hist(posterior$proportion_clicks, 
     xlim = c(0, 0.25))
# Take a look at the new posterior! Due to the new informative prior it has shifted to the left, favoring lower rates.

# Fit the model using another dataset
# Let's fit the binomial model to both the video ad data (13 out of 100 clicked) and the new text ad 
# data (6 out of a 100 clicked).
# To the right, you again have the model you developed in the last chapter. Here posterior_video is the posterior proportion of clicks for the video ad data.
# Add a row where you calculate posterior_text in the same way as posterior_video but using the text ad data instead.
# Plot posterior_text as a histogram just as posterior_video is plotted.
# Define parameters
n_draws <- 100000
n_ads_shown <- 100
proportion_clicks <- runif(n_draws, min = 0.0, max = 0.2)
n_visitors <- rbinom(n = n_draws, size = n_ads_shown, 
                     prob = proportion_clicks)
prior <- data.frame(proportion_clicks, n_visitors)
# Create the posteriors for video and text ads
posterior_video <- prior[prior$n_visitors == 13, ]
posterior_text <- prior[prior$n_visitors == 6, ]
head(posterior_text)
head(posterior_video)
# Visualize the posteriors
hist(posterior_video$proportion_clicks, xlim = c(0, 0.25))
hist(posterior_text$proportion_clicks, xlim = c(0, 0.25))
# Question
# Looking at the histogram of posterior_text what can be said about the value of the proportion of clicks for the text ad?
# Possible Answers
# It's likely between 0.03 and 0.13. +
# It's likely between 0.10 and 0.15.
# It's exactly 0.06.
# It's likely between 0.06 and 0.10
# That seems about right! Since we have so little data, the estimate is pretty uncertain.


# Calculating the posterior difference
# The posterior proportion_clicks for the video and text ad has been put into a single posterior data 
# frame. The reason for [1:4000] is because these proportion_clickss are not necessarily of the same 
# length, which they need to be when put into a data frame.
# Now it's time to calculate the posterior probability distribution over what the difference in 
# proportion of clicks might be between the video ad and the text ad.
# Add a new column posterior$prop_diff that should be the posterior difference between video_prop and 
# text_prop (that is, video_prop minus text_prop).
# Plot posterior$prop_diff as a histogram using hist()
posterior <- data.frame(
  video_prop = posterior_video$proportion_clicks[1:4000],
  text_prop  = posterior_text$proportion_click[1:4000])
# Calculate the posterior difference: video_prop - text_prop
posterior$prop_diff <- posterior$video_prop - posterior$text_prop
# Visualize prop_diff
hist(posterior$prop_diff)
# Calculate "a most likely" difference by taking the median of posterior$prop_diff
median(posterior$prop_diff)
mean(posterior$prop_diff)
# Finally, calculate the probability that proportion of clicks is larger for the video ad than for the 
# text ad.
# That is, calculate the proportion of samples in posterior$prop_diff that are more than zero.
sum(posterior$prop_diff > 0) / length(posterior$prop_diff)
# Question
# Given the model and the data, what is the probability that the video ad is better than the text ad? (Here better means having a higher proportion of clicks.)
# Possible Answers
# 5%
# 50%
# 90%
# 95% +
# Right! It's pretty likely that the video ad is better.

# A small decision analysis 1
# Each visitor spends $2.53 on average, a video ad costs $0.25 and a text ad costs $0.05. Let's figure 
# out the probable profit when using video ads and text ads!
# The data frame posterior contains the probability distribution over the underlying proportion of 
# clicks for video ads and text ads.
# Add the column posterior$video_profit which should be the probability distribution over the average 
# profit you'll make on showing a video ad. That is, the underlying proportion of clicks times the 
# average spend minus the cost of showing the video.
visitor_spend <- 2.53
video_cost <- 0.25
text_cost <- 0.05
# Add the column posterior$video_profit
posterior$video_profit <- posterior$video_prop * visitor_spend - video_cost
# Add the column posterior$text_profit: The probability distribution over the average profit you'll 
# make on showing a text ad.
# (optional) Take a look at the first rows of the final posterior using head().
posterior$text_profit <- posterior$text_prop * visitor_spend - text_cost
head(posterior)
# Finally, take a look at the probability distributions of posterior$video_profit and 
# posterior$text_profit by plotting them as two separate histograms using the hist() function.
ggplot(data = posterior) + geom_histogram(aes(x = video_profit), fill = 'red', alpha = 0.3) +
  geom_histogram(aes(x = text_profit), fill = 'blue', alpha = 0.3)
ggplot(data = posterior) + geom_density(aes(x = video_profit), fill = 'red', alpha = 0.3) +
  geom_density(aes(x = text_profit), fill = 'blue', alpha = 0.3)
# Great! Take a look at the two histograms you've plotted, which method seems most profitable, if any?

# A small decision analysis 2
# Using the columns video_profit and text_profit that you added to posterior in the last exercise, let's conclude the decision analysis.
# Add the new column posterior$profit_diff: The probability distribution over the difference in profits between video ads and text ads. That is, the profit from video ads minus the profit from text ads.
# Take a look at posterior$profit_diff by plotting it using hist().
# Add the column posterior$profit_diff
posterior$profit_diff <- posterior$video_profit - posterior$text_profit
# Visualize posterior$profit_diff
hist(posterior$profit_diff)
# There are many ways to calculate a "best guess" for what the difference in profits might be. Here, use the posterior median.
# Calculate the median of posterior$profit_diff.
median(posterior$profit_diff)
# -0.03300742
# Finally (phew!) calculate the probability that running text ads will result in higher profits than 
# video ads. That is:
# Calculate the proportion of samples in posterior$profit_diff that is lower than 0, in favor of text ads.
# Calculate the probability that text ads are better than video ads
sum(posterior$profit_diff < 0) / length(posterior$profit_diff)
# 0.631
# Question
# So it seems that the evidence does not strongly favor neither text nor video ads.
# But if forced to choose at this point, what would you choose?
# Possible Answers
# Video ads
# Text ads +
# Right! Even though text ads get a lower proportion of clicks, they are also much cheaper. And, as 
# you have calculated, there is a 63% probability that text ads are better.
# The Poisson distribution
# The Poisson distribution simulates a process where the outcome is a number of occurrences per 
# day/year/area/unit/etc. Before using it in a Bayesian model, let's explore it!
# The Poisson distribution has one parameter, the average number of events per unit. In R you can 
# simulate from a Poisson distribution using rpois where lambda is the average number of occurrences:
# rpois(n = 10000, lambda = 3)
# Use the code above to simulate 10000 draws from a Poisson distribution, assign the result to x.
# Visualize x using a histogram (hist()).
# Simulate from a Poisson distribution and visualize the result
x <- rpois(n = 10000, lambda = 3)
ggplot(data = tibble(x = x)) + geom_histogram(aes(x = x), binwidth = 1, color = 'grey')
# Let's say that you run an ice cream stand and on cloudy days you on average sell 11.5 ice creams. 
# It's a cloudy day.
# Change the rpois call to visualize the probability distribution over how many ice creams you'll sell.
# Simulate from a Poisson distribution and visualize the result
x <- rpois(n = 10000, lambda = 11.5)
ggplot(data = tibble(x = x)) + geom_histogram(aes(x = x), binwidth = 1, color = 'grey')
# It's still a cloudy day, and unfortunately, you won't break even unless you sell 15 or more ice creams.
# Assuming the Poisson model is reasonable, use x to calculate the probability that you'll break even.
# Tip: For this, you need to calculate what proportion of samples in x are >= 15.
sum(x > 15) / length(x)
# 0.115
# Question
# Is it likely that you will break even on a cloudy day, or should you stay at home?
# Possible Answers
# Let's go out and sell ice cream!
# Let's stay at home and wait for a sunny day
# Yes! Let's stay at home and do DataCamp courses instead!

# Clicks per day instead of clicks per ad
# When you put up a banner on your friend's site you got 19 clicks in a day, how many daily clicks 
# should you expect this banner to generate on average? Now, modify your model, one piece at a time, 
# to calculate this.
# To the right is the old code for the binomial Bayesian model. To accommodate the new banner data 
# you're now going to change the model so that it uses a Poisson distribution instead.
# Start by replacing the prior distribution over proportion_clicks by a prior over mean_clicks. 
# Make the prior a uniform (runif) distribution from 0 to 80 clicks per day.
# Replace proportion_clicks with mean_clicks and change the parameters
n_draws <- 100000
# n_ads_shown <- 100
# proportion_clicks <- runif(n_draws, min = 0.0, max = 0.2)
mean_clicks <- runif(n_draws, min = 0, max = 80)
# n_visitors <- rbinom(n_draws, size = n_ads_shown, prob = proportion_clicks)
n_visitors <- rpois(n = n_draws, lambda = mean_clicks)
prior <- data.frame(mean_clicks, n_visitors)
head(prior)
ggplot(data = prior) + geom_point(aes(x = n_visitors, y = mean_clicks))
posterior <- prior[prior$n_visitors == 13, ]
# Nice! But you're still using the wrong data.
# The banner got 19 clicks in a day, not 13, so change it.
# Plot histograms of prior$mean_clicks and posterior$mean_clicks.
posterior <- prior[prior$n_visitors == 19, ]
head(posterior)
# Visualize mean_clicks
hist(prior$mean_clicks)
hist(posterior$mean_clicks)
ggplot() + 
  geom_histogram(data = prior, aes(x = mean_clicks), fill = 'blue', alpha = 0.5) +
  geom_histogram(data = posterior, aes(x = mean_clicks), fill = 'red', alpha = 0.5)

ggplot(data = posterior) + geom_jitter(aes(x = n_visitors, y = mean_clicks), ) + geom_violin(aes(x = n_visitors, y = mean_clicks), alpha = 0.5)

# Question
# The model is complete! Like before you could now calculate credible probability intervals using quantile 
# or calculate the probability of getting more than, say, 15 clicks next day. But just looking at the 
# posterior probability distribution:
# What range could you expect the mean number of daily clicks to be in?
# Possible Answers
# 10 to 20 daily clicks on average
# 20 to 30 daily clicks on average
# 12 to 28 daily clicks on average +
# 12 to 19 daily clicks on average

# 15 Probability rules.mp4
# Cards and the sum rule
# A standard French-suited deck of playing cards contains 52 cards; 13 each of hearts (♥), spades (♠), 
# clubs (♦), and diamonds (♣). Assuming that you have a well-shuffled deck in front of you, the 
# probability of drawing any given card is 1/52 ≈ 1.92%.
# Calculate the probability of drawing any of the four aces! That is, calculate the probability of 
# drawing 🂡 or 🂱 or 🃁 or 🃑 using the sum rule and assign it to prob_to_draw_ace.
# Calculate the probability of drawing any of the four aces
prob_to_draw_ace <- 1/52 + 1/52 + 1/52 + 1/52
# Yes! The probability to draw an ace is 1/52 + 1/52 + 1/52 + 1/52 = 4/52 = 7.7% .
# Cards and the product rule
# Again, assuming that you have a well-shuffled deck in front of you, the probability of drawing any 
# given card is 1/52 ≈ 1.92% . The probability of drawing any of the four aces is 
# 1/52 + 1/52 + 1/52 + 1/52 = 4/52. Once an ace has been drawn, the probability of picking any of the 
# remaining three is 3/51. If another ace is drawn the probability of picking any of the remaining two 
# is 2/50, and so on.
# Use the product rule to calculate the probability of picking the four aces in a row from the top 
# of a well-shuffled deck and assign it to prob_to_draw_four_aces.
# Calculate the probability of picking four aces in a row
prob_to_draw_four_aces <- 4/52 * 3/51 * 2/50 * 1/49
# Yes! The probability to draw four aces is 4/52 * 3/51 * 2/50 * 1/49 = 0.0004%. Pretty unlikely!

n_visitors <- rbinom(n=10000, size = 100, prob = 0.1)
sum(n_visitors == 13) / length(n_visitors)
dbinom(x = 13, size = 100, prob = 0.1)
df <- tibble(x = seq(0,100,by=1), y = dbinom(x = x, size = 100, prob = 0.1))
df
ggplot(df) + geom_point(aes(x,y))

# From rbinom to dbinom
# To the right is currently code that
# Simulates the number of clicks/visitors (n_clicks) from 100 shown ads using the rbinom function given 
# that the underlying proportion of clicks is 10%.
# Calculates the probability of getting 13 visitors (prob_13_visitors).
# That is, in probability notation it's calculating P(n_visitors = 13 | proportion_clicks = 10%).

# First, try running this code.
# Then, rewrite this code so that it calculates prob_13_visitors using dbinom instead.
# Remember: dbinom directly returns a probability and takes the following arguments: 
# dbinom(x = , size = , prob = ) where size and prob are the same as for rbinom, but x is now the 
# data you want to calculate the probability for.
# Rewrite this code so that it uses dbinom instead of rbinom
n_ads_shown <- 100
proportion_clicks <- 0.1
n_visitors <- rbinom(n = 99999, 
                     size = n_ads_shown, prob = proportion_clicks)
prob_13_visitors <- sum(n_visitors == 13) / length(n_visitors)
prob_13_visitors
prob_13_visitors <- dbinom(x = 13, size = 100, prob = 0.1)
prob_13_visitors
# Alright! You got a similar result as with rbinom, but dbinom is much more efficient!

# Calculating probabilities with dbinom
# To the right is roughly the code you ended up with in the last exercise.
# Currently the code calculates P(n_visitors = 13 | proportion_clicks = 10%): The probability of 
# getting 13 visitors given that the proportion of clicks is 10%.
# Change the code to instead calculate P(n_visitors | proportion_clicks = 10%): The probability 
# distribution over all possible numbers of visitors.
# Tip: As dbinom is vectorized it suffices to change the value of n_visitors into a vector with the 
# numbers 0, 1, 2, …, 100. Try using the seq function!
# Change the code to calculate probability distribution of n_visitors
n_ads_shown <- 100
proportion_clicks <- 0.1
n_visitors <- 13
n_visitors <- seq(from = 0, to = 100, by = 1)
prob <- dbinom(n_visitors, size = n_ads_shown, prob = proportion_clicks)
prob
# Plot the probability distribution P(n_visitors | proportion_clicks = 10%).
# Use the plot() function with n_visitors on the x-axis, prob on the y-axis and set type = "h" to turn 
# it into a bar plot.
# Plot the distribution
plot(x = n_visitors, y = prob, type = 'h')
ggplot(data = tibble(n_visitors = n_visitors, prob = prob)) + geom_col(aes(x = n_visitors, y = prob))
# It seems that with proportion_clicks = 10% the most probable n_visitors values are between 1 and 20. Now, 
# let's flip the equation:
# Fix n_visitors to 13 again.
#Calculate prob for many different values, that is: Assign a vector of values to proportion_clicks. Here, 
# use the vector seq(0, 1, by = 0.01).
# Calculate prob for a range of values of proportion_clicks, here use seq(0, 1, by = 0.01).
# Change the plot statement to have proportion_clicks on the x-axis instead.
# Change the code according to the instructions
n_ads_shown <- 100
proportion_clicks <- 0.1
proportion_clicks <- seq(from = 0, to = 1, by = 0.01)
n_visitors <- seq(0, 100)
n_visitors <- 13
prob <- dbinom(n_visitors, 
               size = n_ads_shown, prob = proportion_clicks)
prob
plot(proportion_clicks, prob, type = "h")
ggplot(data = tibble(proportion_clicks = proportion_clicks, prob = prob)) + 
  geom_col(aes(x = proportion_clicks, y = prob))
# Question
# You have now almost done some Bayesian computation. The plot you just produced almost looks like it shows the probability distribution over different values of proportion_clicks, but it does not. For one, the values in prob do not sum up to one. What you have calculated is the likelihood of different values of proportion_clicks to result in n_visitors = 13.
# Looking at the plot, what value of proportion_clicks seems to give the maximum likelihood to produce n_visitors = 13?
# Possible Answers
# proportion_clicks = 0.07
# proportion_clicks = 0.13 +
# proportion_clicks = 0.20
# proportion_clicks = 0.50
# That seems right! What you've found by eyeballing this graph is the so-called maximum likelihood estimate 
# of proportion_clicks.

#
# Calculating a joint distribution
# To the right, you have parts of the code we developed in the last video. It defines a grid over the 
# underlying proportions of clicks (proportion_clicks) and possible outcomes (n_visitors) in pars. It 
# adds to it the prior probability of each parameter combination and the likelihood that each 
# proportion_clicks would generate the corresponding n_visitors.
# Instructions
# Add the column pars$probability: The probability of each proportion_clicks and n_visitors combination. 
# As in the video, this should be calculated by multiplying the likelihood by the prior
# Make sure the column pars$probability sums to 1.0 by normalizing it, that is, by dividing pars$probability 
# by the total sum of pars$probability.
n_ads_shown <- 100
proportion_clicks <- seq(0, 1, by = 0.01)
n_visitors <- seq(0, 100, by = 1)
pars <- expand.grid(proportion_clicks = proportion_clicks,
                    n_visitors = n_visitors)
pars$prior <- dunif(pars$proportion_clicks, min = 0, max = 0.2)
pars$likelihood <- dbinom(pars$n_visitors, 
                          size = n_ads_shown, prob = pars$proportion_clicks)
# Add the column pars$probability and normalize it
pars$probability <- pars$likelihood * pars$prior # The probability of each proportion_clicks and n_visitors combination
pars$probability <- pars$probability / sum(pars$probability)
head(pars)

# Conditioning on the data (again)
# Let's resurrect the zombie site example where you tested text ads. Out of a 100 impressions of the text ad, 
# 6 out of a 100 clicked and visited your site.
# To the right is roughly the code you developed in the last exercise. pars is currently the joint 
# distribution over all combinations of proportion_clicks and n_visitors.
# Instructions
# Condition on the data and keep only the rows in pars where n_visitors == 6.
# Normalize pars$probability again, to make sure it sums to 1.0.
# Plot the posterior pars$probability using plot(x = , y = , type = "h") with pars$proportion_clicks on 
# the x-axis and pars$probability on the y-axis.
n_ads_shown <- 100
proportion_clicks <- seq(0, 1, by = 0.01)
n_visitors <- seq(0, 100, by = 1)
pars <- expand.grid(proportion_clicks = proportion_clicks, n_visitors = n_visitors)
pars$prior <- dunif(pars$proportion_clicks, min = 0, max = 0.2)
pars$likelihood <- dbinom(pars$n_visitors, size = n_ads_shown, prob = pars$proportion_clicks)
pars$probability <- pars$likelihood * pars$prior # probability of combination proportion_clicks and n_visitors
pars$probability <- pars$probability / sum(pars$probability) # normalizing
# Condition on the data 
pars <- pars[pars$n_visitors == 6,]
# Normalize again
pars$probability <- pars$probability / sum(pars$probability)
# Plot the posterior pars$probability
plot(x = pars$proportion_clicks, pars$probability, type = 'h')
head(pars)
# Cool! You have now calculated (rather than simulated) your first posterior probability distribution!

# A conditional shortcut
# Great, you've now done some Bayesian computation, without doing any simulation! The plot you produced 
# should be similar to the posterior distribution you calculated in chapter 3. However, if you look to the 
# right you see that it required an awful lot of code, isn't there anything we can cut?
# Yes, there is! You can directly condition on the data, no need to first create the joint distribution.
# Instructions
# Set n_visitors directly to 6, just replace the seq-statement.
# Now you can remove the line that conditions on the data, and the line after that, that normalizes 
# pars$probability.
# Take an extra look at the final code and convince yourself that the result of this modified code will 
# be the same as before. :)
# Simplify the code below by directly conditioning on the data
n_ads_shown <- 100
proportion_clicks <- seq(0, 1, by = 0.01)
#n_visitors <- seq(0, 100, by = 1)
n_visitors <- 6
pars <- expand.grid(proportion_clicks = proportion_clicks,
                    n_visitors = n_visitors)
pars$prior <- dunif(pars$proportion_clicks, min = 0, max = 0.2)
pars$likelihood <- dbinom(pars$n_visitors, 
                          size = n_ads_shown, prob = pars$proportion_clicks)
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability / sum(pars$probability)
#pars <- pars[pars$n_visitors == 6, ]
#pars$probability <- pars$probability / sum(pars$probability)
plot(pars$proportion_clicks, pars$probability, type = "h")
# Unnecessary lines are removed, the code is easier to read, and the program runs quicker! Nice!

# A Poisson model description
# At the end of last chapter you tweaked the binomial model into a Poisson model using the following R code:
# n_draws <- 100000
# mean_clicks <- runif(n_draws, min = 0, max = 80)
# n_visitors <- rpois(n = n_draws, mean_clicks)
# Which of the following mathematical model descriptions best describe this model?
# http://joxi.ru/Q2KWnY7COdRZb2
# A

# 19 The temperature in a Normal lake.mp4
# rnorm, dnorm, and the weight of newborns
# Here is a small data set with the birth weights of six newborn babies in grams.
# c(3164, 3362, 4435, 3542, 3578, 4529)
# Let's assume that the Normal distribution is a decent model of birth weight data.
# Given the birth weight data, what would be reasonable values of the mean (mu) and standard deviation (sigma) that generated this data?
# Assign these value to mu and sigma.
# No need to do anything fancy here, just eyeball the data and try to come up with something reasonable.
# Remember: The standard deviation sigma describes the spread of the distribution, where around 70% of the drawn values will be within one standard deviation from the mean.
# Assign mu and sigma
mu <- 3800
sigma <- 500
weight_distr <- rnorm(n = 100000, mean = mu, sd = sigma)
hist(weight_distr, 60, xlim = c(0, 6000), col = "lightgreen")
# The resulting histogram gives you a sense of the uncertainty over the birth weight of newborns. Let's recreate this plot, but calculating the distribution using dnorm instead of simulating using rnorm.
# Create a vector called weight that should contain all values from 0 to 6000 in increments of 100, that is, 0, 100, 200, …, 5900, 6000.
# Tip: use seq(from = , to = , by = )
# Create weight
weight <- seq(0, 6000, by = 100)
# Use dnorm to calculate the likelihood of all the values in weight given the mean mu and standard deviation sigma.
# Assign the result to likelihood.
# Tip: Here is how to call dnorm:
# dnorm(x = , mean = , sd = )
# Here mean and sd are the same as for rnorm, and x is the vector of numbers you want to calculate the 
# likelihoods for.
# Calculate likelihood
likelihood <- dnorm(x = weight, mean = mu, sd = sigma)
# Finally, let's use plot to plot the resulting distribution with weight on the x-axis and likelihood 
# on the y-axis.
# Also, set type="h" in plot to make the plot look nicer.
# Plot the distribution of weight
plot(x = weight, y = likelihood, type = 'h')

# 20 A Bayesian model of water temperature.mp4
temp <- c(19, 23, 20, 17, 23)
mu <- seq(8,30, by = 0.5)
sigma <- seq(0.1, 10, by = 0.3)
pars <- expand.grid(mu = mu, sigma = sigma)
pars$mu_prior <- dnorm(pars$mu, mean = 18, sd = 5)
pars$sigma_prior <- dunif(pars$sigma, min = 0, max = 10)
pars$prior <-  pars$mu_prior * pars$sigma_prior
for (i in 1:nrow(pars)) {
  likelihood <- dnorm(temp, pars$mu[i], pars$sigma[i])
  pars$likelihood[i] <- prod(likelihood)
}
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability / sum(pars$probability)
head(pars)

# A Bayesian model of Zombie IQ
# Zombies are stupid, and you and your colleagues at the National Zombie Research Laboratory are 
# interested in how stupid they are. To the right, you have the Normal model we developed in the 
# last video, but with the temperature data switched out with some zombie IQs fresh from the lab. 
# What we're interested in is how much we can learn about the mean zombie IQ from this data. The 
# model is complete, save for that we need to calculate the probability of each parameter 
# combination in pars.

# Use Bayes Theorem to calculate these probabilities and assign them to pars$probability to complete the model.
# Here's Bayes theorem:
# P(θ|D)=P(D|θ)×P(θ)/∑P(D|θ)×P(θ)
# Where
# θ is a parameter combination,
# D is the data,
# P(D|θ) is the likelihood
# P(θ)is the prior
# P(θ|D) is the probability of different parameter values given the data. This is what we want!
# The IQ of a bunch of zombies
iq <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 46)
# Defining the parameter grid
pars <- expand.grid(mu = seq(0, 150, length.out = 100), sigma = seq(0.1, 50, length.out = 100))
# Defining and calculating the prior density for each parameter combination
pars$mu_prior <- dnorm(pars$mu, mean = 100, sd = 100)
pars$sigma_prior <- dunif(pars$sigma, min = 0.1, max = 50)
pars$prior <- pars$mu_prior * pars$sigma_prior
# Calculating the likelihood for each parameter combination
for(i in 1:nrow(pars)) {
  likelihoods <- dnorm(iq, pars$mu[i], pars$sigma[i])
  pars$likelihood[i] <- prod(likelihoods)
}
# Calculate the probability of each parameter combination
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability / sum(pars$probability)
# Ok! The code for calculating the likelihood was a bit messy, but Bayes theorem stays the same.
# Eyeballing the mean IQ of zombies?
# In the last exercise, you computed the probability for each mean (mu) and SD (sigma) combination. 
# Using the levelplot function from the lattice package we can now visualize this 2D probability 
# distribution:
# levelplot(probability ~ mu * sigma, data = pars)
library(lattice)
levelplot(probability ~ mu * sigma, data = pars)
# We could calculate all kinds of credible intervals and probabilities from this plot, but 
# just eyeballing it:
# What is the most probable mean (mu) of the IQ of Zombies? Roughly how uncertain is the 
# estimate of mu?

# 21 Answering the question Should I have a beach party.mp4
# Sampling from the zombie posterior
# Again pars contains the data frame representing the posterior zombie IQ distribution 
# you calculated earlier. The code to the right draws sample_indices: a sample of row 
# numbers (a.k.a. indices) from the posterior. Now, let's sample from pars to calculate 
# some new measures!
# Use sample_indices to create a new data frame pars_sample from pars with the columns mu and sigma drawn from the rows indicated by sample_indices.
# Tip: If df is a data frame with many rows and columns here is how you would extract rows 1, 3 and 1 (again), and the columns named "height" and "weight":
# df[c(1,3,1), c("height","weight")]
head(pars)
sample_indices <- sample( nrow(pars), size = 10000, replace = TRUE, prob = pars$probability)
head(sample_indices)
# Sample from pars to calculate some new measures
pars_sample <- pars[sample_indices, c('mu','sigma')]
head(pars_sample)

# play with sample
df <- tibble(x = c(1,2,3,4,5,6,7), y = c(0,0,0.3,0,0,0,0.7))
sample(df$x, 15, replace = T, prob = df$y)

# Nice! Now take a look at the posterior probability distribution over the mean IQ.
# Plot pars_sample$mu using hist().
head(pars)
sample_indices <- sample( nrow(pars), size = 10000, replace = TRUE, prob = pars$probability)
head(sample_indices)
# Sample from pars to calculate some new measures
pars_sample <- pars[sample_indices, c("mu", "sigma")]
# Visualize the mean IQ
hist(pars_sample$mu, breaks = 30)
# another way (AK)
pars[pars$probability == max(pars$probability),]
max(pars$probability)
# Finally, use the quantile function to calculate the 0.025, 0.5 and 0.975 quantiles of 
# pars_sample$mu.

# Finally, use the quantile function to calculate the 0.025, 0.5 and 0.975 quantiles of pars_sample$mu.
# Calculate quantiles
quantile(pars_sample$mu, probs = c(0.025, 0.5, 0.975))
# Question
# The 50% quantile you just calculated is the same as the median and a good candidate for a 
# "best guess" for the mean IQ of a zombie, and the 2.5% and 97.5% quantiles form a 95% 
# credible interval.
# When submitting this result to The Journal of Zombieology, which of the following sentence 
# should we put into the result section?
# Keep in mind that as pars_sample$mu is a random sample, the numbers you've calculated might 
# differ slightly from the possible answers below.
# Possible Answers
# - We estimate the mean zombie IQ to be 42.424.
# - The mean zombie IQ is at most 50.0.
# - We estimate the mean zombie IQ to be 42 (95% CI: [35, 50]) +
# - The mean zombie IQ is 34 - 50.

# But how smart will the next zombie be?
# So we have an idea about what the mean zombie IQ is but what range of zombie IQs should 
# we expect? And how likely is it that the next zombie you encounter is, at least, moderately 
# intelligent?
# pars_sample is the data frame you worked with last exercise, and the code to the right 
# simulates from a normal distribution incorporating all the uncertainty in the posterior 
# estimates of the mean mu and standard deviation sigma.
# Take a look at the resulting probability distribution pred_iq using hist().
head(pars_sample)
pred_iq <- rnorm(10000, mean = pars_sample$mu, sd = pars_sample$sigma) # generates 10000 with different mu and sigma = randomisation
# experiment (AK): take the best parameters and generate samples with the normal model (we agreed for our data)
pred_iq1 <- rnorm(10000, mean = pars[pars$probability == max(pars$probability), 'mu'], sd = pars[pars$probability == max(pars$probability), 'sigma'])
pars[pars$probability == max(pars$probability), ]
rnorm(10, c(0,10), c(1, 1))
dim(pars)
head(pred_iq)
length(pred_iq)
# Visualize pred_iq
hist(pred_iq, breaks = 30, xlim = c(0,100))
hist(pred_iq1, breaks = 30, xlim = c(0,100)) # try to make samples 10000 -> 100000 plots will be really similar 

# The pred_iq distribution can be interpreted as the uncertainty over what IQ the next zombie 
# you'll meet will have.
# Calculate the probability that the next zombie you'll meet will have an IQ of 60 or more 
# by calculating the proportion of samples where pred_iq >= 60.
# Calculate the probability of a zombie being "smart" (+60 IQ)
sum(pred_iq >= 60) / length(pred_iq)
# Question
# Zombies with an IQ of 60 or more are of moderate intelligence, and much more dangerous! (They can open doors!)
# How nervous should you be that the next zombie you meet will be one of these smart zombies?
# Possible Answers
# - I'm 100% confident I'll never encounter one of those smart zombies!
# - I'm afraid to go out! From what I know, all zombies might be super smart.
# - The risk is relatively low but still very real. I always carry my zombie repellent spray! +
# - It's basically the flip of a coin whether a zombie is smart or not

# 22 A practical tool BEST.mp4
install.packages("BEST")
library(BEST)
iq <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 46)
fit <- BESTmcmc(iq)
plot(fit)

# The BEST models and zombies on a diet
# The t-test is a classical statistical procedure used to compare the means of two data sets. In 2013 John Kruschke developed a 
# souped-up Bayesian version of the t-test he named BEST (standing for Bayesian Estimation Supersedes the t-test). Let's try 
# out BEST as implemented in the BEST package.
# Zombies are stupid, but you and your colleagues at the National Zombie Research Laboratory are interested in how diet affects 
# zombie intelligence. You have done a small experiment where you measured the IQ of 10 zombies on a regular diet and 10 zombies 
# on a brain-based diet. The hypothesis is that zombies that eat more brains perform better on IQ tests. To the right, the data 
# from the experiment is put into the variables iq_brains and iq_regular.
# Calculate the mean difference in IQ between the two groups by taking the mean of iq_brains minus the mean of iq_regular. 
# Remember that mean(iq_brains) calculates the sample mean of iq_brains.
# The IQ of zombies on a regular diet and a brain based diet.
iq_brains <- c(44, 52, 42, 66, 53, 42, 55, 57, 56, 51)
iq_regular <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 46)
# Calculate the mean difference in IQ between the two groups
mean(iq_brains) - mean(iq_regular)
# It looks like zombies eating brains have higher IQ, but how sure should we be of this?
# Load in the BEST package using the library() function.
# Fit the BEST model using the BESTmcmc function, like this: BESTmcmc(iq_brains, iq_regular). Assign the output of BESTmcmc to 
# the variable best_posterior.
# Fit the BEST model to the data from both groups
library(BEST)
best_posterior <- BESTmcmc(iq_brains, iq_regular)
# Hey, the model ran! But we're none the wiser…
# Take a look at the posterior estimate of the difference in IQ between the normal and brain-diet zombies by running 
# plot(best_posterior).
# Plot the model result
plot(best_posterior)
# Question
# This plot shows the posterior probability distribution over the difference in means between iq_brains and iq_regular. On top of this you get:
# (1) The mean of the posterior as a "best guess" for the difference.
# (2) A 95% credible interval (called a 95% Highest Density Interval in the plot).
# (3) The amount of probability below and above zero difference.
# What would be a reasonable conclusion to draw from this analysis?
# Possible Answers
# - There is no evidence that eating brains makes zombies smarter.
# - There is some evidence that eating brains makes zombies smarter, but it's uncertain by how much. +
# - There is some evidence that eating brains makes zombies dumber, but it's uncertain by how much.
# - Brain-eating zombies score 8 or more in IQ tests compared to normal zombies.
# Seems reasonable. Turns out it's rational for zombies to like brains. Best to wear a helmet.

# BEST is robust
# The Bayesian model behind BEST assumes that the generative model for the data is a t-distribution; a more flexible 
# distribution than the normal distribution as it assumes that data points might be outliers to some degree. This makes BEST's 
# estimate of the mean difference robust to outliers in the data.
# Assume that a super smart mutant zombie (IQ = 150) got into the iq_regular group by mistake. This might mess up the results 
# as you and your colleagues really were interested in how diet affects normal zombies.
# Replace the last value in iq_regular with 150.
# Again, calculate the difference in means between the two groups using the mean() function, that is, the mean of iq_brains 
# minus the mean of iq_regular.
# The IQ of zombies given a regular diet and a brain based diet.
iq_brains <- c(44, 52, 42, 66, 53, 42, 55, 57, 56, 51)
iq_regular <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 150)
# Modify the data above and calculate the difference in means
mean(iq_brains) - mean(iq_regular)
# Just looking at the difference in sample means, it now seems like there is little to no effect of diet on IQ. Let's see how 
# BEST deals with this outlier mutant zombie.
# Load in the BEST library.
# Fit the model using the BESTmcmc(iq_brains, iq_regular) function and assign the result to best_posterior.
# Plot best_posterior using the plot() function.
# Fit the BEST model to the modified data and plot the result
library(BEST)
best_posterior <- BESTmcmc(iq_brains, iq_regular)
plot(best_posterior)
# Question
# Looking at the plot, we see that the mutant zombie data point has made BEST more uncertain to some degree. But since BEST is robust to outliers, it still estimates that brain-eating zombies are more likely to have a higher IQ than zombies on a regular diet.
# What conclusion should we draw?
# Possible Answers
# - There is zero evidence that eating brains make zombies smarter.
# - There is strong evidence that eating brains make zombies smarter.
# - There is weak evidence that eating brains make zombies smarter. And we should be better at screening for mutant zombies 
#   when doing experiments. +
# There is weak evidence. But we need more data and better screening procedures!


















































































