library(dplyr)
library(ggplot2)

# 1 The prior model.mp4
# Simulating a Beta prior
# Suppose you're running in an election for public office. Let p be your underlying support, the proportion of voters that plan to 
# vote for you. Based on past polls, your prior model of p is captured by a Beta distribution with shape parameters 45 and 55.
# You will approximate the Beta(45, 55) prior using random samples from the rbeta() function. 
# This function takes three arguments: sample size (n) and two shape parameters (shape1,shape2). Subsequently, you will 
# construct a density plot of the samples using ggplot(). This function takes two arguments: the data set containing the samples 
# and, within aes(), the variable to be plotted on the x axis. The density plot layer is added using geom_density().
# Use rbeta() to sample 10,000 draws from Beta(45, 55). Assign the output to prior_A.
# The prior_sim data frame includes the prior_A sample. Apply ggplot() to prior_sim to construct a density plot of the prior samples.
# Sample 10000 draws from Beta(45,55) prior
prior_A <- rbeta(n = 10000, shape1 = 45, shape2 = 55)
# Store the results in a data frame
prior_sim <- data.frame(prior_A)
# Construct a density plot of the prior sample
ggplot(prior_sim, aes(x = prior_A)) + geom_density()
# Great! Take a quick look - the distribution of your sample approximates the features of the Beta(45,55) prior.

# Comparing & contrasting Beta priors
# The Beta(a, b) distribution is defined on the interval from 0 to 1, thus provides a natural and flexible prior for your underlying 
# election support, p. You can tune the Beta shape parameters a and b to produce alternative prior models. Below you will compare 
# your original Beta(45,55) prior with two alternatives: Beta(1, 1) and Beta(100, 100). The original 10,000 prior_A samples drawn 
# from Beta(45,55) are in your workspace.
# Instructions:
# Sample 10,000 draws from the Beta(1,1) prior. Assign the output to prior_B.
# Sample 10,000 draws from the Beta(100,100) prior. Assign the output to prior_C.
# The prior_sim data frame combines the prior_A, prior_B, and prior_C prior samples with a corresponding indicator of the priors. 
# To construct a ggplot() density plot of these 3 separate prior samples on the same frame, specify fill = priors in aes().
# Sample 10000 draws from the Beta(1,1) prior
prior_B <- rbeta(n = 10000, shape1 = 1, shape2 = 1)    
# Sample 10000 draws from the Beta(100,100) prior
prior_C <- rbeta(n = 10000, shape1 = 100, shape2 = 100)
# Combine the results in a single data frame
prior_sim <- data.frame(samples = c(prior_A, prior_B, prior_C), priors = rep(c("A - Beta(45,55)","B - Beta(1,1)","C - Beta(100,100)"), each = 10000))
prior_sim <- data.frame(samples = c(prior_A, prior_B, prior_C), priors = c(rep("A - Beta(45,55)", length(prior_A)),
                                                                           rep("B - Beta(1,1)", length(prior_B)),
                                                                           rep("C - Beta(100,100)", length(prior_C))
                                                                           )
                        )

# Plot the 3 priors
ggplot(prior_sim, aes(x = samples, fill = priors)) + geom_density(alpha = 0.5)
# Nice work! The three Beta priors here are just a few of the countless different priors we can obtain by tuning the shape parameters.
# Which prior?
# The density plots below illustrate 3 potential prior models for p, the underlying proportion of voters that plan to vote for 
# you. Prior A reflects your original Beta(45,55) prior. In what scenarios would Prior B (Beta(1,1)) or Prior C (Beta(100,100)) 
# be more appropriate?
# Possible Answers
# - Prior C reflects more precise and optimistic prior information about your chances of winning the election. Prior B reflects 
#   a lack of specific prior information. +
# - Prior B reflects more precise and optimistic prior information about your chances of winning the election. Prior C reflects a lack of specific prior information.
# - Priors A, B, C reflect similar prior information about p, thus are equally appropriate in any scenario
# Exactly! Prior B reflects 'vague' prior information about p - it gives equal prior weight to all values of p between 0 and 1. 
# Prior C reflects more prior certainty about p - it has less spread and is centered around a mean that's greater than that 
# for Prior A.
c(
  rep('a', 4), 
  rep('b', 6), 
  rep('c', 8)
)

# Simulating the dependence of X on p
# In your quest for election to public office, your campaign polls 10 likely voters. Let X be the number that support you. 
# Of course, X varies from sample to sample and depends upon p, your underlying support in the broader population. Since X 
# is a count of successes in 10 independent trials, each having probability of success p, you can model its dependence on p 
# by the Binomial distribution: Bin(10, p).
#  You will simulate the Binomial model using random samples from the rbinom(n, size, prob) function. This vectorized function 
# draws n samples from a Bin(size, prob) distribution. Given a vector of prob values, the first prob value will be used for 
# the first draw, the second prob value will be used for the second draw, etc.
# Instructions
# Define a seq() of 1000 possible values of p that range from 0 to 1. Store this as p_grid.
# Use rbinom() to simulate one poll result  X for each of the 1000 p in p_grid. Assign these to poll_result. The likelihood_sim 
# data frame combines p_grid and poll_result. Use ggplot() with a geom_density_ridges() layer to illustrate the distribution 
# of p_grid values (x axis) from which each poll_result was simulated (y axis).
# Define a vector of 1000 p values    
p_grid <- seq(from = 0, to = 1, length.out = 1000)
# Simulate 1 poll result for each p in p_grid   
poll_result <- rbinom(n = 1000, size = 10, prob = p_grid)
# Create likelihood_sim data frame
likelihood_sim <- data.frame(p_grid, poll_result)    
# Density plots of p_grid grouped by poll_result
install.packages("ggridges")
library(ggridges)
ggplot(likelihood_sim, aes(x = p_grid, y = poll_result, group = poll_result)) + 
  geom_density_ridges()
?geom_density_ridges()
# Great! Notice that polls in which 0 people supported you (poll_result = 0) correspond to smaller values of underlying support p 
# (p_grid). The opposite is true for polls in which all 10 people supported you.

# The first election poll is in! 
# X = 6 of 10 polled voters plan to vote for you. You can use these data to build insight into your underlying support p. To this 
# end, you will use the likelihood_sim data frame (in your workspace). This contains the values of X (poll_result) simulated 
# from each of 1,000 possible values of p between 0 to 1 (p_grid).
# The ggplot() here constructs the distribution of p from which each possible outcome of X was generated. Modify this code, 
# supplying a fill condition in order to highlight the distribution which corresponds to your observed poll_result, X = 6. 
# This provides insight into which values ofp are the most compatible with your observed poll data!
# Note: do not wrap this condition in parentheses ().
# Density plots of p_grid grouped by poll_result
ggplot(likelihood_sim, aes(x = p_grid, y = poll_result, group = poll_result, fill = poll_result == 6)) + 
  geom_density_ridges()
# Great! Reexamine the highlighted density plot. This is a scaled approximation of the likelihood function! It indicates that 
# the simulated surveys in which 6 of 10 voters supported you corresponded to underlying support p that ranged from approximately 
# 0.25 to 1, with p around 0.6 being the most common.

# Interpreting the likelihood function
# In the previous exercise you approximated the likelihood function shown below. The likelihood highlights the relative compatibility 
# of different possible values of p, your underlying election support, with the observed poll in which X = 6 of n = 10 voters supported you. 
# Specifically, the height of the likelihood function at any given value of p reflects the relative plausibility of observing these 
# particular polling data if your underlying support were equal to p. Thus which of the following two scenarios is more compatible with 
# the poll data?
# Scenario 1: your underlying support p is around 45%.
# Scenario 2: your underlying support p is around 55%. +
# That's right! The likelihood of observing X = 6 is greater when p = 0.55 than when p = 0.45
prob_p_grid <- dbinom(x = 6, size = 10, prob = p_grid)
prob_sim <- tibble(prob_p_grid = prob_p_grid, p_grid = p_grid)
ggplot(prob_sim, aes(x = p_grid, y = prob_p_grid)) + geom_point()

# Define, compile, and simulate
# In your election quest, let p
# be the proportion of the underlying voting population that supports you. Built from previous polls & election data, your prior model of 
# p is a Beta(a, b) with shape parameters a = 45 and b = 55. For added insight into p, you also polled n potential voters. The dependence 
# of X, the number of these voters that support you, on p is modeled by the Bin(n, p) distribution. In the completed poll, X = 6 of n = 10 
# voters supported you. The next goal is to update your model of p in light of these observed polling data! To this end, you will use the 
# rjags package to approximate the posterior model of p. We break this exercise down into the 3 rjags steps: define, compile, simulate.
# Define the Bayesian model:
# Specify that the likelihood model of X is Bin(n, p) and that the p prior is Beta(a, b). In rjags syntax, these are specified by dbin(p, n) 
# and dbeta(a, b), respectively.Store this model string as vote_model.
# DEFINE the model
library(rjags)
vote_model <- "model{
    # Likelihood model for X
    X ~ dbin(p, n)
    
    # Prior model for p
    p ~ dbeta(a, b)
}"
# COMPILE the model    
vote_jags <- jags.model(textConnection(vote_model), 
                        data = list(a = 45, b = 55, X = 6, n = 10),
                        inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 100))
# SIMULATE the posterior
vote_sim <- coda.samples(model = vote_jags, variable.names = c("p"), n.iter = 10000)
# PLOT the posterior
plot(vote_sim, trace = FALSE)
# Nice work! You've successfully defined, compiled, and simulated your Bayesian model. Notice that after observing a poll in which 6 of 10 
# (60%) of voters supported you, your updated posterior optimism and certainty about your underlying support, p, are slightly higher than 
# they were prior to the poll.

# Updating the posterior
# The posterior model of your underlying election support p is informed by both the prior model of p and polling data X. Run the script to the 
# right to remind yourself of the posterior that evolved from your original prior (Beta(45, 55)) and original poll data ( X = 6 of n = 10 
# polled voters support you). The defined vote_model is in your workspace.
# In a 3-step exercise, you will explore how using a different prior model or observing new data (or a combination of the two!) might impact 
# the posterior.
# Re-compile, simulate, and plot the p posterior to reflect the setting in which you start with a Beta(1,1) prior but observe the same polling 
# data (X = 6, n = 10). NOTE: Recall that Beta(1,1) is uniform across the (0,1) interval.
# COMPILE the model    
vote_jags <- jags.model(textConnection(vote_model), 
                        data = list(a = 1, b = 1, X = 6, n = 10),
                        inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 100))
# SIMULATE the posterior
vote_sim <- coda.samples(model = vote_jags, variable.names = c("p"), n.iter = 10000)
# PLOT the posterior
plot(vote_sim, trace = FALSE, xlim = c(0,1), ylim = c(0,18))

# In a new poll, 214 of 390 voters support you. Combined with the first poll, a total X = 220 of n = 400 voters support you. Re-compile, 
# simulate, and plot the p posterior to reflect these combined poll results and a Beta(1,1) prior.
# COMPILE the model    
vote_jags <- jags.model(textConnection(vote_model), 
                        data = list(a = 1, b = 1, X = 220, n = 400),
                        inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 100))
# SIMULATE the posterior
vote_sim <- coda.samples(model = vote_jags, variable.names = c("p"), n.iter = 10000)
# PLOT the posterior
plot(vote_sim, trace = FALSE, xlim = c(0,1), ylim = c(0,18))

# Finally, re-compile, simulate, and plot the p posterior to reflect the combined poll results (X = 220 of n = 400) and your original 
# Beta(45,55) prior.
# COMPILE the model    
vote_jags <- jags.model(textConnection(vote_model), 
                        data = list(a = 45, b = 55, X = 220, n = 400),
                        inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 100))
# SIMULATE the posterior
vote_sim <- coda.samples(model = vote_jags, variable.names = c("p"), n.iter = 10000)
# PLOT the posterior
plot(vote_sim, trace = FALSE, xlim = c(0,1), ylim = c(0,18))
# Nice work! Re-visit the plots you constructed throughout this exercise to review the impact of the prior and data on the posterior.

# Influence of the prior & data on the posterior
# Examine the density plots below. The first set of density plots illustrates 2 potential priors for your underlying support p. 
# The other 2 sets of density plots illustrate the posterior models for p that evolve from these different priors and different 
# polling data X and n. Which of the following best describes the combined influence of the prior and data on the posterior?
# http://joxi.ru/gmvLRvgCen8E4A
# Possible Answers
# - Even in light of the same data, different priors lead to different posteriors.
# - The influence of the prior on the posterior diminishes as the sample size of your data increases.
# - As sample size increases, your posterior understanding becomes more precise.
# - All of the above. +
# AK: when we have little data it's more important to choose correct prior!
# Excellent! The posterior combines insight from both the data and prior, thus is influenced by the features of both.

# Normal-Normal priors
# Researchers developed a test to evaluate the impact of sleep deprivation on reaction time. For subject i, 
# let Yi be the change in reaction time (in ms) after 3 sleep deprived nights. Of course, people react 
# differently to sleep deprivation. It's reasonable to assume that Y i are Normally distributed around 
# some average m with standard deviation s: Yi ∼ N(m, s^2). 
# In the first step of your Bayesian analysis, you'll simulate the following prior models for parameters m 
# and s: m ∼ N(50, 25^2) and s ∼ Unif(0, 200). This requires the rnorm(n, mean, sd) and runif(n, min, max) 
# functions.
# Use rnorm(n, mean, sd) to sample 10,000 draws from the m prior. Assign the output to prior_m. Use 
# runif(n, min, max) to sample 10,000 draws from the s prior. Assign the output to prior_s. After 
# storing these results in the samples data frame, construct a density plot of the prior_m samples 
# and a density plot of the prior_s samples.
# Take 10000 samples from the m prior
prior_m <- rnorm(n = 10000, mean = 50, sd = 25)
# Take 10000 samples from the s prior    
prior_s <- runif(n = 10000, min = 0, max = 200)
# Store samples in a data frame
samples <- data.frame(prior_m, prior_s)
# Density plots of the prior_m & prior_s samples    
ggplot(samples, aes(x = prior_m)) + geom_density()
ggplot(samples, aes(x = prior_s)) + geom_density()
# Right! The distributions of these random samples approximate the features of your Normal prior for m and Uniform prior for s.

# Sleep study data
# Researchers enrolled 18 subjects in a sleep deprivation study. Their observed sleep_study data are loaded 
# in the workspace. These data contain the day_0 reaction times and day_3 reaction times after 3 sleep deprived 
# nights for each subject.
# You will define and explore diff_3, the observed difference in reaction times for each subject. This 
# will require the mutate() & summarize() functions. For example, the following would add variable day_0_s, 
# day_0 reaction times in seconds, to sleep_study:
#  sleep_study <- sleep_study %>% 
#  mutate(day_0_s = day_0 * 0.001)
# You can then summarize() the day_0_s values, here by their minimum & maximum:
#  sleep_study  %>% 
#  summarize(min(day_0_s), max(day_0_s))
# Instructions
# Check out the first 6 rows of sleep_study.
# Define a new sleep_study variable diff_3, the day_3 minus the day_0 reaction times.
# Use ggplot() with a geom_histogram() layer to construct a histogram of the diff_3 data.
# summarize() the mean and standard deviation of the diff_3 observations.
# Check out the first 6 rows of sleep_study
head(sleep_study,6)
# Define diff_3
sleep_study <- sleep_study %>% 
  mutate(diff_3 = day_3 - day_0)
# Histogram of diff_3    
ggplot(sleep_study, aes(x = diff_3)) + geom_histogram(binwidth = 20, color = "white")
# Mean and standard deviation of diff_3
sleep_study %>% summarize(mean(diff_3), sd(diff_3))
# Great work! Reaction times increased by an average of ~26 ms with a standard deviation of ~37 ms. Further, 
# only 4 of the 18 test subjects had faster reaction times on day 3 than on day 0.

# Insights from the prior and data
# Your prior & (scaled) likelihood models for m, the average change in reaction time, are shown together here. 
# The prior reflects information we had about m prior to running the sleep study. The likelihood reflects 
# the relative compatibility of different m values with the data observed on the 18 sleep study subjects. 
# Which of the following best describes the insights from the prior and likelihood?
# http://joxi.ru/eAOZqY1CvDdEp2
# Possible Answers
# - The prior model and likelihood provide conflicting insights about the impact of sleep deprivation on reaction time - the data suggest that reaction times decrease whereas the prior suggests they increase.
# - The prior model and likelihood are both consistent with the hypothesis that, on average, reaction times likely decrease under sleep deprivation.
# - The prior model and likelihood are both consistent with the hypothesis that, on average, reaction times likely increase under sleep deprivation. +
# Right! Though not in perfect agreement about the degree to which the average reaction time changes under 
# sleep deprivation, both the likelihood and prior are consistent with the hypothesis that the average 
# increases relative to reaction time under normal sleep conditions.

# Define, compile, & simulate the Normal-Normal
# Upon observing the change in reaction time Yi for each of the 18 subjects i enrolled in the sleep study, 
# you can update your posterior model of the effect of sleep deprivation on reaction time. This requires 
# the combination of insight from the likelihood and prior models: likelihood: Yi ∼ N(m, s^2) 
# priors: m ∼ N(50, 25^2) and s ∼ Unif(0, 200)
# In this series of exercises, you'll define, compile, and simulate your Bayesian posterior. The observed 
# sleep_study data are in your work space.
# Instructions
# DEFINE your Bayesian model and store the model string as sleep_model. In doing so, note that: dnorm(a, b) 
# defines a N(a, b^−1) model with precision (ie. inverse variance) b.dunif(a,b) defines a Unif(a, b) model. 
# The model of Yi depends upon m and s. The number of subjects i is defined by length(Y).
# DEFINE the model    
sleep_model <- "model{
    # Likelihood model for Y[i]
    for(i in 1:length(Y)) {
        Y[i] ~ dnorm(m, s^(-2))
    }

    # Prior models for m and s
    m ~ dnorm(50, 25^(-2))
    s ~ dunif(0, 200)
}"
# COMPILE sleep_model using jags.model():
# Establish a textConnection() to sleep_model and provide the observed vector of Y[i] data from sleep_study. 
# (Ignore inits for now!)
# Store the output in a jags object named sleep_jags.
# COMPILE the model (didn't work)
sleep_jags <- jags.model(
  textConnection(sleep_model),
  data = list(Y = sleep_study$diff_3),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989)
)
# COMPILE the model (worked)
sleep_jags <- jags.model(
  file = textConnection('model{
    # Likelihood model for Y[i]
    for(i in 1:length(Y)) {
        Y[i] ~ dnorm(m, s^(-2))
    }

    # Prior models for m and s
    m ~ dnorm(50, 25^(-2))
    s ~ dunif(0, 200)
}'), 
  data = list(Y = sleep_study$diff_3), 
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989)
)
# SIMULATE the posterior    
sleep_sim <- coda.samples(model = sleep_jags, variable.names = c("m", "s"), n.iter = 10000)
# PLOT the posterior    
plot(sleep_sim, trace = FALSE)
# Nice work! You've successfully defined, compiled, and simulated your Bayesian Normal-Normal model.

# Posterior insights on sleep deprivation
# Your prior, (scaled) likelihood, and posterior models of m, the average change in reaction time after 3 
# sleep deprived nights, are shown here. Which of the following best describes the evolution in your insight 
# about m from the prior to the posterior in light of the observed data (likelihood)?
# http://joxi.ru/BA0yd0vu1ZoNQr
# - You're less certain about whether average reaction time increases or decreases.
# - You're more certain that average reaction time increases but that the magnitude of the increase is less than assumed prior to observing the data. +
# - You're more certain that average reaction time increases and that the magnitude of the increase is larger than assumed prior to observing the data.
# Exactly! Your posterior model is more narrow and lies almost entirely above 0, thus you're more confident that 
# the average reaction time increases under sleep deprivation. Further, the location of the posterior is below 
# that of the prior. This reflects the strong insight from the observed sleep study data in which the increase 
# in average reaction time was only ~26 ms.

# Storing Markov chains
# Let m be the average change in reaction time after 3 days of sleep deprivation. In a previous exercise, you 
# obtained an approximate sample of 10,000 draws from the posterior model of m. You stored the resulting mcmc.
# list object as sleep_sim which is loaded in your workspace:
# sleep_sim <- coda.samples(model = sleep_jags, variable.names = c("m", "s"), n.iter = 10000)
# In fact, the sample of m values in sleep_sim is a dependent Markov chain, the distribution of which converges 
# to the posterior. You will examine the contents of sleep_sim and, to have finer control over your analysis, 
# store the contents in a data frame.
# Instructions
# Check out the head() of the sleep_sim list object. 
# The first sleep_sim list item contains the m and  s chains. Store these in a data frame named sleep_chains. 
# Include a variable iter that records the corresponding iteration number, 1:10000, for each chain element. 
# Check out the first 6 rows of sleep_chains.
# Check out the head of sleep_sim
head(sleep_sim)
sleep_sim[[1]][1]
# Store the chains in a data frame
sleep_chains <- data.frame(sleep_sim[[1]], iter = 1:10000)
# Check out the head of sleep_chains
head(sleep_chains)
# Great! Next, you'll visualize the contents of these Markov chains.

# Markov chain trace plots
# A trace plot provides a visualization of a Markov chain's longitudinal behavior. Specifically, a trace plot for 
# the m chain plots the observed chain value (y-axis) against the corresponding iteration number (x-axis).
# You will construct trace plots of the m chain using two different approaches: by applying the built-in plot() 
# function to the mcmc.list object sleep_sim and, for finer control over this graphic (and finer control over 
# analyses in later chapters), by applying ggplot() to the data.frame object sleep_chains. Both sleep_sim and 
# sleep_chains are in your workspace:
#   sleep_sim <- coda.samples(model = sleep_jags, variable.names = c("m", "s"), n.iter = 10000)
#   sleep_chains <- data.frame(sleep_sim[[1]], iter = 1:10000)
# Instructions:
# Apply plot() to sleep_sim with density = FALSE to construct trace plots for the m and s chains. NOTE: The 
# 10,000 recorded Iterations start after a "burn-in" period in which samples are discarded. Thus the 
# Iterations count doesn't start at 1! Apply ggplot() with a geom_line() layer to sleep_chains to re-construct 
# the trace plot of the m chain. Zoom in: construct a ggplot() trace plot of the first 100 iterations of 
# the m chain.
# Use plot() to construct trace plots of the m and s chains
plot(sleep_sim, density = FALSE) # http://joxi.ru/gmvLRvgCenJvDA
head(sleep_chains)
# Use ggplot() to construct a trace plot of the m chain
ggplot(sleep_chains, aes(x = iter, y = m)) + geom_line() # http://joxi.ru/BA0yd0vu1ZKPor
# Trace plot the first 100 iterations of the m chain
ggplot(sleep_chains[0:100,], aes(x = iter, y = m)) + geom_line() # http://joxi.ru/5mdlkYMTq0ZeDm
# Nice work! Note that the longitudinal behavior of the chain appears quite random and that the trend remains 
# relatively constant. This is a good thing - it indicates that the Markov chain (likely) converges quickly to 
# the posterior distribution of m.

# Markov chain density plots
# Whereas a trace plot captures a Markov chain's longitudinal behavior, a density plot illustrates the final 
# distribution of the chain values. In turn, the density plot provides an approximation of the posterior model. 
# You will construct and examine density plots of them Markov chain below. The mcmc.list object sleep_sim and 
# sleep_chains data frame are in your workspace:
#   sleep_sim <- coda.samples(model = sleep_jags, variable.names = c("m", "s"), n.iter = 10000)
#   sleep_chains <- data.frame(sleep_sim[[1]], iter = 1:10000)
# Apply plot() to sleep_sim with trace = FALSE to construct density plots for the m and s chains.
# Apply ggplot() to sleep_chains to re-construct a density plot of the m chain.
# Use plot() to construct density plots of the m and s chains
plot(sleep_sim, trace = F) # http://joxi.ru/1A5qbxpc4JWa9A
head(sleep_chains)
#          m        s iter
# 1 17.25796 31.46256    1
# 2 34.58469 37.88655    2
# 3 36.45480 39.58056    3
# 4 25.00971 39.69494    4
# 5 29.95475 35.90001    5
# 6 28.43894 37.46466    6
# Use ggplot() to construct a density plot of the m chain
ggplot(sleep_chains, aes(x = m)) + geom_density() # http://joxi.ru/5mdlkYMTq05Wym
# Check it out! These density plots approximate the posterior models of m and s.

# 7 Markov chain diagnostics & reproducibility.mp4
# Multiple chains
# Trace plots help us diagnose the quality of a Markov chain simulation. A "good" Markov chain will exhibit stability as the chain length increases and consistency across repeated simulations, or multiple chains. You will use RJAGS to run and construct trace plots for four parallel chains below. The defined sleep_model is in your workspace.
# Instructions
# Use jags.model() to COMPILE sleep_model and initialize 4 parallel chains. Store the output in a jags object named sleep_jags_multi.
# SIMULATE a sample of 1,000 draws from the posterior model of m and s. Store this mcmc.list in sleep_sim_multi.
# Check out the head() of sleep_sim_multi. Note the 4 list items containing the 4 parallel chains.
# Use plot() to construct trace plots for the multiple chains. Suppress the density plots.
# COMPILE the model
sleep_jags_multi <- jags.model(textConnection(sleep_model), data = list(Y = sleep_study$diff_3), n.chains = 4)   
sleep_model
# SIMULATE the posterior    
sleep_sim_multi <- coda.samples(model = sleep_jags_multi, variable.names = c("m", "s"), n.iter = 1000)
# Check out the head of sleep_sim_multi
head(sleep_sim_multi)
# [[1]]
# Markov Chain Monte Carlo (MCMC) output:
#   Start = 1001 
# End = 1007 
# Thinning interval = 1 
# m        s
# [1,] 25.09114 40.31183
# [2,] 28.25678 35.69511
# [3,] 29.15563 34.70137
# [4,] 39.31957 43.49014
# [5,] 39.34107 35.07370
# [6,] 40.87822 39.51125
# [7,] 23.27865 33.95734
# [[2]]
# Markov Chain Monte Carlo (MCMC) output:
#   Start = 1001 
# End = 1007 
# Thinning interval = 1 
# m        s
# [1,] 32.77292 40.28647
# [2,] 16.98794 42.08350
# [3,] 18.27248 38.30303
# [4,] 43.91044 40.95723
# [5,] 41.93223 48.98346
# [6,] 36.32960 34.46202
# [7,] 21.62053 32.80040
# [[3]]
# Markov Chain Monte Carlo (MCMC) output:
#   Start = 1001 
# End = 1007 
# Thinning interval = 1 
# m        s
# [1,] 45.84651 40.54377
# [2,] 38.51897 46.14603
# [3,] 14.70668 52.55889
# [4,] 27.58105 29.82151
# [5,] 28.85706 24.74147
# [6,] 13.44038 65.46654
# [7,] 23.33012 67.60403
# [[4]]
# Markov Chain Monte Carlo (MCMC) output:
#   Start = 1001 
# End = 1007 
# Thinning interval = 1 
# m        s
# [1,] 36.97889 29.53560
# [2,] 27.92829 36.72924
# [3,] 25.50912 35.85252
# [4,] 35.44511 39.15338
# [5,] 20.61251 33.52648
# [6,] 36.18543 34.10972
# [7,] 29.66876 31.88005
# attr(,"class")
# [1] "mcmc.list"
# Construct trace plots of the m and s chains
plot(sleep_sim_multi, density = F) # http://joxi.ru/12ML1YjCg1OZGm
# Good work! The most important thing to notice here is the similarity and stability among the 4 parallel chains. 
# This provides some reassurance about the quality and consistency of our Markov chain simulation.

# Naive standard errors
# The mean of the m Markov chain provides an estimate of the posterior mean of m. The naive standard error 
# provides a measure of the potential error in this estimate. In turn, we can use this measure to determine an appropriate chain length. For example, suppose your goal is to estimate the posterior mean of m within a standard error of 0.1 ms. If your observed naive standard error exceeds this target, no problem! Simply run a longer chain - the error in using a Markov chain to approximate a posterior tends to decrease as chain length increases.
# The defined sleep_model and compiled sleep_jags object are your workspace.
# Instructions
# SIMULATE 1,000 draws from the posterior model of m and s. Store these in sleep_sim_1.
# Obtain a summary() of the sleep_sim_1 chains.
# If the naive standard error of the m chain exceeds the 0.1 target, adjust your simulation: try using either 
# 500 draws or 10,000 draws (instead of 1,000). Store the results in sleep_sim_2.
# Obtain a summary() of the sleep_sim_2 chains. Confirm that your new simulation meets the criterion. If not, 
# return to the previous step & repeat!
# SIMULATE the posterior    
sleep_sim_1 <- coda.samples(model = sleep_jags, variable.names = c("m", "s"), n.iter = 1000)
sleep_jags
# JAGS model:
# model{
# Likelihood model for Y[i]
#   for(i in 1:length(Y)) {
#     Y[i] ~ dnorm(m, s^(-2))
#   }
# Prior models for m and s
#   m ~ dnorm(50, 25^(-2))
#   s ~ dunif(0, 200)
# }
# Fully observed variables: Y 
# Summarize the m and s chains of sleep_sim_1
summary(sleep_sim_1)
# Iterations = 2501:3500
# Thinning interval = 1 
# Number of chains = 1 
# Sample size per chain = 1000 
# 1. Empirical mean and standard deviation for each variable, plus standard error of the mean:
#    Mean    SD Naive SE Time-series SE
# m 29.16 8.940   0.2827         0.2701
# s 40.12 7.335   0.2320         0.3463
# 2. Quantiles for each variable:
#    2.5%   25%   50%   75% 97.5%
# m 10.69 23.33 29.15 34.95 47.31
# s 28.51 35.23 39.36 43.82 57.75
# RE-SIMULATE the posterior    
sleep_sim_2 <- coda.samples(model = sleep_jags, variable.names = c("m", "s"), n.iter = 500)
# Summarize the m and s chains of sleep_sim_2
summary(sleep_sim_2)
# Iterations = 3501:4000
# Thinning interval = 1 
# Number of chains = 1 
# Sample size per chain = 500 
# 1. Empirical mean and standard deviation for each variable, plus standard error of the mean:
#    Mean    SD Naive SE Time-series SE
# m 29.64 8.909   0.3984         0.4398
# s 39.58 7.045   0.3150         0.4520
# 2. Quantiles for each variable:
#    2.5%   25%   50%   75% 97.5%
# m 13.29 23.90 28.98 34.99 50.64
# s 28.66 34.14 38.73 43.71 55.23
# No problem! You've proved to yourself that if the standard errors associated with your Markov chain are too big, 
# simply increase the number of iterations. In general, naive standard error will decrease as the chain length 
# increases.

# Reproducibility
# Now that you've completed (and passed!) some Markov chain diagnostics, you're ready to finalize your RJAGS 
# simulation. To this end, reproducibility is crucial. To obtain reproducible simulation output, you must set 
# the seed of the RJAGS random number generator. This works differently than in base R. Instead of using 
# set.seed(), you will specify a starting seed using 
# inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = ___) when you compile your model.
# Instructions
# Run the provided code a few times. Notice that the summary() statistics change each time.
# For reproducible results, supply the random number generator inits to jags.model(). Specify a starting seed 
# of 1989.
# Run the new code a few times. Notice that the summary() statistics do NOT change!
# COMPILE the model
sleep_jags <- jags.model(textConnection(sleep_model), data = list(Y = sleep_study$diff_3), inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed=1989)) 
# SIMULATE the posterior    
sleep_sim <- coda.samples(model = sleep_jags, variable.names = c("m", "s"), n.iter = 10000)
# Summarize the m and s chains of sleep_sim
summary(sleep_sim)
# Excellent work. Now that you can diagnose and reproduce your Markov chain simulations, you're ready for the next 
# Bayesian model!

#
# Regression priors
# Let Yi be the weight (in kg) of subject i. Past studies have shown that weight is linearly related to height 
# Xi (in cm). The average weight mi among adults of any shared height Xi can be written as mi = a + bXi. But 
# height isn't a perfect predictor of weight - individuals vary from the trend. To this end, it's reasonable 
# to assume that Yi are Normally distributed around mi with residual standard deviation s: Yi ∼ N(mi, s^2). 
# Note the 3 parameters in the model of weight by height: intercept a, slope b, & standard deviation s. In 
# the first step of your Bayesian analysis, you will simulate the following prior models for these parameters: 
# a ∼ N(0,200^2), b ∼ N(1,0.52), and s ∼ Unif(0,20).
# Instructions
# Sample 10,000 draws from each of the a, b, and s priors. Assign the output to a, b, and s. These are 
# subsequently combined in the samples data frame along with set = 1:10000, an indicator of the draw numbers. 
# Construct separate density plots of each of the a, b, and s samples.
# Take 10000 samples from the a, b, & s priors
a <- rnorm(n = 10000, mean = 0, sd = 200)
b <- rnorm(n = 10000, mean = 1, sd = 0.5)
s <- runif(n = 10000, min = 0, max = 20)
# Store samples in a data frame
samples <- data.frame(set = 1:10000, a, b, s)
head(samples)
#   set          a           b          s
# 1   1   76.60869 1.645611331 18.8286616
# 2   2  296.44934 0.923454357  7.4092863
# 3   3  123.13936 0.882344467  0.9347714
# 4   4  143.46733 0.003128991 16.5828444
# 5   5  253.59725 1.361153508 15.1645130
# 6   6 -102.67422 0.688204015  2.8365200
# Construct density plots of the prior samples    
ggplot(samples, aes(x = a)) + geom_density()
ggplot(samples, aes(x = b)) + geom_density()
ggplot(samples, aes(x = s)) + geom_density()
# Great work! These simulations approximate your prior models of each separate model parameter. There's likely 
# a positive association between weight & height (b > 0) but more uncertainty about the intercept a. Further, 
# at any given height, the typical deviation of individuals' weights from the trend is equally likely to be 
# anywhere between 0 and 20 kg.

# Visualizing the regression priors
# In the previous exercise, you simulated 10,000 samples for each parameter (a, b, s) in the Bayesian regression 
# model of weight Y by height X: Y ∼ N(m, s^2) with mean m = a + bX. The set of a, b, and s values in each row 
# of samples represents a prior plausible regression scenario. To explore the scope of these prior scenarios, 
# you will simulate 50 pairs of height and weight values from each of the first 12 sets of prior parameters 
# a, b, and s.
# Instructions
# Create a data frame prior_simulation which includes n = 50 replicates of the first 12 sets of prior parameters 
# in samples (600 rows in total!).
# For each of the 600 prior_simulation rows:
# Simulate a height value from a N(170, 10^2) model. Simulate a weight value from N(a + bX, s^2) where X is 
# height and (a,b,s) are the prior parameter set. You now have 50 simulated height and weight pairs for each 
# of the 12 parameter sets. Use ggplot() to construct a scatterplot of these 50 pairs for each set of parameter 
# values. Be sure to put weight on the y-axis!
# Replicate the first 12 parameter sets 50 times each
prior_scenarios_rep <- bind_rows(replicate(n = 50, expr = samples[1:12, ], simplify = FALSE)) 
dim(samples)
samples[1:12, ]
prior_scenarios_rep
# Simulate 50 height & weight data points for each parameter set
prior_simulation <- prior_scenarios_rep %>% 
  mutate(height = rnorm(n = 600, mean = 170, sd = 10)) %>% 
  # mutate(height = rep(x = 170, times = 600)) %>%
  mutate(weight = rnorm(n = 600, mean = a + b * height, sd = s))
# Plot the simulated data & regression model for each parameter set
ggplot(prior_simulation, aes(x = height, y = weight)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, size = 0.75) + 
  facet_wrap(~ set)
# Exciting! These 12 plots demonstrate the range of prior plausible models. These models have different 
# intercepts, slopes, and residual standard deviations. Almost all of the models have positive slopes, 
# demonstrating the prior information that there is likely a positive association between weight & height. 
# Given your vague prior for a, some of these models are even biologically impossible!
# http://joxi.ru/12ML1YjCg1dbQm

install.packages("openintro")
library(openintro)
head(bdims)
summary(bdims)
str(bdims)
# Weight & height data
# The bdims data set from the openintro package is loaded in your workspace. bdims contains physical 
# measurements on a sample of 507 individuals, including their weight in kg (wgt) and height in cm (hgt). 
# You will use these data to build insights into the relationship between weight and height.
# Instructions
# Construct a scatterplot of wgt (y-axis) vs hgt (x-axis) using ggplot() with a geom_point() layer.
# Construct a scatterplot of wgt vs hgt which includes a geom_smooth() of the linear relationship between 
# these 2 variables.
# Construct a scatterplot of wgt vs hgt
ggplot(bdims, aes(x = hgt, y = wgt)) + geom_point()
# Add a model smooth
ggplot(bdims, aes(x = hgt, y = wgt)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# Nice work. These data support your prior information about a positive association between weight and height. 
# With insights from the priors and data in place, you're ready to simulate the posterior regression model in RJAGS!

# Insights from observed weight and height data
wt_model <- lm(wgt ~ hgt, data = bdims)
coef(wt_model)
summary(wt_model)
summary(wt_model)$sigma

# 9 Bayesian regression in RJAGS.mp4
# Define, compile, & simulate the regression model
# Upon observing the relationship between weight Yi and height Xi for the 507 subjects i in the bdims data set, 
# you can update your posterior model of this relationship. To build your posterior, you must combine your insights from the likelihood and priors: likelihood: Yi ∼ N(mi, s^2) where mi = a + bXi priors: a ∼ N(0, 200^2), b ∼ N(1, 0.5^2) and s ∼ Unif(0, 20)
# In this series of exercises, you'll define, compile, and simulate your Bayesian regression posterior. The bdims 
# data are in your work space.
# Instructions
# DEFINE your Bayesian model.
# Define the likelihood model of Y[i] given m[i] and s where m[i] <- a + b * X[i].
# Specify the priors for a, b, and s.
# Store the model string as weight_model.
# DEFINE the model    
weight_model <- "model{
    # Likelihood model for Y[i]
    for (i in 1:length(Y)) {
        Y[i] ~ dnorm(m[i], s^(-2))
        m[i] <- a + b * X[i]
    }
    # Prior models for a, b, s
    a ~ dnorm(0, 200^(-2))
    b ~ dnorm(1, 0.5^(-2))
    s ~ dunif(0, 20) 
}"

# COMPILE weight_model using jags.model():
# Establish a textConnection() to weight_model.
# Provide the observed vectors of Y and X data from bdims.
# Store the output in a jags object named weight_jags.
# COMPILE the model
weight_jags <- jags.model(
  textConnection(weight_model),
  data = list(Y = bdims$wgt, X = bdims$hgt),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989)
)
# to see the unstability of the Markov chain
weight_jags_4 <- jags.model(
  textConnection(weight_model),
  data = list(Y = bdims$wgt, X = bdims$hgt),
  #inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989),
  n.chains = 4
)
# SIMULATE a sample of 1,000 draws from the posterior model of a, b, and s. Store this mcmc.list as weight_sim.
# Use plot() to construct trace and density plots of the posterior samples in weight_sim.
# SIMULATE the posterior    
weight_sim <- coda.samples(model = weight_jags, variable.names = c('a', 'b', 's'), n.iter = 1000)
weight_sim_4 <- coda.samples(model = weight_jags_4, variable.names = c('a', 'b', 's'), n.iter = 1000)
# PLOT the posterior    
plot(weight_sim)
plot(weight_sim_4)
# Yuck! You've successfully defined, compiled, and simulated your Bayesian regression model. But the results 
# don't look that great. Let's fix that.

# Regression Markov chains
# In the previous exercise, you ran 4 parallel Markov chains of length 1,000 to approximate the 
# posterior model of regression parameters a, b, and s. Your output was similar to that below:
# http://joxi.ru/DmBVXLDiqWpOBm
# Though it requires too much computing power to run here, the results of a new RJAGS simulation 
# run for 100,000 iterations is in your workspace. This mcmc.list object is stored as weight_sim_big. 
# Construct and examine a plot() of the Markov chains in weight_sim_big. Which of the following best 
# describes the comparison of these with the original simulation results?
weight_sim_big <- coda.samples(model = weight_jags, variable.names = c('a', 'b', 's'), n.iter = 100000)
plot(weight_sim_big)
# Possible Answers
# - The Markov chains of length 1,000 are too short. They have not stabilized, thus are unlikely to provide a reliable 
# approximation of the posterior.
# - The Markov chains appear to stabilize after 100,000 iterations, thus provide a more reliable approximation of the posterior. 
# - Both of the above +
# Right! Trace plots indicate that after only 1,000 iterations, the a and b parallel chains had not stabilized. 
# However, after 100,000 iterations, the chains demonstrate greater stability. We might also increase the stability 
# of our simulation by standardizing the height data, but this is beyond the scope of our current discussion.

# Posterior point estimates
# Recall the likelihood of the Bayesian regression model of weight Y by height X: Y ∼ N(m, s^2) where m = a + bX. 
# A 100,000 iteration RJAGS simulation of the posterior, weight_sim_big, is in your workspace along with a data 
# frame of the Markov chain output:
# > head(weight_chains, 2)
# a        b        s iter
# 1 -113.9029 1.072505 8.772007    1
# 2 -115.0644 1.077914 8.986393    2
# The posterior means of the intercept & slope parameters, a & b, reflect the posterior mean trend in the relationship 
# between weight & height. In contrast, the full posteriors of a & b reflect the range of plausible parameters, thus 
# posterior uncertainty in the trend. You will examine the trend and uncertainty in this trend below. The bdims 
# data are in your workspace.
# Instructions
# Obtain summary() statistics of the weight_sim_big chains.
# The posterior mean of b is reported in Table 1 of the summary(). Use the raw weight_chains to verify this 
# calculation.
# Construct a scatterplot of the wgt vs hgt data in bdims. Use geom_abline() to superimpose the posterior mean trend.
# Construct another scatterplot of wgt vs hgt. Superimpose the 20 regression lines defined by the first 20 sets 
# of a & b parameter values in weight_chains.

# Summarize the posterior Markov chains
summary(weight_sim_big)

head(weight_chains)
#           a        b        s iter
# 1 -113.9029 1.072505 8.772007    1
# 2 -115.0644 1.077914 8.986393    2
# 3 -114.6958 1.077130 9.679812    3
# 4 -115.0568 1.072668 8.814403    4
# 5 -114.0782 1.071775 8.895299    5
# 6 -114.3271 1.069477 9.016185    6
# Calculate the estimated posterior mean of b
mean(weight_chains$b)
# [1] 1.011945
# Plot the posterior mean regression model
ggplot(bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_abline(intercept = mean(weight_chains$a), slope = mean(weight_chains$b), color = "red")
# Visualize the range of 20 posterior regression models
ggplot(bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_abline(intercept = weight_chains$a[1:20], slope = weight_chains$b[1:20], color = "gray", size = 0.25)
# http://joxi.ru/8An4XoVTNWMXjm
# Excellent work. You now have a sense of the posterior mean trend in the linear relationship between weight 
# and height as well as the posterior uncertainty in this trend. Given the size of the data and selection of 
# priors, the posterior uncertainty is noticeably small as evidenced by the tight distribution of the gray 
# posterior plausible lines around the trend.

# Posterior credible intervals
# Let's focus on slope parameter b, the rate of change in weight over height. The posterior mean of b reflects 
# the trend in the posterior model of the slope. In contrast, a posterior credible interval provides a range of 
# posterior plausible slope values, thus reflects posterior uncertainty about b. For example, the 95% credible 
# interval for b ranges from the 2.5th to the 97.5th quantile of the b posterior. Thus there's a 95% (posterior) 
# chance that b is in this range.
# You will use RJAGS simulation output to approximate credible intervals for b. The 100,000 iteration RJAGS 
# simulation of the posterior, weight_sim_big, is in your workspace along with a data frame of the Markov chain 
# output, weight_chains.
# Instructions
# Obtain summary() statistics of the weight_sim_big chains.
# The 2.5% and 97.5% posterior quantiles for b are reported in Table 2 of the summary(). Apply quantile() to the 
# raw weight_chains to verify these calculations. Save this as ci_95 and print it. 
# Similarly, use the weight_chains data to construct a 90% credible interval for b. Save this as ci_90 and print it. 
# Construct a density plot of the b Markov chain values. Superimpose vertical lines representing the 90% credible 
# interval for b using geom_vline() with xintercept = ci_90.
head(weight_sim_big)
# Markov Chain Monte Carlo (MCMC) output:
# Start = 1 
# End = 7 
# Thinning interval = 1 
# a        b        s
# [1,] -113.9029 1.072505 8.772007
# [2,] -115.0644 1.077914 8.986393
# [3,] -114.6958 1.077130 9.679812
# [4,] -115.0568 1.072668 8.814403
# [5,] -114.0782 1.071775 8.895299
dim(weight_sim_big)
# [1] 100000      3
typeof(weight_sim_big)
# [1] "double"
# Summarize the posterior Markov chains
summary(weight_sim_big)
head(weight_chains) # extract of data from weight_sim_big
# a        b        s iter
# 1 -113.9029 1.072505 8.772007    1
# 2 -115.0644 1.077914 8.986393    2
# 3 -114.6958 1.077130 9.679812    3
# 4 -115.0568 1.072668 8.814403    4
# 5 -114.0782 1.071775 8.895299    5
# 6 -114.3271 1.069477 9.016185    6
dim(weight_chains)
# [1] 100000      4
typeof(weight_chains)
# [1] "list"
# Calculate the 95% posterior credible interval for b
ci_95 <- quantile(weight_chains$b, probs = c(0.025, 0.975))
ci_95
#      2.5%     97.5% 
# 0.9152492 1.0975006
# Calculate the 90% posterior credible interval for b
ci_90 <- quantile(weight_chains$b, probs = c(0.05, 0.95))
ci_90
#        5%       95% 
# 0.9318473 1.0833141
# Mark the 90% credible interval 
ggplot(weight_chains, aes(x = b)) + geom_density() + geom_vline(xintercept = ci_90, color = "red")
# http://joxi.ru/Y2LdjY0txq1XG2
# Right! Based on your calculations we can say that there's a 90% (posterior) probability that, on average, the 
# increase in weight per 1 cm increase in height is between 0.93 and 1.08 kg.

# Posterior probabilities
# You've used RJAGS output to explore and quantify the posterior trend & uncertainty b . You can also use RJAGS 
# output to assess specific hypotheses. For example: What's the posterior probability that, on average, weight 
# increases by more than 1.1 kg for every 1 cm increase in height? That is, what's the posterior probability 
# that b > 1.1?
# You will approximate this probability by the proportion of b Markov chain values that exceed 1.1. The 
# weight_chains data frame with the 100,000 iteration Markov chain output is in your workspace.
# Instructions
# Construct a density plot of the b Markov chain values and use geom_vline() to superimpose a vertical line at 1.1.
# Use table() to summarize the number of  b Markov chain values that exceed 1.1.
# Use mean() to calculate the proportion of b Markov chain values that exceed 1.1.
# Mark 1.1 on a posterior density plot for b
ggplot(weight_chains, aes(x = b)) + 
  geom_density() + 
  geom_vline(xintercept = 1.1, color = "red")

# Summarize the number of b chain values that exceed 1.1
table(weight_chains$b > 1.1)
# FALSE  TRUE 
# 97835  2165
# Calculate the proportion of b chain values that exceed 1.1
mean(weight_chains$b > 1.1)
# [1] 0.02165
# Great. Based on your calculations we can say that there's only a ~2% (posterior) chance that, on average, the 
# increase in weight per 1 cm increase in height exceeds 1.1 kg.

# 11 Posterior prediction.mp4
# Inference for the posterior trend
# Recall the likelihood of the Bayesian regression model of weight Y by height X: Y ∼ N(m, s^2) where m = a + bX. 
# In earlier exercises you approximated the form of the posterior trend m (solid line). From this, notice that 
# the typical weight among 180 cm adults is roughly 80 kg (dashed lines): http://joxi.ru/vAWkDKZI3l4oeA
# You will use RJAGS simulation output to approximate the posterior trend in weight among 180 cm tall adults as 
# well as the posterior uncertainty in this trend. The 100,000 iteration RJAGS simulation of the posterior, 
# weight_sim_big, is in your workspace along with a data frame of the Markov chain output, weight_chains.
# weight_chains contains 100,000 sets of posterior plausible parameter values of 
# a and b. From each, calculate the mean (typical) weight among 180 cm tall adults, a + b ∗ 180. Store these trends 
# as a new variable m_180 in weight_chains.
# Construct a posterior density plot of 100,000 m_180 values.
# Use the 100,000 m_180 values to calculate a 95% posterior credible interval for the mean weight among 180 cm 
# tall adults.
head(weight_chains)
# Calculate the trend under each Markov chain parameter set
weight_chains <- weight_chains  %>% mutate(m_180 = a + b * 180)
# Construct a posterior density plot of the trend
ggplot(weight_chains, aes(x = m_180)) + geom_density() 
# Construct a posterior credible interval for the trend (trend = means, not weights itsels!, because we need to introduce variability with N())
quantile(weight_chains$m_180, probs = c(0.025, 0.975))
#     2.5%    97.5% 
# 76.95054 79.23619
# Yes! The posterior trend of your regression model indicates that the typical weight among 180 cm tall 
# adults is roughly 78 kg. However, posterior uncertainty in the regression model trickles down to uncertainty 
# about this trend. This uncertainty is communicated through your credible interval: there's a 95% (posterior) 
# chance that the typical weight at a height of 180 cm is between 76.95 and 79.24 kg.
# Instructions
# Use rnorm() to simulate a single prediction of weight under the parameter settings in the first row of weight_chains.
# Repeat the above using the parameter settings in the second row of weight_chains.
# Simulate a single prediction of weight under each of the 100,000 parameter settings in weight_chains. Store these as a new variable Y_180 in weight_chains.
# Print the first 6 rows of parameter values & predictions in weight_chains.
head(weight_chains)
weight_chains$m_180[1]
weight_chains$s[1]
# Simulate 1 prediction under the first parameter set
rnorm(n = 1, mean = weight_chains$m_180[1], sd = weight_chains$s[1])
# [1] 71.77335
# Simulate 1 prediction under the second parameter set
rnorm(n = 1, mean = weight_chains$m_180[2], sd = weight_chains$s[2])
# [1] 83.42277
# Simulate & store 1 prediction under each parameter set
weight_chains <- weight_chains  %>% mutate(Y_180 = rnorm(n = 100000, mean = m_180, sd = s))
# Print the first 6 parameter sets & predictions
head(weight_chains)
#           a        b        s iter    m_180     Y_180
# 1 -113.9029 1.072505 8.772007    1 79.14803 101.35342
# 2 -115.0644 1.077914 8.986393    2 78.96014  78.79213
# 3 -114.6958 1.077130 9.679812    3 79.18771  70.90783
# 4 -115.0568 1.072668 8.814403    4 78.02352  67.75137
# 5 -114.0782 1.071775 8.895299    5 78.84138  82.32659
# 6 -114.3271 1.069477 9.016185    6 78.17877  72.77069
# You now have 100,000 predictions for the weight of a 180 cm tall adult that reflect the range of posterior 
# plausible parameter settings.

# Posterior predictive distribution
# The weight_chains data frame (in your workspace) contains your 100,000 posterior predictions, Y_180, for the 
# weight of a 180 cm tall adult: > head(weight_chains, 2)
# a        b        s iter    m_180    Y_180
# 1 -113.9029 1.072505 8.772007    1 79.14803 71.65811
# 2 -115.0644 1.077914 8.986393    2 78.96014 75.78893
# You will use these 100,000 predictions to approximate the posterior predictive distribution for the weight of 
# a 180 cm tall adult. The bdims data are in your workspace.
# Instructions
# Use the 10,000 Y_180 values to construct a 95% posterior credible interval for the weight of a 180 cm tall adult.
# Construct a density plot of your 100,000 posterior plausible predictions.
# Construct a scatterplot of the wgt vs hgt data in bdims.
# Use geom_abline() to superimpose the posterior regression trend.
# Use geom_segment() to superimpose a vertical line at a hgt of 180 that represents the lower & upper limits 
# (y and yend) of ci_180.
# Construct a posterior credible interval for the prediction
ci_180 <- quantile(weight_chains$Y_180, probs = c(0.025, 0.975))
ci_180
#     2.5%    97.5% 
# 59.68305 96.25340 
ci_180[1]
# Construct a density plot of the posterior predictions
ggplot(weight_chains, aes(x = Y_180)) + geom_density() + geom_vline(xintercept = ci_180, color = "red")
# Visualize the credible interval on a scatterplot of the data
ggplot(bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_abline(intercept = mean(weight_chains$a), slope = mean(weight_chains$b), color = "red") + 
  geom_segment(x = 180, xend = 180, y = ci_180[1], yend = ci_180[2], color = "red")
# Congratulations! You've simulated your first posterior predictive distribution. Your 100,000 posterior plausible 
# weights for a given 180 cm tall adult ranged from roughly 36 to 117 kg. Eliminating the most extreme 5% of these 
# predictions, you observed that there's a 95% (posterior) chance that the weight is between 72 and 84 kg.

# 12 Bayesian regression with a categorical predictor.mp4
# RailTrail sample data
# The RailTrail data frame from the mosaic package is loaded in your workspace. RailTrail contains data collected by the Pioneer Valley Planning Commission on the usage of a local rail-trail. For each of 90 days, they recorded the rail-trail volume (number of users) and whether it was a weekday (TRUE if yes and FALSE otherwise). You will explore the trends in weekday vs weekend volume below.
# Instructions
# Confirm that weekday is recorded as a factor variable.
# Construct density plots of weekday volume and weekend volume on the same frame.
install.packages("mosaic")
library(mosaic)
head(RailTrail)
# Confirm that weekday is a factor variable
class(RailTrail$weekday)
RailTrail$weekday <- as.factor(RailTrail$weekday)
head(RailTrail)
str(RailTrail)
dim(RailTrail)
table(RailTrail$weekday)
# Construct a density plot of volume by weekday
ggplot(RailTrail, aes(x = volume, fill = weekday)) + 
  geom_density(alpha = 0.5)
# Right! Notice that, as might be intuitive, rail-trail volume tends to be slightly higher on weekends 
# (~430 users per day) than on weekdays (~350 users per day).

# RJAGS simulation with categorical variables
# Consider the Normal regression model of volume Yi by weekday status Xi: 
#   likelihood: Yi ∼ N(mi, s^2) where mi = a + bXi 
#   priors: a ∼ N(400, 100^2), b ∼ N(0, 200^2), s ∼ Unif(0, 200). You explored the relationship between Yi and Xi for the 90 days recorded in RailTrail (in your workspace). In light of these data and the priors above, you will update your posterior model of this relationship. This differs from previous analyses in that Xi is categorical. In rjags syntax, its coefficient b is defined by two elements, b[1] and b[2], which correspond to the weekend and weekday levels, respectively. For reference, b[1] is set to 0. In contrast, b[2] is modeled by the prior for b.
# Instructions
# DEFINE your Bayesian model.
# Define the likelihood model of Y[i] given m[i] and s where m[i] <- a + b[X[i]]. Note the new notation b[X[i]] here!
# Specify the priors for a, b (via b[1] and b[2]), and s. 
# Store the model string as rail_model_1.
mean(RailTrail[RailTrail$weekday == T,]$volume)
mean(RailTrail[RailTrail$weekday == F,]$volume)
# DEFINE the model    
rail_model_1 <- "model{
    # Likelihood model for Y[i]
    for(i in 1:length(Y)){
      Y[i] ~ dnorm(m[i], s^(-2))
      m[i] <- a + b[X[i]]
    }
    
    # Prior models for a, b, s
    a ~ dnorm(400, 100^(-2))
    b[1] <- 0
    b[2] ~ dnorm(0, 200^(-2))
    s ~ dunif(0,200)
}"
# COMPILE rail_model_1 using jags.model():
# Establish a textConnection() to rail_model_1.
# Provide the observed RailTrail data.
# Specify a starting random number generating seed of 10.
# Store the output in a jags object named rail_jags_1.
# COMPILE the model
rail_jags_1 <- jags.model(
  textConnection(rail_model_1),
  data = list(Y = RailTrail$volume, X = RailTrail$weekday),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 10)
)

# COMPILE the model
rail_jags_1 <- jags.model(
  textConnection(rail_model_1),
  data = list(Y = RailTrail$volume, X = RailTrail$weekday),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 10)
)

# SIMULATE a sample of 10,000 draws from the posterior model of a, b, and s. Store this mcmc.list as rail_sim_1.
# Store the Markov chain output in a data frame named rail_chains_1.
# plot() the posterior samples in rail_sim_1.
# SIMULATE the posterior    
rail_sim_1 <- coda.samples(model = rail_jags_1, variable.names = c('a','b','s'), n.iter = 10000)
# Store the chains in a data frame
rail_chains_1 <- data.frame(rail_sim_1[[1]])
# PLOT the posterior
plot(rail_sim_1)
# Nice work! You've successfully defined, compiled, and simulated a Bayesian regression model with a categorical predictor.

# Interpreting categorical coefficients
# In your Bayesian model, mi = a + bXi specified the dependence of typical trail volume on weekday status Xi 
# (1 for weekdays and 0 for weekends). A summary() of your RJAGS model simulation provides posterior mean 
# estimates of parameters a and b, the latter corresponding to b.2. here.
summary(rail_sim_1)
#        Mean     SD Naive SE Time-series SE
# a    428.47 23.052  0.23052         0.5321
# b.1.   0.00  0.000  0.00000         0.0000
# b.2. -77.78 27.900  0.27900         0.6422
# s    124.25  9.662  0.09662         0.1335
# Which of the following is the best interpretation of these posterior summaries?
# Answer the question
# Possible Answers
# - Typically, there are 428.47 trail users on a weekday and 77.78 fewer users (~350.69) on a weekend day.
# - Typically, there are 428.47 trail users on a weekend day and 77.78 fewer users (~350.69) on a weekday. +
# - Typically, there are 428.47 trail users on a weekday and only -77.78 users on a weekend day.
# - Typically, there are 428.47 trail users on a weekend day and only -77.78 users on a weekday.
# Excellent! Parameter a describes the typical weekend volume whereas b describes the contrast between weekday and weekend volume.

# Inference for volume by weekday
# The 10,000 iteration RJAGS simulation output, rail_sim_1, is in your workspace along with a data frame of the 
# Markov chain output:
# > head(rail_chains_1, 2)
# a b.1.       b.2.        s 
# 1 420.6966    0  -54.30783 118.2328
# 2 399.5823    0  -52.02570 119.9499
# These chains provide 10,000 unique sets of values for a, the typical trail volume on weekend days, and b.2., 
# the contrast between typical weekday volume vs weekend volume. For example, the first set of parameters 
# indicate that there are typically 420.6966 riders on weekend days and 54.30783 fewer riders on weekdays. 
# Thus there are typically 420.6966 - 54.30783 = 366.3888 riders on weekdays. You will utilize these simulation 
# data to make inferences about weekday trail volume.
# Instructions
# Combine the a and b.2. chain values to construct a chain of 10,000 values for the typical weekday trail volume. 
# Store this as weekday_mean in rail_chains_1.
# Use ggplot() to construct a density plot of the weekday_mean chain values.
# Construct a 95% credible interval for the typical weekday trail volume.
head(rail_chains_1)
#          a b.1.       b.2.        s weekday_mean
# 1 420.6966    0  -54.30783 118.2328     366.3888
# 2 399.5823    0  -52.02570 119.9499     347.5566
# Construct a chain of values for the typical weekday volume
rail_chains_1 <- rail_chains_1 %>% mutate(weekday_mean = a + b.2.)
# Construct a density plot of the weekday chain
ggplot(rail_chains_1, aes(x = weekday_mean)) + geom_density()
# 95% credible interval for typical weekday volume
quantile(rail_chains_1$weekday_mean, c(0.025, 0.975))
#     2.5%    97.5% 
# 318.9520 381.7573
# AK: just to compare
head(RailTrail)
quantile(RailTrail[RailTrail$dayType == "weekend", ]$volume, c(0.025, 0.975)) 
#   2.5%  97.5% 
# 218.10 677.95 
# Do you understand the difference between two quantiles??
# Excellent! You've shown that there's a 95% posterior chance that the typical weekday volume is between 319 and 382 trail users.

# 13 Multivariate Bayesian regression.mp4
# Re-examining the RailTrail data
# In your previous work, you observed that rail-trail volume tends to be lower on a weekday than a weekend. 
# Some of the variability in volume might also be explained by outside temperature. For example, we might 
# expect trail volume to increase on warm, pleasant days.
# The RailTrail data set in your workspace includes hightemp, the observed high temperature (F) for each 
# of the 90 days in the study period. You will use these data to explore the associations between trail 
# volume, weekday status, and hightemp
# Instructions
# Construct a scatterplot of volume by hightemp:
# Use color to distinguish between weekdays & weekends.
# Use geom_smooth() to highlight the linear relationship between the observed volume & hightemp values.
head(RailTrail)
# Construct a plot of volume by hightemp & weekday
ggplot(RailTrail, aes(y = volume, x = hightemp, color = weekday)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
# Nice work. Notice that for the 90 days in the study period, volume tends to increase with temperature. 
# Further, volume tends to be higher on weekends than on weekdays of the same temperature.

# RJAGS simulation for multivariate regression
# Consider the following Bayesian model of volume Yi by weekday status Xi and temperature Zi:
# likelihood: Yi ∼ N(mi, s^2) where mi = a + bXi + cZi.
# priors: a ∼ N(0, 200^2), b ∼ N(0, 200^2), c ∼ N(0, 20^2), s ∼ Unif(0, 200)
# Your previous exploration of the relationship between volume, weekday, and hightemp in the RailTrail 
# data provided some insight into this relationship. You will combine this with insight from the priors 
# to develop a posterior model of this relationship using RJAGS. The RailTrail data are in your work space.
# Instructions
# DEFINE your Bayesian model and store it as rail_model_2. Specifically, utilizing the dnorm() and dunif() 
# rjags functions:
# For each of the 90 subjects i, define mi and the model of Yi given mi and s using RJAGS notation. 
# To this end, remember that b[X[i]] is the rjags equivalent of bXi.
# Specify the priors for a, b, c, and s.
# DEFINE the model    
rail_model_2 <- "model{
    # Likelihood model for Y[i]
    for (i in 1:length(Y)){
        Y[i] ~ dnorm(m[i], s^(-2))
        m[i] <- a + b[X[i]] + c * Z[i]
    }
    # Prior models for a, b, c, s
    a ~ dnorm(0, 200^(-2))
    b[1] <- 0
    b[2] ~ dnorm(0, 200^(-2))
    c ~ dnorm(0, 20^(-2))
    s ~ dunif(0,200)

}"
# DEFINE your Bayesian model and store it as rail_model_2. Specifically, utilizing the dnorm() and dunif() 
# rjags functions:
# For each of the 90 subjects i, define mi and the model of Yi given mi and s using RJAGS notation. To this 
# end, remember that b[X[i]] is the rjags equivalent of bXi.
# Specify the priors for a, b, c, and s.
# COMPILE the model
rail_jags_2 <- jags.model(textConnection(rail_model_2), 
                          data = list(Y = RailTrail$volume, X = RailTrail$weekday, Z = RailTrail$hightemp), 
                          inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 10))
# SIMULATE a sample of 10,000 draws from the posterior model of a, b, c, and s. Store this mcmc.list as 
# rail_sim_2.
# Store the Markov chain output in a data frame named rail_chains_2.
# plot() the posterior samples in rail_sim_2.
# SIMULATE the posterior    
rail_sim_2 <- coda.samples(model = rail_jags_2, variable.names = c("a", "b", "c", "s"), n.iter = 10000)
# Store the chains in a data frame
rail_chains_2 <- data.frame(rail_sim_2[[1]])
# PLOT the posterior
plot(rail_sim_2)
# Wow! You've successfully defined, compiled, and simulated your multivariate Bayesian regression model.

# Interpreting multivariate regression parameters
# Your Bayesian model explored the dependence of typical trail volume on weekday status 
# Xi and temperature Zi: mi = a + bXi + cZi. A summary() of your RJAGS model simulation provides posterior 
# mean estimates of parameters a, b, and c:
summary(rail_sim_2)
# Mean      SD Naive SE Time-series SE
# a     36.592 60.6238 0.606238        4.19442
# b[1]   0.000  0.0000 0.000000        0.00000
# b[2] -49.610 23.4930 0.234930        0.55520
# c      5.417  0.8029 0.008029        0.05849
# s    103.434  7.9418 0.079418        0.11032
# For example, the posterior mean of c indicates that for both weekends and weekdays, typical rail volume 
# increases by ~5.4 users for every 1 degree increase in temperature. Which of the following interpretations 
# of b (represented here by b[2]) is the most accurate?
# Possible Answers
# - Typical volume decreases by ~50 for every extra weekday.
# - Typical volume is ~50 less on weekends than on weekdays.
# - Typical volume is ~50 less on weekdays than on weekends.
# - Typical volume is ~50 less on weekends than on weekdays of the same temperature. 
# - Typical volume is ~50 less on weekdays than on weekends of the same temperature. +
# Exactly! The b coefficient represents the relationship between volume and weekday status when controlling 
# for, or on days with similar hightemp.

# Posterior inference for multivariate regression
# The 10,000 iteration RJAGS simulation output, rail_sim_2, is in your workspace along with a data frame of the Markov chain output:
head(rail_chains_2, 2)
# a b.1.      b.2.        c         s
# 1 49.76954    0 -12.62112 4.999202 111.02247
# 2 30.22211    0  -3.16221 4.853491  98.11892 
# You will use these 10,000 unique sets of parameter values to summarize the posterior mean trend in the 
# relationships between trail volume, weekday status, and hightemp.
# Instructions
# Construct a scatterplot of volume by hightemp.
# Use color to distinguish between weekdays & weekends.
# Superimpose a red line that represents the posterior mean trend of the linear relationship between 
# volume and hightemp for weekends: m = a + c Z
# Superimpose a turquoise3 line that represents the posterior mean trend of the linear relationship 
# between volume and hightemp for weekdays: m = (a + b.2.) + c Z
head(rail_chains_2)
head(rail_sim_2)
# Plot the posterior mean regression models
ggplot(RailTrail, aes(x = hightemp, y = volume, color = weekday)) + 
  geom_point() + 
  geom_abline(intercept = mean(rail_chains_2$a), slope = mean(rail_chains_2$c), color = "red") + 
  geom_abline(intercept = mean(rail_chains_2$a) + mean(rail_chains_2$b.2.), slope = mean(rail_chains_2$c), color = "turquoise3")
# Your posterior analysis suggests that there's a positive association between volume and temperature. 
# Further, the typical weekday volume is less than that on weekends of the same temperature.

# 14 Bayesian Poisson regression.mp4
# RJAGS simulation for Poisson regression
# In the previous video we engineered a Poisson regression model of volume Yi by weekday status Xi and 
# temperature Zi:
#  likelihood: Yi ∼ Pois(li) where log(li) = a + bXi + cZi
#  priors: a ∼ N(0, 200^2), b ∼ N(0,2^2), and c ∼ N(0,2^2)
# Combining your insights from the observed RailTrail data and the priors stated here, you will define, 
# compile, and simulate a posterior model of this relationship using RJAGS. To challenge yourself in this 
# last RJAGS simulation of the course, you'll be provided with less helpful code than usual!
# The RailTrail data are in your work space.
# Instructions
# DEFINE your Bayesian model:
# Use dpois() to define the likelihood model of Y[i] given l[i].
# Define the prior models for a, b, c.
# Store the model string as poisson_model.

# COMPILE poisson_model with the following details:
# For reproducible results, specify a starting seed of 10 for the base::Wichmann-Hill random number 
# generator inits.
# Run 1 chain and store the output in a jags object named poisson_jags.
poisson_jags <- jags.model(
  textConnection(poisson_model), 
  data = list(Y = RailTrail$volume, X = RailTrail$weekday, Z = RailTrail$hightemp),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 10)
)
# SIMULATE a sample of 10,000 draws from the posterior model of a, b, and c. Store this mcmc.list 
# as poisson_sim.
# Store the Markov chain output in a data frame named poisson_chains.
# plot() the posterior samples in poisson_sim.
# SIMULATE the posterior    
poisson_sim <- coda.samples(model = poisson_jags, variable.names = c("a", "b", "c"), n.iter = 10000)
# Store the chains in a data frame
poisson_chains <- data.frame(poisson_sim[[1]])
# PLOT the posterior
plot(poisson_sim)
# You've successfully defined, compiled, and simulated a Bayesian Poisson regression model!

# Plotting the Poisson regression model
# Recall the likelihood structure for your Bayesian Poisson regression model of volume Yi by weekday status 
# Xi and temperature Zi: Yi ∼ Pois(li) where 
# log(li) = a + bXi + cZi; thus 
# li = exp(a + bXi + cZi)
# Your 10,000 iteration RJAGS simulation of the model posterior, poisson_sim, is in your workspace along 
# with a data frame of the Markov chain output:
# > head(poisson_chains, 2)
# a b.1.       b.2.          c
# 1 5.019807    0 -0.1222143 0.01405269
# 2 5.018642    0 -0.1217608 0.01407691
# You will use these results to plot the posterior Poisson regression trends. These nonlinear trends can 
# be added to a ggplot() using stat_function(). For example, specifying fun = function(x){x^2} would return 
# a quadratic trend line.
# Instructions
# Construct a scatterplot of volume by hightemp with the following features:
# Use color to distinguish between weekdays & weekends.
# Superimpose a red curve that represents the posterior mean Poisson regression trend li of the linear 
# relationship between volume and hightemp for weekends: l = exp(a + c Z)
# Superimpose a turquoise3 curve that represents the posterior mean Poisson regression trend li of the 
# linear relationship between volume and hightemp for weekdays: l = exp((a + b.2.) + c Z)
head(poisson_chains)
# Plot the posterior mean regression models
ggplot(RailTrail, aes(x = hightemp, y = volume, color = weekday)) + 
  geom_point() + 
  stat_function(fun = function(x){exp(mean(poisson_chains$a) + mean(poisson_chains$c) * x)}, color = "red") + 
  stat_function(fun = function(x){exp(mean(poisson_chains$a) + mean(poisson_chains$b.2.) + mean(poisson_chains$c) * x)}, color = "turquoise3")
# Excellent work. Notice that unlike the Normal regression trend, the Poisson regression trend is curved.

# Inference for the Poisson rate parameter
# Again, recall the likelihood structure for your Bayesian Poisson regression model of volume Yi by weekday 
# status Xi and temperature Zi:  
# Yi ∼ Pois(li) where li = exp(a + bXi + cZi)
# Your 10,000 iteration RJAGS simulation of the model posterior, poisson_sim, is in your workspace along with a data frame of the Markov chain output:
#   > head(poisson_chains, 2)
# a b.1.       b.2.          c
# 1 5.019807    0 -0.1222143 0.01405269
# 2 5.018642    0 -0.1217608 0.01407691
# Using these 10,000 unique sets of posterior plausible values for parameters a, b, and c you will make 
# inferences about the typical trail volume on 80 degree days.
# Instructions
# From each set of poisson_chains parameter values, calculate the typical trail volumes l on an 80 degree 
# weekend day. Store these trends as a new variable, l_weekend, in poisson_chains.
# Similarly, calculate the typical trail volumes on an 80 degree weekday. Store these as a new variable, 
# l_weekday.
# Calculate 95% posterior credible intervals for the typical volume on an 80 degree weekend day and the 
# typical volume on an 80 degree weekday.
head(poisson_chains)
# Calculate the typical volume on 80 degree weekends & 80 degree weekdays
poisson_chains <- poisson_chains %>% 
  mutate(l_weekend = exp(a + c * 80)) %>% 
  mutate(l_weekday = exp(a + b.2. + c * 80))
# Construct a 95% CI for typical volume on 80 degree weekend
quantile(poisson_chains$l_weekend, c(0.025, 0.975))
# Construct a 95% CI for typical volume on 80 degree weekday
quantile(poisson_chains$l_weekday, c(0.025, 0.975))
# This was your first analysis that incorporated transformations. It wasn't so bad (so long as you remember to back-transform)!

# Poisson posterior prediction
# Your l_weekday variable reflects the trend in volume on 80 degree weekdays:
#  > head(poisson_chains, 2)
# a b.1.    b.2.      c l_weekend l_weekday
# 1 5.0198    0 -0.1222 0.0141   465.924   412.324
# 2 5.0186    0 -0.1218 0.0141   466.284   412.829
# Now that you understand the trend, let's make some predictions! Specifically, let's predict trail volumes on the next 80 degree weekday. To do so, you must take into account individual variability from the trend, modeled by the likelihood Yi ∼ Pois(li).
# Using rpois(n, lambda) for sample size n and rate parameter lambda, you will simulate Poisson predictions of volume under each value of the posterior plausible trend in poisson_chains.
# Instructions
# From each of the 10,000 l_weekday values in poisson_chains, use rpois() to predict volume on an 80 degree weekday. Store these as Y_weekday in poisson_chains.
# Use ggplot() to construct a density plot of your Y_weekday predictions.
# Approximate the posterior probability that the volume on an 80 degree weekday is less than 400 users.
head(poisson_chains)
# Simulate weekday predictions under each parameter set
poisson_chains <- poisson_chains %>% mutate(Y_weekday = rpois(n = 10000, lambda = l_weekday))
head(poisson_chains)  
# Construct a density plot of the posterior weekday predictions
ggplot(poisson_chains, aes(x = Y_weekday)) + geom_density()
# Posterior probability that weekday volume is less 400
mean(poisson_chains$Y_weekday < 400)
# Recall that one of our motivations in applying the Poisson model was to accommodate the count nature of the volume data. This trickled down to your volume predictions Y_weekday - notice that these predictions, like the raw volume data, are discrete counts.










