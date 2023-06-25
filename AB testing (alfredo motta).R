# AB testing from scratch
# http://www.alfredo.motta.name/ab-testing-from-scratch/
# and (below)
# A/B Test Statistics Made Easy. Part 1: Continuous Metrics
# https://towardsdatascience.com/a-b-test-statistics-made-easy-4e94098758d9


library(ggplot2)
library(dplyr)

#  Assuming we have a jar with 30% of red balls, what is the probability that we extract exactly 10 red balls out of a 100?
# 1
dbinom(10,100,0.3)
# 2
choose(100,10) * 0.3^10 * (1-0.3)^(100-10)
# 3
((factorial(100))/(factorial(10) * factorial(100-10))) * 0.3^10 * (1-0.3)^(100-10)

# Now, let’s plot these values for x (number of successes) ranging between 0 and 100
x <-  1:100
y <- dbinom(x, 100, 0.3)
ggplot(data = tibble(x = x, y = y), aes(x,y)) +
  geom_point(aes(x,y)) + labs(x = "Number of success", y = "Probability", title = "Probability of success 1..100 red balls extraction with p=0.3")

# Experiment results
n_A <- 10000; p_A <- 0.01
n_B <- 10000; p_B <- 0.012

# One way of assessing if B is better than A is to plot their expected distributions. Assuming that A follows a Binomial distribution with 𝑝=0.01 (we had 100 conversions over 10000 trials) and that B follows a Binomial distribution with 𝑝=0.012 (we had 120 conversions over 10000 trials) this is how they relate to each other
x_A <- 1:n_A
y_A <- dbinom(x_A,length(x_A),p_A)
#qbinom(0.5,10000,0.01)
x_B <- 1:n_B
y_B <- dbinom(x_B,length(x_B), p_B)

data <- tibble(x_A = x_A,
               y_A = y_A,
               x_B = x_B,
               y_B = y_B)
colors_legend <- c("A"="green", "B"="orange")
data %>% 
  ggplot() +
  geom_point(aes(x=x_A,y=y_A,color="A")) +
  geom_point(aes(x=x_B,y=y_B,color="B")) +
  scale_color_manual(name="Variants", values = colors_legend) +
  labs(x="Number of successes", y="Probability") +
  xlim(0,200)

# So if the true mean of the two distribution is 𝑝𝑎=0.01 and 𝑝𝑏=0.012 respectively we can conclude that B is better than A. If we repeat the experiment several times (always with 10000 participants) for A we will get most of the time values between 70 and 129 while for B we will get values between 87 and 152. You can check these boundaries on the plot, or you can compute them manually with the 3 times standard deviation rule of thumb 5
# experiment A
n=n_A; p=p_A; q=1-p;mean=n_A*p_A; sigma=sqrt(n*p*q)
paste(
  mean - 3*sigma, ", ", mean + 3*sigma
)
# experiment B
n=n_B; p=p_B; q=1-p;mean=n_B*p_B; sigma=sqrt(n*p*q)
paste(
  mean - 3*sigma, ", ", mean + 3*sigma
)
# But hold on one minute. How do we know that 𝑝𝑎=0.01 and 𝑝𝑏=0.012 are indeed the true means of our distributions? In the end we simply did one extraction from our jars. If these numbers are wrong our distributions will look different and the previous analysis will be flawed. Can we do better?

# More rigorous experiment assessment
# In order to estimate what is the true mean of our variants statisticians rely on the Central Limit Theorem (CLT) 6 which states that the distribution of the mean of a large number of independent, identically distributed variables will be approximately normal, regardless of the underlying distribution.
# In our case we are trying to estimate the distribution of the mean of the proportions 𝑝𝑎 and 𝑝𝑏 for our variants. Suppose you run your A/B test experiment 𝑁=100 times and that each time you collect 𝑛=10000 samples you will end up having for variant A:
# p1a, p2a,...,pNa
# and the CLT tells us that these are normally distributed with parameters:
# mu_p = p, sigma_p = sigma/sqrt(n) * (N-n)/(N-1) ~ sqrt(p*(1-p)/n)
# where sigma=sqrt(p*(1-p)) is the standard deviation of the binomial distribution
# I know, this is hard to believe and proving these numbers goes definitely beyond the scope of this blog post so you will find some maths-heavy material in the footnotes 7 8.

# Back to our question, what is the true mean of 𝑝𝑎 and 𝑝𝑏? Well, we don’t know really, but they are distributed normally like this:
x_A <- seq(from=0.005, to=0.02, by=0.00001)
y_A <- dnorm(x_A, mean=p_A, sd=sqrt(p_A*(1-p_A)/n_A))
x_B <- seq(from=0.005, to=0.02, by=0.00001)
y_B <- dnorm(x_B, mean=p_B, sd=sqrt(p_B*(1-p_B)/n_B))
data <- tibble(x_A=x_A, y_A=y_A, x_B=x_B, y_B=y_B)
colors_legend <- c("A"="green", "B"="orange")
data %>% 
  ggplot() +
  geom_point(aes(x=x_A,y=y_A,color="A")) +
  geom_point(aes(x=x_B,y=y_B,color="B")) +
  scale_color_manual(name="Variants", values = colors_legend) +
  labs(x="Proportion mean value", y="PDF of Proportion Mean for A and B") 

# As you can see we are dealing with a risky business here. There is a good chance that the estimation of the true values of 𝑝𝑎 and 𝑝𝑏 are not correct 
# since they can span anywhere between all the values plotted above. For a good number of them 𝑝𝑎 may actually outperform 𝑝𝑏 violating
# the conclusion we did in the previous section. There is no magic bullet that will solve this problem, this is the intrinsic nature of the probabilistic 
# world in which we live. However, we can do our best to quantify the risk and take a conscious decision.

# Quantitative evaluation
# In the previous section we have seen that it is likely that Variant B is better than Variant A, but how can we quantify this statement? There are 
# different ways in which we can look at this problem, but the ones that statisticians use is Hypothesis testing.
# In a series of papers in the early 20th century, J. Neyman and E. S. Pearson developed a decision-theoretic approach to hypothesis-testing 9. 
# The theory was later extended and generalised by Wald 10. For a full account of the theory, see the book of Lehmann and Romano 11.
# In this framework we state a hypothesis (also called null-hypothesis) and by looking at the number we will try to reject it. 
# In our example we hypothesize that the true conversion of our visitors is 𝑝𝑎 and that the proportion 𝑝𝑏 we collected in the B variant
# is simply due to chance. In other words we assume that our real world visitors behave like in variant A and we quantify the probability of seeing 
# variant B’s proportions under this hypothesis.
# So, what is the probability of having 120 or more conversions if the true mean of the binomial distribution is 𝑝𝑎=0.01? We simply have to sum the
# probability of all possible events:
# (𝑋𝑎>=120)=𝑃(𝑋𝑎=120)+𝑃(𝑋𝑎=121)+…+𝑃(𝑋𝑎=100
# To actually compute the number you can use the probability mass function in Formula (a) or you can use R
binom.test(x=120,n=10000,p=0.01,alternative = "greater")
pbinom(q=120, size = 10000, p = 0.01, lower.tail = F)
#I deliberately specified alternative = "greater" in the function call to compute the chance of getting more than 120. But there are other ways 12 to approach the problem. The value we are looking for is the p-value 13 0.0276 which is exactly the probability of getting more than 120 successes, i.e. 𝑃(𝑋𝑎>=120). Visually it corresponds to the area under the right end tail of the distribution of A:
n_A <- 10000; p_A=0.01
x_A <- 1:n_A
y_A <- dbinom(x_A,n_A,p_A)
data <- tibble(x_A = x_A,
               area = append(rep(0,119), seq(from=120,to=10000,by=1)),
               y_A = y_A)
ggplot(data = data) +
  geom_point(aes(x=x_A,y=y_A)) + 
  geom_area(aes(x=area,y=y_A), color="green", fill="green") +
  xlim(50,150)
# We are now ready to quantify to what degree our null-hypothesis is true or false according to the numbers we collected in our experiment.

# Type I and Type II errors
# It is well known that statisticians do not have the same talent in the art of giving names as computer scientists do.  This is well proved by the 
# definition of Type I and Type II errors which are equivalent to the Machine learning definitions of False positive and False negative.
# A Type I error is the probability of rejecting the null-hypothesis when the null-hypothesis is true. In our example this happens when we conclude 
# there is an effect in our AB test variant, while in reality there is none.
# A Type II error is the probability of failing to reject the null-hypothesis when the null-hypothesis is false. In our example this happens when we 
# conclude the AB test variant has no effect, while in reality it actually did.
# In the previous section we quantified the probability of Type I error, which is equivalent to the p-value returned by the binom.test function. To 
# quantify the Type II error we need to know for what 𝑝𝑣𝑎𝑙𝑢𝑒=𝛼 we are willing to reject the null-hypothesis. It is common practice to use 𝛼=0.05
# so I’ll just go with that.
# Now, what is the critical number of conversions such that we reject the null-hypothesis at 𝛼=0.05? This is called the percentile 14 and it is simply
# the 𝑥𝛼 such that:
# 𝑃(𝑋<=𝑥𝛼)=1–𝛼
# which can also be computed easily in R with the following:
alpha <- 0.05; p_A = 0.01
qbinom(p=1-alpha, size=10000,prob=p_A) # =117
qbinom(p=alpha,size = 10000,prob = p_A, lower.tail = F) # =117, the same as previous
# Now we know that starting from 117+1 observations we will reject the null-hypothesis.
# To compute the Type II error we need to assume that the null-hypothesis is false (𝑝𝑏=0.012) and quantify how likely it is to measure a value of 117 or
# less since this is exactly what will lead us to a mistake!
# 𝑃(𝑋𝑏<=117)=𝑃(𝑋𝑏=0)+𝑃(𝑋𝑏=1)…+𝑃(𝑋𝑏=117)
# which is equivalent to:
pbinom(117, 10000, 0.012, lower.tail = T) # =0.4147333
#  gplot(data = tibble(x=1:200,y=dbinom(1:200,10000,0.012))) + geom_point(aes(x,y))
#  ggplot(data = tibble(x=1:200,y=pbinom(1:200,10000,0.012))) + geom_point(aes(x,y))
# which means that we have a ~= 40% chance to conclude our experiment did not have any effect, while in reality there was some (reject alternative hypothesis).
# That’s seems harsh to me. What can we do? Apply the first law of the modern age of statistics which is, get more data. Assuming we can double our data and 
# get 20.000 visitors instead of 10000 and that we measure the same proportions, our Type II error will go down drammatically:
qbinom(p=1-alpha, size=20000,prob=p_A) # =223, the mean also moved to the right!
pbinom(223, 20000, 0.012) # =0.1415655
ggplot(data = tibble(x=1:10000,y1=dbinom(1:10000,10000,0.012), y2=dbinom(1:10000,20000,0.012))) + geom_point(aes(x,y1),color="red") + geom_point(aes(x,y2),color="blue") + xlim(0,500)
#  why means do not correspond? these are binomial distributions; if we build means distribution, means of the means will correspond!
# now we have ~= 14% chance of concluding our experiment did not have any effect. We can even go one step further and see for every possible observation 
# count what’s the expected Type II error by simply brute forcing the computation as follows:
v <- c(); n <- seq(from=1000, to=50000, by=100)
for (i in n) {
  critical_q <- qbinom(p=0.95, size=i, prob = 0.01)
  type2_error <- pbinom(q=critical_q, size=i, prob = 0.012)
  v <- append(v, type2_error)
}
ggplot(data=tibble(x=n,y=v), aes(x,y)) + geom_point() + xlab("Number of observations") + ylab("P(type II error)")
# which seems a fair result, starting from 30.000 visitors we can safely assume that our probability or Type II error will be low.

# Type II error for p_A quantiles
# The analysis as I just presented it is flawed by a fundamental assumption. To estimate the Type I error we assumed that 𝑝𝑏 is exactly 0.012 while 
# to estimate the Type II error we assumed that 𝑝𝑎 is exactly 0.01. We know from the CLT that this is not exactly true and the distributions of those 
# values span across a certain range (see here). So let’s see what happens if I take several points across these distributions, for example 
# the 1%, 25%, 50%, 75%, 99% percentiles and check what happens to our hypothesis testing errors.
# For Type II errors I first collect the possible values of 𝑝𝑎 for my parametric analysis:
p_A <- 0.01; mean_of_means <- p_A; n_A <- 10000; sigma_of_means <- sqrt(p_A * (1-p_A)/n_A)
p_A_means_quantiles <- c(
  qnorm(p=0.01, mean = mean_of_means, sd = sigma_of_means),
  qnorm(p=0.25, mean = mean_of_means, sd = sigma_of_means),
  qnorm(p=0.50, mean = mean_of_means, sd = sigma_of_means),
  qnorm(p=0.75, mean = mean_of_means, sd = sigma_of_means),
  qnorm(p=0.99, mean = mean_of_means, sd = sigma_of_means)
)
# and then I estimate the error exactly like I did previously:
start <- 1000; end <- 50000;
df <- tibble(x = numeric(0), error = numeric(0), parametric_mean = character(0))
#p_A_quantiles <- as.factor(p_A_quantiles)
for (p_A_means_quantile in p_A_means_quantiles) {
  n <- start:(start+end)
  x <- rep(0,length(n))
  error <- rep(0,length(n))
  #parametric_mean <- rep('0', length(n))
  parametric_mean <- rep(0, length(n))
  for (i in n) {
    #p_A_quantile_numeric <- as.numeric(p_A_quantile)
    #p_A_means_quantile_numeric <- p_A_quantile
    critical_q <- qbinom(p=0.95, size=i, prob=p_A_means_quantile)
    type2_error <- pbinom(q=critical_q, size=i, prob=0.012)
    #print(p_A_means_quantile); print(critical_q)
    index <- i - start +1
    x[index] <- i
    error[index] <- type2_error
    parametric_mean[index] <- p_A_means_quantile
    
  }
  df <- rbind(df, tibble(x = x, error = error, parametric_mean = parametric_mean))
}
ggplot(data = df, aes(x=x, y=error, color=as.factor(parametric_mean), group=as.factor(parametric_mean))) +
  geom_line() + xlab("Sample size") + ylab("P(type II error)")
# where the green line is the same as the one plotted here. It is quite interesting to observe the pink line at 𝑝𝑎=0.0123. This is the worst case 
# scenario for us since we picked a value that is greater than 𝑝𝑏=0.012 and because of that our Type II error actually increases with the number 
# of observations! However it is also worth mentioning that this value is very unlikely because the more data I collect the more the uncertainty 
# around the values of 𝑝𝑎 decrease. You may also notice that the lines in the graph are quite thick, and this is because discrete tests like the 
# binomial one 15 have quite large oscillations 16.

# Type I error for p_B means quantiles (parametric)
# The same type of parametric analysis can be performed on the Type I error. Previously we saw how to compute it with the binom.test() function, 
# but we can do the computation manually as follows when 𝑝𝑎=0.01 and 𝑝𝑏=0.012:
pbinom(q=119, size=10000, prob = 0.01, lower.tail = F)
# where 119 is the value starting from which we fail to reject the null-hypothesis. The parametric analysis is a generalisation of this code like 
# we did for the Type II error:
p_B <- 0.012; mean_of_means <- p_B; n_B <- 10000; sigma_of_means <- sqrt(p_B * (1-p_B)/n_B)
p_B_means_quantiles <- c(
  qnorm(p=0.01, mean = mean_of_means, sd = sigma_of_means),
  qnorm(p=0.25, mean = mean_of_means, sd = sigma_of_means),
  qnorm(p=0.50, mean = mean_of_means, sd = sigma_of_means),
  qnorm(p=0.75, mean = mean_of_means, sd = sigma_of_means),
  qnorm(p=0.99, mean = mean_of_means, sd = sigma_of_means)
)
start <- 1000; end <- 50000;
df <- tibble(x = numeric(0), error = numeric(0), parametric_mean = character(0))
for (p_B_means_quantile in p_B_means_quantiles) {
  n <- start:(start+end)
  x <- rep(0,length(n))
  error <- rep(0,length(n))
  parametric_mean <- rep(0, length(n))
  for (i in n) {
    #p_A_quantile_numeric <- as.numeric(p_A_quantile)
    #p_A_means_quantile_numeric <- p_A_quantile
    #critical_q <- qbinom(p=0.95, size=i, prob=p_A_means_quantile)
    expected_B <- i * p_B_means_quantile
    type1_error <- pbinom(q=expected_B - 1, size=i, prob=0.01, lower.tail = F)
    #print(p_A_means_quantile); print(critical_q)
    index <- i - start +1
    x[index] <- i
    error[index] <- type1_error
    parametric_mean[index] <- p_B_means_quantile
    
  }
  df <- rbind(df, tibble(x = x, error = error, parametric_mean = parametric_mean))
}
ggplot(data = df, aes(x=x, y=error, color=as.factor(parametric_mean), group=as.factor(parametric_mean))) +
  geom_line() + xlab("Sample size") + ylab("P(type I error)")
# as in the Type II error we notice that for 𝑝𝑏=0.009 the Type I error actually increase with the amount of data but this value become more and more
# unlikely as the data grows.
# It also very interesting to notice how the value of the two type of errors goes down at a completely different rate. Overall, with this design, we 
# are more likely to stick with the “button colour makes no difference” conclusion. When the reality is that button colour makes no difference, 
# the tests will stick to reality most of the times (Type I error goes down quickly). When the reality is that button colour does make a difference, 
# the test does take the risk of saying there is actually no difference between the two (Type II error goes down slowly).

# Estimate the sample size
# Before wrapping up let’s make a step back and position ourself back in time before we started the experiment. How we can estimate for how long we 
# should run our experiment in order to be confident that our results are statistically significant? To answer this question you simply need to use 
# the same tools we just saw, from a different perspective.
# First, you need to make an estimate around your current baseline conversion. If you use google analytics it should be straightforward to know what
# is the conversion of your checkout page with the green button.
# Second, you need to make a guess on what type of effect size you are willing to detect (minimum detectable effect size). In our example we would 
# have chosen an effect size of 20%. In various disciplines effect sizes have been standardised to make different experiments comparable. Most famous 
# effect size measure is Cohen’s d 17 18.
# Third, you need to assert what risk you are willing to take on the Type I error, or equivalently, what 𝛼 level you are willing to choose for your
# p-value. This is the value you will refer to at the end of the experiment to make your conclusion.
# Finally, you need to assert what risk you are willing to take on the Type II error, or equivalently, how likely you are willing to say your orange 
# button did not perfomed better when in reality there was an effect (i.e. orange button is better). This is equivalent to the power, where you assert 
# how likely you are going to be correct when you conclude that the orange button is better, when it is actually real.
# Mathematically speaking computing the required sample size seems to be a difficult problem in general and I would like to point you to the footnotes 
# for a deeper discussion 19 20 21. Here I will show the approach taken from the Engineering statistics handbook 22.
# First, we are look at the statistics:
#  𝛿=|𝑝𝑎–𝑝𝑏|
# and we would like to compute the sample size n such that:
# (1) P(reject H0 | H0 is true) = 𝛼
# (2) P(reject H0 | H0 is false) = power = 1–𝛽 #???
# Now, you have to use some faith and intuition. What is the smallest value of 𝛿, say 𝛿𝑚𝑖𝑛, that we care about? Our minimum detectable effect 
# size! You can imagine 𝛿𝑚𝑖𝑛 is a function of both (1) and (2). The smallest value in (1) for which we start rejecting is:
#  𝑧1−𝛼∗𝑝𝑎∗(1−𝑝𝑎)𝑁‾‾‾‾‾‾‾‾√
# while the smallest value in (2) for which we start rejecting is:
#  𝑧1−𝛽∗𝑝𝑏∗(1−𝑝𝑏)𝑁‾‾‾‾‾‾‾‾√
# where 𝑝𝑜𝑤𝑒𝑟=1–𝛽 putting it all together we have:
# 𝛿𝑚𝑖𝑛=𝑧1−𝛼∗𝑝𝑎∗(1−𝑝𝑎)𝑁‾‾‾‾‾‾‾‾√+𝑧1−𝛽∗𝑝𝑏∗(1−𝑝𝑏)𝑁‾‾‾‾‾‾‾‾√
# Take the N out of this equation and you get:
# 𝑁=(𝑧1−𝛼∗𝑝𝑎∗(1−𝑝𝑎)√+𝑧1−𝛽∗𝑝𝑏∗(1−𝑝𝑏)√𝛿𝑚𝑖𝑛)2
# So, using our example we state that: (i) our baseline conversion for the green button is 1% or 𝑝𝑎=0.01; (ii) our estimate of effect size is 20% which leads to the orange button converting at 1.2% or 𝑝𝑏=0.012; (iii) we accept a Type I error probability of 5% or 𝛼=0.05; (iv) we want a Power of 80% to make sure we detect the effect when it is there.  This is translated in R as follows:
p_A <- 0.01
p_B <- 0.012
alpha <- 0.05
beta <- 0.2
delta <- abs(p_A - p_B)
t_alpha <- qnorm(1-alpha) # delta = t_alpha + t_beta (draw curves and you will understand)
t_beta <- qnorm(1 - beta)
sd1 <- sqrt(p_A * (1 - p_A))
sd2 <- sqrt(p_B * (1 - p_B))
n <- ((t_alpha * sd1 + t_beta * sd2) / delta)^2


# comparison with even miller's calculator https://www.evanmiller.org/ab-testing/sample-size.html
p_A <- 0.2
p_B <- 0.3
alpha <- 0.05
beta <- 0.2
delta <- abs(p_A - p_B)
t_alpha <- qnorm(1-alpha);t_alpha
t_beta <- qnorm(1-beta); t_beta # or abs(qnorm(beta)) - distance from 0 to quantile
sd1 <- sqrt(p_A * (1-p_A))
sd2 <- sqrt(p_B * (1-p_B))
n <- ((t_alpha * sd1 + t_beta * sd2) / abs(p_A - p_B))^2;n

x <- seq(-4,4,0.01)
df <- tibble(x=x,y=dnorm(x,mean = 0,sd=1))
ggplot(data=df, aes(x,y)) + geom_point() + 
  geom_vline(xintercept =qnorm(1-alpha), color='red') +
  geom_vline(xintercept =qnorm(beta), color='green')

##############################################
# 2. A/B Test Statistics Made Easy. Part 1: Continuous Metrics
# https://towardsdatascience.com/a-b-test-statistics-made-easy-4e94098758d9
# Inputs
se.1 <- 75.3
se.2 <- 72.7
n1 <- 1000
n2 <- 1000
mean.delta <- 7.7

# Outputs

print('standard error')
se <- sqrt((se.1/sqrt(n1))^2 + (se.2/sqrt(n2))^2)
se

print('p-value')
pnorm(q = -mean.delta/se, mean = 0, sd = 1) * 2

print('critical value at alpha = 95%')
crit <- qnorm(p=0.975, mean = 0, sd = 1) * se
crit

print('confidence interval, lower bound')
mean.delta-crit

print('confidence interval, upper bound')
mean.delta+crit

##############################################
# 3. A/B Test Statistics Made Easy. Part 2: Proportion Metrics
# https://towardsdatascience.com/a-b-test-statistics-made-easy-8805ac2533d6

# Now imagine that an A/B test yields the following results:
# A/B group   -   Sample size   Conversions   CR
# Test        -   1000          335           0.335
# Control     -   1000          290           0.290
# Difference  -                 45            0.045
# Has the Test treatment really improved the conversion by 45 customers? Or do the Test and Control groups have identical conversion rates? In which case any difference we observe is just a result of random sampling error?
# Let’s capture these hypotheses more formally:
# - H0 (null hypothesis): The difference in population conversion rates = 0. Any observed difference is due to sampling error.
# - HA (alternative hypothesis): The difference in population conversion rates ≠ 0. It is unlikely that the observed difference is due to sampling error alone.
# So let’s begin by determining what the sampling distribution would look like given the null hypothesis. All we need to know is what the true (population) conversion rate might be. Our best guess would be the average of the sample conversion rates: (0.335 + 0.290) / 2 = 0.3125. And now let’s simulate the distribution that we would see if in fact both Test and Control had conversion rates of 0.3125:
# Simulation
df = tibble(x1 = rbinom(n=1000, size=1000, prob = 0.3125),
            cr1 = x1/1000,
            x2 = rbinom(n=1000, size=1000, prob = 0.3125),
            cr2 = x2/1000,
            delta=cr1-cr2)
ggplot(data=df) + geom_histogram(aes(x=delta), bins = 100)
# A faster way to generate the sampling distribution
# Simulation is a great tool for generating a sampling distribution to demonstrate the impact of sampling error. If we want to move faster, we can, in fact, compute the sampling distribution as follows:
# Step 1: Calculate the pooled sample proportion (p).
# We already calculated this to be 0.3125.
# Here’s the actual formula which is especially useful when the Test and Control groups have different sample sizes:
c1 <- 335 # test conversions
c2 <- 290 # control conversion
n1 <- 1000 # test sample size
n2 <- 1000 # control sample size
p_pooled <- (c1 + c2) / (n1 + n2)
# Step 2: Use p to calculate the standard error (se).
se <- sqrt(p_pooled * (1 - p_pooled) * ((1/n1) + (1/n2)))
# Step 3: Compute a normal distribution with mean = 0, standard deviation = 0.0207.
# In R we would simply write:
y_norm_h0 <- rnorm(1000, mean = 0, sd = 0.0207)
# let's combine simulation with normal data
df$y_norm_h0 = y_norm_h0
ggplot(data=df) +
  geom_density(aes(x=delta), color = 'black', size = 2) +
  geom_density(aes(x=y_norm_h0), color = 'red', size = 2)
  #geom_histogram(aes(x=delta), bins = 100) +
  #geom_histogram(aes(x=y_norm_h0), bins = 100, fill = 'red')

# Statistical analyses in R
c1 <- 335
n1 <- 1000
x2 <- 290
n2 <- 1000
stat <- data.frame(
  metric=c(
    'difference in proportions',
    'pooled sample proportion',
    'se',
    'p-value',
    'critical value',
    'CI-lower',
    'CI-upper'),
  values = c(
    delta <- abs(c1/n1 - c2/n2),
    p <- (c1 + c2) / (n1 + n2),
    se <- sqrt(p * (1-p) * (1/n1 + 1/n2)),
    pnorm(q=-delta, mean=0, sd=se) * 2,
    critical_value <- -qnorm(p=0.025, mean=0, sd=se),
    delta + critical_value,
    delta - critical_value
  )
)
stat


