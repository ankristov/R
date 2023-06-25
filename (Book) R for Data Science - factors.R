#####################################
### Factors
#####################################
library(tidyverse)
library(forcats)
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
sort(x1) # sorting of the month-strings doesn't work in the way we want
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(x = x1, levels = month_levels)
sort(y1)
# And any values not in the set will be silently converted to NA:
y2 <- factor(x = x2, levels = month_levels)
y2
# to check for the error
y2 <- parse_factor(x = x2, levels = month_levels)
y2

# General Social Survey
forcats::gss_cat
str(gss_cat)
summary(gss_cat)
# write to csv for use in Tableau
dim(gss_cat)
dim(na.omit(gss_cat))
str(na.omit(gss_cat))
write_csv(x = na.omit(gss_cat), 
          path = "/Users/Andrew/Desktop/Tableau/gss_cat.csv")
# When factors are stored in a tibble, you can’t see their levels so easily. 
# One way to see them is with count()
gss_cat %>%
  count(race)
# Or with a bar chart:
ggplot(gss_cat, aes(race)) + geom_bar()
# By default, ggplot2 will drop levels that don’t have any values. You can force them to display with
ggplot(gss_cat, aes(race)) + geom_bar() + scale_x_discrete(drop = F)
table(gss_cat$race)
# 1. Explore the distribution of rincome (reported income). What makes 
# the default bar chart hard to understand? How could you improve the plot?
table(gss_cat$rincome)
gss_cat %>% count(rincome)
ggplot(gss_cat) + geom_bar(aes(rincome)) + coord_flip()
# 2. What is the most common relig in this survey? What’s the most common partyid?
gss_cat %>% count(relig) %>% arrange(desc(n))
ggplot(gss_cat) + geom_bar(aes(relig)) + coord_flip()
ggplot(gss_cat) + geom_bar(aes(relig)) + coord_flip()
# 3. Which relig does denom (denomination) apply to? How can you find out 
# with a table? How can you find out with a visualization?
gss_cat %>% count(denom)
table(gss_cat$denom, gss_cat$relig)
ggplot(gss_cat) + geom_tile(aes(gss_cat$denom, gss_cat$relig))
ggplot(gss_cat) + geom_tile(aes(gss_cat$relig, gss_cat$denom))

# Modifying Factor Order
# It’s often useful to change the order of the factor levels in a visualization. 
# For example, imagine you want to explore the average number of hours 
# spent watching TV per day across religions
ggplot(gss_cat) + 
  geom_boxplot(aes(x = relig, y = tvhours, fill = relig)) +
  coord_flip()
relig <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = T),
    tvhours = mean(tvhours, na.rm = T),
    n = n()
  )
relig
relig %>% ggplot() + geom_point(aes(tvhours, relig))
# It is difficult to interpret this plot because there’s no overall pattern. 
# We can improve it by reordering the levels of relig using fct_reorder()
# fct_reorder() takes three arguments:
# • f, the factor whose levels you want to modify.
# • x, a numeric vector that you want to use to reorder the levels.
# • Optionally, fun, a function that’s used if there are multiple values of x for each value of f. The default value is median.
relig %>% ggplot() + geom_point(aes(tvhours, fct_reorder(relig, tvhours)))
# As you start making more complicated transformations, I’d recommend 
# moving them out of aes() and into a separate mutate() step. For example, you could rewrite the preceding plot as
relig %>% 
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) + geom_point()
# What if we create a similar plot looking at how average age varies
# across reported income level?
gss_cat %>% count(rincome)
gss_cat %>% count(age)
income_age <- gss_cat %>% group_by(rincome) %>%
  summarise(mean_age = mean(age, na.rm = T),
            n = n()) 
income_age
ggplot(income_age) + geom_point(aes(x = mean_age, y = fct_reorder(rincome, mean_age)))
# Here, arbitrarily reordering the levels isn’t a good idea! That’s 
# because rincome already has a principled order that we shouldn’t mess with
# However, it does make sense to pull “Not applicable” to the front with
# the other special levels. You can use fct_relevel(). 
ggplot(
  income_age, aes(mean_age, fct_relevel(rincome, "Not applicable"))
) + geom_point()
# Another type of reordering is useful when you are coloring the lines 
# on a plot. fct_reorder2() reorders the factor by the y values associated 
# with the largest x values. This makes the plot easier to read because 
# the line colors line up with the legend
by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count() %>%
  mutate(prop = as.double(n / sum(n))) # prop not double, but int. why??
by_age
dim(by_age)
dim(gss_cat)
ggplot(by_age, aes(age, n / sum(n), color = marital)) +
  geom_line(na.rm = T)
ggplot(by_age, aes(age, prop, color = fct_reorder2(marital, age, by_age$n / sum(by_age$n)))) +
  geom_line() +
  labs(color = "marital")
# Finally, for bar plots, you can use fct_infreq() to order levels in 
# increasing frequency: . You may want to combine with fct_rev()
gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) + geom_bar()

# 1. There are some suspiciously high numbers in tvhours. Is the mean a 
# good summary?
ggplot(gss_cat) + geom_boxplot(aes(1, tvhours))
ggplot(gss_cat) + geom_violin(aes(1, tvhours)) +
  geom_hline(yintercept = mean(gss_cat$tvhours, na.rm = T), color = "red") +
  geom_hline(yintercept = median(gss_cat$tvhours, na.rm = T), color = "magenta")

# Modifying factors levels
gss_cat %>% count(partyid)
# The levels are terse and inconsistent. Let’s tweak them to be longer 
# and use a parallel construction
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong" = "Strong republican",
                              "Republican, weak" = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  )) %>%
  count(partyid)
# To combine groups, you can assign multiple old levels to the same 
# new level:
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party"
  )) %>%
  count(partyid)
# If you want to collapse a lot of levels, fct_collapse() is a useful variant of fct_recode()
gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)
# Sometimes you just want to lump together all the small groups to 
# make a plot or table simpler. That’s the job of fct_lump()
table(gss_cat$relig)
levels(gss_cat$relig)
gss_cat %>% count(relig)
gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)
# In this case it’s not very helpful: it is true that the majority of 
# Americans in this survey are Protestant, but we’ve probably overcollapsed.
gss_cat %>% 
  mutate(relig = fct_lump(f = relig, n = 5)) %>%
  count(relig, sort = T) %>%
  print(n = Inf)
gss_cat %>% count(rincome)
gss_cat %>%
  mutate(rincome = fct_lump(rincome)) %>%
  count(rincome)

# 1. How have the proportions of people identifying as Democrat, Republican,
# and Independent changed over time?
head(gss_cat)
levels(gss_cat$partyid)
gss_cat %>% 
  mutate(partyid = fct_collapse(partyid,
                                Democrat = c("Strong democrat", "Not str democrat"),
                                Independent = c("Ind,near dem", "Independent", "Ind,near rep"),
                                Republican = c("Not str republican", "Strong republican"),
                                Others = c("Other party", "Don't know", "No answer")
  )) %>% 
  select(year, partyid) %>%
  group_by(year, partyid) %>% count() %>%
  ggplot() + geom_line(aes(year, n / sum(n), color = partyid))
