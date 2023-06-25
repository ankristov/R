install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
flights
view(flights)

jul12 <- filter(flights, month == 7, day == 12)
jul12

sqrt(2) ^ 2 == 2
near(sqrt(2) ^ 2, 2)

1/49 * 49 == 1
near(1/49 * 49, 1)

filter(flights, month == 11 | month == 12)
filter(flights, month %in% c(11,12))
# that weren’t delayed (on arrival or departure) 
# by more than two hours
filter(flights, !(arr_delay > 120 & dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

# Had an arrival delay of two or more hours
x <- filter(flights, arr_delay > 60)
ggplot(data = x) + geom_bar(aes(carrier))
ggplot(data = x) + geom_bar(aes(month, color = carrier))
ggplot(data = flights) + geom_bar(aes(month), fill = "blue")

# Flew to Houston (IAH or HOU)
x <- filter(flights, dest %in% c("IAH","HOU"))
table(x$dest)
# Were operated by United, American, or Delta
x <- filter(flights, carrier %in% c("AA","UA","DL"))
table(x$carrier)
# Departed in summer (July, August, and September)
x <- filter(flights, month %in% c(7,8,9))
table(x$month)
# Arrived more than two hours late, but didn’t leave late
x <- filter(flights, arr_delay > 120 & dep_delay <= 0)
ggplot(data = x) + geom_bar(aes(arr_delay), fill = "red") + geom_bar(aes(dep_delay), fill = "blue")
# Were delayed by at least an hour, but made up over 30 minutes in flight
x <- filter(flights, dep_delay > 60 & ((sched_arr_time - arr_time) >= 30))
x[,c("arr_time","sched_arr_time")]
# Departed between midnight and 6 a.m. (inclusive)
x <- filter(flights, dep_time <= 600)
# How many flights have a missing dep_time?
x <- filter(flights, is.na(dep_time))
nrow(x)
view(x)


ggplot(data = flights) + geom_bar(aes(arr_delay), fill = "red", alpha = 0.5)
ggplot(data = flights) + geom_bar(aes(dep_delay), fill = "red", alpha = 0.5)

# arrange
length(flights$dep_time)
length(flights$dep_time[is.na(flights$dep_time)])
flights_ordered <- arrange(flights, dep_time) %>% mutate(orderN = seq_len(nrow(flights)))
ggplot(data = flights_ordered) + 
  geom_point(aes(x = orderN, y = dep_time)) +
  geom_smooth(aes(x = orderN, y = dep_time))
# Sort flights to find the most delayed flights. 
# Find the flights that left earliest.
arrange(flights, desc(dep_delay))
arrange(flights, dep_delay)
# Sort flights to find the fastest flights
x <- mutate(flights, velocity = distance/(hour + minute/60)) %>% arrange(desc(velocity))
x$velocity
ggplot(data = x) + geom_bar(aes(velocity), binwidth = 10)
summary(x$velocity)
ggplot(x) + geom_boxplot(aes(x = 1, y = velocity))

qq <- quantile(x$velocity, seq(0,1,0.2), na.rm = T)
qq  
x <- mutate(x, velocity.quint = cut(velocity,qq))
x$velocity.quint
ggplot(x) + geom_boxplot(aes(x = velocity.quint, y = velocity, color = velocity.quint))
# Which flights traveled the longest? Which traveled the shortest?
x <- arrange(flights, distance)
head(x$distance,20)
flights[which(flights$distance < 20),]
view(filter(flights, distance < 20))
ggplot(flights) + geom_bar(aes(distance), binwidth = 100)
# lets find distance between origin and destinationa
# for the flight with distance 17 km
filter(flights, distance < 20)
filter(airports, faa %in% c("EWR","LGA"))

# select
select(flights, time_hour, air_time, everything()) # moves to columns to the beginning
# Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights
select(flights, dep_time:arr_delay, -sched_dep_time, -sched_arr_time)
select(flights, dep_time,dep_delay,arr_time,arr_delay)
select(flights, starts_with("dep"), starts_with("arr")) 
# What happens if you include the name of a variable multiple times in a select() call?
select(flights, air_time, air_time)
# What does the one_of() function do?
vars <- c("year1", "month","dep_delay3","arr_delay")
select(flights, one_of(vars))

select(flights, contains("TIME"))
select(flights, contains("TIME", ignore.case = F))

# mutate
flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
)
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)
transmute(flights_sml,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)
transmute(flights,
          dep_time,
          dep_time_h = dep_time %/% 100,
          dep_time_m = dep_time %% 100)

# Convert dep_time and sched_dep_time to a more convenient representa‐ tion of number of minutes since midnight.
flights1 <- mutate(flights, dep_time = (dep_time %/% 100) * 60 + dep_time %% 100) %>%
  mutate(sched_dep_time = (sched_dep_time %/% 100) * 60 + sched_dep_time %% 100) %>% 
  mutate(arr_time = (arr_time %/% 100) * 60 + arr_time %% 100) %>%
  mutate(sched_arr_time = (sched_arr_time %/% 100) * 60 + sched_arr_time %% 100) %>%
  mutate(arr_dep_time = arr_time - dep_time)

select(flights1, dep_time,arr_time,arr_dep_time,sched_dep_time,sched_arr_time)
# Compare air_time with arr_time - dep_time. What do you expect to see? What do you see? What do you need to do to fix it?
f <- select(flights1,arr_time, dep_time,air_time, arr_dep_time) %>%
  arrange(air_time) %>% 
  mutate(N = seq(nrow(flights1)))
f
ggplot(data = f) + 
  geom_smooth(aes(x = N, y = air_time / 60), color = "red",se = F) + 
  geom_smooth(aes(x = N, y = arr_dep_time / 60), color = "blue", se = F)
ggplot(data = f) + geom_boxplot(aes(x = 1, y = air_time), color = "red") + geom_boxplot(aes(x = 2, y = arr_dep_time), color = "blue")
summary(f$air_time)
summary(f$arr_dep_time)
filter(f, air_time > 1440)
ggplot(f) + geom_point(aes(x = N, y = air_time / 60))
#Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers to be related?
f <- select(flights1,dep_time, sched_dep_time, dep_delay) %>%
  mutate(dep_time_delay = dep_time - dep_delay)
f
# Find the 10 most delayed flights using a ranking function. How do you want to handle ties? Carefully read the documentation for min_rank().
filter(flights1, min_rank(dep_delay) < 10)
f <- mutate(flights1, mr = min_rank(dep_delay))
select(f,dep_delay, mr) %>%
  arrange(dep_delay)
head(arrange(f, desc(mr)),10)
ggplot(f) + geom_point(aes(x = dep_delay, y = mr))

# summarize
summarise(flights, delay = mean(dep_delay, na.rm = T))
by_day <- group_by(flights,year,month,day)
summarise(by_day, delay = mean(dep_delay, na.rm = T))
# explore the relationship between the dis‐ tance and average delay for each location
by_dest <- group_by(flights, dest) %>%
  summarise(count = n(), 
            dist = mean(distance, na.rm = T),
            delay = mean(arr_delay, na.rm = T)
  ) %>%
  filter(count > 20, dest != "HNL")
select(by_dest, dest,count,dist,delay)
ggplot(data = by_dest, aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = F)
ggplot(data = arrange(flights, distance), aes(x = distance, y = arr_delay)) +
  geom_point(alpha = 1/3) + geom_smooth(se = F)

not_cancelled <- flights1 %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

# Whenever you do any aggregation, it’s always a good idea 
# to include either a count (n()), or a count of nonmissing 
# values (sum(!is.na(x)))

# look at the planes (identified by their tail number) 
# that have the highest average delays
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(mean_delay = mean(arr_delay), n = n())
ggplot(data = delays, mapping = aes(x = mean_delay)) +
  geom_freqpoly(binwidth = 10)
ggplot(data = delays, mapping = aes(x = n, y = mean_delay)) +
  geom_point(alpha = 1/3)
# Not surprisingly, there is much greater variation 
# in the average delay when there are few flights.
delays %>% 
  filter(n > 25) %>%
  ggplot(mapping = aes(x = n, y = mean_delay)) +
    geom_point(alpha = 1/10)

# Here I use data from the Lahman package to compute the batting average (number of hits / number of attempts) of every major league baseball player.
# When I plot the skill of the batter (measured by the batting average, ba) against the number of opportunities to hit the ball (measured by at bat, ab)
install.packages("Lahman")
library(Lahman)
Lahman::Batting
batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
  group_by(playerID) %>%
  summarise(
    ba = sum(H, na.rm = T) / sum(AB, na.rm = T),
    ab = sum(AB, na.rm = T)
  )
batters %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x = ab, y = ba)) +
    geom_point() +
    geom_smooth(se = F)
# As above, the variation in our aggregate decreases as we get more data points
# There’s a positive correlation between skill (ba) and opportuni‐ ties to hit the ball (ab)

players <- batting %>%
  select(playerID, yearID, teamID, G, AB:H) %>%
  arrange(playerID,yearID, teamID) %>%
  group_by(playerID)
players
# For each player, find the two years with most hits
p <- mutate(players, H_rank = min_rank(H)) %>% filter(H_rank <= 2 & H >0)
table(p$H_rank)
# Within each player, rank each year by the number of games played
p <- mutate(players, G_rank = min_rank(G))
table(p$G_rank)
# For each player, find every year that was better than the previous year
filter(players, G > lag(G))
players$G
lag(players$G)
# For each player, compute avg change in games played per year
mutate(players, G_change = (G - lag(G)) / (yearID - lag(yearID)))
transmute(players, G, lag(G), G - lag(G))
transmute(players, yearID, lag(yearID), yearID - lag(yearID))
transmute(players, G - lag(G), yearID - lag(yearID))
# For each player, find all where they played more games than average
filter(players, G > mean(G, na.rm = T)) # ?? why shows values less then mean?
filter(players, G > 51)
mean(players$G, na.rm = T)
summary(players$G)
# For each, player compute a z score based on number of games played
mutate(players, G_z = (G - mean(G)) / sd(G))

##################################################
# Which of these two proportions is higher: 
# 4 out of 10, or 300 out of 1000?
# http://varianceexplained.org/r/empirical_bayes_baseball/
##################################################
# data preparation
carrier <- batting %>%
  filter(AB > 0) %>%
  group_by(playerID) %>%
  summarise(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)
# playerID -> to real name
carrier <- Master %>%
  as_tibble() %>%
  select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(carrier, by = "playerID") %>%
  select(-playerID)
 arrange(carrier, desc(average)) 
# Are George Abrams, Andrew Albers, Kolby Allard  the best
# players in history? with average hit = 1?? They are not the best
# but lucky  who went up once or twice and got lucky
 
# Let’s look at the distribution of batting averages across players.
 summary(carrier$AB) # let's filter player AB < 500 (we’ll get a better estimate from the less noisy cases)
 carrier_filtered <- carrier %>%
   filter(AB > 500)
 ggplot(data = carrier_filtered) +
  geom_bar(aes(x = average), binwidth = 0.01)
summary(carrier$average)

m <- MASS::fitdistr(carrier_filtered$average, dbeta,
                    start = list(shape1 = 1, shape2 = 10))
alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

carrier_eb <- carrier %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0))
arrange(carrier_eb, desc(eb_estimate)) # the best players
arrange(carrier_eb, eb_estimate) # the worst players
filter(carrier_eb, AB == 1)

######################################################
# average positive delay
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0])
  )
# Why is distance to some destinations more variable
not_cancelled %>%
  group_by(dest) %>%
  summarise(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))
# min, max
not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    first = min(dep_time%/%60 + (dep_time - (dep_time%/%60) * 60) / 100),
    last = max(dep_time%/%60 + (dep_time - (dep_time%/%60) * 60) / 100)
  )
# time in 24 hour scale
not_cancelled %>%
  select(dep_time) %>%
  mutate(
    dep_time_h = dep_time%/%60,
    dep_time_m = dep_time - dep_time_h * 60,
    dep_time_24 = dep_time_h + dep_time_m/100
  )

t24 <- function(time_minutes) {
  time_h <-  time_minutes%/%60
  time_m <-  time_minutes - time_h * 60
  time_24 <-  time_h + time_m/100
  time_24
}
#first, last
not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    first_dep = t24(first(dep_time)),
    last_dep = t24(last(dep_time))
  )

# min_rank
# https://cran.r-project.org/web/packages/dplyr/vignettes/window-functions.html
not_cancelled %>%
  group_by(year,month,day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r)) %>%
  select(dep_time,r)

s <- sample(flights$dep_time, 1000)
e <- cbind(s,min_rank(s),row_number(s), dense_rank(s),cume_dist(s),percent_rank(s))
e <- as_tibble(e)
names(e) <- c("dep_time", "min_rank", "row_number", "dense_rank","cume_dist","percent_rank")
arrange(e,dep_time)
arrange(e,desc(s))
range(min_rank(s), na.rm = T)
range(row_number(s), na.rm = T)
range(dense_rank(s), na.rm = T)
range(cume_dist(s), na.rm = T)
range(percent_rank(s), na.rm = T)
# To select (for example) the top 10% of records within group
filter(not_cancelled,cume_dist(desc(dep_delay)) < 0.1)
summary(not_cancelled$dep_delay)
ggplot(data = not_cancelled) + 
  geom_point(aes(cume_dist(dep_delay), dep_delay)) +
  geom_point(aes(cume_dist(desc(dep_delay)) < 0.1, dep_delay), color = "red")
# ntile() divides the data up into n evenly sized buckets
# we could use ntile() to divide the players within a team into 
# four ranked groups, and calculate the average number of games within each group
group_by(batting, teamID, playerID) %>%
  summarise(G = sum(G)) %>%
  group_by(quartile = ntile(G,4)) %>%
  summarise(mean(G))

# lead(), lag()
# Compute the relative change in games played
mutate(players, G_delta = G - lag(G)) # but when we shift values of different players mix??
# Find when a player changed teams
filter(players, teamID != lag(teamID))
players %>% group_by(teamID, playerID) %>% 
  summarise(n = n())
# order_by argument
df <- data.frame(year = 2000:2005, value = (0:5) ^ 2)
scrambled <- df[sample(nrow(df)), ]
wrong <- mutate(scrambled, prev_value = lag(value))
arrange(wrong, year)
right <- mutate(scrambled, prev_value = lag(value, order_by = year))
arrange(right, year)

# n(), n_distinct(), count()
flights_sub <- not_cancelled %>%
  group_by(dest) %>%
  summarise(carriers_unique = n_distinct(carrier),carriers_total = n()) %>%
  arrange(desc(carriers_unique))
ggplot(data = filter(not_cancelled, dest == "ATL")) +
  geom_point(aes(t24(dep_time),carrier, color = carrier)) + coord_flip()

not_cancelled %>%
  count(dest)
not_cancelled %>%
  count(carrier)

flight_airlines <- airlines %>%
  inner_join(not_cancelled, by = "carrier")
flight_airlines
flight_airlines %>% rename(airline_name = name) %>%
  count(airline_name)

# to “count” (sum) the total number of miles a plane flew
not_cancelled %>% count(tailnum)
not_cancelled %>% count(tailnum, wt = distance) %>% arrange(desc(n))
not_cancelled %>% group_by(tailnum) %>%
  mutate(total_miles = sum(distance)) %>%
  select(tailnum, total_miles) %>%
  arrange(desc(total_miles))

# How many flights left before 5am? TRUE == 1, FALSE == 0
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(n_early = sum(dep_time < 300))
# What proportion of flights are delayed by more than an hour?
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(delayed_1h = mean(arr_delay > 60))

# A flight is always 10 minutes late
not_cancelled %>% group_by(tailnum) %>%
  summarise(delayed_10min = mean(arr_delay > 10)) %>%
  filter(delayed_10min == 1)
# A flight is 30 minutes early 50% of the time, and 
# 30 minutes late 50% of the time
not_cancelled %>% group_by(tailnum) %>%
  summarise(delayed_30min = mean(arr_delay > 30)) %>%
  filter(delayed_30min == 0.5)
not_cancelled %>% group_by(tailnum) %>%
  summarise(delayed = mean(arr_delay))

# Look at the number of cancelled flights per day. Is there a pattern?
f1 <- not_cancelled %>% group_by(year, month, day) %>% 
  summarise(avg_delay = mean(arr_delay)) %>%
  unite("Date",year,month,day,sep = "-")
f1
f1$Date <- as.POSIXct(f1$Date)
f1
f2 <- flights %>%  group_by(year, month, day) %>%
  summarise(n_canceled = sum(is.na(dep_delay) & is.na(arr_delay)),
            n = n(),
            proportion = n_canceled / n) %>%
  unite("Date",year,month,day,sep = "-")
f2
f2$Date <- as.POSIXct(f2$Date)
f2
ggplot() + 
  geom_smooth(data = f1, aes(x = Date, y = avg_delay), color = "red", se = F) +
  geom_smooth(data = f2, aes(x = Date, y = proportion * 100), color = "blue", se = F)

# Which carrier has the worst delays?
not_cancelled %>% group_by(carrier) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot() + geom_point(aes(carrier, avg_delay, color = carrier))
#can you disen‐ tangle the effects of bad airports versus bad carriers?
not_cancelled %>% group_by(carrier, dest) %>% 
  summarise(n(), avg_delay = mean(dep_delay))
not_cancelled %>% group_by(dest) %>% 
  summarise(n(), avg_delay = mean(dep_delay))
# For each plane, count the number of flights before the first delay of greater than 1 hour.
f <- not_cancelled %>% 
  group_by(year,month,day,tailnum) %>% 
  filter(dep_delay < 60) %>%
  summarise(n())
f %>% filter(tailnum ==  "N0EGMQ")

# let's check
f <- flights %>% filter(tailnum ==  "N0EGMQ") %>% 
  unite("Date",year,month,day,sep="-")
f$Date <- as.POSIXct(f$Date)
f %>% arrange(Date) %>% select(Date, tailnum,dep_delay)

# Find all groups bigger than a threshold
# popular flights
flights %>% group_by(dest) %>%
  filter(n() > 365) %>% summarise(n())

flights %>% group_by(dest) %>%
  filter(n() > 365) %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)
# Which plane (tailnum) has the worst on-time record?
not_cancelled %>%
  group_by(tailnum) %>%
  mutate(avg_delay = mean(dep_delay)) %>%
  arrange(desc(avg_delay)) %>% select(tailnum, avg_delay)

# What time of day should you fly if you want to avoid delays as
# much as possible? 
not_cancelled %>% 
  filter(dep_delay>0) %>%
  mutate(intervals = cut(dep_time/60, breaks = seq(0,24,1))) %>%
  group_by(intervals) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot() + geom_point(aes(intervals, avg_delay, color = intervals), show.legend = F) +
  geom_line(aes(intervals, avg_delay))

# For each destination, compute the total minutes of delay. 
# For each flight, compute the proportion of the total delay for its destination.
f <- not_cancelled %>%
  group_by(dest) %>%
  mutate(total_delay = sum(arr_delay)) %>%
  mutate(prop_delay = arr_delay / total_delay) %>% select(dest,total_delay, prop_delay)
f %>% filter(dest == "ATL")

# Find all destinations that are flown by at least two carriers. Use that information to rank the carriers
not_cancelled %>% group_by(dest) %>%
  summarise(n_distinct(carrier))

#####################################################
# Exploratory data analysis
#####################################################
# categorical var visualizftion
ggplot(data = diamonds) + 
  geom_bar(aes(x = cut))
diamonds %>% count(cut)
# categorical var visualization
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5) +
  geom_vline(aes(xintercept = mean(carat)), color = "green") +
  geom_vline(aes(xintercept = median(carat)), color = "yellow")

diamonds %>%
  count(cut_width(carat, 0.5))

diamonds %>%
  filter(carat < 3) %>%
  ggplot() + geom_histogram(aes(x = carat), binwidth = 0.1)

diamonds %>%
  filter(carat < 3) %>%
  ggplot() + geom_histogram(aes(x = carat), binwidth = 0.01)
diamonds %>%
  filter(carat < 3) %>%
  ggplot() + geom_freqpoly(aes(x = carat, color = cut), binwidth = 0.1)

# unusual values
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0,50))
# This allows us to see that there are three unusual values: 0, ~30, and ~60.
unusual <- diamonds %>%
  filter(y < 3 | y > 20) %>%
  arrange(y)
unusual

# Explore the distribution of each of the x, y, and z variables in diamonds. What do you learn? Think about a diamond and how you might decide which dimension is the length, width, and depth.
diamonds %>% 
  ggplot() + geom_histogram(aes(x = x), binwidth = 0.5)
diamonds %>% 
  ggplot() + geom_histogram(aes(x = y), binwidth = 0.5)
diamonds %>% ggplot() + 
  geom_histogram(aes(x = x), binwidth = 0.1, alpha = 0.5, fill = "red") +
  geom_histogram(aes(x = y), binwidth = 0.1, alpha = 0.5, fill = "blue") +
  geom_histogram(aes(x = z), binwidth = 0.1, alpha = 0.5, fill = "green") +
  coord_cartesian(xlim = c(2,10))
diamonds %>% ggplot() + 
  geom_freqpoly(aes(x = x), binwidth = 0.1, color = "red") +
  geom_freqpoly(aes(x = y), binwidth = 0.1, color = "blue") +
  geom_freqpoly(aes(x = z), binwidth = 0.1, color = "green") +
  coord_cartesian(xlim = c(0,10)) +
  labs(x = "XYZ sizes")
diamonds %>% ggplot() +
  geom_point(aes(x = x, y = y))
# Explore the distribution of price. Do you discover anything unusual or surprising? (Hint: carefully think about the bin width and make sure you try a wide range of values.)
diamonds %>% ggplot() +
  geom_histogram(aes(x = price), binwidth = 100) +
  coord_cartesian(xlim = c(0,2500))
sub <- filter(diamonds, price %in% 1400:1600)
summary(sub$price)

diamonds %>% count(cut_width(price, 100))

# How many diamonds are 0.99 carat? How many are 1 carat? What do you think is the cause of the difference?
diamonds %>% ggplot() +
  geom_histogram(aes(carat), binwidth = 0.01) + 
  coord_cartesian(xlim = c(0.9,1.1))
diamonds %>% filter(carat == 0.99)
diamonds %>% filter(carat == 1)

# Compare and contrast coord_cartesian() versus xlim() or ylim() when zooming in on a histogram. What happens if you leave binwidth unset? What happens if you try and zoom so only half a bar shows?
diamonds %>% ggplot() +
  geom_histogram(aes(carat), binwidth = 0.01) + 
  xlim(c(0.9,1.1))

# missing values
# in nycflights13::flights, missing values in the dep_time variable indicate that the flight was cancelled. 
# let's compare the scheduled departure times for cancelled and noncancelled times
flights %>% mutate(
  cancelled = is.na(dep_time),
  sched_dep_time_24 = sched_dep_time %/% 100 + sched_dep_time %% 100 / 60
) %>%
  ggplot() +
  geom_freqpoly(aes(x = sched_dep_time_24, color = cancelled))

# let's explore different color palettes
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(8, "Dark2")

install.packages("wesanderson")
library(wesanderson)
names(wes_palettes)

# covariance
# let’s explore how the price of a diamond varies with its quality
diamonds %>%
  ggplot() + geom_freqpoly(aes(x = price, color = cut),binwidth = 500) +
  scale_color_brewer(palette = "Set1")
diamonds %>%
  ggplot() + geom_freqpoly(aes(x = price, color = cut), binwidth = 500) +
  scale_color_manual(values = wes_palette("Darjeeling1"))
# It’s hard to see the difference in distribution because the overall counts differ so much
diamonds %>% count(cut)
# let's display on y axis density instead of count
diamonds %>%
  ggplot() + geom_freqpoly(aes(x = price, y = ..density.., color = cut), binwidth = 500) +
  scale_color_manual(values = wes_palette("Darjeeling1"))
# it appears that fair diamonds (the lowest quality) have the highest average price!
diamonds %>% group_by(cut) %>%
  summarise(mean(price))

# boxplot() to display the distribution of a continuous vari‐ able
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()
# It supports the counterintuitive finding that better quality diamonds are cheaper on average!
# reorder
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(
    x = reorder(class, hwy, FUN = median),
    y = hwy))
# if long var names than
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(
    x = reorder(class, hwy, FUN = median),
    y = hwy)) +
  coord_flip()

# Use what you’ve learned to improve the visualization of the departure times of cancelled versus noncancelled flights
f <- flights %>% 
  mutate(cancelled = is.na(dep_time) | is.na(arr_time)) %>%
  unite("date_str", year,month,day,sep = "-") %>%
  mutate(date = as.POSIXct(date_str))
f$cancelled
f %>% ggplot() + geom_bar(aes(cancelled)) +
  scale_color_manual(values = wes_palette("Darjeeling1"))

f %>% ggplot() +
  geom_freqpoly(aes(
    x = sched_dep_time %/% 100 + sched_dep_time %% 100 / 60, 
    y = ..density.., 
    color = cancelled
    ), binwidth = 1)

# What variable in the diamonds dataset is most important for predicting the price of a diamond?

diamonds %>% 
  filter(x > 0, y > 0, z > 0) %>%
  ggplot() +
  geom_count(aes(x = x, y = price, size = ..n.., color= ..n..), alpha = 0.3) +
  guides(color = 'legend') +
  geom_smooth(aes(x = x, y = price), color = "blue", se = F) +
  scale_size_area(max_size = 10)
diamonds %>% 
  filter(x > 0, y > 0, z > 0) %>%
  ggplot() +
  geom_smooth(aes(x = x, y = price), color = "red", se = F) +
  geom_smooth(aes(x = y, y = price), color = "green", se = F) +
  geom_smooth(aes(x = z, y = price), color = "blue", se = F) +
  coord_cartesian(xlim = c(0,60))
ggplot(data = diamonds) +
  geom_smooth(aes(x = carat, y = price), se = F)

# How is that variable correlated with cut? 
ggplot(data = diamonds) +
  geom_smooth(aes(x = carat, y = price, color = cut), se = F) +
  scale_color_brewer(palette = "Set1")
  #scale_color_manual(values = wes_palette("Darjeeling1"))
ggplot(data = diamonds) +
  geom_boxplot(aes(
    x = reorder(cut(carat,breaks = 0:10), price, FUN = median),
    y = price, 
    color = cut)) +
  scale_color_brewer(palette = "Set1")

# Install the ggstance package, and create a horizontal boxplot. How does this compare to using coord_flip()?
install.packages("ggstance")
library("ggstance")
diamonds %>% 
  ggplot() + geom_boxplot(aes(cut,price,fill = cut)) 
diamonds %>% 
  ggplot() + geom_boxplot(aes(cut,price,fill = cut)) +
  coord_flip()
diamonds %>% 
  ggplot() + geom_boxploth(aes(price,cut,fill = cut))
# One problem with boxplots is that they were developed in an era of much smaller datasets and tend to display a prohibitively large number of “outlying values.” One approach to remedy this problem is the letter value plot
install.packages("lvplot")
library("lvplot")
diamonds %>% 
  ggplot() + geom_lv(aes(cut,price,fill = cut)) +
  scale_fill_brewer()
diamonds %>% 
  ggplot() + 
  geom_lv(aes(cut,price,fill = cut)) +
  geom_jitter(aes(cut,price),width = 0.2, alpha = 0.2)
diamonds %>% 
  ggplot() + 
  geom_lv(aes(cut,price,fill = cut)) +
  geom_count(aes(cut,price), alpha = 0.1, position = "jitter")
# Compare and contrast geom_violin() with a faceted geom_his togram(), or a colored geom_freqpoly(). What are the pros and cons of each method?
diamonds %>% 
  ggplot() + 
  geom_violin(aes(cut,price))
diamonds %>% 
  ggplot(aes(cut,price)) + 
  geom_jitter(height = 0, width = 0.4,alpha = 0.1) + 
  geom_violin(color = "cyan")
diamonds %>% ggplot() +
  geom_histogram(aes(price)) +
  facet_wrap(~ cut, nrow = 2, ncol = 3)
diamonds %>% ggplot() +
  geom_freqpoly(aes(price, color = cut)) +
  scale_color_brewer(palette = "Set1")
# The ggbeeswarm package provides a number of methods similar to geom_jitter(). List them and briefly describe what each one does.
install.packages("ggbeeswarm")
library("ggbeeswarm")
packageDescription("ggbeeswarm")
diamonds %>% ggplot() +
  geom_point(aes(x, price), color = "blue", alpha = 0.1)
diamonds %>% ggplot() +
  geom_jitter(aes(x, price), color = "blue", alpha = 0.1, width = 1)
diamonds %>% ggplot() +
  geom_quasirandom(aes(x, price))

mpg %>% ggplot() +
  geom_point(aes(cyl, hwy), color = "blue")
mpg %>% ggplot() +
  geom_jitter(aes(cyl, hwy), color = "blue",width = 0.1)
mpg %>% ggplot() +
  geom_quasirandom(aes(cyl, hwy), color = "blue")

# Two Categorical Variables
ggplot(data = diamonds) +
  geom_count(aes(cut, color))
diamonds %>%
  count(color,cut) %>%
  ggplot(aes(x = cut, y = color)) +
  geom_tile(aes(fill = n)) 
# Use geom_tile() together with dplyr to explore how average flight delays vary by destination and month of year. What makes the plot difficult to read?
install.packages("seriation")
library("seriation")
# n
not_cancelled %>%
  count(month,dest) %>%
  ggplot(aes(dest,month)) +
  geom_tile(aes(fill = n)) +
  scale_color_brewer(palette = "Set1")
not_cancelled %>%
  filter(dest == "ATL") %>%
  count(month) %>%
  ggplot(aes(month, "ATL")) +
  geom_tile(aes(fill = n)) 
not_cancelled %>%
  filter(dest %in% c("ATL","SJU","ORD","MIA","CLT")) %>%
  count(month,dest) %>%
  ggplot(aes(dest,month)) +
  geom_tile(aes(fill = n))
# delay
not_cancelled %>%
  group_by(dest, month) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot() + 
  geom_tile(aes(month, dest, fill = cut(avg_delay, quantile(avg_delay,probs = seq(0, 1, 0.2))))) +
  labs(fill = "Average delay")
# step by step
f <- not_cancelled %>%
  group_by(dest, month) %>%
  summarise(avg_delay = mean(dep_delay))
q <- quantile(f$avg_delay,probs = seq(0, 1, 0.2))
f <- f %>% mutate(avg_delay_q = cut(avg_delay, q))
f %>% ggplot() +
  geom_tile(aes(month, dest, fill = cut(avg_delay, quantile(avg_delay,probs = seq(0, 1, 0.2))))) +
  labs(fill = "Average delay")
# for several dest
not_cancelled %>%
  filter(dest %in% c("ATL","SJU","ORD","MIA","CLT","SYR","STT","HOU","PHX")) %>%
  group_by(dest, month) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot() + 
  geom_tile(aes(month, dest, fill = cut(avg_delay, quantile(avg_delay,probs = seq(0, 1, 0.2))))) +
  labs(fill = "Average delay")
# average by month
not_cancelled %>%
  group_by(month) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot() + 
  geom_tile(aes(month, "Average", fill = cut(avg_delay, quantile(avg_delay,probs = seq(0, 1, 0.25))))) +
  labs(fill = "Average delay")

# Two Continuous Variables
ggplot(data = diamonds) +
  geom_point(aes(carat,price),color = "blue")  
ggplot(data = diamonds) +
  geom_point(aes(carat,price),color = "blue") +
  geom_point(data = filter(diamonds, carat %in% c(0.5,1,1.5,2,2.5,3,4,5)), aes(carat,price), color = "red") +
  coord_cartesian(xlim = c(0,3))
# alpha
ggplot(data = diamonds) +
  geom_point(aes(carat,price),color = "blue", alpha = 0.01) +
  coord_cartesian(xlim = c(0,3))
# geom_bin2d(), geom_hex
install.packages("hexbin")
library(hexbin)
ggplot(data = diamonds) +
  geom_bin2d(aes(carat,price))
ggplot(data = diamonds) +
  geom_hex(aes(carat,price),bins = 200)
# Another option is to bin one continuous variable so it acts like a cat‐ egorical variable
ggplot(diamonds, aes(carat,price)) +
  geom_boxplot(aes(group = cut_width(carat,0.5)))
# let's make boxplots width proportional to number of items
ggplot(diamonds, aes(carat,price)) +
  geom_boxplot(aes(group = cut_width(carat,0.5)),varwidth = T)
ggplot(diamonds, aes(carat,price)) +
  geom_boxplot(aes(group = cut_number(carat,10)))
# let's use frequency polygon
ggplot(diamonds, aes(price)) +
  geom_freqpoly(aes(group = cut_number(carat,10), color = cut_number(carat,10)))
ggplot(diamonds, aes(price)) +
  geom_freqpoly(aes(group = cut_width(carat,0.1), color = cut_width(carat,0.1)))
# for carat
ggplot(diamonds, aes(carat)) +
  geom_freqpoly(aes(group = cut_number(price,10), color = cut_number(price,10))) 

# outliers
ggplot(diamonds) +
  geom_point(aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4,11), ylim = c(4,11))
ggplot(diamonds) +
  geom_hex(aes(x = x, y = y), bins = 100) +
  coord_cartesian(xlim = c(4,11), ylim = c(0,20)) +
  theme(aspect.ratio = 1)
  #coord_fixed() +




