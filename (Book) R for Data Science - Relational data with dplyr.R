##################################################
### Relational Data with dplyr ###################
##################################################
library(tidyverse)
library(nycflights13)
airlines
airports
planes
weather
flights
# Once you’ve identified the primary keys in your tables, it’s good practice 
# to verify that they do indeed uniquely identify each observation
planes %>% count(tailnum) %>% filter(n > 1)
weather %>% count(year, month, day, hour, origin) %>%
  filter(n > 1)
# Sometimes a table doesn’t have an explicit primary key. For example, what’s 
# the primary key in the flights table? You might think it would be the date 
# plus the flight or tail number, but neither of those are unique
flights %>% count(year, month, day, flight) %>%
  filter(n > 1) %>% nrow()
flights %>% count(year, month, day, tailnum) %>%
  filter(n > 1) %>% nrow()
# If a table lacks a primary key, it’s sometimes useful to add one with mutate() and row_number()
# Exercise
# 1. Add a surrogate key to flights
head(flights)
flights1 <- flights %>% mutate(surrogate_key = paste('flight', row_number(), sep = '')) 
flights1 %>% select(surrogate_key)

# 2. Identify the keys in the following datasets
# a. Lahman::Batting
library(Lahman)
head(Batting) 
# primary key playerID?
Batting %>% count(playerID) %>% filter(n > 1)
# no, why? let's investigate
Batting %>% filter(playerID == 'aardsda01')
# it seems that the key is playerID + yearID
Batting %>% count(playerID, yearID) %>% filter(n > 1)
# no, why?
Batting %>% filter(playerID == 'abadfe01')
# it seems that the key is playerID + yearID + teamID
Batting %>% count(playerID, yearID, teamID) %>% filter(n > 1)
# id seems that the key consists of all variable with 'ID'
Batting %>% count(playerID, yearID, teamID, lgID) %>% filter(n > 1)
# no
Batting %>% filter(playerID == 'alyeabr01')
# we have to include stint?
Batting %>% count(playerID, yearID, teamID, lgID, stint) %>% filter(n > 1)
# yes

# b. babynames::babynames
install.packages("babynames")
library(babynames)
babynames::babynames
# the key is name?
babynames %>% count(name) %>% filter(n > 1)
# no
babynames %>% filter(name == 'Aaban')
babynames %>% count(name, year) %>% filter(n > 1)
babynames %>% count(name, n, year) %>% filter(nn > 1)
babynames %>% count(name, sex) %>% filter(n > 1)
babynames %>% count(name, sex, year) %>% filter(n > 1) # yes

# c. nasaweather::atmos
install.packages("nasaweather")
library(nasaweather)
nasaweather::atmos
atmos %>% count(lat, long, year, month) %>% filter(n > 1) # yes

# 3. Draw a diagram illustrating the connections between the Batting, Master, 
# and Salaries tables in the Lahman package
head(Batting)
head(Master)
head(Salaries)
# Draw another diagram that shows the relationship between Mas ter, Managers, 
# and AwardsManagers
head(Master)
head(Managers)
head(AwardsManagers)

# Mutating joins
flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2
# let's add the full airline name to the flights2 
head(airlines)
flights2 %>% select(-origin, -dest) %>%
  left_join(airlines, by = 'carrier')
# we can do the same with mutate()
flights2 %>%
  select(-origin, -dest) %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])
# how join works
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_x,
  1, "y1",
  2, "y2",
  4, "y3"
)
left_join(x, y, by = "key")
inner_join(x, y, by = "key")
right_join(x, y, by = "key")
# Duplicate Keys
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4")
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2" )
left_join(x, y, by = "key")

# Exercise
# 1. Compute the average delay by destination, then join on the air ports data frame so 
# you can show the spatial distribution of delays
library(nycflights13)
head(flights)
head(airports)
flights %>% group_by(dest) %>% 
  summarise(avg_delay = mean(dep_delay, na.rm = T)) %>%
  left_join(airports, by = c('dest' = 'faa')) %>%
  ggplot(aes(lon, lat)) +
  borders('state') + 
  geom_point(aes(color = avg_delay + 10)) +
  #scale_color_gradient(low = "green", high = "red") +
  coord_quickmap()
# 2. Add the location of the origin and destination (i.e., the lat and lon) to flights
head(flights)
head(airports)
flights %>% left_join(select(airports, faa, lat, lon), by = c('dest' = 'faa')) %>%
  left_join(select(airports, faa, lat, lon), by = c('origin' = 'faa'))
# 3. Is there a relationship between the age of a plane and its delays?
head(planes)
flights %>% group_by(tailnum) %>% 
  summarise(avg_delay = mean(dep_delay, na.rm = T)) %>%
  left_join(select(planes, tailnum, year), by = c('tailnum' = 'tailnum')) %>%
  na.omit() %>% mutate(age = 2019 - year) %>%
  group_by(age) %>% summarise(mean_delay_age = mean(avg_delay)) %>%
  ggplot() + 
  geom_point(aes(age, mean_delay_age)) + 
  geom_smooth(aes(age, mean_delay_age), se = F)
# 4. What weather conditions make it more likely to see a delay?
head(weather)
head(flights)
range(flights$hour)
range(weather$hour)
range(flights$day)
range(weather$day)
?weather
# let's check weather delay depends on Precipitation (осадки)
flights %>% mutate(dep_hour = dep_time %/% 100, dep_minuts = dep_time %% 100) %>%
  na.omit() %>%
  left_join(weather, by = c('origin' = 'origin', 
                            'year' = 'year', 
                            'month' = 'month',
                            'day' = 'day',
                            'dep_hour' = 'hour')) %>%
  select(dep_time, precip) %>%
  group_by(precip) %>% summarise(mean_dep_time_precip = mean(dep_time)) %>%
  ggplot(aes(precip, mean_dep_time_precip)) + geom_point() + geom_smooth(se = F)
# 5. What happened on June 13, 2013? Display the spatial pattern of delays, and then use Google to cross-reference with the weather.
# let's plot dep delay versus date
head(flights)
head(select(flights, year, month, day, dep_delay))
range(flights$year)
range(flights$day)
flights %>% select(year, month, day, dep_delay) %>% filter(!is.na(dep_delay)) %>%
  mutate(month_day = month + 0.01 * day) %>%
  group_by(month_day) %>% summarise(mean_dep_delay = mean(dep_delay)) %>%
  ggplot(aes(month_day, mean_dep_delay)) + geom_point() + geom_smooth(se = F) +
  geom_vline(xintercept = 6.13, color = 'magenta')

# Filtering joints
# semi-join, which connects the two tables like a mutating join, but instead of 
# adding new columns, only keeps the rows in x that have a match in y
top_dest <- flights %>%
  count(dest, sort = T) %>%
  head(10)
top_dest  
flights %>% semi_join(top_dest)
table(semi_join(flights, top_dest)$dest)
# Anti-joins are useful for diagnosing join mismatches. 
# How can know that which flights don’t have a match in planes
flights %>% anti_join(planes, by = 'tailnum') %>%
  count(tailnum, sort = T)

# Exercise
# 1. What does it mean for a flight to have a missing tailnum? What do the tail 
# numbers that don’t have a matching record in planes have in common? (Hint: one 
# variable explains ~90% of the problems.)
head(planes)
flights %>% anti_join(planes, by = 'tailnum') %>% select(tailnum, carrier) # common - letter N 
planes %>% anti_join(flights, by = 'tailnum')
# 2. Filter flights to only show flights with planes that have flown at least 100 flights
pop_flights <- flights %>% count(tailnum, sort = T) %>% head(100) 
flights %>% semi_join(pop_flights)
# 3. Combine fueleconomy::vehicles and fueleconomy::common to find only the records 
# for the most common models
install.packages("fueleconomy")
library(fueleconomy)
head(fueleconomy::vehicles)
head(fueleconomy::common)
dim(vehicles)
dim(common)
vehicles %>% semi_join(common, by = c('make' = 'make'))
vehicles %>% semi_join(common[1:3,], by = c('make' = 'make'))
# 4. Find the 48 hours (over the course of the whole year) that have the worst delays. 
# Cross-reference it with the weather data. Can you see any patterns?
worst_delay_48 <- flights %>% arrange(desc(dep_delay)) %>% head(2)
weather %>% semi_join(worst_delay_48)
# 6. You might expect that there’s an implicit relationship between plane and airline, 
# because each plane is flown by a single airline. Confirm or reject this hypothesis 
# using the tools you’ve learned in the preceding section
head(airlines)
head(planes)
flights %>% group_by(tailnum, carrier) 
%>% summarise(n_carriers = n())
f <- select(flights, tailnum, carrier)
f
f %>% group_by(tailnum, carrier) %>% summarise(n_carriers = n())
f %>% filter(tailnum == 'D942DN')
f %>% count(tailnum)
f %>% arrange(tailnum)
f %>% semi_join(airlines)
f %>% anti_join(airlines)

# Set operations
df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1,
  3,  7,
  0,  0
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2,
  3, 7
)

intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)
setdiff(df2, df1)